# Import libraries for Shiny App
library(shiny)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)

# Import libraries for data and modeling
library(fpp3)
library(dplyr)
library(vars)
library(corrplot)
library(WaveletANN)

# Load in Albany, NY climate data
data <- read.csv("hourly_data.csv")

# Create new date column with proper format
data$date <- strptime(data$DATE, format = "%Y-%m-%dT%H:%M:%S", tz = "EST")

# Convert to tsibble
df <- data %>%
  as_tsibble(index = date)

# Check to make sure that are no missing values
df[rowSums(is.na(df)) > 0, ]

# Check to see if df dates are regular
is.regular(df)

# Save data as POSIXct for ts
df <- df %>%
  mutate(Date = as.POSIXct(date))

# Create a hourly column
df$Date <- floor_date(df$Date, "hour")

# Specify hourly data
df <- df %>%
  as_tsibble(index = Date)

# Subset data with needed columns
df <- subset(df, select = -c(
  STATION, DATE, date, REPORT_TYPE, SOURCE,
  BackupElements, BackupElevation, BackupEquipment,
  BackupLatitude, BackupLongitude, BackupName,
  WindEquipmentChangeDate, HourlyPrecipitation
))

# Save data as numeric
cols.num <- colnames(df)
cols.num <- cols.num[-11]
df[cols.num] <- sapply(df[cols.num], as.numeric)

# Check gaps
df <- df %>%
  update_tsibble(regular = TRUE) %>%
  fill_gaps()

# Fill gaps with a straight line for rough estimation
df <- df %>%
  mutate(across(-Date, ~ na.approx(.x, na.rm = FALSE, maxgap = 40)))

# Split data into train and test/holdout
set.seed(475)
holdout <- tail(df, 168)
train <- df[!(df$Date %in% holdout$Date), ]

# Fit simple models
# fit_simp <- train %>%
#   model(
#     mean = MEAN(HourlyRelativeHumidity),
#     naive = NAIVE(HourlyRelativeHumidity),
#     snaive = SNAIVE(HourlyRelativeHumidity),
#     drift = NAIVE(HourlyRelativeHumidity ~ drift())
#   )
# saveRDS(fit_simp, "fit_simp.rds")
fit_simp <- readRDS("fit_simp.rds")

# Fit ets models
# fit_ets <- train %>%
#   model(
#     holtwint = ETS(HourlyRelativeHumidity ~ error("A") + trend("A") + season("A")),
#     holt = ETS(HourlyRelativeHumidity ~ error("A") + trend("A") + season("N")),
#     manualets = ETS(HourlyRelativeHumidity ~ error("M") + trend("A") + season("A"))
#   )
# saveRDS(fit_ets, "fit_ets.rds")
fit_ets <- readRDS("fit_ets.rds")

# Fit arima models
# fit_arima <- train %>%
#   model(
#     auto = ARIMA(HourlyRelativeHumidity), #14.2 and 202,210
#     manual = ARIMA(HourlyRelativeHumidity ~ pdq(1,0,0) + PDQ(1,1,0)),
#   )
# saveRDS(fit_arima, "fit_arima.rds")
fit_arima <- readRDS("fit_arima.rds")

# Fit a dynamic hermonic regression (using Fourier Terms for different seasonality)
# fit_dhr <- train %>%
#   model(
#     dhr = ARIMA(HourlyRelativeHumidity ~ PDQ(0, 0, 0) + pdq(d=0) +
#                   fourier(period = 24, K = 6) +
#                   fourier(period = 24*7, K = 4) +
#                   fourier(period = 24*7*52, K = 2)) #12.2 *
#   )
# saveRDS(fit_dhr, "fit_dhr.rds")
fit_dhr <- readRDS("fit_dhr.rds")

# STL model
# stl_dcmp <- decomposition_model(
#   STL(HourlyRelativeHumidity ~ season(period = 24) +
#         season(period = 24*7) +
#         season(period = 24*7*28) +
#         season(period = 24*7*52)),
#   ETS(season_adjust ~ season("N")) #14 *
# )
# saveRDS(stl_dcmp, "stl_dcmp.rds")
stl_dcmp <- readRDS("stl_dcmp.rds")

# Wavelet ANN
# WaveletForecastANN<- WaveletFittingann(
#   ts=train$HourlyRelativeHumidity,
#   Waveletlevels=3,
#   Filter='haar',
#   hidden=3,
#   NForecast = 168 #5.02, overfit
# )
# saveRDS(WaveletForecastANN, "WaveletForecastANN.rds")
WaveletForecastANN <- readRDS("WaveletForecastANN.rds")

###############################################################################
# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Hourly Humidity"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("house")),
    menuItem("Simple Forecast", icon = icon("chart-simple"), tabName = "simple"),
    menuItem("ETS Forecast", icon = icon("chart-line"), tabName = "ets"),
    menuItem("ARIMA Forecast", icon = icon("arrow-trend-up"), tabName = "arima"),
    menuItem("Complex Forecast", icon = icon("robot"), tabName = "complex"),
    menuItem("Wavelet Forecast", icon = icon("wave-square"), tabName = "wave"),
    menuItem("Final Forecast", icon = icon("book"), tabName = "final")
  )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        h3("Description"),
        box(h4("We want to forecast the relative hourly humidity in Albany, NY.  Let's look at the data before modeling!"), width = 8),
        h2(""),
        prettySwitch(
          inputId = "hourly",
          label = "Want to see Humidity Overtime?! (Click Slider)",
          status = "primary",
          slim = TRUE
        ),
        h3(""),
        uiOutput(outputId = "generalplot") %>%
          withSpinner(),
        h3(""),
        uiOutput(outputId = "generaltxt"),
        prettyRadioButtons(
          inputId = "selected",
          label = "Select a graph", # User chooses graph
          choices = c(
            "Autocorrelation",
            "Partial Correlation",
            "Seasonality",
            "Decomposition"
          ),
          icon = icon("check"),
          bigger = TRUE,
          # status = "info",
          animation = "jelly"
        ),
        plotlyOutput(outputId = "choseplt") %>%
          withSpinner(), # Display chosen graph
        h3(""),
        p(uiOutput(outputId = "chosetxt"),
          style = "text-align:left; color:black"
        )
      ),
      tabItem(
        tabName = "simple",
        h3("Simple Models"),
        h4(""),
        box(h4("Since there is a lot of data, it is difficult to visualize these forecast with the historical data. So, this app shows how each forecast model fits the data that the model has not seen yet."), width = 8),
        prettyRadioButtons(
          inputId = "simple_selected",
          label = "Choose your Simple Model!",
          choices = c(
            "Mean",
            "Naive",
            "Seasonal Naive",
            "Drift"
          ),
          selected = "Mean",
          icon = icon("check"),
          # status = "info",
          animation = "tada"
        ),
        plotOutput(outputId = "simpleplt") %>%
          withSpinner(),
        h3(""),
        p(uiOutput(outputId = "simptxt"),
          style = "text-align:left; color:black"
        ),
        h3(""),
        prettySwitch(
          inputId = "rmse_simp",
          label = "Click me to see the Accuracy Table for the Simple Models!",
          status = "primary",
          slim = TRUE
        ),
        tableOutput("table_simp")
      ),
      tabItem(
        tabName = "ets",
        h3("Exponential Smoothing Models"),
        h4(""),
        box(h4("Now that we have seen how the simple models work, let's try some ETS (Error/Trend/Season) models to see if the results are better!"), width = 8),
        prettyRadioButtons(
          inputId = "ets_selected",
          label = "Choose your ETS Model!",
          choices = c(
            "Holt",
            "Holt-Winters Additive",
            "Manual Holt-Winters"
          ),
          selected = "Holt",
          icon = icon("check"),
          # status = "info",
          animation = "tada"
        ),
        plotOutput(outputId = "etsplt") %>%
          withSpinner(),
        h3(""),
        p(uiOutput(outputId = "etstxt"),
          style = "text-align:left; color:black"
        ),
        h3(""),
        prettySwitch(
          inputId = "rmse_ets",
          label = "Click me to see the Accuracy Table for the ETS Models!",
          status = "primary",
          slim = TRUE
        ),
        tableOutput("table_ets")
      ),
      tabItem(
        tabName = "arima",
        h3("Autoregressive Integrated Moving Average Models"),
        h4(""),
        box(h4("Some of the ETS models seemed to work fairly well, let's look at some ARIMA models to see if we can get a better forecast!"), width = 8),
        prettyRadioButtons(
          inputId = "arima_selected",
          label = "Choose your ARIMA Model!",
          choices = c(
            "Auto",
            "Manual"
          ),
          selected = "Auto",
          icon = icon("check"),
          # status = "info",
          animation = "tada"
        ),
        plotOutput(outputId = "arimaplt") %>%
          withSpinner(),
        h3(""),
        p(uiOutput(outputId = "arimatxt"),
          style = "text-align:left; color:black"
        ),
        h3(""),
        prettySwitch(
          inputId = "rmse_arima",
          label = "Click me to see the Accuracy Table for the ARIMA Models!",
          status = "primary",
          slim = TRUE
        ),
        tableOutput("table_arima")
      ),
      tabItem(
        tabName = "complex",
        h3("Complex Seasonality Models"),
        h4(""),
        box(h4("The issue with previous models is that we were assuming there was only one seasonal componenet, but what if there are mulitple? Using these complex seasonality models we should be able to capture daily, weekly, monthly, and yearly effects!"), width = 8),
        prettyRadioButtons(
          inputId = "complex_selected",
          label = "Choose your Complex Model!",
          choices = c(
            "STL Decomposition",
            "Dynamic Harmonic Regression"
          ),
          selected = "STL Decomposition",
          icon = icon("check"),
          # status = "info",
          animation = "tada"
        ),
        plotOutput(outputId = "complexplt") %>%
          withSpinner(),
        h3(""),
        p(uiOutput(outputId = "complextxt"),
          style = "text-align:left; color:black"
        ),
        h3("")
      ),
      tabItem(
        tabName = "wave",
        h3("Wavelet Forecasting"),
        box(h4("Wavelets is a similar technique to Fourier terms that use sin and cos waves to pick up on frequencies. However, Fourier terms require the frequency to be the same across time, whereas wavelets can change across time. Thus wavelets are theorticially better at capturing randomness or different changes."), width = 8),
        h3(""),
        prettyRadioButtons(
          inputId = "wavelet_selected",
          label = "Choose the comparison for this Wavelet ANN Model!",
          choices = c(
            "Forecast",
            "Historic"
          ),
          selected = "Forecast",
          icon = icon("check"),
          # status = "info",
          animation = "tada"
        ),
        plotOutput(outputId = "waveplt") %>%
          withSpinner(),
        p(uiOutput(outputId = "wavetxt"),
          style = "text-align:left; color:black"
        ),
        h3(""),
        prettySwitch(
          inputId = "wavelets",
          label = "Click me to see the 15 Wavelets for this ANN Model!",
          status = "primary",
          slim = TRUE
        ),
        plotOutput(outputId = "waveletsplt") %>%
          withSpinner()
      ),
      
      tabItem(
        tabName = "final",
        box(h4("For the final forecast, I chose the STL Decomposition model. Despite having a higher RMSE than some models, it appears to fit the data the best. So, here is the final forecast!")),
        h3(""),
        prettySwitch(
          inputId = "finalchoice",
          label = "Click me to see the Forecast with the Entire Series!",
          status = "primary",
          slim = TRUE
        ),
        h3(""),
        plotOutput(outputId = "finalplt") %>%
          withSpinner()
      )
      
    )
  )
)


# shinyWidgets::shinyWidgetsGallery()

# Server
server <- function(input, output) {
  output$generalplot <- renderUI({
    if (input$hourly == TRUE) {
      general <- autoplot(df, HourlyRelativeHumidity) +
        ggtitle("Hourly Humidity in Albany") +
        ylab("Relative Humidity") +
        xlab("Time (Hour)") +
        theme_classic()

      ggplotly(general) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$hourly == FALSE) {
      h3("")
    }
  })

  output$generaltxt <- renderUI({
    if (input$hourly == TRUE) {
      tags$ul(
        tags$li("Looking at hourly relative humidity, we can see that the data
                appears somewhat stationary"),
        tags$li("There is little trend, and there might be some strong seasonality"),
        tags$li("It is difficult to tell from this plot because of how much data
                is being displayed at one time"),
        tags$li("Let's look at other graphs to help us out!")
      )
    } else if (input$hourly == FALSE) {
      h3("")
    }
  })

  output$choseplt <- renderPlotly({
    if (input$selected == "Autocorrelation") {
      p1 <- autoplot(ACF(df, HourlyRelativeHumidity)) +
        ggtitle("ACF Plot") +
        ylab("ACF") +
        xlab("Lag (1 Hour)") +
        theme_classic()

      ggplotly(p1) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected == "Partial Correlation") {
      p2 <- autoplot(PACF(df, HourlyRelativeHumidity)) +
        ggtitle("PACF Plot") +
        ylab("PACF") +
        xlab("Lag (1 Hour)") +
        theme_classic()

      ggplotly(p2) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected == "Seasonality") {
      p3 <- gg_season(df, HourlyRelativeHumidity) +
        ggtitle("Humidity Seasonality Plot") +
        ylab("Hits") +
        theme_classic()

      ggplotly(p3) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected == "Decomposition") {
      dcmp_ec <- df %>%
        model(classical_decomposition(HourlyRelativeHumidity, type = "additive")) %>%
        components()

      p4 <- autoplot(dcmp_ec) +
        theme_bw()

      ggplotly(p4) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    }
  })

  output$chosetxt <- renderUI({
    if (input$selected == "Autocorrelation") {
      tags$ul(
        tags$li("From the ACF plot we can see what lags (previous values) are highly correlated with the most recent values"),
        tags$li("We can see that there is a sinusoidal trend as the ACF bars are generally decreasing, but form a
                wave like pattern"),
        tags$li("From this we can infer that there is some seasonality every 24 hours and that
                humidity is highly correlated with recent values"),
        tags$li("Since all of the bars are above the threshold, then we can guess
                that the q and Q components of an ARIMA model are 0")
      )
    } else if (input$selected == "Partial Correlation") {
      tags$ul(
        tags$li("From the PACF plot we can see how much information we gain from each lag"),
        tags$li("We can see that there is a sinusoidal trend that is similar to the ACF plot"),
        tags$li("From this we can infer that there is some seasonality every 24 hours and that
                humidity is highly correlated with the most recent value"),
        tags$li("Since there is a large spike at lag 1 bars and 24 hours, we can guess
                that the p and P components of an ARIMA model are 1")
      )
    } else if (input$selected == "Seasonality") {
      tags$ul(
        tags$li("This seasonality graph indicates that there has been a general increase in relative humidity over time"),
        tags$li("The earliest years tend to be at the bottom of the graph, meaning that they had a lower humidity score"),
        tags$li("Granted because of the amount of data we have, it is hard to see yearly, monthly, and daily trends"),
        tags$li("From the previous plots, we can guess that we would have seasonally difference at least once (D = 1) for
                an ARIMA model")
      )
    } else if (input$selected == "Decomposition") {
      tags$ul(
        tags$li("For the Classical Decomposition model, I chose an additive model instead of multiplicative"),
        tags$li("Since the cycles appeared to be somewhat consistent (no monotonic change in the variance of the cyles),
                      I did not see a need for a Box Cox transformation or multiplicative model"),
        tags$li("From the decomposition itself, we see a somewhat constant trend and additive seasonality, which is what we
      expected from looking at the original time series and other models"),
        tags$li("Note how the trend is cyclical, which may indicate there are multiple seasonal trends that this is not capturing"),
        tags$li("Lastly, the random residuals look similar to white noise and cover a fairly small range"),
        tags$li("This indicates that relative humidity in Albany can largely be accounted for by its trend and seasonality components")
      )
    }
  })

  output$simpleplt <- renderPlot({
    if (input$simple_selected == "Mean") {
      fit_simp %>%
        dplyr::select(mean) %>%
        forecast(h = 168) %>%
        autoplot(holdout) +
        theme_classic()
    } else if (input$simple_selected == "Naive") {
      fit_simp %>%
        dplyr::select(naive) %>%
        forecast(h = 168) %>%
        autoplot(holdout) +
        theme_classic()
    } else if (input$simple_selected == "Seasonal Naive") {
      fit_simp %>%
        dplyr::select(snaive) %>%
        forecast(h = 168) %>%
        autoplot(holdout) +
        theme_classic()
    } else if (input$simple_selected == "Drift") {
      fit_simp %>%
        dplyr::select(drift) %>%
        forecast(h = 168) %>%
        autoplot(holdout) +
        theme_classic()
    }
  })

  output$table_simp <- renderTable({
    if (input$rmse_simp == TRUE) {
      fit_simp %>%
        forecast(h = 168, times = 0) %>%
        accuracy(holdout) %>%
        arrange(RMSE)
    }
  })

  output$simptxt <- renderUI({
    if (input$simple_selected == "Mean") {
      tags$ul(
        tags$li("A mean model takes the average of all of the historical data and fits a straight line"),
        tags$li("Comparing our mean model to the holdout data that our model has not seen, it doesn't fit
                that well, as it misses most of the cylical pattern going on"),
        tags$li("Our predictions on average are 17.2 off of the true value, I believe we can do better")
      )
    } else if (input$simple_selected == "Naive") {
      tags$ul(
        tags$li("A naive model looks at the last data point in the historical data and assumes it will be that value forever"),
        tags$li("Comparing our naive model to the holdout data, we can see it doesn't fit well and also misses
                most of the cyclical pattern going on"),
        tags$li("Our predictions on average are 16.8 off of the true value")
      )
    } else if (input$simple_selected == "Seasonal Naive") {
      tags$ul(
        tags$li("A seasonal naive model is similar to a naive model, but it uses the last data point of every season period will be that value for all future seasons"),
        tags$li("So in this case, it assumes the most recent humidity at 2 pm will be the humidity at 2 pm every day in the future"),
        tags$li("Comparing our seasonal naive model to the holdout data, it looks signifcantly better as we are now capturing the underlying
                cylical patterns in the data"),
        tags$li("Our predictions on average are 15.4 off of the true value")
      )
    } else if (input$simple_selected == "Drift") {
      tags$ul(
        tags$li("A drift model looks at the historical data and draws a straight line from the first observation to the last"),
        tags$li("Comparing our naive model to the holdout data, we can see it doesn't fit well and also misses
                most of the cyclical pattern going on"),
        tags$li("Our predictions on average are 16.8 off of the true value")
      )
    }
  })

  output$etsplt <- renderPlot({
    if (input$ets_selected == "Holt") {
      fit_ets %>%
        dplyr::select(holt) %>%
        forecast(h = 168) %>%
        autoplot(holdout) +
        theme_classic()
    } else if (input$ets_selected == "Holt-Winters Additive") {
      fit_ets %>%
        dplyr::select(holtwint) %>%
        forecast(h = 168) %>%
        autoplot(holdout) +
        theme_classic()
    } else if (input$ets_selected == "Manual Holt-Winters") {
      fit_ets %>%
        dplyr::select(manualets) %>%
        forecast(h = 168) %>%
        autoplot(holdout) +
        theme_classic()
    }
  })

  output$table_ets <- renderTable({
    if (input$rmse_ets == TRUE) {
      fit_ets %>%
        forecast(h = 168, times = 0) %>%
        accuracy(holdout) %>%
        arrange(RMSE)
    }
  })

  output$etstxt <- renderUI({
    if (input$ets_selected == "Holt") {
      tags$ul(
        tags$li("A Holt ETS model only accounts for trend and level of the forecast, in other words it is predicts a line"),
        tags$li("This forecast is unbounded, so it countinues to go straight on forever, surpassing the maximum scale of 100"),
        tags$li("Furthermore our predictions on average are 133.77 off of the true value, which is more than the spread of the scale of our data"),
        tags$li("Note that this forecast could have been capped at 100, and the RMSE would be less, but it would still be a bad overall fit")
      )
    } else if (input$ets_selected == "Holt-Winters Additive") {
      tags$ul(
        tags$li("A Holt-Winters ETS model accounts for trend, level, and seasonality"),
        tags$li("Since there is a heavy seasonal component in our data this should be a better forecast than Holt"),
        tags$li("For this model, we assumed that the errors, seasonality, and trend were all additive and not multiplicative"),
        tags$li("Our predictions on average are 18.52 off of the true value")
      )
    } else if (input$ets_selected == "Manual Holt-Winters") {
      tags$ul(
        tags$li("This is similar to the last model, but I thought there could have been mulitplicative erorr, as the variance changes a lot overtime"),
        tags$li("Comparing it to the other models, this has theb est fit and looks the closest to the holdout set!"),
        tags$li("Our predictions on average are 14.34 off of the true value")
      )
    }
  })


  output$arimaplt <- renderPlot({
    if (input$arima_selected == "Auto") {
      fit_arima %>%
        dplyr::select(auto) %>%
        forecast(h = 168) %>%
        autoplot(holdout) +
        theme_classic()
    } else if (input$arima_selected == "Manual") {
      fit_arima %>%
        dplyr::select(manual) %>%
        forecast(h = 168) %>%
        autoplot(holdout) +
        theme_classic()
    }
  })

  output$table_arima <- renderTable({
    if (input$rmse_arima == TRUE) {
      fit_arima %>%
        forecast(h = 168, times = 0) %>%
        accuracy(holdout) %>%
        arrange(RMSE)
    }
  })

  output$arimatxt <- renderUI({
    if (input$arima_selected == "Auto") {
      tags$ul(
        tags$li("An ARIMA model uses lags to predictions"),
        tags$li("ARIMA model also needs the data to be stable, where the mean, variance, trend, and seasonality is constant, which is done by the pdq and PDQ parameters"),
        tags$li("The auto ARIMA model chose the following pdq and PDQ parameters: pdq(2,0,2) and PDQ(2,1,0)"),
        tags$li("This is better than our manual selection of (1,0,0) and (1,1,0)"),
        tags$li("It is difficult to manually select ARIMA parameters for high frequency data such as hourly"),
        tags$li("Our predictions on average are 14.17 off of the true value, which is one of our best models so far")
      )
    } else if (input$arima_selected == "Manual") {
      tags$ul(
        tags$li("As stated above, I chose a pdq(1,0,0) and PDQ(1,1,0)"),
        tags$li("This results in a worse model than the auto ARIMA model as it is not capturing the magnitudes of the cycles"),
        tags$li("Our predictions on average are 16.12 off of the true value")
      )
    }
  })


  output$complexplt <- renderPlot({
    if (input$complex_selected == "STL Decomposition") {
      fc <- train %>%
        model(stl_dcmp) %>%
        forecast(h = 168)
      fc %>%
        autoplot(holdout) + theme_classic()
    } else if (input$complex_selected == "Dynamic Harmonic Regression") {
      fit_dhr %>%
        forecast(h = 168) %>%
        autoplot(holdout) +
        theme_classic()
    }
  })

  output$complextxt <- renderUI({
    if (input$complex_selected == "STL Decomposition") {
      tags$ul(
        tags$li("A STL model is well equipped to deal with multiple seasonal components"),
        tags$li("It uses a seasonal naïve method for each seasonal component, and then uses the seasonally adjusted data to forecast using ETS to pick up on trend and randomness"),
        tags$li("So the seasonal components that I tried to capture were day, week, month, and year effects"),
        tags$li("Our predictions on average are 14 off of the true value, and visually this appears to be the best model")
      )
    } else if (input$complex_selected == "Dynamic Harmonic Regression") {
      tags$ul(
        tags$li("A Dynamic Harmonic Regression model uses the ARIMA model as its basis"),
        tags$li("Instead of letting the PDQ seasonal parameters be chosen using AIC, it uses Fourier terms (sin and cos waves) to parse out the seasonal component"),
        tags$li("Thus the PDQ of this model is technically PDQ(0,0,0)"),
        tags$li("Similar to the STL Decomposition, exogenous Fourier terms were used to capture day, week, and year seasonal effects"),
        tags$li("Our predictions on average are 12.2 off of the true value, and theoretically this is the best model")
      )
    }
  })

  output$waveletsplt <- renderPlot(
    {
      if (input$wavelets == TRUE) {
        # Display the 15 wavelets
        wavelets <- WaveletFitting(
          ts = train$HourlyRelativeHumidity,
          Filter = "haar"
        )

        # Convert to wavelet tsibble
        df_wav <- as.data.frame(wavelets$WaveletSeries)
        df_wav$Date <- train$Date
        df_wav <- df_wav %>%
          as_tsibble(index = Date)

        df_wav %>%
          pivot_longer(!Date) %>%
          mutate(name = factor(name, levels = names(df_wav)[-ncol(df_wav)])) %>%
          ggplot(aes(Date, value)) +
          theme_classic() +
          geom_line() +
          facet_wrap(vars(name), scales = "free", nrow = 3, ncol = 5)
      }
    },
    bg = "transparent"
  )

  output$waveplt <- renderPlot({
    if (input$wavelet_selected == "Forecast") {
      # Look at forecast ANN
      holdout$wavefc1 <- WaveletForecastANN[["Finalforecast"]]
      ts.plot(cbind(holdout$wavefc1, holdout$HourlyRelativeHumidity),
        gpars = list(col = c("blue", "red")),
        xlab = "2-Week (Hours)",
        ylab = "Humidity",
        lwd = 2
      )
      legend("topright",
        col = c("blue", "red"), lty = c(1, 1),
        legend = c("Forecast", "Actual")
      )
    }

    if (input$wavelet_selected == "Historic") {

      # Look at training ANN
      train$wavepred1 <- WaveletForecastANN[["FinalPrediction"]]
      ts.plot(cbind(train$wavepred1, train$HourlyRelativeHumidity),
        gpars = list(col = c("blue", "red")),
        xlab = "2-Week (Hours)",
        ylab = "Humidity",
        lwd = 2
      )
      legend("topright",
        col = c("blue", "red"), lty = c(1, 1),
        legend = c("Predicted", "Actual")
      )
    }
  })
  
  output$wavetxt <- renderUI({
    if (input$wavelet_selected == "Forecast") {
      tags$ul(
        tags$li("This is a Wavelet Autoregressive Neural Net model"),
        tags$li("It breaks the data into wavelets then forecast each individual wavelet using a Autoregressive Neural Net (in this case we used 3 hidden layers"),
        tags$li("By summing up each individual wavelet forecast, we get the final forecast for our model"),
        tags$li("As you can see from the graph comparing the actual values to the forecast, it does a great job picking out the cycles but fails to capture the level or magnitude of them"),
        tags$li("There are some obvious issue with this model, and we can see this when looking at the historical data")
      )
    } else if (input$wavelet_selected == "Historic") {
      tags$ul(
        tags$li("When evaluating the historical data to the predictions from our model, we see that it does an almost perfect job"),
        tags$li("The predictions were 5 points off on average, by for the best RMSE we have"),
        tags$li("Thus, this is a case of over fitting to our training data"),
        tags$li("To help prevent this there is a couple of things we could try: Removing a hidden layer, manually developing the model instead of using the package to use cross validation, or adding more wavelets")
      )
    }
  })
  
  output$finalplt <- renderPlot({
    if (input$finalchoice == FALSE) {
  finalfc <- df %>%
    model(stl_dcmp) %>%
    forecast(h = 168)
  finalfc %>%
    autoplot(holdout) + theme_classic()}
    
    else if (input$finalchoice == TRUE){
      finalfc <- df %>%
        model(stl_dcmp) %>%
        forecast(h = 168)
      finalfc %>%
        autoplot(df) + theme_classic()
    }
  })
  
}


# Create Shiny App
shinyApp(ui, server)

