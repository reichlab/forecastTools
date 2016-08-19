library(forecastTools)


## read in and process forecast from CDC website
preds <- read_flu_forecast("https://raw.githubusercontent.com/reichlab/ssr-influenza-competition/master/inst/submissions/EW02-KoT-2016-01-25.csv")

## store as new forecast object
fcast <- forecast(forecast_date = Sys.Date(),
                     time_unit = "week",
                     forecast_data = preds$preds)


## plot forecast
plot(fcast, target="season_onset")


## TODOs below here 

## load a set of forecasts into a single object

## load reference data

## make plot comparing reference data to a single prediction

## make plot comparing reference data to multiple predictions