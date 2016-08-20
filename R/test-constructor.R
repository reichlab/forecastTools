library(forecastTools)


## read in and process forecast from CDC website
preds <- read_flu_forecast("https://raw.githubusercontent.com/reichlab/ssr-influenza-competition/master/inst/submissions/EW02-KoT-2016-01-25.csv")

## plot forecast
plot(preds, target="season_onset")
plot(preds, target="peak_week")


## TODOs below here 

## load a set of forecasts into a single object

## load reference data

## make plot comparing reference data to a single prediction

## make plot comparing reference data to multiple predictions