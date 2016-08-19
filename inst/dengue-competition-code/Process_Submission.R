
setwd('inst/processing-code')

library(stringr)
library(dplyr)
library(tidyr)

submission.dir <- "forecast_submissions"
template.dir <- "templates"
targets <- read.csv("dengue_targets.csv", stringsAsFactors=F)

##############################################################
### import forecast and template files
source("forecast_base/import_forecasts.R")
### submissions - reads all csv files in this directory
submissions <- import.forecasts(submission.dir)

### templates
templates <- import.forecasts(template.dir)[["template"]]

##############################################################
### verify forecasts
source("forecast_base/verify_forecasts.R")
submissions <- verify.subs(submissions, templates)

##############################################################
### import forecast data
source("forecast_base/import_forecast_data.R")
my.forecast.data <- import.forecast.data(submissions)

### save as csv file
write.csv(my.forecast.data, file="my_forecast_data.csv")

### save as R object
save(my.forecast.data, file="my_forecast_data.RData")

##############################################################
### plot forecasts
source("forecast_base/plot_forecasts.R")
### generate a pdf of forecast plots, must specify team and set of 4 seasons
this.team <- "baseline"
these.seasons <- "test" # must specify test or train
plot.forecast(this.team, my.forecast.data, 
    paste0("timeseries_plots_", these.seasons, ".pdf"), 
    these.seasons, targets)

### get approximate (binned) intervals on distribution
#forecast.intervals <- summarize.forecasts(forecast.data, this.team, ci=c(0.5, 0.95))

##############################################################
### calculate scores (AE and log score)
source("forecast_base/calculate_metrics.R")
my.forecast.scores <- calculate.metrics(my.forecast.data, targets)

### save as csv file
write.csv(my.forecast.scores, file="my_forecast_scores.csv")

### save as R object
save(my.forecast.scores, file="my_forecast_scores.RData")

##############################################################
### plot log scores
source("forecast_base/plot_log_score.R")
this.team <- "baseline"
plot.log.score.team(filter(my.forecast.scores, team == this.team), 
    paste0("logscore_plots.pdf"))

### plot log scores compared to other teams
load("forecast_scores.Rdata")
plot.log.score.team(filter(my.forecast.scores, team == this.team), 
    paste0("logscore_plots_relative.pdf"), forecast.scores)




