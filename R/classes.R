#' An S4 class to represent a a ``single'' forecast, 
#' meaning a set of data that provides information 
#' about a forecast made at a single point in time. 
#' The single forecast could contain predictions of multiple time-points.
#'
#' @slot forecast_date the date the forecast was made
#' @slot time_unit the time unit of the forecasts
#' @slot forecast_data a data.frame containing the predictions
forecast <- setClass("forecast",
                     slots = c(forecast_date = "Date",
                               time_unit = "character",
                               forecast_data = "data.frame"),
                     prototype = list(
                         forecast_date = Sys.Date(),
                         time_unit = character(0),
                         forecast_data = data.frame(target=NA, value=NA, pred_type=NA)
                     ))


#'method for plotting a single target from a forecast object
#'
#'
#'@param dat the forecast object
#'@param target the desired target to plot, should match label in forecast$target
#'@param ... additional parameters to the plot functions.

setGeneric("plot", function(obj, target, ...) standardGeneric("plot"))
setMethod("plot",
          "forecast",
          function(obj, target, ...) {
              require(dplyr)
              require(ggplot2)
              tgt <- target
              dat <- filter(obj@forecast_data, target==tgt)
              p <- ggplot(dat) + 
                  geom_rect(aes(xmin=bin_lwr, xmax=bin_upr_strict, ymin=0, ymax=value)) +
                  geom_vline(aes(xintercept = as.numeric(dat[dat$pred_type=="point", "value"])), color="red") +
                  scale_y_continuous("probability", limits=c(0,1)) +
                  scale_x_continuous(name=target)
              print(p)
          }
)
              
              


setValidity("forecast", function(object) {
    msg <- NULL
    valid <- TRUE
    
    ## forecast_data must have certain colnames
    required_colnames <- c("value", "target", "pred_type")
    if(!all(required_colnames %in% colnames(object@forecast_data))) {
        msg <- c(msg,
                 "Some required column names are missing.")
     }
})


