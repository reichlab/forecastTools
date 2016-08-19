require(dplyr)
require(tidyr)
require(stringr)

import.forecast.data <- function(submissions) {
  forecast.data <- data.frame()
  for (i in 1:length(submissions)) {
    for (j in 1:length(submissions[[i]])) {
      forecast.data <- rbind(forecast.data, expand.prediction(submissions, i, j))
    }
  }
  return(forecast.data)
}

expand.prediction <- function(submissions, i, j) {
  team <- names(submissions)[i]
  name.data <- unlist(str_split(names(submissions[[i]])[j], "[.]"))
  prediction <- as.data.frame(submissions[[i]][[j]])
  prediction$name <- rownames(prediction)
  prediction.expanded <- gather(prediction, forecast.wk, value, -name) %>% 
      mutate(
          team = team,
          season = str_replace(forecast.wk, "X(\\d{4}).(\\d{4})_wk\\d{1,2}", "\\1/\\2"),
          forecast.wk = as.numeric(str_replace(forecast.wk, ".+wk(\\d{1,2})", "\\1")),
    		  location = name.data[[2]],
    			target = name.data[[1]],
    			pred.type = ifelse(name == 'point', 'point', 'bin'),
      		bin.lwr = as.numeric(ifelse(name == "point", NA,
              str_extract(name, "\\d+"))),
     			bin.upr.strict = as.numeric(
     			    ifelse(!str_detect(name, ".+<(\\d+)\\)") & !str_detect(name, ".+=(\\d+)\\)"), NA,
     			    ifelse(target %in% c("peakinc", "seasoninc"), 
                   str_replace(name, ".+<(\\d+)\\)", "\\1"),
                   str_replace(name, ".+=(\\d+)\\)", "\\1")))),
    			bin.upr.strict = ifelse(target == "peakweek" & pred.type == "bin", 
    			    bin.upr.strict + 1, bin.upr.strict))
  return(prediction.expanded)
}

