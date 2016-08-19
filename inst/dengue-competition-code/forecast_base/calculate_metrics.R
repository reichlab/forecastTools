require(dplyr)

calculate.metrics <- function(forecast.data, targets) {
  ### add obervations
  forecast.data <- inner_join(forecast.data, targets)
  ### create score table
  forecast.scores <- select(forecast.data, team, season, location, target, forecast.wk) %>% 
      distinct()
  ### add point predictions
  forecast.scores <- inner_join(forecast.scores, filter(forecast.data, pred.type == "point")) %>%
      rename(point.pred=value) %>%
      select(team, season, location, target, forecast.wk, observation, point.pred)
  ### calculate absolute errors
  forecast.scores <- mutate(forecast.scores, ae=abs(observation - point.pred))
  ### calculate log scores
  for (i in 1:dim(forecast.scores)[1]) {
    forecast.scores$log.score[i] <- 
        filter(forecast.data, team == forecast.scores$team[i] & 
            season == forecast.scores$season[i] & 
            location == forecast.scores$location[i] & 
            target == forecast.scores$target[i] & 
            forecast.wk == forecast.scores$forecast.wk[i]) %>%
        filter(pred.type != "point") %>%
        calculate.log.score()
  }
  return(forecast.scores)
}


calculate.log.score <- function(these.probs) {
  if (is.na(these.probs$observation[1])) return(NA)
  these.probs <- arrange(these.probs, bin.lwr)
  correct.bin <- max(which(these.probs$bin.lwr <= these.probs$observation))
  return(log(these.probs$value[correct.bin]))
}

