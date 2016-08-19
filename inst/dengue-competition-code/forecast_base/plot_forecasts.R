require(stringr)
require(dplyr)

plot.forecast <- function(this.team, forecast.data, file, these.seasons, targets=NULL) {
  if (these.seasons == "train") seasons <- c("2005/2006", "2006/2007", "2007/2008", "2008/2009")
  else if (these.seasons == "test") seasons <- c("2009/2010", "2010/2011", "2011/2012", "2012/2013")
  else stop("Enter \"train\" or \"test\" for these.seasons")
  these.forecasts <- summarize.forecasts(forecast.data, this.team, ci=c(0.5, 0.95), seasons)
  pdf(file=file, width=8.5, height=11, 
  		onefile=T, paper='letter')
  for (this.target in c("peakweek", "peakinc", "seasoninc")) {
  	par(mfrow=c(4, 2), oma=c(2, 2, 6, 2), mar=c(3, 3, 2, 1), mgp=c(1.5, 0.5, 0))
  	plot.num <- 1
  	for (this.season in seasons) {
  		for (this.location in c("iquitos", "sanjuan")) {
  			this.forecast <- filter(these.forecasts, team == this.team & target == this.target & 
  			   season == this.season & location == this.location)
  			# check for missing predictions; if missing, skip 
  			if (dim(this.forecast)[1] == 0 | all(is.na(this.forecast[ , c("point", "2.5%", "97.5%")]))) {
  				plot(seq(0, 48, by=4), seq(0, 48, by=4), type='n', axes=F, 
  				     main=this.season, xlab="forecast week", ylab='')
  			  axis(1, at=seq(0, 48, by=4))
  			} else {
    			# if there are any upper bounds of Inf, modify to y max
    			if (any(this.forecast[ , c("75%", "97.5%")] == Inf, na.rm=T)) {
    				y.max <- range(as.matrix(this.forecast[ , c("point", "97.5%")]), finite=T)[2]
    				this.forecast[this.forecast[ , "75%"] == Inf, "75%"] <- y.max
    				this.forecast[this.forecast[ , "97.5%"] == Inf, "97.5%"] <- y.max
    			}
    			plot(this.forecast[ , c("forecast.wk", "point")], 
    					 ylim=range(this.forecast[ , c("point", "2.5%", "97.5%")], na.rm=T),
    					 main=this.season, xlab="forecast week", ylab="forecast", axes=F)
    			polygon.band(this.forecast$forecast.wk, this.forecast[ , "2.5%"], this.forecast[ , "97.5%"], 
    					col=adjustcolor("black", alpha=0.25))
    			polygon.band(this.forecast$forecast.wk, this.forecast[ , "25%"], this.forecast[ , "75%"],
    					col=adjustcolor("black", alpha=0.25))
    			lines(this.forecast[ , c("forecast.wk", "point")])
    			if (!is.null(targets)) {
    			  abline(h=targets[targets$target == this.target & targets$season == this.season & 
                targets$location == this.location, "observation"], lty=3, lwd=2)
    			}
    			axis(1, at=seq(0, 48, by=4))
    			axis(2)
  			}
  			if (plot.num == 1) {
  				if (this.target == "peakweek") mtext("Peak Weak Forecasts", line=1, outer=T)
  				if (this.target == "peakinc") mtext("Peak Incidence Forecasts", line=1, outer=T)
  				if (this.target == "seasoninc") mtext("Total Season Incidence Forecasts", line=1, outer=T)
  				mtext("Iquitos", line=2)
  			}
  			if (plot.num == 2) mtext("San Juan", line=2)
  			plot.num <- plot.num + 1
  		}
  	}
  }
  dev.off()
}

summarize.forecasts <- function(forecast.data, this.team, ci=0.95, seasons=NULL) {
  these.preds <- filter(forecast.data, team == this.team)
  if (!is.null(seasons)) these.preds <- filter(these.preds, season %in% seasons)
  these.forecasts <- data.frame()
  for (this.target in levels(as.factor(these.preds$target))) {
	  for (this.season in levels(as.factor(these.preds$season))) {
	    for (this.location in levels(as.factor(these.preds$location))) {
	      for (this.wk in levels(as.factor(these.preds$forecast.wk))) {
    	    this.point <- filter(these.preds, target == this.target & season == this.season & 
              location == this.location & forecast.wk == this.wk & pred.type == "point")[ , "value"]
  	      this.forecast <- filter(these.preds, target == this.target & season == this.season & 
    	        location == this.location & forecast.wk == this.wk & pred.type == "bin") %>% 
              find.bin.intervals(ci) %>%
        	    mutate(
        	        team=this.team, 
                  target=this.target,
        	        location=this.location,
                  season=this.season, 
                  forecast.wk=this.wk,
                  point=this.point)
    	    these.forecasts <- rbind(these.forecasts, this.forecast)
	      }
	    }
	  }
  }
  return(these.forecasts)
}

find.bin.intervals <- function(preds, ci) {
  forecast <- numeric()
  preds <- arrange(preds, bin.lwr)
  for (this.ci in ci) {
    p.lwr <- (1 - this.ci)/2
    p.upr <- 1 - (1 - this.ci)/2
    forecast[paste0(100*p.lwr, "%")] <- 
        preds$bin.lwr[min(which(cumsum(preds$value) >= p.lwr))]
    forecast[paste0(100*p.upr, "%")] <- 
        preds$bin.upr[min(which(cumsum(preds$value) >= p.upr))]
    if (is.na(forecast[paste0(100*p.upr, "%")])) forecast[paste0(100*p.upr, "%")] <- Inf
    if (forecast[paste0(100*p.lwr, "%")] == forecast[paste0(100*p.upr, "%")]) {
      forecast[paste0(100*p.upr, "%")] <- 
          preds$bin.upr.strict[min(which(cumsum(preds$value) >= p.upr))]
    }
  }
  return(as.data.frame(t(forecast)))
}
	
polygon.band <- function(x, lower, upper, ...) {
	polygon(c(x, rev(x)), c(lower, rev(upper)), border=F, ...)
}

