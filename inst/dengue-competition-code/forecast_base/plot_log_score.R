plot.log.score.team <- function(these.forecast.scores, file, other.forecast.scores=NULL) {
  pdf(file=file, width=8.5, height=11, 
  		onefile=T, paper='letter')
	par(mfrow=c(6, 2), oma=c(2, 4, 5, 4), mar=c(3, 2, 0.5, 1), mgp=c(1.5, 0.5, 0))
	plot.num <- 1
	for (this.target in c("peakweek", "peakinc", "seasoninc")) {
		for (this.location in c("iquitos", "sanjuan")) {
			for (these.seasons in c("train", "test")) {
        if (these.seasons == "train") {
          seasons <- c("2005/2006", "2006/2007", "2007/2008", "2008/2009")
          this.color <- 'black'
        } else {
          seasons <- c("2009/2010", "2010/2011", "2011/2012", "2012/2013")
				  this.color <- 'red'
        }
				plot(seq(0, 48, by=4), seq(0, 48, by=4), type='n', ylim=c(-6, 0), axes=F,
						xlab="forecast week", ylab="log score", lwd=1.5, cex.lab=0.75)
				for (this.team in levels(as.factor(other.forecast.scores$team))) {
					lines(filter(other.forecast.scores, team == this.team & 
              target == this.target & location == this.location & season %in% seasons) %>% 
              group_by(forecast.wk) %>% summarize(mean(log.score, na.rm=T)),
							col=adjustcolor(this.color, 0.25), lwd=1.5)
				}
				# plot selected team
				lines(filter(these.forecast.scores, target == this.target & 
				    location == this.location & season %in% seasons) %>% 
				    group_by(forecast.wk) %>% summarize(mean(log.score, na.rm=T)),
						col=this.color, lwd=1.5)
				axis(1, at=seq(0, 48, by=4))
				axis(2)
				box(bty='l')
				if (plot.num %in% c(1, 5, 9)) {
					mtext("Iquitos", side=2, line=2)
				} else if (plot.num %in% c(3, 7, 11)) {
					if (this.target == "peakweek") mtext("Peak Weak Forecasts", side=2, line=4, adj=0)
					if (this.target == "peakinc") mtext("Peak Incidence Forecasts", side=2, line=4, adj=0)
					if (this.target == "seasoninc") mtext("Total Season Incidence Forecasts", side=2, line=4, adj=0)
					mtext("San Juan", side=2, line=2)
				}
				if (plot.num == 1) mtext("Training", side=3, line=2)
				if (plot.num == 2) mtext("Testing", side=3, line=2)
				plot.num <- plot.num + 1
			}
		}
	}
	mtext("Mean Log Scores", line=3, outer=T)
	dev.off()
}

