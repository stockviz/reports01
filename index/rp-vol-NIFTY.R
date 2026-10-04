#source("C:/stockviz/r/config.r")
source("/mnt/hollandr/config.r")
#source("C:/stockviz/r/plot.common.r")
source("/mnt/hollandr/plot.common.r")

library('RODBC')
library('RPostgres')
library('tidyverse')
library('cowplot')
library('reshape2')
library('ggthemes')
library('viridis')
library('ggrepel')
library('quantmod')
library('PerformanceAnalytics')
library('rmarkdown')

#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

#render("rp-vol-NIFTY.Rmd", output_file="rp-vol-NIFTY.html")
#q()

reportPath <- "analysis/plots"

options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)

asofDt<-as.Date(sqlQuery(lcon, "select max(time_stamp) from BHAV_EQ_FUT")[[1]])

pdf(NULL)
################ time series

vixPx <- sqlQuery(lcon, "select top 1000 px_close, time_stamp from VIX_HISTORY order by time_stamp desc")
vixPxts <- xts(vixPx[,1], vixPx[,2])

vixSd <- rollapply(vixPxts, 50, sd)
vixMean <- rollapply(vixPxts, 50, mean)

vixPlt <- na.omit(merge(vixPxts, vixMean, vixMean-vixSd, vixSd+vixMean))
names(vixPlt) <- c('VIX', 'AVG', 'LE', 'UE')
toPlot <- data.frame(vixPlt)
toPlot$T <- index(vixPlt)

plotStart <- first(index(vixPlt))
plotEnd <- last(index(vixPlt))

ggplot(toPlot, aes(x=T)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line(aes(y=VIX)) +
	geom_line(aes(y=AVG), color='blue', linetype = "dotted") +
	geom_ribbon(aes(ymin = LE, ymax=UE), fill='grey70', alpha=0.5) +
	scale_x_date(breaks = "3 month", date_labels="%b-%Y") +
	labs(x = "", y="", fill="", color="", title="INDIA VIX", subtitle=sprintf("50-day rolling [%s:%s]", plotStart, plotEnd)) +
	annotate("text", x=plotEnd, y=0, label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/vix-india.ts.png", reportPath), width=12, height=6, units="in")

################ density plot

vixLast <- as.numeric(last(vixPlt$VIX))
vixLastSd <- as.numeric(last(vixSd))

vixLastLE <- vixLast - vixLastSd
vixLastUE <- vixLast + vixLastSd

ggplot(toPlot, aes(x=VIX)) +
	theme_economist() +
	geom_density() +
	geom_vline(xintercept=vixLast, color='darkred', size=1) +
	geom_vline(xintercept=vixLastLE, color='darkgrey', size=.5, linetype="dotted") +
	geom_vline(xintercept=vixLastUE, color='darkgrey', size=.5, linetype="dotted") +
	geom_text(aes(x=vixLast, label=round(vixLast, 2), y=0.01, hjust='left'), colour='black', angle=90) +
	geom_text(aes(x=vixLastLE, label=round(vixLastLE, 2), y=0.015, hjust='left'), colour='grey', angle=90) +
	geom_text(aes(x=vixLastUE, label=round(vixLastUE, 2), y=0.015, hjust='left'), colour='grey', angle=90) +
	labs(x = "", y="", fill="", color="", title="INDIA VIX", subtitle=sprintf("density [%s:%s]", plotStart, plotEnd)) +
	annotate("text", x=max(vixPlt$VIX, na.rm=T), y=0, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/vix-india.density.png", reportPath), width=12, height=6, units="in")

################ nifty IV plot

spotPx <- sqlQuery(lcon, sprintf("select px_close from bhav_index where index_name='NIFTY 50' and time_stamp='%s'", asofDt))[[1]]
expirys <- sqlQuery(lcon, sprintf("select EXPIRY_DT from bhav_eq_fut where symbol='NIFTY' and time_stamp='%s' and EXPIRY_DT > time_stamp order by EXPIRY_DT", asofDt))$EXPIRY_DT

strike1 <- sqlQuery(lcon, sprintf("select min(STRIKE_PR) from bhav_eq_opt where symbol='NIFTY' and time_stamp='%s' and EXPIRY_DT = '%s' and STRIKE_PR > %f and convert(int, STRIKE_PR) %% 100 = 0", 
							asofDt, expirys[1], spotPx))[[1]]
strike2 <- sqlQuery(lcon, sprintf("select max(STRIKE_PR) from bhav_eq_opt where symbol='NIFTY' and time_stamp='%s' and EXPIRY_DT = '%s' and STRIKE_PR < %f and convert(int, STRIKE_PR) %% 100 = 0", 
							asofDt, expirys[1], spotPx))[[1]]

topGreeksCE <- sqlQuery(lcon, sprintf("select * from EQ_OPTION_GREEKS where SYMBOL='NIFTY' and time_stamp='%s' and strike = %f and OPTION_TYPE='CE' and EXPIRY_DATE <= '%s' order by EXPIRY_DATE", 
							asofDt, strike1, last(expirys)))
topGreeksPE <- sqlQuery(lcon, sprintf("select * from EQ_OPTION_GREEKS where SYMBOL='NIFTY' and time_stamp='%s' and strike = %f and OPTION_TYPE='PE' and EXPIRY_DATE <= '%s' order by EXPIRY_DATE", 
							asofDt, strike2, last(expirys)))

topGreeks <- rbind(topGreeksCE, topGreeksPE)
toPlot <- topGreeks[, c('OPTION_TYPE', 'EXPIRY_DATE', 'IV')]
toPlot$IV <- 100*toPlot$IV
toPlot$EXPIRY <- as.factor(toPlot$EXPIRY_DATE)

ggplot(toPlot, aes(x=EXPIRY, y=IV, color=OPTION_TYPE)) +
	theme_economist() +
	geom_point(size=2) +
	geom_hline(yintercept=vixLast, color='darkred', size=1) +
	geom_text(aes(y=vixLast, label=round(vixLast, 2), x=0.5), hjust='left', vjust="bottom", colour='black') +
	labs(x = "", y="", fill="", color="", title="NIFTY Strangle IV", subtitle=sprintf("%.0fCE/%.0fPE [%s]", strike1, strike2, plotEnd)) +
	annotate("text", x=0.5, y=min(toPlot$IV, na.rm=T), label = "@StockViz", hjust='left', vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/vix-india.IV.png", reportPath), width=12, height=6, units="in")

################ nifty OI weighted strike-spot ratio plot

oiStDate <- as.Date("2020-05-01")
expiries <- sqlQuery(lcon, sprintf("select distinct EXPIRY_DT from bhav_eq_opt where symbol='NIFTY' and time_stamp >= '%s'", oiStDate))$EXPIRY_DT
tradingDays <- sqlQuery(lcon, sprintf("select distinct time_stamp from bhav_eq_opt where symbol='NIFTY' and time_stamp >= '%s'", oiStDate))$time_stamp

expiries <- sort(expiries)
tradingDays <- sort(tradingDays)

nearExpStat <- data.frame(asof = Sys.Date(), option_typ = "", oiwks = 0.0, coiwks = 0.0)
for(i in 1:length(tradingDays)){
	trday <- tradingDays[i]
	expday <- first(expiries[expiries > trday])

	oiData <- sqlQuery(lcon, sprintf("select option_typ, strike_pr, open_interest from bhav_eq_opt where symbol='NIFTY' and time_stamp = '%s' and expiry_dt = '%s'", trday, expday))
	if(nrow(oiData) == 0){
		print(paste("no data for ", trday, "/", expday))
		next
	}
	spotPx <- sqlQuery(lcon, sprintf("select px_close from bhav_index where index_name='NIFTY 50' and time_stamp = '%s'", trday))[[1]]
	
	t1 <- oiData %>% group_by(option_typ) %>% summarize(asof = trday, oiwks = sum(open_interest * (strike_pr/spotPx-1))/sum(open_interest)) 
	t2 <- oiData %>% summarize(asof = trday, oiwks = sum(open_interest * (strike_pr/spotPx-1))/sum(open_interest))
	
	tempDf <- t1 %>% full_join(t2, by='asof') %>% relocate(asof) %>% as.data.frame()
	colnames(tempDf) <- c('asof', 'option_typ', 'oiwks', 'coiwks')
	
	nearExpStat <- rbind(nearExpStat, tempDf)
}

nearExpStat <- nearExpStat[-1,]
nearExpStat$oiwks <- nearExpStat$oiwks * 100
nearExpStat$coiwks <- nearExpStat$coiwks * 100

plotStart <- first(tradingDays)
plotEnd <- last(tradingDays)

ggplot(nearExpStat, aes(x=asof)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_point(aes(y=oiwks, color=option_typ), size=2) +
	geom_line(aes(y=coiwks)) +
	scale_x_date(breaks = "3 month", date_labels="%b-%Y") +
	labs(x = "", y="", fill="", color="", title="NIFTY OI Weighted Strike-Spot Ratio", subtitle=sprintf("nearest expiry [%s:%s]", plotStart, plotEnd)) +
	annotate("text", x=plotEnd, y=min(nearExpStat$oiwks), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/NIFTY.oiwks.png", reportPath), width=12, height=6, units="in")	
	
########### bank-nifty IV plot
	
spotPx <- sqlQuery(lcon, sprintf("select px_close from bhav_index where index_name='NIFTY BANK' and time_stamp='%s'", asofDt))[[1]]
expirys <- sqlQuery(lcon, sprintf("select EXPIRY_DT from bhav_eq_fut where symbol='BANKNIFTY' and time_stamp='%s' and EXPIRY_DT > time_stamp order by EXPIRY_DT", asofDt))$EXPIRY_DT

strike1 <- sqlQuery(lcon, sprintf("select min(STRIKE_PR) from bhav_eq_opt where symbol='BANKNIFTY' and time_stamp='%s' and EXPIRY_DT = '%s' and STRIKE_PR > %f and convert(int, STRIKE_PR) %% 100 = 0", 
							asofDt, expirys[1], spotPx))[[1]]
strike2 <- sqlQuery(lcon, sprintf("select max(STRIKE_PR) from bhav_eq_opt where symbol='BANKNIFTY' and time_stamp='%s' and EXPIRY_DT = '%s' and STRIKE_PR < %f and convert(int, STRIKE_PR) %% 100 = 0", 
							asofDt, expirys[1], spotPx))[[1]]

topGreeksCE <- sqlQuery(lcon, sprintf("select * from EQ_OPTION_GREEKS where SYMBOL='BANKNIFTY' and time_stamp='%s' and strike = %f and OPTION_TYPE='CE' and EXPIRY_DATE <= '%s' order by EXPIRY_DATE", 
							asofDt, strike1, last(expirys)))
topGreeksPE <- sqlQuery(lcon, sprintf("select * from EQ_OPTION_GREEKS where SYMBOL='BANKNIFTY' and time_stamp='%s' and strike = %f and OPTION_TYPE='PE' and EXPIRY_DATE <= '%s' order by EXPIRY_DATE", 
							asofDt, strike2, last(expirys)))

topGreeks <- rbind(topGreeksCE, topGreeksPE)
toPlot <- topGreeks[, c('OPTION_TYPE', 'EXPIRY_DATE', 'IV')]
toPlot$IV <- 100*toPlot$IV
toPlot$EXPIRY <- as.factor(toPlot$EXPIRY_DATE)

ggplot(toPlot, aes(x=EXPIRY, y=IV, color=OPTION_TYPE)) +
	theme_economist() +
	geom_point(size=2) +
	labs(x = "", y="", fill="", color="", title="BANK-NIFTY Strangle IV", subtitle=sprintf("%.0fCE/%.0fPE [%s]", strike1, strike2, plotEnd)) +
	annotate("text", x=0.5, y=min(toPlot$IV, na.rm=T), label = "@StockViz", hjust='left', vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/bank-nifty.IV.png", reportPath), width=12, height=6, units="in")

################ bank-nifty OI weighted strike-spot ratio plot

oiStDate <- as.Date("2020-05-01")
expiries <- sqlQuery(lcon, sprintf("select distinct EXPIRY_DT from bhav_eq_opt where symbol='BANKNIFTY' and time_stamp >= '%s'", oiStDate))$EXPIRY_DT
tradingDays <- sqlQuery(lcon, sprintf("select distinct time_stamp from bhav_eq_opt where symbol='BANKNIFTY' and time_stamp >= '%s'", oiStDate))$time_stamp

expiries <- sort(expiries)
tradingDays <- sort(tradingDays)

nearExpStat <- data.frame(asof = Sys.Date(), option_typ = "", oiwks = 0.0, coiwks = 0.0)
for(i in 1:length(tradingDays)){
	trday <- tradingDays[i]
	expday <- first(expiries[expiries > trday])

	oiData <- sqlQuery(lcon, sprintf("select option_typ, strike_pr, open_interest from bhav_eq_opt where symbol='BANKNIFTY' and time_stamp = '%s' and expiry_dt = '%s'", trday, expday))
	if(nrow(oiData) == 0){
		print(paste("no data for ", trday, "/", expday))
		next
	}
	spotPx <- sqlQuery(lcon, sprintf("select px_close from bhav_index where index_name='NIFTY BANK' and time_stamp = '%s'", trday))[[1]]
	
	t1 <- oiData %>% group_by(option_typ) %>% summarize(asof = trday, oiwks = sum(open_interest * (strike_pr/spotPx-1))/sum(open_interest)) 
	t2 <- oiData %>% summarize(asof = trday, oiwks = sum(open_interest * (strike_pr/spotPx-1))/sum(open_interest))
	
	tempDf <- t1 %>% full_join(t2, by='asof') %>% relocate(asof) %>% as.data.frame()
	colnames(tempDf) <- c('asof', 'option_typ', 'oiwks', 'coiwks')
	
	nearExpStat <- rbind(nearExpStat, tempDf)
}

nearExpStat <- nearExpStat[-1,]
nearExpStat$oiwks <- nearExpStat$oiwks * 100
nearExpStat$coiwks <- nearExpStat$coiwks * 100

plotStart <- first(tradingDays)
plotEnd <- last(tradingDays)

ggplot(nearExpStat, aes(x=asof)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_point(aes(y=oiwks, color=option_typ), size=2) +
	geom_line(aes(y=coiwks)) +
	scale_x_date(breaks = "3 month", date_labels="%b-%Y") +
	labs(x = "", y="", fill="", color="", title="BANK-NIFTY OI Weighted Strike-Spot Ratio", subtitle=sprintf("nearest expiry [%s:%s]", plotStart, plotEnd)) +
	annotate("text", x=plotEnd, y=min(nearExpStat$oiwks), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/BANK-NIFTY.oiwks.png", reportPath), width=12, height=6, units="in")	

############## subsequent distribution

prevDates <- index(vixPlt[vixPlt$VIX > vixLast * 0.99 &  vixPlt$VIX < vixLast * 1.01,])
nextVixDf <- data.frame(D=0, VAL=0.0)
for(i in 1:length(prevDates)){
	vixSubset <- vixPlt[paste0(prevDates[i], '/'),'VIX']
	toDays <- min(nrow(vixSubset), 11)
	if(toDays < 2) next
	for(j in 2:toDays){
		nextVixDf <- rbind(nextVixDf, c(j, as.numeric(vixSubset[j])))
	}
}
nextVixDf <- nextVixDf[-1,]
nextVixDf$D <- nextVixDf$D - 1
nextVixDf$D <- factor(nextVixDf$D, levels=unique(nextVixDf$D))

plotStartVix <- floor(min(nextVixDf$VAL)) - 2
plotEndVix <- ceiling(max(nextVixDf$VAL)) + 2
ggplot(nextVixDf, aes(x=D, y=VAL, fill=D)) +
	theme_economist() +
	scale_fill_viridis(discrete = TRUE) +
	geom_violin(trim = F, scale = "count") + 
	stat_summary(fun = "mean", geom = "point", color = "white") +
	geom_jitter(height = 0, width = 0.1, size=1) +
	scale_y_continuous(breaks=seq(plotStartVix, plotEndVix, by=1)) +
	guides(color="none", fill="none") +
	labs(color='', fill='', y='VIX', x='Days', title='Historical next n-day VIX', subtitle=sprintf("%d matches [%s:%s]", length(prevDates), plotStart, plotEnd)) +
	annotate("text", x=10, y=plotStartVix+1, label = "@StockViz", hjust='right', vjust='bottom', col="white", cex=6, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/vix-india.next.png", reportPath), width=12, height=6, units="in")	

doIndexVolatility <- function(indexBhavName, indexDisplayName){
	################### nifty realized volatility

	doVolCharts <- function(lb, spotXts, freq){

		c2cVol <- TTR::volatility(spotXts, n=lb, calc = "close")
		gkVol <- TTR::volatility(spotXts, n=lb, calc = "garman.klass")
		parkVol <- TTR::volatility(spotXts, n=lb, calc = "parkinson")
		rsVol <- TTR::volatility(spotXts, n=lb, calc = "rogers.satchell")
		gkyzVol <- TTR::volatility(spotXts, n=lb, calc = "gk.yz")
		yzVol <- TTR::volatility(spotXts, n=lb, calc = "yang.zhang")

		volXts <- na.trim(merge(c2cVol, gkVol, parkVol, rsVol, gkyzVol, yzVol), sides='left')
		names(volXts) <- c("Close-to-Close", "Garman-Klass", "Parkinson", "Rogers-Satchell", "Garman-Klass-Yang-Zhang", "Yang-Zhang")

		toPlot <- data.frame(volXts)
		toPlot$T <- index(volXts)

		toPlot <- melt(toPlot, id='T')

		plotStart <- first(index(volXts))
		plotEnd <- last(index(volXts))

		ggplot(toPlot, aes(x=T, y=value, color=variable)) +
			theme_economist() +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			geom_line(size=1.25) +
			scale_x_date(breaks = "3 month", date_labels="%b-%Y") +
			scale_color_viridis_d() +
			labs(x = "", y="", fill="", color="", title=sprintf("%s Realized Volatility (%s)", indexDisplayName, freq), subtitle=sprintf("%d-day rolling [%s:%s]", lb, plotStart, plotEnd)) +
			annotate("text", x=plotEnd, y=0, label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
			
		ggsave(sprintf("%s/realized-%s.ts.%s.png", reportPath, indexDisplayName, freq), width=12, height=6, units="in")

		rvolMin <- as.numeric(max(volXts))

		ggplot(toPlot, aes(x=value, color=variable)) +
			theme_economist() +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			stat_density(size=1.25, geom="line", position="identity") +
			scale_color_viridis_d() +
			labs(x = "", y="", fill="", color="", title=sprintf("%s Realized Volatility Density (%s)", indexDisplayName, freq), subtitle=sprintf("%d-day rolling [%s:%s]", lb, plotStart, plotEnd)) +
			annotate("text", x=rvolMin, y=0, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
			
		ggsave(sprintf("%s/realized-%s.density.%s.png", reportPath, indexDisplayName, freq), width=12, height=6, units="in")
	}

	spotPxs <- sqlQuery(lcon, sprintf("select time_stamp, px_high, px_low, px_open, px_close from bhav_index where index_name='%s' and time_stamp >= '2010-01-01'", indexBhavName))
	spotXts <- xts(spotPxs[,-1], spotPxs[,1])
	names(spotXts) <- c('High', 'Low', 'Open', 'Close')
	doVolCharts(20, spotXts, "daily")

	spotWeekly <- to.weekly(spotXts)
	names(spotWeekly) <- c('Open', 'High', 'Low', 'Close')
	doVolCharts(20, spotWeekly, "weekly")

	spotMonthly <- to.monthly(spotXts)
	names(spotMonthly) <- c('Open', 'High', 'Low', 'Close')
	index(spotMonthly) <- as.Date(index(spotMonthly))
	doVolCharts(20, spotMonthly, "monthly")

	################# daily return sources

	iXts <- spotXts["2020-05-01/",]
	hlocStr <- names(iXts)
	iXts <- merge(iXts, stats::lag(iXts$Close, 1))
	names(iXts) <- c(hlocStr, 'Close_1')

	retXts <- merge(100*(iXts$Open/iXts$Close_1 -1), 100*(iXts$Close/iXts$Open-1), 100*(iXts$High/iXts$Low-1), log(100*abs((iXts$Close - iXts$Close_1)/(iXts$Close - iXts$Open))))
	names(retXts) <- c('OVERNIGHT', 'DAY', 'RANGE', 'OVERNIGHT.DAY')
	retXts <- na.omit(retXts)

	description <- c('Open - Prev. Close (%)', 'Close - Open (%)', 'High - Low (%)', 'Close-to-Prev. Close over Close-to-Open log(x100)') 

	plts <- list()
	for(i in 1:ncol(retXts)){
		src <- names(retXts)[i]
		toPlot <- data.frame(retXts[, src])
		toPlot$T <- index(retXts)

		plotStart <- first(index(retXts))
		plotEnd <- last(index(retXts))

		plts[[src]] <- ggplot(toPlot, aes(x=T, y=.data[[src]], group=1)) +
						theme_economist() +
						theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
						geom_point(size=1.25) +
						scale_x_date(breaks = "1 month", date_labels="%b-%Y") +
						labs(x = "", y = description[i], fill="", color="", title=src)
	}

	plot_grid(plotlist = plts, ncol=1)
	ggsave(sprintf("%s/%s.returns-periods.png", reportPath, indexDisplayName), width=12, height=6*ncol(retXts), units="in")

	retXts <- merge(iXts$Open/iXts$Close_1 -1, iXts$Close/iXts$Open-1, dailyReturn(iXts$Close))
	names(retXts) <- c('OVERNIGHT', 'DAY', 'BH')

	mRetXts <- apply.monthly(retXts, Return.cumulative)
	mRetXts <- tail(mRetXts, 25)
	toPlot <- data.frame(mRetXts*100)
	toPlot$T <- strftime(index(mRetXts), '%Y-%b')
	toPlot <- melt(toPlot, id='T')
	toPlot$T <- factor(toPlot$T, levels=as.character(unique(toPlot$T)))

	minRet <- as.numeric(min(toPlot$value))

	ggplot(toPlot, aes(x=T, y=value, fill=variable)) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		geom_bar(stat = "identity", position = position_dodge()) +
		geom_text_repel(aes(label = sprintf('%.2f', value)), position = position_dodge(0.9)) +
		scale_fill_viridis(discrete = TRUE) +
		labs(x = "Year", y="Returns (%)", fill="", color="", size="", title=sprintf("%s Period Returns", indexDisplayName), subtitle=sprintf("Monthly Returns [%s:%s]", first(index(mRetXts)), last(index(mRetXts)))) +
		annotate("text", x=1, y=minRet, label = "@StockViz", hjust='left', vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
		
	ggsave(sprintf("%s/%s.period.monret.png", reportPath, indexDisplayName), width=12, height=6)

	Common.PlotCumReturns(retXts[strftime(first(index(mRetXts)), "%Y-%m-01/"),], sprintf("%s Period Returns", indexDisplayName), "", sprintf("%s/%s.period.cumret.png", reportPath, indexDisplayName))
}

doIndexVolatility("NIFTY 50", "NIFTY")
doIndexVolatility("NIFTY BANK", "BANK-NIFTY")

############## index constituent volatility distribution

doConstituentVolatility <- function(indexBhavName, indexDisplayName){
	pxLb <- 500
	volLb <- 50
	asof <- sqlQuery(lcon, sprintf("select max(time_stamp) from INDEX_CONST_HISTORY where INDEX_NAME='%s'", indexBhavName))[[1]]
	symbolList <- sqlQuery(lcon, sprintf("select symbol from INDEX_CONST_HISTORY where INDEX_NAME='%s' and time_stamp = '%s' order by CAP_WEIGHT desc", indexBhavName, asof))[,1]
	dateSeg <- sqlQuery(lcon, sprintf("select top %d time_stamp from bhav_index where INDEX_NAME='%s' and time_stamp <= '%s' order by time_stamp desc", pxLb, indexBhavName, asofDt))[,1]
	
	rvol <- NULL
	symList <- c()
	for(sym in symbolList){
		pDf <- dbGetQuery(pgCon, sprintf("select date_stamp, h as High, l as Low, o as Open, c as Close from eod_adjusted_nse where ticker=$1 and date_stamp >= $2 order by date_stamp desc limit %d", pxLb), 
								param=list(sym, last(dateSeg)))
								
		if(nrow(pDf) < pxLb*0.95) next
		
		rvol <- merge.xts(rvol, volatility(xts(pDf[,-1], pDf[,1]), calc="parkinson", n=volLb))
		symList <- c(symList, sym)
	}
	names(rvol) <- symList
	rvol <- na.trim(rvol, sides=c('left'))
	
	lastDf <- data.frame(t(xts::last(rvol)))
	colnames(lastDf) <- c('value')
	lastDf$SYMBOL <- row.names(lastDf)
	
	toPlot <- data.frame(t(rvol))
	toPlot$SYMBOL <- row.names(toPlot)
	toPlot <- melt(toPlot, id='SYMBOL')
	#toPlot$SYMBOL <- factor(toPlot$SYMBOL, levels=unique(toPlot$SYMBOL))
	
	plotStart <- first(index(rvol))
	plotEnd <- last(index(rvol))
	
	ggplot(toPlot, aes(x=SYMBOL,y=value, fill=SYMBOL)) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		scale_fill_viridis_d() +
		geom_violin(trim = T, scale = "count") + 
		stat_summary(fun = "mean", geom = "point", color = "white") +
		geom_point(data=lastDf, aes(x=SYMBOL, y=value), color="black", fill="black", shape=18, size=3) +
		guides(color="none", fill="none") +
		labs(color='', fill='', y='volatility', x='', title=sprintf('%s Constituent Historical %d-day Volatility', indexDisplayName, volLb), subtitle=sprintf("%d-day rolling [%s:%s]", pxLb, plotStart, plotEnd)) +
		annotate("text", x=1, y=min(toPlot$value), label = "@StockViz", hjust='left', vjust='bottom', col="white", cex=6, fontface = "bold", alpha = 0.5)
		
	ggsave(sprintf("%s/%s.constituent.volatility.png", reportPath, indexDisplayName), width=12, height=6)
}

doConstituentVolatility("NIFTY 50", "NIFTY")
doConstituentVolatility("NIFTY BANK", "BANK-NIFTY")

############################################################################

render("rp-vol-NIFTY.Rmd", output_file="rp-vol-NIFTY.html")
