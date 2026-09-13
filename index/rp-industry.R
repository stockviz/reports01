library(rmarkdown)

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('viridis')
#library('DT')
library('ggrepel')

options(stringsAsFactors = FALSE)
options("scipen"=100)

#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

#render("rp-risk.Rmd", output_file="rp-risk.html")
#q()

#source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/hollandr/config.r")
#source("/mnt/data/blog/common/plot.common.r")
source("/mnt/hollandr/plot.common.r")
#source("/mnt/siberia/stockviz-dashboard/reports01/common/fan-chart.R")
source("../common/fan-chart.R")

plotPath <- "industry/plots"
idPath <- "../"

smaLbs <- c(20, 50, 100, 200) #days

benchName <- "NIFTY TOTAL MARKET TR"
startDate <- as.Date("2015-01-01")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

maxDt <- sqlQuery(lcon, "select max(time_stamp) from px_history")[[1]]

industryDf <- sqlQuery(lcon, "select industry, count(*) cnt from bhav_industry group by industry")
industryDf <- industryDf |> filter(cnt >= max(smaLbs)*2)

plottedIndices <- c()
createPlots <- function(){
	bDf1 <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s' and time_stamp >= '%s' order by time_stamp", benchName, startDate))
	bXts <- xts(bDf1[,1], bDf1[,2])
	
	if(last(index(bXts)) != maxDt) return

	bRetDaily <- dailyReturn(bXts)
	bRetAnn <- annualReturn(bXts)

	names(bRetDaily) <- c(benchName)
	names(bRetAnn) <- c(benchName)

	smaStats <- data.frame(matrix(c("", "", smaLbs), nrow=1))
	colnames(smaStats) <- c("INDEX_NAME", "WT", sapply(smaLbs, function(X) paste0("SMA_", X)))
	
	smaDist <- data.frame(matrix(c("", "", smaLbs), nrow=1))
	colnames(smaDist) <- c("INDEX_NAME", "WT", sapply(smaLbs, function(X) paste0("SMA_", X)))


	#for(i in 1:1){
	for(i in 1:nrow(industryDf)){
		iName <- industryDf$industry[i]
		fName <- gsub("[^[:alnum:] ]| ", "", iName)
		
		#calculate equal weight industry returns
		print(iName)

		tryCatch({
			retDailyDf <- sqlQuery(lcon, sprintf("select TIME_STAMP, RET_EQ_WT, RET_CAP_WT from bhav_industry where industry = '%s' and time_stamp >= '2015-01-01'", iName))
			retDaily <- xts(retDailyDf[,-1], retDailyDf[,1])
			iXts <- merge(cumprod(1 + retDaily[,1]), cumprod(1 + retDaily[,2]))
			retAnn <- merge(annualReturn(iXts[,1]), annualReturn(iXts[,2]))

			names(iXts) <- c('EQUAL_WT', 'CAP_WT')
			names(retDaily) <- c('EQUAL_WT', 'CAP_WT')
			names(retAnn) <- c('EQUAL_WT', 'CAP_WT')

			toPlot <- na.trim(merge(retDaily, bRetDaily), sides='left')
			Common.PlotCumReturns(toPlot, iName, "", sprintf("%s/%s.cumret.png", plotPath, fName))
			
			for(j in 1:ncol(iXts)){
			  wtName <- names(iXts)[j]
  			allXts <- merge(iXts[,j], stats::lag(retDaily[,j], -1))
  			names(allXts) <- c(iName, 'INDEX')
  
  			smaLoRets <- NULL
  			smaGaps <- c()
  			for(smaLb in smaLbs){
  				sma <- SMA(allXts[,1], smaLb)
  				smaGaps <- c(smaGaps, paste0(round(100 * (coredata(last(allXts[,1])) / coredata(last(sma)) - 1), 2), '%'))
  				smaLoRets <- merge.xts(smaLoRets, ifelse(allXts[,1] > sma, allXts$INDEX, 0))
  			}
  			names(smaLoRets) <- sapply(smaLbs, function(X) paste0('SMA_', X))
  			smaDist <- rbind(smaDist, c(iName, wtName, smaGaps))
  			
  			tryCatch({
  				smaIR <- unlist(lapply(1:ncol(smaLoRets), function(X) as.numeric(UpsidePotentialRatio(smaLoRets[,X]))))
  				smaStats <- rbind(smaStats, c(iName, wtName, smaIR))
  			}, error = function(e){})
  			
  			smaLoRets <- na.omit(merge(smaLoRets, allXts$INDEX))
  			Common.PlotCumReturns(smaLoRets, sprintf("%s (%s) SMA Profile", iName, wtName), "long-only", sprintf("%s/%s.sma.cumret.%s.png", plotPath, fName, wtName))
  			
  			tryCatch({
    			toPlotAnn <- data.frame(na.trim(100*merge(retAnn[,j], bRetAnn), sides='left'))
    			toPlotAnn$Y <- year(index(retAnn))
    			
    			maxYear <- length(unique(toPlotAnn$Y))
    			toPlotAnn <- melt(toPlotAnn, id='Y')
    			minRet <- min(toPlotAnn$value)
    			toPlotAnn$Y <- factor(toPlotAnn$Y, levels = unique(toPlotAnn$Y))
    
    			ggplot(toPlotAnn, aes(x=Y, y=value, fill=variable)) +
    				theme_economist() +
    				geom_bar(stat = "identity", position = position_dodge()) +
    				geom_text_repel(aes(label = sprintf('%.2f', value)), position = position_dodge(0.9)) +
    				scale_fill_viridis(discrete = TRUE) +
    				labs(x = "Year", y="Returns (%)", fill="", color="", size="", 
    				     title=sprintf("%s (%s)", iName, wtName), 
    				     subtitle=sprintf("Annual Returns [%s:%s]", first(index(retDaily)), last(index(retDaily))),
    				     caption = '@StockViz')
    				
    			ggsave(sprintf("%s/%s.annret.%s.png", plotPath, fName, wtName), width=12, height=6)
  			}, error = function(e){})
  			
  			rollStDt <- first(index(iXts))
  			if(month(rollStDt) != 1){
  				rollStDt <- as.Date(sprintf("%d-01-01", year(rollStDt)+1))
  			}
  			iXts2 <- iXts[paste0(rollStDt, "/"), j]
  			
  			if(nrow(iXts2) > 1){
    			rAnnRets <- annualReturn(iXts2)
    			
    			if(nrow(rAnnRets) > 10){
      			rAnns <- merge(rollapply(rAnnRets, 3, function(X) Return.annualized(X)), rollapply(rAnnRets, 5, function(X) Return.annualized(X)), rollapply(rAnnRets, 10, function(X) Return.annualized(X)))
      			rAnns <- rAnns*100
      			rAnns <- rAnns[rowSums(is.na(rAnns)) != ncol(rAnns),]
      			names(rAnns) <- c("y3", "y5", "y10")
      			
      			toPlotAnn <- data.frame(rAnns)
      			toPlotAnn$Y <- year(index(rAnns))
      			
      			maxYear <- length(unique(toPlotAnn$Y))
      			toPlotAnn <- melt(toPlotAnn, id='Y')
      			minRet <- min(toPlotAnn$value)
      			toPlotAnn$Y <- factor(toPlotAnn$Y, levels = unique(toPlotAnn$Y))
      
      			ggplot(toPlotAnn, aes(x=Y, y=value, fill=variable)) +
      				theme_economist() +
      				geom_bar(stat = "identity", position = position_dodge()) +
      				geom_text_repel(aes(label = sprintf('%.2f', value)), position = position_dodge(0.9)) +
      				scale_fill_viridis(discrete = TRUE) +
      				labs(x = "Year", y="Returns (%)", fill="", color="", size="", 
      				     title=sprintf("%s (%s) Rolling Annualized Returns", iName, wtName), 
      				     subtitle=sprintf("[%s:%s]", rollStDt, last(index(iXts2))),
      				     caption='@StockViz')
      				
      			ggsave(sprintf("%s/%s.roll.%s.png", plotPath, fName, wtName), width=12, height=6)
    			}
    			
    			if(nrow(rAnnRets) > 5){
      			iDf <- data.frame(iXts[,j])
      			iDf$time_stamp <- index(iXts)
      			fanPlot <- common.CreateFanChart(iDf, paste0(iName, '(', wtName, ')'), sprintf("%d:%s", min(year(index(iXts))), max(index(iXts))))
      			ggsave(sprintf("%s/%s.fan.%s.png", plotPath, fName, wtName), fanPlot, width=12, height=6)
    			}
  			}
			}
		}, error=function(e){print(e)})
	}
	
	smaStats <- smaStats[-1,]
	save(smaStats, file="smaStats-industry.Rdata")
	
	smaDist <- smaDist[-1,]
	save(smaDist, file="smaDist-industry.Rdata")
}

renderIndices <- function(){
	
	plotFiles <- list.files(plotPath, pattern="*.png")
	plottedNames <- unique(unlist(lapply(strsplit(plotFiles, ".", fixed=T), `[[`, 1)))
	
	for(i in 1:nrow(industryDf)){
		
	  iName <- industryDf$industry[i]
		fName <- gsub("[^[:alnum:] ]| ", "", iName)
		
		if (!(fName %in% plottedNames)) {
			print(paste(iName, "NOT PLOTTED!!!"))
			next
		}
		
		print(iName)
		tryCatch({
			render("industry/rp-industry-risk.Rmd", output_file=paste0("rp-", fName, ".html"), params=list(index_name = iName, fName = fName))
		}, error=function(e){print(e)})
	}
}

print("creating plots...")
createPlots()

print("rendering indices...")
renderIndices()

print("rendering master page...")

render("rp-industry.Rmd", output_file="rp-industry.html")

