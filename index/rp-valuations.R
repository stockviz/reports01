#source("C:/stockviz/r/config.r")
source("/mnt/hollandr/config.r")

library('RODBC')
library('tidyverse')
library('reshape2')
library('ggthemes')
library('viridis')
library('ggrepel')
library('quantmod')

library('rmarkdown')

#render("rp-valuations.Rmd", output_file="rp-valuations.html")
#q()

#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

reportPath <- "analysis/plots-valuation"

options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

pdf(NULL)

indexListNse <- c('NIFTY 50', 'NIFTY NEXT 50', 'NIFTY MIDCAP 50')

maxTs <- as.Date(sqlQuery(lcon, "select max(AS_OF) from FUNDA_RATIOS where src='NSE'")[[1]])
minTs <- as.Date(sqlQuery(lcon, sprintf("select max(AS_OF) from FUNDA_RATIOS where src='NSE' and as_of < '%s'", maxTs-365))[[1]])

for(i in 1:length(indexListNse)){
	indexName <- indexListNse[i]

	ratioMax <- sqlQuery(lcon, sprintf("select I.SYMBOL, CAP_WEIGHT, JSON_VALUE (F.RATIOS, '$.PE') as PE
								from INDEX_NSE_3 I, FUNDA_RATIOS F
								where INDEX_NAME='%s'
								and SRC='NSE'
								and AS_OF='%s'
								and i.SYMBOL=F.SYMBOL
								order by CAP_WEIGHT desc", indexName, maxTs))

	ratioMin <- sqlQuery(lcon, sprintf("select I.SYMBOL, JSON_VALUE (F.RATIOS, '$.PE') as PE
								from INDEX_NSE_3 I, FUNDA_RATIOS F
								where INDEX_NAME='%s'
								and SRC='NSE'
								and AS_OF='%s'
								and i.SYMBOL=F.SYMBOL
								order by CAP_WEIGHT desc", indexName, minTs))
	
	#ratioMax$PE <- ifelse(ratioMax$PE < -200, NA, ratioMax$PE)
	#ratioMin$PE <- ifelse(ratioMin$PE < -200, NA, ratioMin$PE)
	exch <- 'NSE'
	ratioName <- 'PE'							
	data <- merge(ratioMax, ratioMin, by='SYMBOL')
	dataSlice <- data[, c('SYMBOL', 'CAP_WEIGHT', sprintf('%s.x', ratioName), sprintf('%s.y', ratioName))]
	colnames(dataSlice) <- c('SYMBOL', 'CAP_WEIGHT', toString(maxTs), toString(minTs))
	dataMelt <- melt(dataSlice, id=c('SYMBOL', 'CAP_WEIGHT'))
								
	ggplot(data = dataMelt, aes(x=SYMBOL, y=log(value), size=CAP_WEIGHT, color=variable)) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		geom_point(alpha=0.5) +
		scale_size(range = c(0, 20)) +
		geom_text_repel(aes(label=round(value, 2)), nudge_x = 1, na.rm = TRUE, size=2) +
		guides(size=F) +
		labs(x='', y=sprintf("log(%s)", ratioName), color='', title=sprintf("%s %s (%s)", indexName, ratioName, exch)) +
		annotate("text", x=nrow(data), y=max(dataSlice[, c(3, 4)]), label = "@StockViz", hjust=1, vjust=1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/%s-Constituents.%s.png", reportPath, indexName, ratioName), width=16, height=8, units="in")
}

###################################################################

startDate<-as.Date("2010-01-01")
endDate<-Sys.Date()

ratios<-c('PE', 'PB')

indexListNse <- list(`Cap-Weighted` = c('NIFTY 50', 'NIFTY MIDCAP 50'),
	`Sector` = c('NIFTY AUTO', 'NIFTY FMCG', 'NIFTY IT', 'NIFTY MEDIA', 'NIFTY PHARMA'),
	`Infra` = c('NIFTY COMMODITIES', 'NIFTY ENERGY', 'NIFTY INFRASTRUCTURE', 'NIFTY METAL', 'NIFTY REALTY'),
	`Ownership-based` = c('NIFTY CPSE', 'NIFTY MNC', 'NIFTY PSE'),
	`Financials` = c('NIFTY BANK', 'NIFTY FINANCIAL SERVICES', 'NIFTY PRIVATE BANK', 'NIFTY SERVICES SECTOR'))
	
plotRatio<-function(indexGrpName, indices, ratioName, exch){
	allXts<-NULL
	for(indexName in indices){
		nDf1<-sqlQuery(lcon, sprintf("select TIME_STAMP, %s from INDEX_%s_VALUATION where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", ratioName, exch, indexName, startDate, endDate))
		nXts1<-xts(nDf1[,2], as.Date(nDf1[,1]))
		nXts1<-ifelse(nXts1 > 50 | nXts1 < -10, NA, nXts1)
		allXts<-merge.xts(allXts, nXts1)
	}
	
	names(allXts)<- gsub('S&P BSE ', '', indices)
	
	############
	
	firstDate<-first(index(allXts))
	lastDate<-last(index(allXts))
	xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

	ctr2Df<-data.frame(allXts)
	ctr2Df$T<-as.Date(index(allXts))

	ctr2Melt<-melt(ctr2Df, id='T')
	ctr2Melt$label<-ifelse(ctr2Melt$T == max(ctr2Melt$T), as.character(ctr2Melt$variable), NA)

	pdf(NULL)
	ggplot(ctr2Melt, aes(x=T, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		geom_label_repel(aes(label=label), nudge_x = 1, na.rm = TRUE) +
		guides(color=F) +
		scale_color_viridis_d() +
		scale_x_date(breaks = xAxisTicks) +
		labs(x='', y=ratioName, color='', title=sprintf("%s %s (%s)", indexGrpName, ratioName, exch), subtitle=sprintf("[%s:%s]", firstDate, lastDate)) +
		annotate("text", x=lastDate, y=min(allXts, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
			
	ggsave(sprintf("%s/val-ratio.%s.%s.%s.png", reportPath, indexGrpName, ratioName, exch), width=16, height=8, units="in")
}	

indexNames<-names(indexListNse)
for(iName in indexNames){
	for(ratio in ratios){
		plotRatio(iName, indexListNse[[iName]], ratio, "NSE")
	}
}	

render("rp-valuations.Rmd", output_file="rp-valuations.html")