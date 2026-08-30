library(rmarkdown)

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')
library('ggthemes')
library('reshape2')
library('viridis')

options(stringsAsFactors = FALSE)
options("scipen"=100)

#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")
#source("C:/stockviz/r/config.r")
source("/mnt/hollandr/config.r")

plotPath <- "risk/plots"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

###### relative strength

relDf <- sqlQuery(lcon, "select top 3100 time_stamp, look_back, score_cnx500 from RELATIVE_STRENGTH where SYMBOL='CNX 500' order by time_stamp desc")
for(lb in c(50, 100, 200, 500, 1000)){
	prodSeries <- relDf %>% pivot_wider(names_from = look_back, values_from = score_cnx500) %>% 
					slice_max(order_by=time_stamp, n=lb) %>% 
					arrange(time_stamp)
	
	toPlot <- melt(prodSeries, id='time_stamp')

	ggplot(toPlot, aes(x=time_stamp, y=value, color=variable)) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		geom_line(size=1) +
		scale_x_date(date_breaks='1 month', date_labels="%Y-%b-%d") +
		scale_colour_viridis_d() +
		labs(x='', y='spread', fill="", color="look back (days)", 
								title="Dispersion of NIFTY 500 Components", 
								subtitle=sprintf("Median Bottom and Top Decile Return Ratio; %s:%s", min(toPlot$time_stamp), max(toPlot$time_stamp))) +
		annotate("text", x=toPlot$time_stamp[1], y=min(toPlot$value, na.rm=T), label = "@StockViz", hjust=0, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/dispersion.%d.png", plotPath, lb), width=12, height=6)
}

###### mean reversion indices

synDf <- sqlQuery(lcon, "select index_name, time_stamp, ret_pct from synthetic_index where time_stamp >= '2021-03-01' order by time_stamp")

for(lb in c(50, 100, 200, 500)){
	prodSeries <- synDf %>% pivot_wider(names_from = index_name, values_from = ret_pct) %>% 
							slice_max(order_by=time_stamp, n=lb) %>% 
							arrange(time_stamp) %>%
							mutate_at(vars(-time_stamp), ~replace(., 1, 0)) %>% 
							mutate_at(vars(-time_stamp), ~cumprod(1 + .))
							
	
	toPlot <- melt(prodSeries, id='time_stamp')

	ggplot(toPlot, aes(x=time_stamp, y=value, color=variable)) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		geom_line(size=1) +
		scale_x_date(date_breaks='1 month', date_labels="%Y-%b-%d") +
		scale_colour_viridis_d() +
		labs(x='', y='growth of Rs. 1', fill="", color="", title="Mean Reversion of NIFTY 50 Components", subtitle=sprintf("%s:%s", min(toPlot$time_stamp), max(toPlot$time_stamp))) +
		annotate("text", x=toPlot$time_stamp[1], y=min(toPlot$value, na.rm=T), label = "@StockViz", hjust=0, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/mean-rev.%d.png", plotPath, lb), width=12, height=6)
}

print("rendering master page...")

render("rp-syn.Rmd", output_file="rp-syn.html")
