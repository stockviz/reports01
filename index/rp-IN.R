library('RODBC')
library('rmarkdown')
library('tidyverse')
library('lubridate')

render("rp-amfi-mktcap.Rmd", output_file="rp-MKT-CAP.html")

#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

#source("C:/stockviz/r/config.r")
source("/mnt/hollandr/config.r")

options("scipen"=100)
options(stringsAsFactors = FALSE)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indices <- sqlQuery(lcon, "select INDEX_NAME, min(time_stamp) st, max(time_stamp) ed from INDEX_CONST_HISTORY group by INDEX_NAME")

indices$prev <- indices$ed
for(i in 1:nrow(indices)){
	if(indices$ed[i] == indices$st[i]) next
	
	indices$prev[i] <- sqlQuery(lcon, sprintf("select max(time_stamp) from INDEX_CONST_HISTORY where index_name = '%s' and time_stamp < '%s'", indices$INDEX_NAME[i], indices$ed[i]))[[1]]
}

changeLog <- NULL

tryCatch({
	load("changeLog.RData")
}, error = function(e){ print(e) })

if(is.null(changeLog)){
	changeLog <- data.frame(INDEX_NAME = "", INAME = "", INDEX_CHANGE_DATE = Sys.Date(), REPORT_UPDATE_DATE = Sys.Date())
} 

changeLog <- indices %>% full_join(changeLog, by='INDEX_NAME') %>% 
							filter(INDEX_NAME != "" & ed > Sys.Date() - 500) %>% 
							mutate(INDEX_CHANGE_DATE = ed, INAME = gsub("[^[:alnum:] ]| ", "", INDEX_NAME)) %>% 
							select(INDEX_NAME, INDEX_CHANGE_DATE, REPORT_UPDATE_DATE, INAME) %>% 
							as.data.frame()

#print(changeLog)
							
for(i in 1:nrow(changeLog)){
	#print(changeLog[i,])

	if(!is.na(changeLog$REPORT_UPDATE_DATE[i]) && is.Date(changeLog$REPORT_UPDATE_DATE[i]) && changeLog$REPORT_UPDATE_DATE[i] >= changeLog$INDEX_CHANGE_DATE[i]) next
	
	changeLog$REPORT_UPDATE_DATE[i] <- Sys.Date()
	
	tryCatch({
		render("analysis/rp-index.Rmd", 
					output_file=paste0("rp-", changeLog$INAME[i], ".html"), 
					params=list(index_name = changeLog$INDEX_NAME[i]))
	}, error=function(e){print(e)})
}

save(changeLog, file="changeLog.RData")

print("rendering master page...")

render("rp-IN.Rmd", output_file="rp-IN.html")
