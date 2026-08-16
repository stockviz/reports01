library(rmarkdown)
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

render("52wk-US.Rmd", output_file="52wk-US.html")
render("ath-US.Rmd", output_file="ath-US.html")
render("drawdowns-US.Rmd", output_file="drawdowns-US.html")
render("streaks-US.Rmd", output_file="streaks-US.html")