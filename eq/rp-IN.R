library(rmarkdown)
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

render("results-IN.Rmd", output_file="results-IN.html")
render("52wk-IN.Rmd", output_file="52wk-IN.html")
render("ath-IN.Rmd", output_file="ath-IN.html")
render("drawdowns-IN.Rmd", output_file="drawdowns-IN.html")
render("streaks-IN.Rmd", output_file="streaks-IN.html")