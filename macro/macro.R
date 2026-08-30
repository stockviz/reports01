library(rmarkdown)
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

render("rp-cmdy.Rmd", output_file="rp-cmdy.html")
render("rp-bnd.Rmd", output_file="rp-bnd.html")
render("rp-cur.Rmd", output_file="rp-cur.html")