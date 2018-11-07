
library(outliers)

dados <- rnorm(10)

teste10 <- outliers::grubbs.test(x=dados, type=10)
teste11 <- outliers::grubbs.test(x=dados, type=11)
teste11 <- outliers::grubbs.test(x=dados, type=20)

str(teste)

teste$p.value

f <- function(x){
  x <- 10

  cat("\nCall:\n")
  print(10)
  cat("\nResiduals:\n")
  print(11)
  cat("\n")

  return(x)
}
f()

# Test load txt file
setwd("C:/Users/gabri/Dropbox/Projects - DIMQT/Projeto - Outliers/Outliers.App_master/inst/App")
read.table("exemplo_outlier_3.txt", sep="\t", header=TRUE)

# Deploy at Shinyapps.io
rsconnect::setAccountInfo(name=)
library(rsconnect)
rsconnect::deployApp(appName = "OutliersApp",
                     "C:/Users/gabri/Dropbox/Projects - DIMQT/Projeto - Outliers/Outliers.App_master/inst/App")

#
git remote add origin git@github.com:gfsarmanho/Outliers.App.git
git remote add origin https://github.com/gfsarmanho/Outliers.App.git
