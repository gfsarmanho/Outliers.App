# Outliers.App
R package containing Shiny App for outliers analysis. Online version: https://gfsarmanho.shinyapps.io/OutliersApp/

## Instalation

* [R](https://cloud.r-project.org/)
* [RStudio](https://www.rstudio.com/products/rstudio/download/)
* [Rtools](https://cran.r-project.org/bin/windows/Rtools/) (Windows machine)

```r
# Installing repository from GitHub
devtools::install_github(repo="gfsarmanho/Outliers.App")
```

## Usage
```r
# Loading
library(Outliers.App)

# Running application
run_outliers_app()
```

## PDF report generation

You do not need to install directly MikTex or similar distributions in your computer in order save the reports in PDF format. Just install [tinytex](https://yihui.name/tinytex/) package:
```r
install.packages('tinytex')
tinytex::install_tinytex()
```



