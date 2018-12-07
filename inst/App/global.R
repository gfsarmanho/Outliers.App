#==============================================================================#
#=========================== Global variables =================================#
#==============================================================================#

# Customized colors (from ggplot2 3 colors pattern)
plot_colors <- c("#619CFF", "#F8766D", "#00BA38")

#---------------------------------------------#
# IQR.test() - Interquartile Rule for Outlier #
#---------------------------------------------#
IQR.test <- function(x){

  x <- sort(x[complete.cases(x)])  # eliminates NA
  DNAME <- deparse(substitute(x))
  method <- "Interquartile rule for outlier detection"
  out.val <- NA

  # Statistics
  q75 <- quantile(x, probs=0.75, na.rm=TRUE)
  q25 <- quantile(x, probs=0.25, na.rm=TRUE)
  iqr <- q75-q25
  LI  <- q25 - iqr*1.5
  LS  <- q75 + iqr*1.5
  idx <- which(x < LI | x > LS)

  # Message
  if(length(idx)>0){
    out.val <- x[idx]
    msg <- paste("Outliers: ", paste(out.val, collapse=", "), sep="")
  } else {
    msg <- "No outliers according to IQR rule!"
  }
  cat(paste("\n", as.character(method),"\n\n", msg, "\n"))

  # Results vector
  RVAL <- list(q25, q75, iqr, LI, LS, out.val, out.ind=idx, method=method,
               alternative=NULL, p.value=NULL, data.name=DNAME)
  names(RVAL)[1:5] <- c("q25", "q75", "iqr", "LI", "LS")
  names(RVAL)[6:7] <- c("outliers", "index")

  class(RVAL) <- "htest"
  return(RVAL)
}

#-------------------------------------------------------#
# fun_outlier(): Function to customize outliers outputs #
#-------------------------------------------------------#
fun_outlier <- function (x, x.data, language="PT", alpha=0.05){

  # create output vector
  res <- list(out.ind=NULL)
  res$pval        <- as.numeric(x$p.value)
  res$test.name   <- x$method
  res$test.stat   <- paste(names(x$statistic), round(as.numeric(x$statistic), 4), sep="=", collapse=", ")
  res$out <- ifelse(language=="PT", "Nenhum outlier sugerido", "No suggested outliers")

  if(!is.null(x$p.value)){

    if(x$p.value < alpha){
      tmp0 <- suppressWarnings(  as.numeric(unlist(strsplit(x$alternative, " ")))  )
      tmp <- tmp0[!is.na(tmp0)]

      res$out.ind <- which(x.data %in% tmp)
      res$out     <- paste(tmp, collapse=", ")
    }

  }

  # Translate test names
  if(language=="PT"){
    res$test.name <- paste0("Teste de ",
                            switch(x$method,
                                   "Interquartile rule for outlier detection" = "Intervalo Interquartil",
                                   "Grubbs test for one outlier"              = "Grubbs para 1 outlier",
                                   "Grubbs test for two outliers"             = "Grubbs para 2 outliers",
                                   "Grubbs test for two opposite outliers"    = "Grubbs 2 outliers (lados opostos)",
                                   "Dixon test for outliers"                  = "Dixon para outliers",
                                   "chi-squared test for outlier"             = "Qui-quadrado para outliers")
    )
  }

  # Tabela
  if(x$method=="Interquartile rule for outlier detection"){
    tab_test <- data.frame(Parameter=NA, Value=c(x$q25, x$q75, x$iqr, x$LI, x$LS, res$out))
    if(language=="PT"){
      tab_test$Parameter <- c("1o Quartil - Q1 (25%)", "3o Quartil - Q3 (75%)",
                           "Amplitude Interquartil (IIQ=Q3-Q1)", "Limite Inferior (Q1-1.5*IQR)",
                           "Limite Superior (Q3+1.5*IQR)", "Outlier(s)")
      names(tab_test) <- c("Parâmetro", "Valor")
    } else {
      tab_test$Parameter <- c("Parameter", "1st Quartile - Q1 (25%)", "3rd Quartile - Q3 (75%)",
                           "interquartile range (IQR=Q3-Q1)", "Lower Limit (Q1-1.5*IQR)",
                           "Upper Limit (Q3+1.5*IQR)", "Outlier(s)")
    }
  } else {
    tab_test <- data.frame(Parameter=NA, Value=c(res$test.stat, res$pval, res$out))
    if(language=="PT"){
      tab_test$Parameter <- c("Estatística do teste", "P-valor", "Outlier(s)")
      names(tab_test) <- c("Parâmetro", "Valor")
    } else {
      tab_test$Parameter <- c("Statistic", "P-value", "Outlier(s)")
    }
  }

  res$tab_test <- tab_test

  # Return
  return(invisible(res))

}

# Tests
# library(outliers)
# xx <- c(-7, -5, rnorm(10), 2, NA)
# xx <- c(44.7, 46.4593, 47, 50.62, 15.6863, 41.2, 47.82, 49, 43.79, 46, 41, 48, 45.1)
#
# x <- grubbs.test(x=xx, type=20) ; x$method
# str(fun_outlier(x, x.data=xx))
# fun_outlier(x, x.data=xx)$tab_test
#
# x <- IQR.test(x=xx) ; x$method ; x$p.value
# str(fun_outlier(x, x.data=xx))
# fun_outlier(x, x.data=xx)$tab_test
#
# x <- grubbs.test(x=xx, type=20) ; x$method
# str(fun_outlier(x, x.data=xx))
#
# x <- grubbs.test(x=xx, type=11) ; x$method
# str(fun_outlier(x, x.data=xx))
#
# x <- dixon.test(x=xx, type=0)   ; x$method
# str(fun_outlier(x, x.data=xx))
#
# x <- chisq.out.test(x=xx)       ; x$method
# str(fun_outlier(x, x.data=xx))
# fun_outlier(x, x.data=xx)$tab_test
