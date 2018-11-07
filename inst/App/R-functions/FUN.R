
#---------------------------------------------#
# IQR.test() - Interquartile Rule for Outlier #
#---------------------------------------------#
IQR.test <- function(x){

  if(any(is.na(x)))
    stop("x is missing values")
  if(!is.numeric(x))
    stop("x is not numeric")

  res <- list(method="Interquartile Rule for Outlier Detection",
              outliers=NA)

  q75 <- quantile(x, probs=0.75, na.rm=TRUE)
  q25 <- quantile(x, probs=0.25, na.rm=TRUE)
  iqr <- q75-q25
  LI  <- q25 - iqr*1.5
  LS  <- q75 + iqr*1.5
  idx <- which(x < LI | x > LS)

  if(length(idx)>0){
    res$outliers <- x[idx]
    msg <- paste("Outliers: ", paste(res$outliers, collapse=", "), sep="")
  } else {
    msg <- "No outliers according to IQR rule!"
  }

  cat(paste("\n", as.character(res$method),"\n\n", msg, "\n"))

  res$q25 <- q25
  res$q75 <- q75
  res$iqr <- iqr
  res$LI  <- LI
  res$LS  <- LS
  res$idx <- idx
  return(invisible(res))

}
