#' function for calculate monthly values of prepared data from DHIME (IDEAM, Colombia)
#'
#' function for calculate monthly values of prepared data from DHIME (IDEAM, Colombia),
#' apply to precipitation, minimum and maximum temperature.
#' @param x dataframe of prepared data of a station from DHIME (IDEAM, Colombia)
#' @param param default value is sum, sum is for precipitation, mean for temperature.
#' @param na.rm default value is TRUE, TRUE for consider NA, FALSE for not consider NA.
#' @import stats
#' @export
#' @name ideam2annual

ideam2annual <-function(x, ...) UseMethod("ideam2annual")

ideam2annual <- function(x, param = NULL, na.rm = TRUE){

  colnames(x) <- c("date", "values")

  if(is.null(x)){
    stop("values not recognized")
  }

  if(is.null(param)){
    opt = "sum"

  } else if(param == "sum"){
    opt = "sum"

  } else if(param == "mean"){
    opt = "mean"
  } else {
    stop("variable not recognized")
  }

  date = strftime(x$date, "%Y")
  if(isTRUE(na.rm)){
    x[is.na(x$values),][,2] <- 0
    annual.sum <- aggregate( as.numeric(as.vector(x$values)), by = list(date), FUN = opt)
  } else if(!isTRUE(na.rm)){
    annual.sum <- aggregate( as.numeric(as.vector(x$values)), by = list(date), FUN = opt)
  }

  colnames(annual.sum) <- c("date","values")
  return(annual.sum)

}

#' @rdname ideam2annual
