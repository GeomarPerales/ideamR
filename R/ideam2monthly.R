#' function for calculate monthly values of prepared data from DHIME (IDEAM, Colombia)
#'
#' function for calculate monthly values of prepared data from DHIME (IDEAM, Colombia),
#' apply to precipitation, minimum and maximum temperature.
#' @param x dataframe of prepared data of a station from DHIME (IDEAM, Colombia).
#' @param param default value is sum, sum is for precipitation and evaporation, mean for temperature and relative humidity.
#' @param na.rm default value is TRUE, TRUE for consider NA, FALSE for not consider NA.
#' @import stats
#' @export
#'
#' @author Geomar Perales Apaico
#'
#' @name ideam2monthly

ideam2monthly <-function(x, ...) UseMethod("ideam2monthly")

ideam2monthly <- function(x, param = NULL, na.rm = TRUE){

  colnames(data) <- c("date", "values")
  x <- data.frame(x, stringsAsFactors = FALSE)
  if(is.null(x)){
    stop("values not recognized")
  }

  if(is.null(param)){
    opt = "sum"
  } else if(param == "sum"){
    opt = "sum"
  } else if(param == "mean"){
    opt = "mean"
  } else if(is.na(match(param, c("sum","mean")))){
    stop("parameter not recognized")
  } else{
    stop("parameter not recognized")
  }

  date <- strftime(x$date, "%Y-%m")

  if(isTRUE(na.rm)){
    x$values <- ifelse(is.na(x$values), 0, x$values)
    values.sum <- aggregate(as.numeric(as.vector(x$values)), by = list(date), FUN = opt)
    values.sum <- data.frame(values.sum, stringsAsFactors = FALSE)
    colnames(values.sum) <- c("date","values")
    return(values.sum)

  } else if(!isTRUE(na.rm)){
    values.sum <- aggregate(as.numeric(as.vector(x$values)), by = list(date), FUN = opt)
    colnames(values.sum) <- c("date","values")
    return(values.sum)
  }

}

#' @rdname ideam2monthly
