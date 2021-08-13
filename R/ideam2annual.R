#' function for calculate monthly values of prepared data from DHIME (IDEAM, Colombia)
#'
#' function for calculate monthly values of prepared data from DHIME (IDEAM, Colombia),
#' apply to precipitation, minimum and maximum temperature.
#' @param x dataframe of prepared data of a station from DHIME (IDEAM, Colombia)
#' @param param default value is sum, sum is for precipitation and evaporation, mean for temperature and relative humidity, max for maximum values.
#' @param na.rm default value is TRUE, TRUE for consider NA, FALSE for not consider NA.
#' @import stats
#' @export
#'
#' @author Geomar Perales Apaico
#'
#' @examples
#' data <- ideamprep(Bucaramanga)
#' ideam2annual(data)
#'
#' @name ideam2annual

ideam2annual <-function(x, ...) UseMethod("ideam2annual")

ideam2annual <- function(x, param = NULL, na.rm = TRUE){

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
  } else if(param == "max"){
    opt = "max"
  } else if(is.na(match(param, c("sum","mean")))){
    stop("parameter not recognized")
  } else{
    stop("parameter not recognized")
  }

  date = strftime(x$date, "%Y")

  if(isTRUE(na.rm)){
    x$values <- ifelse(is.na(x$values), 0, x$values)
    annual.sum <- aggregate(as.numeric(as.vector(x$values)), by = list(date), FUN = opt)
    annual.sum <- data.frame(annual.sum, stringsAsFactors = FALSE)
    colnames(annual.sum) <- c("date","values")
    return(annual.sum)

  } else if(!isTRUE(na.rm)){
    annual.sum <- aggregate(as.numeric(as.vector(x$values)), by = list(date), FUN = opt)
    colnames(annual.sum) <- c("date","values")
    return(annual.sum)
  }

}

#' @rdname ideam2annual
