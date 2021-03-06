#' function for calculate monthly values of prepared data from DHIME (IDEAM, Colombia)
#'
#' function for calculate monthly values of prepared data from DHIME (IDEAM, Colombia),
#' apply to precipitation, minimum and maximum temperature.
#' @param x dataframe of prepared data of a station from DHIME (IDEAM, Colombia).
#' @param param default value is sum, sum is for precipitation and evaporation, mean for temperature and relative humidity, max for maximum values.
#' @param na.rm default value is TRUE, TRUE for consider NA, FALSE for not consider NA.
#' @import stats
#' @export
#'
#' @author Geomar Perales Apaico
#'
#' @examples
#' data <- ideamprep(Bucaramanga)
#' ideam2monthly(data)
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
  } else if(param == "max"){
    opt = "max"
  } else if(is.na(match(param, c("sum", "mean", "max")))){
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

    ini.yr <- as.Date(paste0(min(as.character(values.sum$date)), "-01-01"))
    end.yr <- as.Date(paste0(max(as.character(values.sum$date)), "-12-01"))
    values.sum$date <- data.frame(date = seq.Date(ini.yr, end.yr, by = "month"))

    return(values.sum)

  } else if(!isTRUE(na.rm)){
    values.sum <- aggregate(as.numeric(as.vector(x$values)), by = list(date), FUN = opt)
    colnames(values.sum) <- c("date","values")

    ini.yr <- as.Date(paste0(min(as.character(values.sum$date)), "-01-01"))
    end.yr <- as.Date(paste0(max(as.character(values.sum$date)), "-12-01"))
    values.sum[,1] <- data.frame(date = seq.Date(ini.yr, end.yr, by = "month"))

    return(values.sum)
  }}


#' @rdname ideam2monthly
