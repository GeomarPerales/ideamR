#' function for calculate days without information by month of prepared data of station from DHIME (IDEAM, Colombia)
#'
#' function for calculate days without information by month of prepared data of station from DHIME (IDEAM, Colombia),
#' apply to precipitation, minimum and maximum temperature.
#' @param x dataframe of prepared data of a station from DHIME (IDEAM, Colombia).
#' @param type default value is NULL, NULL is for days without information by month, inv is for days with information by month.
#' @import stats
#' @export
#' @name ideamna

ideamna <- function(x, type = NULL){

  if(!is.data.frame(x)){
    stop("values not recognized")
  }

  colnames(x) <- c("date","values")
  x$value[x$value >= 0] <- 1
  x$value[is.na(x$value)] <- 0
  date <- strftime(x$date, "%Y-%m")
  info <- aggregate(as.numeric(as.vector(x$value)), by = list(date), FUN = sum)
  colnames(info) <- c("date","values")

  ini.yr <- as.numeric(substr(min(as.character(x$date)), 1, 4))
  end.yr <- as.numeric(substr(max(as.character(x$date)), 1, 4))
  n.yrs <- end.yr - ini.yr + 1

  matrix.info <- t(matrix(info$value, 12, n.yrs))
  matrix.info <- data.frame(matrix.info)
  colnames(matrix.info) <- month.abb
  rownames(matrix.info) <- seq(ini.yr,end.yr)

  if(is.null(type)){
    return(matrix.info)

  }else if(type == "inv"){
    days.date <- seq.Date(as.Date(paste0(ini.yr,"-01-01")), as.Date(paste0(end.yr,"-12-31")), by = "days")
    days.sum <-  data.frame(date = days.date, value = 1)
    sum <- aggregate( as.numeric(as.vector(days.sum$value)), by = list(date), FUN = sum)
    colnames(sum) <- c("date","values")
    matrix.info <- t(matrix(sum$value, 12, n.yrs)) - t(matrix(info$value, 12, n.yrs))
    matrix.info <- data.frame(matrix.info)
    colnames(matrix.info) <- month.abb
    rownames(matrix.info) <- seq(ini.yr,end.yr)
    return(matrix.info)
  }
}

#' @rdname ideamna
