#' function for prepare data from DHIME (IDEAM, Colombia)
#'
#' function for prepare data from DHIME (IDEAM, Colombia), apply to precipitation,
#' minimum and maximum temperature.
#' @param x dataframe of a station from DHIME (IDEAM, Colombia).
#' @export
#'
#' @author Geomar Perales Apaico
#'
#' @examples
#' ideamprep(Bucaramanga)
#'
#' @name ideamprep

ideamprep <- function(x){

  if(!is.data.frame(x)){
    stop("values not recognized")
  }
  if(!isTRUE("Fecha" %in% names(x))){
    stop("Fecha column not recognized or not exist")
  }
  if(!isTRUE("Valor" %in% names(x))){
    stop("valor column not recognized or not exist")
  }

  ini.yr <- as.Date(paste0(substr(min(as.character(x$Fecha)),1,4),"-01-01"))
  end.yr <- as.Date(paste0(substr(max(as.character(x$Fecha)),1,4),"-01-01"))
  date <- data.frame(date = seq.Date(ini.yr, end.yr, by = "days"))

  data <- data.frame(date = as.Date(substr(x$Fecha, 1, 10)), values = x$Valor)
  data <- merge(data, date, by = "date", all = TRUE)
  data <- data.frame(data, stringsAsFactors = FALSE)
  colnames(data) <- c("date", "values")
  return(data)

}

#' @rdname ideamprep
