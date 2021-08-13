#' function for obtain information summary of data downloaded of station from DHIME (IDEAM, Colombia)
#'
#' function for obtain information summary of data downloaded of station from DHIME (IDEAM, Colombia),
#' apply to precipitation, minimum and maximum temperature.
#' @param x dataframe of a station from DHIME (IDEAM, Colombia).
#'
#' @author Geomar Perales Apaico
#'
#' @examples
#' ideamsummary(Bucaramanga)
#'
#' @export
#' @name ideamsummary

ideamsummary <- function(x){
  info <- list()

  info$ubication <- data.frame(matrix(NA, 1, 3))
  colnames(info$ubication) <- c("Longitud", "Latitud", "Altitud")
  if("Longitud" %in% names(x)){
    info$ubication[1] <- unique(x$Longitud)
  } else if(!"Longitud" %in% names(x)){
    info$ubication[1] <- NA
  }

  if("Latitud" %in% names(x)){
    info$ubication[2] <- unique(x$Longitud)
  } else if(!"Latitud" %in% names(x)){
    info$ubication[3] <- NA
  }

  if("Altitud" %in% names(x)){
    info$ubication[3] <- unique(x$Altitud)
  } else if(!"Altitud" %in% names(x)){
    info$ubication[3] <- NA
  }

  ini.yr <- as.Date(substr(min(as.character(x$Fecha)), 1, 10))
  end.yr <- as.Date(substr(max(as.character(x$Fecha)), 1, 10))
  date <- data.frame(date = seq.Date(ini.yr, end.yr, by = "days"))
  data <- data.frame(date = as.Date(substr(x$Fecha, 1, 10)), value = x$Valor)
  data <- merge(data, date, by = "date", all = TRUE)

  info$ParameterData <- data.frame(matrix(NA, 1, 4))
  colnames(info$ParameterData) <- c("Inicio", "Final", "Datos", "NAs")
  info$ParameterData[1] <- as.Date(substr(min(as.character(x$Fecha)), 1, 10))
  info$ParameterData[2] <- as.Date(substr(max(as.character(x$Fecha)), 1, 10))
  info$ParameterData[3] <- sum(!is.na(data$value))
  info$ParameterData[4] <- sum(is.na(data$value))
  return(info)
}

#' @rdname ideamsummary
