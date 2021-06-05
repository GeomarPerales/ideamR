#' function for obtain information about data downloaded of station from DHIME (IDEAM, Colombia)
#'
#' function for obtain information about data downloaded of station from DHIME (IDEAM, Colombia),
#' apply to precipitation, minimum and maximum temperature.
#' @param x dataframe of a station from DHIME (IDEAM, Colombia).
#' @export
#' @name ideaminfo

ideaminfo <- function(x){

  if(!is.data.frame(x)){
    stop("values not recognized")
  }

  info <- list()
  if("NombreEstacion" %in% names(x)){
    info$name <- unique(x$NombreEstacion)
  } else if(!"NombreEstacion" %in% names(x)){
    info$name <- NA
  }

  info$ubication <- data.frame(matrix(NA, 1, 3))
  colnames(info$ubication) <- c("Longitud","Latitud","Altitud")

  if("Longitud" %in% names(x)){
    info$ubication[1] <- unique(x$Longitud)
  }else if(!"Longitud" %in% names(x)){
    info$ubication[1] <- NA
  }

  if("Latitud" %in% names(x)){
    info$ubication[2] <- unique(x$Longitud)
  }else if(!"Latitud" %in% names(x)){
    info$ubication[2] <- NA
  }

  if("Altitud" %in% names(x)){
    info$ubication[3] <- unique(x$Altitud)
  }else if(!"Altitud" %in% names(x)){
    info$ubication[3] <- NA
  }

  info$location <- data.frame(matrix(NA, 1, 2))
  colnames(info$location) <- c("Departamento","Municipio")

  if("Departamento" %in% names(x)){
    info$location[1] <- unique(x$Departamento)
  }else if(!"Departamento" %in% names(x)){
    info$location[1] <- NA
  }

  if("Municipio" %in% names(x)){
    info$location[2] <- unique(x$Municipio)
  }else if(!"Municipio" %in% names(x)){
    info$location[2] <- NA
  }

  info$ParameterInfo <- data.frame(matrix(NA, 1, 3))
  colnames(info$ParameterInfo) <- c("Parametro","Frecuencia","Categoria")

  if("IdParametro" %in% names(x)){
    info$ParameterInfo[1] <- unique(x$IdParametro)
  }else if(!"IdParametro" %in% names(x)){
    info$ParameterInfo[1] <- NA
  }

  if("Frecuencia" %in% names(x)){
    info$ParameterInfo[2] <- unique(x$Frecuencia)
  }else if(!"Frecuencia" %in% names(x)){
    info$ParameterInfo[2] <- NA
  }

  if("Categoria" %in% names(x)){
    info$ParameterInfo[3] <- unique(x$Categoria)
  }else if(!"Categoria" %in% names(x)){
    info$ParameterInfo[3] <- NA
  }


  ini.yr <- as.Date(substr(min(as.character(x$Fecha)), 1, 10))
  end.yr <- as.Date(substr(max(as.character(x$Fecha)), 1, 10))
  date <- data.frame(date = seq.Date(ini.yr, end.yr, by = "days"))
  data <- data.frame(date = as.Date(substr(x$Fecha, 1, 10)), value = x$Valor)
  data <- merge(data, date, by = "date", all = TRUE)

  info$ParameterData <- data.frame(matrix(NA, 1, 4))
  colnames(info$ParameterData) <- c("Inicio","Final","Datos","NAs")
  info$ParameterData[1] <- as.Date(substr(min(as.character(x$Fecha)), 1, 10))
  info$ParameterData[2] <- as.Date(substr(max(as.character(x$Fecha)), 1, 10))
  info$ParameterData[3] <- sum(!is.na(data$value))
  info$ParameterData[4] <- sum(is.na(data$value))
  class(info) <- "IDEAM"
  return(info)
}

#' @rdname ideaminfo
