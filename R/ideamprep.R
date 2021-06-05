#' function for prepare data from DHIME (IDEAM, Colombia)
#'
#' function for prepare data from DHIME (IDEAM, Colombia), apply to precipitation,
#' minimum and maximum temperature.
#' @param x dataframe of a station from DHIME (IDEAM, Colombia).
#' @param level prepared information level of station data, default information is standard. standard level:
#' dataframe of date, station, parameter and parameter values, basic level : dataframe of date and
#' parameter values. advanced level: dataframe of date, station, parameter, latitude, longitude,
#' height and parameter values
#' @export
#' @name ideamprep

ideamprep <- function(x, level = NULL){

  if(!is.data.frame(x)){
    stop("values not recognized")
  }
  if(!isTRUE("Fecha" %in% names(x))){
    stop("Fecha column not recognized or not exist")
  }
  if(!isTRUE("Valor" %in% names(x))){
    stop("valor column not recognized or not exist")
  }

  ini.yr <- as.Date(substr(min(as.character(x$Fecha)), 1, 10))
  end.yr <- as.Date(substr(max(as.character(x$Fecha)), 1, 10))
  date <- data.frame(date = seq.Date(ini.yr, end.yr, by = "days"))

  if(is.null(level)){
    data <- data.frame(date = as.Date(substr(x$Fecha, 1, 10)),
                       station = as.character(x$NombreEstacion[1]),
                       param = as.character(x$IdParametro[1]), values = x$Valor)
    data <- merge(data, date, by = "date", all = TRUE)
    return(data)

  } else if(level == "standard"){

    if(!isTRUE("NombreEstacion" %in% names(x))){
      stop("NombreEstacion column not recognized or not exist, standard and advanced level is not possible")
    }
    if(!isTRUE("IdParametro" %in% names(x))){
      stop("IdParametro column not recognized or not exist, standard and advanced level is not possible")
    }

    data <- data.frame(date = as.Date(substr(x$Fecha, 1, 10)),
                       station = as.character(x$NombreEstacion[1]),
                       param = as.character(x$IdParametro[1]), values = x$Valor)
    data <- merge(data, date, by = "date", all = TRUE)
    return(data)

  } else if(level == "basic"){
    data <- data.frame(date = as.Date(substr(x$Fecha, 1, 10)), values = x$Valor)
    data <- merge(data, date, by = "date", all = TRUE)
    return(data)

  } else if(level == "advanced"){

    if(!isTRUE("Latitud" %in% names(x))){
      stop("Latitud column not recognized or not exist, advanced level is not possible")
    }
    if(!isTRUE("Longitud" %in% names(x))){
      stop("Longitud column not recognized or not exist, advanced level is not possible")
    }
    if(!isTRUE("Altitud" %in% names(x))){
      stop("Altitud column not recognized or not exist, advanced level is not possible")
    }

    data <- data.frame(date = as.Date(substr(x$Fecha, 1, 10)),
                       station = as.character(x$NombreEstacion[1]),
                       param = as.character(x$IdParametro[1]),
                       lat = as.numeric(x$Latitud[1]),
                       lon = as.numeric(x$Longitud[1]),
                       altitud = as.numeric(x$Altitud[1]),
                       values = x$Valor)
    data <- merge(data, date, by = "date", all = TRUE)
    return(data)

  } else if(is.na(match(level, c("standard", "basic", "advanced")))){
    stop("level not recognized")
  } else {
    stop("level not recognized")
  }
}

#' @rdname ideamprep
