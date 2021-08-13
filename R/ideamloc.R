#' function for obtain location of data downloaded of station from DHIME (IDEAM, Colombia)
#'
#' function for obtain location of data downloaded of station from DHIME (IDEAM, Colombia),
#' apply to precipitation, minimum and maximum temperature.
#' @param x dataframe of a station from DHIME (IDEAM, Colombia).
#'
#' @author Geomar Perales Apaico
#'
#' @examples
#' ideamloc(Bucaramanga)
#'
#' @export
#' @name ideamloc

ideamloc <- function(x){

  if(!is.data.frame(x)){
    stop("values not recognized")
  }

  data <- data.frame(matrix(NA, 1, 3))
  colnames(data) <- c("Longitud", "Latitud", "Altitud")
  if("Longitud" %in% names(x)){
    data[1] <- unique(x$Longitud)
  }else if(!"Longitud" %in% names(x)){
    data[1] <- NA
  }

  if("Latitud" %in% names(x)){
    data[2] <- unique(x$Longitud)
  }else if(!"Latitud" %in% names(x)){
    data[2] <- NA
  }

  if("Altitud" %in% names(x)){
    data[3] <- unique(x$Altitud)
  }else if(!"Altitud" %in% names(x)){
    data[3] <- NA
  }

  return(data)
}

#' @rdname ideamloc
