# ideamR

ideamR is a package for hydrometeorological data management from DHIME (IDEAM, Colombia). DHIME is an Information System for the management of Hydrological and Meteorological data from Colombia. functions from ideamR are for precipitation, maximum and minimum temperature.

A goal from ideamR is meteorological data preparation for use it in R.

## Installation

You can install the released version of ideamR from Github with:

Step 1: Install devtools from CRAN:
```	
> install.packages("devtools")
```
Step 2: Install ideamR from Github:

``` r
> library(devtools)
> install_github("GeomarPerales/ideamR")
```

## Example

In this example, I use daily precipitation data from Bucaramanga (Santander, Colombia) station for run ideamR.

```	
library(ideamR)
data("Bucaramanga")

#Bucaramanga station information
station.info <- ideaminfo(Bucaramanga)
head(station.info)

#Bucaramanga station localization (coordinates and elevation)
station.loc <- ideamloc(Bucaramanga)
head(station.loc)

#data summary from Bucaramanga station  
station.summary <- ideamsummary(Bucaramanga)
head(station.summary)

#data preparation from Bucaramanga station
data.prep <- ideamprep(Bucaramanga)
head(data.prep)

# monthly time serie from bucaramanga station 
monthly.ts <- ideam2monthly(data.prep)
head(monthly.ts)

# annual time serie from bucaramanga station  
annual.ts <- ideam2annual(data.prep)
head(annual.ts)

#information about NA' from monthly time serie of Bucaramanga station 
data.na <- ideamna(data.prep)
```
## Credits

ideamR was developed by Geomar Perales Apaico. or any issue or suggestion please write
to: perales.geomar@gmail.com.
