\name{climate}
\alias{climate}

\title{WorldClim climate data}


\usage{
worldclim_global(var, res, path, ...)
wordclim_country(country, var, ...)
wordclim_tile(var, lon, lat, path, ...)
}

\arguments{
  \item{var}{character. Valid variables names are "tmin", "tmax", "tavg", "prec" and "bio"}
  \item{res}{numeric. Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a degree)}
  \item{path}{character. Path where to download the data to} 
  \item{country}{chraracter. Country name or code} 
  \item{lon}{numeric. Longitude} 
  \item{lat}{numeric. Latitude} 
  \item{...}{additional arguments (none implemented)}  
}

\value{
SpatRaster
}


\examples{
\dontrun{
# To get current conditions:
worldclim(var="tmin", res=10)
}
}

\seealso{
\url{http://www.worldclim.org/} 
}

