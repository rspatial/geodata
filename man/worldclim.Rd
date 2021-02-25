\name{worldclim}

\alias{worldclim_global}
\alias{worldclim_country}
\alias{worldclim_tile}

\title{WorldClim climate data}

\description{
Download WorldClim data
}

\usage{
worldclim_global(var, res, path)
worldclim_country(country, var, path)
worldclim_tile(var, lon, lat, path)
}

\arguments{
  \item{var}{character. Valid variables names are "tmin", "tmax", "tavg", "prec" and "bio"}
  \item{res}{numeric. Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a degree)}
  \item{path}{character. Path where to download the data to} 
  \item{country}{character. Country name or code} 
  \item{lon}{numeric. Longitude} 
  \item{lat}{numeric. Latitude} 
}

\value{
SpatRaster
}


\examples{
\donttest{
lux <- worldclim_country("Luxembourg", var="tmin", res=10, path=".")
}
}

\seealso{
\url{http://www.worldclim.org/} 
}

