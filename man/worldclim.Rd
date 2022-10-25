\name{worldclim}

\alias{worldclim_global}
\alias{worldclim_country}
\alias{worldclim_tile}

\title{WorldClim climate data}

\description{
Download climate data from WorldClim version 2.1
}

\usage{
worldclim_global(var, res, path, version="2.1", ...)
worldclim_country(country, var, path, version="2.1", ...)
worldclim_tile(var, lon, lat, path, version="2.1", ...)
}

\arguments{
  \item{var}{character. Valid variables names are "tmin", "tmax", "tavg", "prec" and "bio"}
  \item{res}{numeric. Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a degree)}
  \item{path}{character. Path where to download the data to} 
  \item{country}{character. Country name or code} 
  \item{lon}{numeric. Longitude} 
  \item{lat}{numeric. Latitude} 
  \item{version}{character or numeric. WorldClim version number. Only "2.1" supported at the moment} 
  \item{...}{additional arguments passed to \code{\link{download.file}}}  
}

\value{
SpatRaster
}


\examples{
\donttest{
lux <- worldclim_country("Luxembourg", var="tmin", path=tempdir())
}
}

\seealso{
\url{https://www.worldclim.org/} 
}

