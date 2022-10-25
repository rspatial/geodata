\name{worldclim}

\alias{worldclim_global}
\alias{worldclim_country}
\alias{worldclim_tile}

\title{WorldClim climate data}

\description{
Download climate data from WorldClim version 2.1. See Details for variables and units.
}

\usage{
worldclim_global(var, res, path, version="2.1", ...)
worldclim_country(country, var, path, version="2.1", ...)
worldclim_tile(var, lon, lat, path, version="2.1", ...)
}

\arguments{
  \item{var}{character. Valid variables names are "tmin", "tmax", "tavg", "prec", "wind", "vapr", and "bio"}
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

\details{
These are the WorldClim monthly average climate data.

\tabular{lllr}{
\tab \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \cr
\tab\code{tmin} \tab minimum temperature \tab °C \cr
\tab\code{tmax} \tab maximum temperature \tab °C \cr
\tab\code{tavg} \tab average temperature \tab °C \cr
\tab\code{prec} \tab total precipitation \tab mm \cr
\tab\code{srad} \tab incident solar radiation \tab 
\ifelse{latex}{\out{$kJ \cdot m^{-2} \cdot day^{-1}$}}{\out{kJ m<sup>-2</sup> day<sup>-1</sup>}} \cr
\tab\code{wind} \tab wind speed (2 m above the ground) \tab 
\ifelse{latex}{\out{$m \cdot  s^{-1}$}}{\out{m<sup>-1</sup>}} \cr
\tab\code{vapr} \tab vapor pressure \tab kPa \cr
}
}

\examples{
\donttest{
lux <- worldclim_country("Luxembourg", var="tmin", path=tempdir())
}
}

\seealso{
\url{https://www.worldclim.org/} 
}

