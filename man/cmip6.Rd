\name{cmip6}

\alias{cmip6_world}

\title{CMIP6 climate model data}

\description{
Download downscaled and callibrated CMIP6 climate data for projected future climates. 
}

\usage{
cmip6_world(model, ssp, time, var, res, path)
}

\arguments{
  \item{model}{character. Climate model abbrevation. One of "BCC-CSM2-MR", "CNRM-CM6-1", "CNRM-ESM2-1", "CanESM5", "GFDL-ESM4", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MRI-ESM2-0"}
  \item{ssp}{character. A valid Shared Socio-economic Pathway code: "126", "245", "370" or "585".}
  \item{time}{character. A valid  time period. One of "2021-2040", "2041-2060", or "2061-2080"} 
  \item{var}{character. Valid variables names are "tmin", "tmax", "tavg", "prec" and "bioc"}
  \item{res}{numeric. Valid resolutions are 10, 5, 2.5 (minutes of a degree)}
  \item{path}{character. Path where to download the data to} 
}

\value{
SpatRaster
}


\seealso{
\url{http://www.worldclim.org/} 
}


\examples{
\donttest{
bio10 <- cmip6_world("CNRM-CM6-1", "585", "2061-2080", var="bio", res=10, path=tempdir())
}
}

