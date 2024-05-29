\name{cmip6}

\alias{cmip6_world}
\alias{cmip6_tile}

\title{CMIP6 climate model data}

\description{
Download downscaled and calibrated CMIP6 climate data for projected future climates. Either for the entire world or for a 30 degrees tile. For more information see \url{https://www.worldclim.org/} 

}

\usage{
cmip6_world(model, ssp, time, var, res, path, ...)

cmip6_tile(lon, lat, model, ssp, time, var, path, ...)
}

\arguments{
  \item{model}{character. Climate model abbreviation. One of "ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CanESM5", "CanESM5-CanOE", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", "HadGEM3-GC31-LL", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL"}
  \item{ssp}{character. A valid Shared Socio-economic Pathway code: "126", "245", "370" or "585"}
  \item{time}{character. A valid time period. One of "2021-2040", "2041-2060", or "2061-2080"} 
  \item{var}{character. Valid variables names are "tmin", "tmax", "tavg", "prec" and "bioc"}
  \item{res}{numeric. Valid resolutions are 10, 5, 2.5 and 0.5 (minutes of a degree). Silently ignored if provided to cmip6_tile(), available only at res=0.5}
  \item{path}{character. Path for storing the downloaded data. See \code{\link{geodata_path}}}   
  \item{...}{additional arguments passed to \code{\link{download.file}}}
  \item{lon}{numeric. Longitude} 
  \item{lat}{numeric. Latitude}   
}

\value{
SpatRaster
}


\seealso{
\code{\link[terra]{vrt}} to combine tiles
}


\examples{
\donttest{
bio10 <- cmip6_world("CNRM-CM6-1", "585", "2061-2080", 
					var="bioc", res=10, path=tempdir())
}
}

