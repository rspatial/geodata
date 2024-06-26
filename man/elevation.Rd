\name{elevation}

\alias{elevation_3s}
\alias{elevation_30s}
\alias{elevation_global}

\title{Elevation}

\description{
Elevation data for any country. The main data source is Shuttle Radar Topography Mission (SRTM) , specifically the hole-filled CGIAR-SRTM (90 m resolution) from https://srtm.csi.cgiar.org/. These data are only available for latitudes between -60 and 60. 

The 1 km (30 arc seconds) data were aggregated from SRTM 90 m resolution data and supplemented with the GTOP30 data for high latitudes (>60 degrees). 
}

\usage{
elevation_3s(lon, lat, path, ...)
elevation_30s(country, path, mask=TRUE, subs="", ...)
elevation_global(res, path, ...)
}

\arguments{
  \item{lon}{numeric. Longitude} 
  \item{lat}{numeric. Latitude} 
  \item{path}{character. Path for storing the downloaded data. See \code{\link{geodata_path}}} 
  \item{country}{character. Country name or code} 
  \item{mask}{logical. set grid cells outside of the country boundaries to NA} 
  \item{subs}{character}  
  \item{res}{numeric. Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a degree)}
  \item{...}{additional arguments passed to \code{\link{download.file}}}
}

\value{
SpatRaster
}


\examples{
be <- elevation_30s(country="BEL", path=tempdir() )
}

