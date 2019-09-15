\name{elevation}
\alias{elevation}

\title{Elevation}

\usage{
elevation(country, res='1km', download=TRUE, path, ...)
}

\arguments{
  \item{country}{Character. Three-letter ISO code or full country name}
  \item{res}{Character. Use '90m' or '1km'.}
  \item{download}{Logical. If \code{TRUE} data will be downloaded if not locally available}
  \item{path}{Character. Path name indicating where to store the data. Default is the current working directory}
  \item{...}{Additional arguments (none implemented)}  
}

\value{
RasterLayer
}

\description{
Get elevation data for any country in the world. The main data source is Shuttle Radar Topography Mission (SRTM) , specifically the hole-filled CGIAR-SRTM (90 m resolution) \url{http://srtm.csi.cgiar.org/}. These data are only available for latitudes between -60 and 60. 

The 1 km (30 arc seconds) data were aggregated from SRTM 90 m resolution data and supplemented with the GTOP30 data for high latitudes (>60 degrees). 
}

\examples{
\dontrun{
elevation(country='FRA', res='1km')
}
}

\author{
Robert Hijmans, Aniruddha Ghosh, Alex Mandel
}
