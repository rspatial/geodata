\name{osm}
\alias{osm}

\title{OpenStreetMap data}

\description{
Get OpenStreetMap (OSM) data 
}

\usage{
osm(country, var, path, proxy=FALSE, ...)
}

\arguments{
  \item{country}{character. Three-letter ISO code or full country name}
  \item{var}{character. Currently it can be one of "places", "highways", or "railway"}
  \item{path}{character. Path for storing the downloaded data. See \code{\link{geodata_path}}}
  \item{proxy}{logical. Return a SpatVectorProxy?}
  \item{...}{additional arguments passed to \code{\link{download.file}}}
}

\value{
SpatVector
}


\details{
License: Open Data Commons Open Database License (ODbL). 

See \url{https://www.openstreetmap.org/copyright}
}

\examples{
aruba <- osm(country="Aruba", "places", path=tempdir())
}

