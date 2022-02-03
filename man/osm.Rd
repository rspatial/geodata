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
  \item{var}{character. Currently either "places" or "highways"}
  \item{path}{character. Path name indicating where to store the data}
  \item{proxy}{logical. Return a SpatVectorProxy?}
  \item{...}{additional arguments passed to \code{\link{download.file}}}
}

\value{
SpatVector
}


\details{
tbd

License: Open Data Commons Open Database License (ODbL). See \url{https://www.openstreetmap.org/copyright}
}

\examples{
\donttest{
aruba <- osm(country="Aruba", "places", path=tempdir())
}
}

