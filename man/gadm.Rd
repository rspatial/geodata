\name{gadm}
\alias{gadm}
\alias{world}

\title{Administrative boundaries}

\description{
Get administrative boundaries for any country in the world. Data are read from files that are downloaded if necessary.
}

\usage{
gadm(country, level=1, path, version=3.6)
world(resolution=5, level=0, path, version=3.6)
}

\arguments{
  \item{country}{character. Three-letter ISO code or full country name}
  \item{level}{numeric. level of administrative subdivision (starting with 0 for country, then 1 for the first level of subdivision). For \code{world} only level 0 is currently avaialable}
  \item{path}{character. Path name indicating where to store the data}
  \item{version}{numeric. GADM version number (only 3.6 or higer)}
  \item{resolution}{integer between 1 and 5 indicating the level of detail. 1 is high 5 is low}
}

\value{
SpatVector
}


\details{
These data are from the \url{https://www.gadm.org}
}

\examples{
\donttest{
fra <- gadm(country='FRA', level=1, path=tempdir())
w <- world(path=tempdir())
}
}

