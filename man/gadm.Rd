\name{gadm}
\alias{gadm}

\title{Administrative boundaries}

\description{
Get administrative boundaries for any country in the world. Data are read from files that are downloaded if necessary.
}

\usage{
gadm(country, level=1, path, version="latest", resolution=1, ...)
}

\arguments{
  \item{country}{character. Three-letter ISO code or full country name. If you provide multiple names they are all downloaded and \code{rbind}-ed together }
  \item{level}{numeric. The level of administrative subdivision requested. (starting with 0 for country, then 1 for the first level of subdivision)}
  \item{path}{character. Path name indicating where to store the data}
  \item{version}{character. Either "latest" or GADM version number (can be "3.6", "4.0" or "4.1")}
  \item{resolution}{integer indicating the level of detail. Only for version 4.1. It should be either 1 (high) or 2 (low)}
  \item{...}{additional arguments passed to \code{\link{download.file}}}
}

\seealso{\code{\link{world}}}


\value{
SpatVector
}


\details{
The data are from \url{https://gadm.org}
}

\examples{
\donttest{
fra <- gadm(country="FRA", level=1, path=tempdir())
}
}

