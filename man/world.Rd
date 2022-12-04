\name{world}
\alias{world}

\title{Administrative boundaries}

\description{
Get the borders for all the countries in the world. Data are read from files that are downloaded if necessary.
}

\usage{
world(resolution=5, level=0, path, version="latest", ...)
}

\arguments{
  \item{resolution}{integer between 1 and 5 indicating the level of detail. 1 is high 5 is low}
  \item{level}{numeric. The level of administrative subdivision requested. (starting with 0 for country, then 1 for the first level of subdivision). Only level 0 is currently available}
  \item{path}{character. Path for storing the downloaded data. See \code{\link{geodata_path}}}
  \item{version}{character. Only "3.6" is currently supported}
  \item{...}{additional arguments passed to \code{\link{download.file}}}
}

\value{
SpatVector
}

\seealso{\code{\link{gadm}}}

\details{
The data are from \url{https://gadm.org}
}

\examples{
\donttest{
w <- world(path=tempdir())
}
}

