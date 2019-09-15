\name{gadm}
\alias{gadm}

\title{Administrative boundaries for a country}

\usage{
gadm(country, level=1, path, version=3.6, ...)
}

\arguments{
  \item{country}{Character. Three-letter ISO code or full country name}
  \item{level}{Numeric. level of administrative subdivision (starting with 0 for country, then 1 for the first level of subdivision)}
  \item{path}{Character. Path name indicating where to store the data}
  \item{...}{Additional arguments (none implemented)}
}

\value{
SpatVector
}

\description{
Get administrative boundaries for any country in the world. Data are read from files that are first downloaded if necessary.
}

\details{
These data are from the \url{www.gadm.org}
}

\examples{
\dontrun{
gadm(country='FRA', level=1)
}
}

