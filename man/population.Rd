\name{population}

\alias{population}

\title{population density}

\description{
Download population density data.

Source: Gridded Population of the World (GPW), v4
Documentation: http://sedac.ciesin.columbia.edu/data/collection/gpw-v4/documentation

}

\usage{
population(year, res, path)
}

\arguments{
  \item{year}{numeric. One of 2000, 2005, 2010, 2015, 2020}
  \item{res}{numeric. Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a degree)}
  \item{path}{character. Path where to download the data to} 
}

\value{
SpatRaster
}


\examples{
\donttest{
pop <- population(2020, 10, path=tempdir())
}
}

