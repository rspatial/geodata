\name{population}

\alias{population}

\title{population density}

\description{
Download population density data.

Source: Gridded Population of the World (GPW), v4
Documentation: http://sedac.ciesin.columbia.edu/data/collection/gpw-v4/documentation

}

\usage{
population(year, res, path, ...)
}

\arguments{
  \item{year}{numeric. One of 2000, 2005, 2010, 2015, 2020}
  \item{res}{numeric. Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a degree)}
  \item{path}{character. Path where to download the data to} 
  \item{...}{additional arguments passed to \code{\link{download.file}}}  
}

\value{
SpatRaster
}

\references{
Center for International Earth Science Information Network - CIESIN - Columbia University. 2018. Gridded Population of the World, Version 4 (GPWv4): Population Density, Revision 11. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H49C6VHW. Accessed 6 July 2021.
}


\examples{
\donttest{
pop <- population(2020, 10, path=tempdir())
}
}

