\name{crop_calendar_sacks}

\alias{crop_calendar_sacks}
\alias{sacksCrops}


\title{Sacks crop calendar data}

\description{
Download Sacks crop calendar data. The crops available are returned by \code{sacksCrops} 
}

\usage{
crop_calendar_sacks(crop="", path, ...)

sacksCrops()
}

\arguments{
 \item{crop}{character. Crop name. See \code{sacksCrops} for valid names}  
 \item{path}{character. Path for storing the downloaded data. See \code{\link{geodata_path}}}  
  \item{...}{additional arguments passed to \code{\link{download.file}}} 
}

\value{
SpatRaster
}

\references{
Sacks, W.J., D. Deryng, J.A. Foley, and N. Ramankutty, 2010. Crop planting dates: an analysis of global patterns. Global Ecology and Biogeography 19: 607-620. doi:10.1111/j.1466-8238.2010.00551.x.
}

\seealso{
\url{https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset/}
}


\examples{
cas <- crop_calendar_sacks("cassava", path=tempdir())
}
