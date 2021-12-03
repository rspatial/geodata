\name{crop_calendar_sacks}

\alias{crop_calendar_sacks}


\title{Sachs crop calendar data}

\description{
Sachs crop calendar data
}

\usage{
crop_calendar_sacks(crop="", path, ...)
}

\arguments{
 \item{crop}{character. Crop name. See \code{monfredaCrops} for valid names}  
 \item{path}{character. Path name where the data should be downloaded to}  
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
\donttest{
cas <- crop_calendar_sacks("cassava", path=tempdir())
}
}
