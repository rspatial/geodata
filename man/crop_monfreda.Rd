\name{crop_monfreda}

\alias{crop_monfreda}
\alias{monfredaCrops}


\title{Monfreda crop data}

\description{
Monfreda global crop data (area, yield) for 175 crops
}

\usage{
monfredaCrops()
crop_monfreda(crop="", path, ...)
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
Monfreda, C., N. Ramankutty, and J. A. Foley (2008), Farming the planet: 2. Geographic distribution of crop areas, yields, physiological types, and net primary production in the year 2000, Global Biogeochem. Cycles, 22, GB1022, doi:10.1029/2007GB002947.
}

\seealso{
\url{http:://www.earthstat.org/harvested-area-yield-175-crops/}
}


\examples{
\donttest{
mcas <- crop_monfreda("cassava", path=tempdir())
mcas
names(mcas)
}
}
