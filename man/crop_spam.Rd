\name{crop_spam}

\alias{crop_spam}
\alias{spamCrops}


\title{SPAM crop data}

\description{
SPAM crop data
}

\usage{
spamCrops()
crop_spam(crop="", var="area", path=".", africa=FALSE, ...)
}

\arguments{
 \item{crop}{character. See \code{spamCrops} for valid names}  
 \item{var}{character. variable of interest. Must be one of "yield", "harv_area" (harvested area), "phys_area" (physical area), "prod" (production) or "val_prod" (value of production)}  
 \item{path}{character. Path for storing the downloaded data. See \code{\link{geodata_path}}}  
 \item{africa}{logical. retrieve the (updated) data for Africa instead of global data}  
  \item{...}{additional arguments passed to \code{\link{download.file}}}
}

\value{
SpatRaster
}

\references{
International Food Policy Research Institute, 2020. Spatially-Disaggregated Crop Production Statistics Data in Africa South of the Sahara for 2017. https://doi.org/10.7910/DVN/FSSKBW, Harvard Dataverse, V2
}

\seealso{
https://www.mapspam.info/data/
}


\examples{
\donttest{
cas <- crop_spam("cassava", "area", path=tempdir(), TRUE)
}
}
