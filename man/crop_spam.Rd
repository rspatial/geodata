\name{crop_spam}

\alias{crop_spam}
\alias{spamCrops}


\title{SPAM crop data}

\description{
SPAM crop data
}

\usage{
spamCrops()
crop_spam(crop="", var="area", path=".", africa=FALSE)
}

\arguments{
 \item{crop}{character. See \code{spamCrops} for valid names}  
 \item{var}{character. variable of interest. Must be "area" or "yield"}  
 \item{path}{character. Path name where the data should be downloaded to}  
 \item{africa}{logical. retrieve the (updated) data for Africa instead of global data}  
}

\value{
SpatRaster
}

\references{
International Food Policy Research Institute, 2020. Spatially-Disaggregated Crop Production Statistics Data in Africa South of the Sahara for 2017. https://doi.org/10.7910/DVN/FSSKBW, Harvard Dataverse, V2
}

\seealso{
\url{https://www.mapspam.info/data/}
}


\examples{
\donttest{
cas <- crop_spam("cassava", "area", path=tempdir(), TRUE)
}
}
