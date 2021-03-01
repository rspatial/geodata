\name{sp_occurrence}

\alias{sp_occurrence}

\title{
Download species occurrence data from GBIF
}

\description{
This function downloads species occurence records from the Global Biodiversity Information Facility (\href{http://www.gbif.org}{GBIF}) data portal. You can download either a single species (if you append a '*' to the species name) or a subspecies of comparable level. You can download the data for an entire genus by using \code{species='*'}. Before using this function, please first check the GBIF \href{http://www.gbif.org/terms/licences}{data use agreement}. 
}


\usage{
sp_occurrence(genus, species="", ext=NULL, args=NULL, geo=TRUE, 
   removeZeros=FALSE, download=TRUE, ntries=5, nrecs=300, start=1, end=Inf)
}
 
\arguments{
  \item{genus}{character. genus name}
  \item{species}{character. species name. Use '*' to download the entire genus. Append '*' to the species name to get all naming variants (e.g. with and witout species author name) and sub-taxa }
  \item{ext}{Extent object to limit the geographic extent of the records. An extent can be created using functions like \code{\link[terra]{draw}} and \code{\link[terra]{ext}}}
  \item{args}{character. Additional arguments to refine the query. See query parameters in http://www.gbif.org/developer/occurrence for more details}
  \item{geo}{logical. If \code{TRUE}, only records that have a georeference (longitude and latitude values) will be downloaded}
  \item{removeZeros}{logical. If \code{TRUE}, all records that have a latitude OR longitude of zero will be removed if \code{geo==TRUE}, or set to \code{NA} if \code{geo==FALSE}. If \code{FALSE}, only records that have a latitude AND longitude that are zero will be removed or set to \code{NA}}
  \item{download}{logical. If \code{TRUE}, records will be downloaded, else only the number of records will be shown}
  \item{ntries}{integer. How many times should the function attempt to download the data, if an invalid response is returned (perhaps because the GBIF server is very busy)}
  \item{nrecs}{integer. How many records to download in a single request (max is 300)?}
  \item{start}{integer. Record number from which to start requesting data}
  \item{end}{integer. Last record to request}
}


\value{
data frame 
}


\references{
\url{http://www.gbif.org/occurrence}
}

\examples{
\dontrun{

sp_occurrence("solanum", download=FALSE)
sp_occurrence("solanum", "acaule", download=FALSE)

sp_occurrence("Batrachoseps", "" , down=FALSE)
sp_occurrence("Batrachoseps", "luciae", down=FALSE)
g <- sp_occurrence("Batrachoseps", "luciae", geo=TRUE)
plot(g$lon, g$lat)

gs <- sp_occurrence("Batrachoseps", "luciae")
plot(gs)
}
}


\keyword{spatial}

