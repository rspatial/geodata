\name{sp_occurrence}

\alias{sp_occurrence}
\alias{sp_occurrence_split}
\alias{sp_genus}

\title{
Download species occurrence data from GBIF
}

\description{
Download data from the Global Biodiversity Information Facility (\href{https://www.gbif.org}{GBIF}) data portal.

\code{sp_genus} returns a data.frame with all the species names associated with a genus. 

\code{sp_occurrence} downloads species occurrence records. You can download data for a single species or for an entire genus by using \code{species=""}. Note that the maximum number of records that can be downloaded for a single search is 100,000. 

You can check the number of records returned by using the option \code{download=FALSE}.

To avoid getting more than 100,000 records, you can do separate queries for different geographic areas. This has been automated in \code{sp_occurrence_split}. This function recursively splits the area of interest into smaller areas until the number of records in an area is less than 50,000. It then downloads these records and saves them in a folder called "gbif". After all areas have been evaluated, the data are combined into a single file and returned as a data.frame). If the function is interrupted, it can be run again, and it will resume where it left off.

If you want to download data for an entire genus, first run \code{sp_genus} and then download data for the returned species names one by one. 

Before using this function, please first check the GBIF \href{https://www.gbif.org/terms/licences}{data use agreement} and see the note below about how to cite these data. 
}


\usage{
sp_genus(genus, simple=TRUE, ...)

sp_occurrence(genus, species="", ext=NULL, args=NULL, geo=TRUE, 
	removeZeros=FALSE, download=TRUE, ntries=5, nrecs=300, 
	start=1, end=Inf, fixnames=TRUE, ...)

sp_occurrence_split(genus, species="", path=".", ext=c(-180,180,-90,90),
	args=NULL, geo=TRUE, removeZeros=FALSE, ntries=5, nrecs=300,
	fixnames=TRUE, prefix=NULL, ...)
}
 
\arguments{
  \item{genus}{character. genus name}
  \item{species}{character. species name. Can be left blank to get the entire genus}
  \item{ext}{SpatExtent object to limit the geographic extent of the records. A SpatExtent can be created using functions like \code{\link[terra]{ext}} and \code{\link[terra]{draw}}}
  \item{args}{character. Additional arguments to refine the query. See query parameters in http://www.gbif.org/developer/occurrence for more details}
  \item{geo}{logical. If \code{TRUE}, only records that have a georeference (longitude and latitude values) will be downloaded}
  \item{removeZeros}{logical. If \code{TRUE}, all records that have a latitude OR longitude of zero will be removed if \code{geo==TRUE}, or set to \code{NA} if \code{geo==FALSE}. If \code{FALSE}, only records that have a latitude AND longitude that are zero will be removed or set to \code{NA}}
  \item{download}{logical. If \code{TRUE}, records will be downloaded, else only the number of records will be shown}
  \item{ntries}{integer. How many times should the function attempt to download the data, if an invalid response is returned (perhaps because the GBIF server is very busy)}
  \item{nrecs}{integer. How many records to download in a single request (max is 300)?}
  \item{start}{integer. Record number from which to start requesting data}
  \item{end}{integer. Last record to request}
  \item{fixnames}{If \code{TRUE} a few unwieldy and poorly chosen variable names are changed as follows. "decimalLatitude" to "lat", "decimalLongitude" to "lon", "stateProvince" to "adm1", "county" to "adm2", "countryCode" to "ISO2". The names in "country" are replaced with the common (short form) country name, the original values are stored as "fullCountry"}

  \item{path}{character. Where should the data be downloaded to (they will be put in a subdirectory "gbif")?}
  \item{prefix}{character. prefix of the downloaded filenames (best left NULL, the function will then use "genus_species"}

  \item{simple}{logical. If \code{TRUE}, a vector the accepted species names are returned. Otherwise a data.frame with much more information is returned}  
  \item{...}{additional arguments passed to \code{\link{download.file}}}
}


\value{
data.frame 
}

\note{
Under the terms of the GBIF data user agreement, users who download data agree to cite a DOI. Citation rewards data-publishing institutions and individuals and provides support for sharing open data [\href{https://docs.ropensci.org/rgbif/articles/gbif_citations.html}{1}][\href{https://www.gbif.org/citation-guidelines}{2}]. You can get a DOI for the data you downloaded by creating a \href{https://www.gbif.org/derived-dataset/about}{"derived" dataset}. For this to work, you need to keep the "datasetKey" variable in your dataset.
}


\references{
\url{https://www.gbif.org/occurrence}
\url{https://www.gbif.org/derived-dataset/about}
}

\examples{
sp_occurrence("solanum", download=FALSE)
sp_occurrence("solanum", "acaule", download=FALSE)

sp_occurrence("Batrachoseps", "" , down=FALSE)
sp_occurrence("Batrachoseps", "luciae", down=FALSE)
g <- sp_occurrence("Batrachoseps", "luciae", geo=TRUE, end=25)
#plot(g[, c("lon", "lat")])


## args
a1 <- sp_occurrence("Elgaria", "multicarinata", 
			args="recordNumber=Robert J. Hijmans RH-2")
a2 <- sp_occurrence("Batrachoseps", "luciae",
			args=c("year=2023", "identifiedBy=Anthony Ye"))

## year supports "range queries"
a3 <- sp_occurrence("Batrachoseps", "luciae", 
			args=c("year=2020,2023", "identifiedBy=Kuoni W"))
#table(a3[,c("year")])
}


\keyword{spatial}

