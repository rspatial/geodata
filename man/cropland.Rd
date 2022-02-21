\name{cropland}

\alias{cropland_world}
\alias{cropland_africa}

\title{Cropland distribution data}

\description{

\code{cropland_world} downloads a 30-seconds raster for (most of) the world. Values are the fraction cropland in each cell. The values are derived from the ESA WorldCover data set at 0.3-seconds resolution. (License CC BY 4.0), see \url{https://esa-worldcover.org/en}

\code{cropland_africa} has cropland distribution data for Africa. The values are probabilities of cropland presence estimated with a neural network that was trained on an initial 1-million point Geosurvey (https://geosurvey.qed.ai) conducted in 2015. (License: CC-BY-SA 4.0; \url{https://about.maps.qed.ai/}
}

\usage{
cropland_africa(path, ...) 

cropland_world(path, ...) 
}

\arguments{
  \item{path}{character. Path name where the data should be downloaded to} 
  \item{...}{additional arguments passed to \code{\link{download.file}}}  
}

\value{
SpatRaster
}

\seealso{
\code{\link{landcover}}
}

\references{
(for WorldCover) Zanaga, D., Van De Kerchove, R., De Keersmaecker, W., Souverijns, N., Brockmann, C., Quast, R., Wevers, J., Grosu, A., Paccini, A., Vergnaud, S., Cartus, O., Santoro, M., Fritz, S., Georgieva, I., Lesiv, M., Carter, S., Herold, M., Li, Linlin, Tsendbazar, N.E., Ramoino, F., Arino, O., 2021. ESA WorldCover 10 m 2020 v100. doi:10.5281/zenodo.5571936. 
}


\examples{
\donttest{
#cropland <- cropland_ESA(path=tempdir())
#cropland <- cropland_africa(path=tempdir())
}
}
