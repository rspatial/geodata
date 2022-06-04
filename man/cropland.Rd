\name{cropland}

\alias{cropland}

\title{Cropland distribution data}

\description{
Cropland distribution data at a 30-seconds spatial resolution from three sources:

\code{worldcover} is derived from the ESA WorldCover data set at 0.3-seconds resolution. (License CC BY 4.0), see \url{https://esa-worldcover.org/en}. Values were aggregated and represent the fraction cropland in each cell. 

\code{glad} is derived from the "Global cropland expansion in the 21st century" (Potatov et al) data available \href{https://glad.umd.edu/dataset/croplands}{here}. Values were aggregated and resampled. They represent the fraction cropland in each cell. There are five layers representing the following years: 2003, 2007, 2011, 2015, and 2019.

\code{QED} has cropland distribution data for Africa. The values are probabilities of cropland presence estimated with a neural network that was trained on an initial 1-million point \href{https://geosurvey.qed.ai}{Geosurvey} conducted in 2015. License: CC-BY-SA 4.0; \url{https://about.maps.qed.ai/}
}

\usage{
cropland(source, path, year, ...) 
}

\arguments{
  \item{source}{character. One of "WorldCover", "GLAD", or "geosurvey"} 
  \item{path}{character. Path name where the data should be downloaded to} 
  \item{year}{numeric. Optional for the GLAD dataset to get data for a single year. One of 2003, 2007, 2011, 2015, and 2019} 
  \item{...}{additional arguments passed to \code{\link{download.file}}}  
}

\value{
SpatRaster
}

\seealso{
\code{\link{landcover}}
}

\references{
WorldCover: 
Zanaga, D., Van De Kerchove, R., De Keersmaecker, W., Souverijns, N., Brockmann, C., Quast, R., Wevers, J., Grosu, A., Paccini, A., Vergnaud, S., Cartus, O., Santoro, M., Fritz, S., Georgieva, I., Lesiv, M., Carter, S., Herold, M., Li, Linlin, Tsendbazar, N.E., Ramoino, F., Arino, O., 2021. ESA WorldCover 10 m 2020 v100. doi:10.5281/zenodo.5571936. 

GLAD:
Potapov, P., S. Turubanova, M.C. Hansen, A. Tyukavina, V. Zalles, A. Khan, X.-P. Song, A. Pickens, Q. Shen, J. Cortez, 2021. Global maps of cropland extent and change show accelerated cropland expansion in the twenty-first century. Nature Food. \url{ https://doi.org/10.1038/s43016-021-00429-z}
}



\examples{
\donttest{
#x <- cropland("WorldCover", path=tempdir())
}
}
