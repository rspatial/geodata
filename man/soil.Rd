\name{soil}

\alias{soil_africa}

\title{Soil data}

\description{
Download soil data. Currently only iSDA soil data for Africa. Aggregated to 30-seconds resolutin.
}

\usage{
soil_africa(var, depth=20, version=0.13, path) 
}

\arguments{
  \item{var}{character. Variables name. One of: "ph_h2o", "log.ecec.f", "log.oc", "log.k_mehlich3", "log.p_mehlich3"}
  \item{depth}{numeric. One of 20 (for 0-20 cm) and 50 (for 20-50 cm)}
  \item{version}{integer. Only version 0.13 is currently available} 
  \item{path}{character. Path name where the data should be downloaded to}  
}

\value{
SpatRaster
}


\seealso{
\url{https://envirometrix.nl/isdasoil-open-soil-data-for-africa/} 
\url{https://zenodo.org/search?page=1&size=20&q=iSDAsoil}
}



\examples{
\donttest{
ph <- soil_africa(var="ph_h2o", path=tempdir())
}
}
