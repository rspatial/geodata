\name{soil}

\alias{soil_africa}

\title{Soil data}

\description{
Download soil data. Currently only iSDA soil data for Africa. Aggregated to 30-seconds resolutin.
}

\usage{
soil_africa(var, depth=20, version=13, path) 
}

\arguments{
  \item{var}{character. Valid variables names are "ph", "log-ecec", "log-oc", "log-p-mehlich3"}
  \item{depth}{numeric. Valid depths are 20 (for 0-20 cm) or 50 (for 20-50 cm)}
  \item{version}{integer. Only version 13 is currently available} 
  \item{path}{character. Path name where the data should be downloaded to}  
}

\value{
SpatRaster
}


\examples{
\donttest{
ph <- soil_africa(var="ph", path=".")
}
}

\seealso{
\url{https://envirometrix.nl/isdasoil-open-soil-data-for-africa/} 
\url{https://zenodo.org/search?page=1&size=20&q=iSDAsoil}
}

