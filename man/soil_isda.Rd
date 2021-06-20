\name{soil_africa}

\alias{soil_africa}

\title{Soil data for Africa}

\description{
Download iSDA soil data for Africa. Aggregated to 30-seconds resolution.
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


\references{
Tomislav Hengl, Matthew A. E. Miller, Josip Križan, Keith D. Shepherd, Andrew Sila, Milan Kilibarda, Ognjen Antonijević, Luka Glušica, Achim Dobermann, Stephan M. Haefele, Steve P. McGrath, Gifty E. Acquah, Jamie Collinson, Leandro Parente, Mohammadreza Sheykhmousa, Kazuki Saito, Jean-Martial Johnson, Jordan Chamberlin, Francis B. T. Silatsa, Martin Yemefack, John Wendt, Robert A. MacMillan, Ichsani Wheeler & Jonathan Crouch, 2021. African soil properties and nutrients mapped at 30 m spatial resolution using two-scale ensemble machine learning. Scientific Reports 11: 6130. 
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
