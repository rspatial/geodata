\name{af_soil_elements}

\alias{af_soil_elements}

\title{Elements in African soil}

\description{
Connect to or download chemical soil element concentration (for the 0-30 cm topsoil) data for Africa.
The spatial resolution is 30 arc-seconds (about 1 km2), aggregated from the original 250m resolution.

The data have an Open Data Commons Open Database License (ODbL)

For more information, see \url{https://www.isric.org/projects/soil-property-maps-africa-250-m-resolution}
}

\usage{
af_soil_elements(var, path)
}


\arguments{
  \item{var}{character. Variables name. One of: \code{"Al", "B", "Ca", "Cu", "Fe", "K", "Mg", "Mn", "N", "Na", "P", "Ptot", "Zn"}. See Details}
  
  \item{path}{character. Path to download the data to}
} 

\value{
SpatRaster
}


\details{
\tabular{lllr}{
\tab \bold{Name} \tab \bold{Description} \tab \bold{Unit} \cr
\tab Al \tab Extractable aluminum \tab mg kg-1 \cr
\tab B \tab Extractable boron \tab mg kg-1 \cr
\tab Ca \tab Extractable calcium \tab mg kg-1 \cr
\tab Cu \tab Extractable copper \tab mg kg-1 \cr
\tab Fe \tab Extractable iron \tab mg kg-1 \cr
\tab K \tab Extractable potassium \tab mg kg-1 \cr
\tab Mg \tab Extractable magnesium \tab mg kg-1 \cr
\tab Mn \tab Extractable manganese \tab mg kg-1 \cr
\tab N \tab Organic nitrogen \tab in mg kg-1 \cr
\tab Na \tab Extractable sodium \tab mg kg-1 \cr
\tab P \tab Extractable phosphorus \tab 100 mg kg-1 \cr
\tab Ptot \tab Total phosphorus \tab 100 mg kg-1 \cr
\tab Zn \tab Extractable zinc \tab mg kg-1 \cr
}
}


\references{
Hengl T, Heuvelink GBM, Kempen B, Leenaars JGB, Walsh MG, Shepherd KD, et al. (2015) Mapping Soil Properties of Africa at 250 m Resolution: Random Forests Significantly Improve Current Predictions. PLoS ONE 10(6): e0125814. doi:10.1371/journal.pone.0125814
}


\seealso{
\code{\link{af_soil}}, \code{\link{af_soil_isda}}, \code{\link{soil_grids}}
}


\examples{
\donttest{
ph <- soil_elements("Fe", path=tempdir())
}
}

