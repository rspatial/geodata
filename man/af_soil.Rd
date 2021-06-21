\name{af_soil}

\alias{af_soil}

\title{af_soil}

\description{
Download chemical soil properties data for Africa for different soil depths. The spatial resolution is 30 arc-seconds (about 1 km2), aggregated from the original 250m resolution.

There are more recent estimations for some of the properties available in other data sets. See \code{\link{af_soil_isda}} and \code{\link{soil_grids_vrt}}. 


For more info, see \url{https://www.isric.org/projects/soil-property-maps-africa-250-m-resolution}


The data have a CC-BY 4.0 NC license
}

\usage{
af_soil(var, depth, path)
}


\arguments{
  \item{var}{character. Variables name such as "pH" or "clay". See Details}
  
  \item{depth}{numeric. One of \code{5, 15, 30, 60, 100, 200}. This is shorthand for the following depth ranges: 0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm. Or one of \code{20, 50} for 0-20 or 20-50 cm}
  
  \item{path}{character. Path to download the data to}
} 

\value{
SpatRaster
}


\details{
\tabular{llllr}{
\tab \bold{Name} \tab \bold{Description} \tab \bold{Unit} \cr
\tab clay \tab Soil texture fraction clay \tab g 100 g-1 \cr
\tab sand \tab Soil texture fraction sand \tab g 100 g-1 \cr
\tab silt \tab Soil texture fraction silt \tab g 100 g-1 \cr
\tab coarse \tab Coarse fragments volumetric \tab m3 100 m-3  \cr
\tab SOC \tab Organic carbon \tab g kg-1 \cr
\tab BLKD \tab Bulk density (fine earth) \tab kg dm-3 \cr
\tab poros \tab Porosity (volumetric fraction) based on PTF\tab - \cr
\tab AWpF2.0 \tab Available soil water capacity (volumetric fraction) for FC = pF 2.0 \tab - \cr
\tab AWpF2.3 \tab Available soil water capacity (volumetric fraction) for FC = pF 2.3 \tab - \cr
\tab AWpF2.5 \tab Available soil water capacity (volumetric fraction) for FC = pF 2.4 \tab - \cr
\tab AWpF4.2 \tab Available soil water capacity (volumetric fraction) at wilting point (pF 4.2) \tab - \cr
\tab BDR \tab Depth to bedrock \tab cm \cr

\tab ---- \tab -----  \tab ----- \cr

\tab pH \tab Soil pH (H2O) \tab - \tab \cr
\tab ECN \tab Electrical condutivity \tab mS/m (?) \tab \cr
\tab acid-exch \tab Exchangeable acidity \tab cmol+ kg-1 \cr
\tab bases-exch \tab Sum of exchangeable bases \tab cmol+ kg-1 \cr
\tab CEC \tab Cation Exchange Capacity \tab cmol+ kg-1 \cr
\tab Al-extr \tab Extractable Aluminum (Mehlich 3) \tab mg kg-1 \cr
\tab Al-exch \tab Exchangeable Aluminum \tab cmol+ kg-1 \cr
\tab Ca-exch \tab Exchangeable Calcium \tab cmol+ kg-1 \cr
\tab K-exch \tab Exchangeable Potassium \tab cmol+ kg-1  \cr
\tab Mg-exch \tab Exchangeable Magnesium \tab cmol+ kg-1 \cr
\tab Na-exch \tab Exchangeable Sodium \tab cmol+ kg-1 \cr
\tab Ntot \tab Total nitrogen \tab g kg-1 \cr
}
}


\references{
Hengl T, Heuvelink GBM, Kempen B, Leenaars JGB, Walsh MG, Shepherd KD, et al. (2015) Mapping Soil Properties of Africa at 250 m Resolution: Random Forests Significantly Improve Current Predictions. PLoS ONE 10(6): e0125814. doi:10.1371/journal.pone.0125814
}


\seealso{
\code{\link{af_elements}}, \code{\link{af_soil_isda}}, \code{\link{soil_grids_vrt}}
}


\examples{
\donttest{
ph <- soil_afsis(var="phh2o", depth=5)
}
}
