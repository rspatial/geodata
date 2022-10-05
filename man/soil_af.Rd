\name{soil_af}

\alias{soil_af}

\title{Soil data for Africa}

\description{
Download chemical soil properties data for Africa for different soil depths. The spatial resolution is 30 arc-seconds (about 1 km2), aggregated from the original 250m resolution.

There are more recent estimations for some of the properties available in other data sets. See \code{\link{soil_af_isda}} and \code{\link{soil_world}}. 


For more info, see https://www.isric.org/projects/soil-property-maps-africa-250-m-resolution


The data have a CC-BY 4.0 NC license
}

\usage{
soil_af(var, depth, path, ...)
}


\arguments{
  \item{var}{character. Variables name such as "pH" or "clay". See Details}
  
  \item{depth}{numeric. One of \code{5, 15, 30, 60, 100, 200}. This is shorthand for the following depth ranges: 0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm. Or one of \code{20, 50} for 0-20 or 20-50 cm}
  
  \item{path}{character. Path to download the data to}
  
  \item{...}{additional arguments passed to \code{\link{download.file}}}
  
} 

\value{
SpatRaster
}


\details{
\tabular{lllr}{
\tab \bold{var} \tab \bold{description} \tab \bold{unit} \cr
\tab clay \tab Soil texture fraction clay \tab \% \cr
\tab sand \tab Soil texture fraction sand \tab \% \cr
\tab silt \tab Soil texture fraction silt \tab \% \cr
\tab coarse \tab Coarse fragments volumetric \tab \% \cr
\tab SOC \tab Organic carbon \tab \ifelse{latex}{\out{$g\cdot kg^{-1}$}}{\out{g kg<sup>-1</sup>}} (‰) \cr
\tab BLKD \tab Bulk density (fine earth) \tab \ifelse{latex}{\out{$kg\cdot m^{-3}$}}{\out{kg m<sup>-3</sup>}} \cr
\tab poros \tab Porosity (volum. fraction) based on PTF\tab - \cr
\tab AWpF2.0 \tab Avail. soil water capacity (volum. frac.) for FC = pF 2.0 \tab - \cr
\tab AWpF2.3 \tab Avail. soil water capacity (volum. frac.) for FC = pF 2.3 \tab - \cr
\tab AWpF2.5 \tab Avail. soil water capacity (volum. fract.) for FC = pF 2.4 \tab - \cr
\tab AWpF4.2 \tab Avail. soil water capacity (volum. fract.) at wilting point (pF 4.2) \tab - \cr
\tab BDR \tab Depth to bedrock \tab cm \cr
\tab . \tab . \tab . \cr
\tab pH \tab pH (\ifelse{latex}{\out{$H_{2}O$}}{\out{H<sub>2</sub>O}})  \tab - \cr
\tab ECN \tab Electrical conductivity \tab mS/m (?) \cr
\tab acid-exch \tab Exchangeable acidity \tab \ifelse{latex}{\out{$cmol(+)\cdot kg^{-1}$}}{\out{cmol(+) kg<sup>-1</sup>}} \cr
\tab bases-exch \tab Sum of exchangeable bases \tab \ifelse{latex}{\out{$cmol(+)\cdot kg^{-1}$}}{\out{cmol(+) kg<sup>-1</sup>}} \cr
\tab CEC \tab Cation Exchange Capacity \tab \ifelse{latex}{\out{$cmol(+)\cdot kg^{-1}$}}{\out{cmol(+) kg<sup>-1</sup>}} \cr
\tab Al-extr \tab Extractable Aluminum (Mehlich 3) \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
\tab Al-exch \tab Exchangeable Aluminum \tab \ifelse{latex}{\out{$cmol(+)\cdot kg^{-1}$}}{\out{cmol(+) kg<sup>-1</sup>}} \cr
\tab Ca-exch \tab Exchangeable Calcium \tab \ifelse{latex}{\out{$cmol(+)\cdot kg^{-1}$}}{\out{cmol(+) kg<sup>-1</sup>}} \cr
\tab K-exch \tab Exchangeable Potassium \tab  \ifelse{latex}{\out{$cmol(+)\cdot kg^{-1}$}}{\out{cmol(+) kg<sup>-1</sup>}} \cr
\tab Mg-exch \tab Exchangeable Magnesium \tab \ifelse{latex}{\out{$cmol(+)\cdot kg^{-1}$}}{\out{cmol(+) kg<sup>-1</sup>}} \cr
\tab Na-exch \tab Exchangeable Sodium \tab \ifelse{latex}{\out{$cmol(+)\cdot kg^{-1}$}}{\out{cmol(+) kg<sup>-1</sup>}} \cr
\tab Ntot \tab Total nitrogen \tab \ifelse{latex}{\out{$g\cdot kg^{-1}$}}{\out{g kg<sup>-1</sup>}} \cr
}
}


\references{
Hengl T, Heuvelink GBM, Kempen B, Leenaars JGB, Walsh MG, Shepherd KD, et al. (2015) Mapping Soil Properties of Africa at 250 m Resolution: Random Forests Significantly Improve Current Predictions. PLoS ONE 10(6): e0125814. doi:10.1371/journal.pone.0125814
}


\seealso{
\code{\link{soil_af_elements}}, \code{\link{soil_af_isda}}, \code{\link{soil_world_vsi}}
}


\examples{
\donttest{
aph <- soil_af(var="ph", depth=5, path=tempdir())
}
}
