\name{soil_af_elements}

\alias{soil_af_elements}

\title{Soil elements data for Africa}

\description{
Connect to or download chemical soil element concentration (for the 0-30 cm topsoil) data for Africa.
The spatial resolution is 30 arc-seconds (about 1 km2), aggregated from the original 250m resolution.

The data have an Open Data Commons Open Database License (ODbL)

For more information, see \url{https://www.isric.org/projects/soil-property-maps-africa-250-m-resolution}
}

\usage{
soil_af_elements(var, path)
}


\arguments{
  \item{var}{character. Variables name. One of: "Al", "B", "Ca", "Cu", "Fe", "K", "Mg", "Mn", "N", "Na", "P", "Ptot", "Zn". See Details}
  
  \item{path}{character. Path to download the data to}
} 

\value{
SpatRaster
}

\details{
\tabular{lllr}{
\tab \bold{var} \tab \bold{description} \tab \bold{unit} \cr
\tab Al \tab Extractable aluminum \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
\tab B \tab Extractable boron \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
\tab Ca \tab Extractable calcium \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
\tab Cu \tab Extractable copper \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
\tab Fe \tab Extractable iron \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
\tab K \tab Extractable potassium \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
\tab Mg \tab Extractable magnesium \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
\tab Mn \tab Extractable manganese \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
\tab N \tab Organic nitrogen \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
\tab Na \tab Extractable sodium \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
\tab P \tab Extractable phosphorus \tab \ifelse{latex}{\out{$mg\cdot (100 \cdot kg^{-1})$}}{\out{mg (100 kg<sup>-1</sup>)}} \cr
\tab Ptot \tab Total phosphorus \tab \ifelse{latex}{\out{$mg\cdot (100 \cdot kg^{-1})$}}{\out{mg (100 kg<sup>-1</sup>)}} \cr
\tab Zn \tab Extractable zinc \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} (ppm) \cr
}
}


\references{
Hengl T, Heuvelink GBM, Kempen B, Leenaars JGB, Walsh MG, Shepherd KD, et al. (2015) Mapping Soil Properties of Africa at 250 m Resolution: Random Forests Significantly Improve Current Predictions. PLoS ONE 10(6): e0125814. doi:10.1371/journal.pone.0125814
}


\seealso{
\code{\link{soil_af}}, \code{\link{soil_af_isda}}, \code{\link{soil_world}}
}


\examples{
\donttest{
ph <- soil_elements("Fe", path=tempdir())
}
}

