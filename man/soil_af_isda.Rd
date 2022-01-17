\name{soil_af_isda}

\alias{soil_af_isda}

\title{iSDA soil data for Africa}

\description{
Download soil data for Africa derived from the iDSA data set. The original data were aligned and aggregated to 30 arc-seconds (about 1 km2). The original spatial resolution was 30m.

For more info see:

\url{https://envirometrix.nl/isdasoil-open-soil-data-for-africa/} 

\url{https://zenodo.org/search?page=1&size=20&q=iSDAsoil}
}

\usage{
soil_af_isda(var, depth=20, error=FALSE, path, ...) 
}

\arguments{
  \item{var}{character. The variables name, one of: "Al", "bdr", "clay", "C.tot", "Ca", "db.od", "eCEC.f", "Fe", "K", "Mg", "N.tot", "oc", "P", "pH.H2O", "sand", "silt", "S", "texture", "wpg2", "Zn".see Details}
  \item{depth}{numeric. One of 20 (for 0-20 cm) and 50 (for 20-50 cm). Ignored if \code{var="bdr"} for which the depth is always 0-200 cm}
  \item{error}{logical. If \code{TRUE} the error estimates are returned} 
  \item{path}{character. Path name where the data should be downloaded to}  
  \item{...}{additional arguments passed to \code{\link{download.file}}}  
}

\value{
SpatRaster
}

\details{
\tabular{lllr}{
\tab \bold{var} \tab \bold{description} \tab \bold{unit} \cr
\tab Al \tab extractable aluminum \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} \cr
\tab bdr \tab bed rock depth \tab \ifelse{latex}{\out{$cm$}}{\out{cm}} \cr
\tab clay \tab clay content \tab \% \cr
\tab C.tot \tab total carbon \tab \ifelse{latex}{\out{$kg^{-1}$}}{\out{kg<sup>-1</sup>}} \cr
\tab Ca \tab extractable calcium \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} \cr
\tab db.od \tab bulk density \tab \ifelse{latex}{\out{$kg\cdot m^{3}$}}{\out{kg m<sup>-3</sup>}} \cr
\tab eCEC.f \tab effective cation exchange capacity \tab \ifelse{latex}{\out{$cmol(+)\dot kg^{-1}$}}{\out{cmol(+) kg<sup>-1</sup>}} \cr
\tab Fe \tab extractable iron \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} \cr
\tab K \tab extractable potassium \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} \cr
\tab Mg \tab extractable magnesium \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} \cr
\tab N.tot \tab total organic nitrogen \tab \ifelse{latex}{\out{$g\cdot kg^{-1}$}}{\out{g kg<sup>-1</sup>}} \cr
\tab OC \tab Organic Carbon \tab  \ifelse{latex}{\out{$g\cdot kg^{-1}$}}{\out{g kg<sup>-1</sup>}} \cr
\tab P \tab Phosphorus content \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} \cr
\tab pH.H2O \tab pH (\ifelse{latex}{\out{$H_{2}O$}}{\out{H<sub>2</sub>O}}) \tab - \cr
\tab sand \tab Sand content \tab \% \cr
\tab silt \tab Silt content \tab \% \cr
\tab S \tab Extractable sulfer \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} \cr
\tab texture \tab texture class \tab - \cr
\tab wpg2 \tab stone content \tab \% \cr
\tab Zn \tab Extractable zinc \tab \ifelse{latex}{\out{$mg\cdot kg^{-1}$}}{\out{mg kg<sup>-1</sup>}} \cr
}
}


\references{
Tomislav Hengl, Matthew A. E. Miller, Josip Križan, Keith D. Shepherd, Andrew Sila, Milan Kilibarda, Ognjen Antonijevic, Luka Glušica, Achim Dobermann, Stephan M. Haefele, Steve P. McGrath, Gifty E. Acquah, Jamie Collinson, Leandro Parente, Mohammadreza Sheykhmousa, Kazuki Saito, Jean-Martial Johnson, Jordan Chamberlin, Francis B. T. Silatsa, Martin Yemefack, John Wendt, Robert A. MacMillan, Ichsani Wheeler & Jonathan Crouch, 2021. African soil properties and nutrients mapped at 30 m spatial resolution using two-scale ensemble machine learning. Scientific Reports 11: 6130. 
}

\seealso{
\code{\link{soil_af_elements}}, \code{\link{soil_af}}, \code{\link{soil_world}}
}


\examples{
\donttest{
afph <- soil_af_isda("ph.h2o", path=tempdir(), quiet=TRUE)
}
}
