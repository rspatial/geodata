\name{soil_af_isda_vsi}

\alias{soil_af_isda_vsi}

\title{iSDA soil data for Africa, virtual connection}

\description{
Virtually connect to the iSDA soil data for Africa. The spatial of these data is 30m.

For more info see:

\url{https://envirometrix.nl/isdasoil-open-soil-data-for-africa/} 

\url{https://zenodo.org/search?page=1&size=20&q=iSDAsoil}
}

\usage{
soil_af_isda_vsi(var) 
}

\arguments{
  \item{var}{character. The variables name, one of: "Al", "bdr", "clay", "C.tot", "CEC", "Ca", "db.od", "eCEC.f", "Fe", "K", "Mg", "N.tot", "oc", "P", "pH.H2O", "sand", "silt", "S", "texture", "wpg2", "Zn".see Details}
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
Tomislav Hengl, Matthew A. E. Miller, Josip Križan, Keith D. Shepherd, Andrew Sila, Milan Kilibarda, Ognjen Antonijevic, Luka Glušica, Achim Dobermann, Stephan M. Haefele, Steve P. McGrath, Gifty E. Acquah, Jamie Collinson, Leandro Parente, Mohammadreza Sheykhmousa, Kazuki Saito, Jean-Martial Johnson, Jordan Chamberlin, Francis B.T. Silatsa, Martin Yemefack, John Wendt, Robert A. MacMillan, Ichsani Wheeler & Jonathan Crouch, 2021. African soil properties and nutrients mapped at 30 m spatial resolution using two-scale ensemble machine learning. Scientific Reports 11: 6130. 
}

\seealso{
\code{\link{soil_af_elements}}, \code{\link{soil_af_isda}}, \code{\link{soil_world}}
}

