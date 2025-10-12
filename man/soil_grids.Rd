\name{soil_world}

\alias{soil_world}

\title{Global soil data}

\description{
Download global soils data. The data are derived from the SoilGRIDS database. The data were aggregated and transformed to a longitude/latitude coordinate reference system with 30-second spatial resolution.

See https://www.isric.org/explore/soilgrids for more info.

data license: CC-BY 4.0

Note that currently only the "mean" values for up to 60cm depth are available.
}

\usage{
soil_world(var, depth, stat="mean", name="", path, vsi=FALSE, ...)
}

\arguments{
  \item{var}{character. Variables name. One of: "bdod", "cfvo", "clay", "nitrogen", "ocd", "ocs", "phh2o", "sand", "silt", "soc", "wrb". See Details}
  \item{depth}{numeric. One of \code{5, 15, 30, 60, 100, 200}. This is shorthand for the following depth ranges: 0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm. Ignored if \code{var="wrb"}}
  \item{stat}{character. One of "mean", "uncertainty", "Q0.05", "Q0.5", "Q0.95". Ignored if \code{var="wrb"}}
  \item{name}{character. One of "Acrisols", "Albeluvisols", "Alisols", "Andosols", "Arenosols", "Calcisols", "Cambisols", "Chernozems", "Cryosols", "Durisols", "Ferralsols", "Fluvisols", "Gleysols", "Gypsisols", "Histosols", "Kastanozems", "Leptosols", "Lixisols", "Luvisols", "Nitisols", "Phaeozems", "Planosols", "Plinthosols", "Podzols", "Regosols", "Solonchaks", "Solonetz", "Stagnosols", "Umbrisols", "Vertisols". Only used when \code{var="wrb"}}  
  \item{path}{character. Path for storing the downloaded data. See \code{\link{geodata_path}}}  
  \item{vsi}{logical. If \code{TRUE}, a virtual connection to the file is used in stead of a download. This allows downloading a small area only by subsequently using \code{\link[terra]{crop}} or \code{\link[terra]{window}}}
  \item{...}{additional arguments passed to \code{\link{download.file}}}

}

\value{
SpatRaster
}


\details{
\tabular{lllr}{
\tab \bold{var} \tab \bold{description} \tab \bold{unit} \cr
\tab\code{bdod} \tab Bulk density of the fine earth fraction \tab \ifelse{latex}{\out{$kg\cdot dm^{-3}$}}{\out{kg dm<sup>-3</sup>}} \cr
\tab\code{cec} \tab Cation Exchange Capacity of the soil \tab \ifelse{latex}{\out{$cmol(+)\dot kg^{-1}$}}{\out{cmol(+) kg<sup>-1</sup>}} \cr
\tab\code{cfvo} \tab Vol. fraction of coarse fragments (> 2 mm) \tab \% \cr
\tab\code{nitrogen} \tab Total nitrogen (N) \tab \ifelse{latex}{\out{$g\cdot kg^{-1}$}}{\out{g kg<sup>-1</sup>}} \cr
\tab\code{phh2o} \tab pH (\ifelse{latex}{\out{$H_{2}O$}}{\out{H<sub>2</sub>O}})  \tab - \cr
\tab\code{sand} \tab Sand (> 0.05 mm) in fine earth \tab \% \cr
\tab\code{silt} \tab Silt (0.002-0.05 mm) in fine earth \tab \% \cr
\tab\code{clay} \tab Clay (< 0.002 mm) in fine earth \tab \% \cr
\tab\code{soc} \tab Soil organic carbon in fine earth \tab \ifelse{latex}{\out{$g\cdot kg^{-1}$}}{\out{g kg<sup>-1</sup>}} \cr
\tab\code{ocd} \tab Organic carbon density \tab \ifelse{latex}{\out{$kg\cdot m^{-3}$}}{\out{kg m<sup>-3</sup>}} \cr
\tab\code{ocs} \tab Organic carbon stocks \tab  \ifelse{latex}{\out{$kg\cdot m^{-2}$}}{\out{kg m<sup>-2</sup>}} \cr
}
}

\references{
Poggio L., de Sousa L.M., Batjes N.H., Heuvelink G.B.M., Kempen B., Ribeiro E., Rossiter D., 2021. SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty. Soil 7:217-240, 2021. doi:10.5194/soil-7-217-2021
}

\seealso{
For virtual access to the original data: \code{\link{soil_world_vsi}}
For Africa: \code{\link{soil_af_isda}}, \code{\link{soil_af}}, \code{\link{soil_af_elements}}
}



\examples{
\donttest{
# this downloads a large file 
gph <- soil_world(var="phh2o", depth=5, path=tempdir())
}
}
