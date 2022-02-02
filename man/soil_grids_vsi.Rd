\name{soil_world_vsi}

\alias{soil_world_vsi}

\title{soil grids_vsi}

\description{
Virtually connect to the global soilgrids data.  See \url{https://www.isric.org/explore/soilgrids} for more info

data license: CC-BY 4.0
}

\usage{
soil_world_vsi(var, depth, stat="mean", name="")
}

\arguments{
  \item{var}{character. Variables name. One of: "bdod", "cfvo", "clay", "nitrogen", "ocd", "ocs", "phh2o", "sand", "silt", "soc", "wrb". See Details}
  \item{depth}{numeric. One of 5, 15, 30, 60, 100, 200. This is shorthand for the following depth ranges: 0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm. Ignored if \code{var="wrb"}}
  \item{stat}{character. One of "mean", "uncertainty", "Q0.05", "Q0.5", "Q0.95". Ignored if \code{var="wrb"}}
  \item{name}{character. One of 'Acrisols', 'Albeluvisols', 'Alisols', 'Andosols', 'Arenosols', 'Calcisols', 'Cambisols', 'Chernozems', 'Cryosols', 'Durisols', 'Ferralsols', 'Fluvisols', 'Gleysols', 'Gypsisols', 'Histosols', 'Kastanozems', 'Leptosols', 'Lixisols', 'Luvisols', 'Nitisols', 'Phaeozems', 'Planosols', 'Plinthosols', 'Podzols', 'Regosols', 'Solonchaks', 'Solonetz', 'Stagnosols', 'Umbrisols', 'Vertisols'. Only used when \code{var="wrb"}}  
}

\value{
SpatRaster
}


\details{
The below table lists the variable names, a description, and the units of the variables. Note that these units are not standard units, and are different from the data for other soil data available through this package.
\tabular{lllr}{
\tab \bold{var} \tab \bold{description} \tab \bold{unit} \cr
\tab\code{bdod} \tab Bulk density of the fine earth fraction \tab \ifelse{latex}{\out{$cg\cdot cm^{-3}$}}{\out{cg cm<sup>-3</sup>}} \cr
\tab\code{cec} \tab Cation Exchange Capacity of the soil \tab \ifelse{latex}{\out{$mmol(+)\dot kg^{-1}$}}{\out{mmol(+) kg<sup>-1</sup>}} \cr
\tab\code{cfvo} \tab Vol. fraction of coarse fragments (> 2 mm) \tab ‰ \cr
\tab\code{nitrogen} \tab Total nitrogen (N) \tab \ifelse{latex}{\out{$cg\cdot kg^{-1}$}}{\out{cg kg<sup>-1</sup>}} \cr
\tab\code{phh2o} \tab pH (\ifelse{latex}{\out{$H_{2}O$}}{\out{H<sub>2</sub>O}}) \tab - \cr
\tab\code{sand} \tab Sand (> 0.05 mm) in fine earth \tab ‰ \cr
\tab\code{silt} \tab Silt (0.002-0.05 mm) in fine earth \tab ‰ \cr
\tab\code{clay} \tab Clay (< 0.002 mm) in fine earth \tab ‰ \cr
\tab\code{soc} \tab Soil organic carbon in fine earth \tab \ifelse{latex}{\out{$dg\cdot kg^{-1}$}}{\out{dg kg<sup>-1</sup>}} \cr
\tab\code{ocd} \tab Organic carbon density \tab \ifelse{latex}{\out{$hg\cdot m^{-3}$}}{\out{hg m<sup>-3</sup>}} \cr
\tab\code{ocs} \tab Organic carbon stocks \tab  \ifelse{latex}{\out{$hg\cdot m^{-2}$}}{\out{hg m<sup>-2</sup>}} \cr
}
}

\references{
Poggio, L., de Sousa, L.M., Batjes, N.H., Heuvelink, G.B.M., Kempen, B., Ribeiro, E., and Rossiter, D., 2021. SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty. Soil 7:217-240, 2021. doi:10.5194/soil-7-217-2021
}

\seealso{
\code{\link{soil_world}} to download these data at 30-seconds spatial resolution.

For Africa: \code{\link{soil_af_isda}}, \code{\link{soil_af}}, \code{\link{soil_af_elements}}
}



\examples{
\donttest{
ph <- soil_world_vsi(var="phh2o", depth=5)
plot(ph, maxcell=10000)
}
}
