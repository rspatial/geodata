\name{soil_grids}

\alias{soil_grids}

\title{soil grids}

\description{
Download global soilgrids data. 

The data were aggregated and transformed to 30 second resolution longitude/latitude

See \url{https://www.isric.org/explore/soilgrids} for more info

data license: CC-BY 4.0
}

\usage{
soil_grids(var, depth, stat="mean", name="", path)
}

\arguments{
  \item{var}{character. Variables name. One of: \code{"bdod", "cfvo", "clay", "nitrogen", "ocd", "ocs", "phh2o", "sand", "silt", "soc", "wrb"}. See Details}
  \item{depth}{numeric. One of \code{5, 15, 30, 60, 100, 200}. This is shorthand for the following depth ranges: 0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm. Ignored if \code{var="wrb"}}
  \item{stat}{character. One of "mean", "uncertainty", "Q0.05", "Q0.5", "Q0.95". Ignored if \code{var="wrb"}}
  \item{name}{character. One of \code{'Acrisols', 'Albeluvisols', 'Alisols', 'Andosols', 'Arenosols', 'Calcisols', 'Cambisols', 'Chernozems', 'Cryosols', 'Durisols', 'Ferralsols', 'Fluvisols', 'Gleysols', 'Gypsisols', 'Histosols', 'Kastanozems', 'Leptosols', 'Lixisols', 'Luvisols', 'Nitisols', 'Phaeozems', 'Planosols', 'Plinthosols', 'Podzols', 'Regosols', 'Solonchaks', 'Solonetz', 'Stagnosols', 'Umbrisols', 'Vertisols'}. Only used when \code{var="wrb"}}  
  \item{path}{character. Path name where the data should be downloaded to}  

}

\value{
SpatRaster
}


\details{
The below table lists the variable names, a brief description, and their units, as well as the conversion factor that can be used to get the conventional units.

\tabular{lllr}{
\tab \bold{var} \tab \bold{description} \tab \bold{unit} \cr
\tab\code{bdod} \tab Bulk density of the fine earth fraction \tab \ifelse{latex}{\out{$kg\cdot dm^{-3}$}}{\out{kg dm<sup>-3</sup>}} \cr
\tab\code{cec} \tab Cation Exchange Capacity of the soil \tab \ifelse{latex}{\out{$cmol(+)\dot kg^{-1}$}}{\out{cmol(+) kg<sup>-1</sup>}} \cr
\tab\code{cfvo} \tab Vol. fraction of coarse fragments (> 2 mm) \tab \ifelse{latex}{\out{$cm\cdot (100\cdot cm^{-3})$}}{\out{cm (100 cm)<sup>-3</sup>}} \cr
\tab\code{nitrogen} \tab Total nitrogen (N) \tab \ifelse{latex}{\out{$g\cdot kg^{-1}$}}{\out{g kg<sup>-1</sup>}} \cr
\tab\code{phh2o} \tab Soil pH \tab - \cr
\tab\code{sand} \tab Sand (>0.05 mm) in fine earth \tab \% \cr
\tab\code{silt} \tab Silt (0.002-0.05 mm) in fine earth \tab \% \cr
\tab\code{clay} \tab Clay (<0.002 mm) in fine earth \tab \% \cr
\tab\code{soc} \tab Soil organic carbon in fine earth \tab \ifelse{latex}{\out{$g\cdot kg^{-1}$}}{\out{g kg<sup>-1</sup>}} \cr
\tab\code{ocd} \tab Organic carbon density \tab \ifelse{latex}{\out{$kg\cdot m^{-3}$}}{\out{kg m<sup>-3</sup>}} \cr
\tab\code{ocs} \tab Organic carbon stocks \tab  \ifelse{latex}{\out{$kg\cdot m^{-2}$}}{\out{kg m<sup>-2</sup>}} \cr
}


}

\references{
Poggio, L., de Sousa, L.M., Batjes, N.H., Heuvelink, G.B.M., Kempen, B., Ribeiro, E., and Rossiter, D., 2021. SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty. Soil 7:217-240, 2021. doi:10.5194/soil-7-217-2021
}

\seealso{
For virtual access to the original data: \code{\link{soil_grids_vsi}}
For Africa: \code{\link{af_soil_isda}}, \code{\link{af_soil}}, \code{\link{af_soil_elements}}
}



\examples{
\donttest{
ph <- soil_grids(var="phh2o", depth=5, path=tmpdir())
}
}
