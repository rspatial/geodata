\name{soil_grids}

\alias{soil_grids}

\title{soil grids}

\description{
Virtually connect to the soilgrids data. 
}

\usage{
soil_grids(var, depth, stat="mean", name="")
}

\arguments{
  \item{var}{character. Variables name. One of: \code{"bdod", "cfvo", "clay", "nitrogen", "ocd", "ocs", "phh2o", "sand", "silt", "soc", "wrb"}. See Details}
  \item{depth}{numeric. One of \code{5, 15, 30, 60, 100, 200}. This is shorthand for the following depth ranges: 0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm. Ignored if \code{var="wrb"}}
  \item{stat}{character. One of "mean", "uncertainty", "Q0.05", "Q0.5", "Q0.95". Ignored if \code{var="wrb"}}
  \item{name}{character. One of \code{'Acrisols', 'Albeluvisols', 'Alisols', 'Andosols', 'Arenosols', 'Calcisols', 'Cambisols', 'Chernozems', 'Cryosols', 'Durisols', 'Ferralsols', 'Fluvisols', 'Gleysols', 'Gypsisols', 'Histosols', 'Kastanozems', 'Leptosols', 'Lixisols', 'Luvisols', 'Nitisols', 'Phaeozems', 'Planosols', 'Plinthosols', 'Podzols', 'Regosols', 'Solonchaks', 'Solonetz', 'Stagnosols', 'Umbrisols', 'Vertisols'}. Only used when \code{var="wrb"}}  
}

\value{
SpatRaster
}


\details{
The below table lists the variable names, a brief description, and their units, as well as the conversion factor that can be used to get the conventional units.

\tabular{lllllr}{
\tab \bold{Name} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Fact} \tab \bold{Convent. units}\cr
\tab\code{bdod} \tab Bulk density of the fine earth fraction \tab cg/cm3 \tab 100 \tab kg/dm3 \cr
\tab\code{cec} \tab Cation Exchange Capacity of the soil \tab mmol(c)/kg \tab 10 \tab cmol(c)/kg \cr
\tab\code{cfvo} \tab Vol. fraction of coarse fragments (> 2 mm) \tab cm3/dm3 \tab 10 \tab cm3/100cm3 \cr
\tab\code{nitrogen} \tab Total nitrogen (N) \tab cg/kg \tab 100 \tab g/kg \cr
\tab\code{phh2o} \tab Soil pH \tab pH x 10 \tab 10 \tab pH \cr
\tab\code{sand} \tab Sand (>0.05 mm) in fine earth \tab g/kg \tab 10 \tab g/100g\cr
\tab\code{silt} \tab Silt (0.002-0.05 mm) in fine earth \tab g/kg \tab 10 \tab g/100g\cr
\tab\code{clay} \tab Clay (<0.002 mm) in fine earth \tab g/kg \tab 10 \tab g/100g\cr
\tab\code{soc} \tab Soil organic carbon in fine earth \tab dg/kg \tab 10 \tab g/kg \cr
\tab\code{ocd} \tab Organic carbon density \tab hg/m3 \tab 10 \tab kg/m3 \cr
\tab\code{ocs} \tab Organic carbon stocks \tab t/ha \tab 10 \tab kg/m2 \cr
}


license: CC-BY 4.0
}

\references{
Poggio, L., de Sousa, L.M., Batjes, N.H., Heuvelink, G.B.M., Kempen, B., Ribeiro, E., and Rossiter, D., 2021. SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty. Soil 7:217-240, 2021. doi:10.5194/soil-7-217-2021
}

\seealso{
\url{https://www.isric.org/explore/soilgrids}
}



\examples{
\donttest{
ph <- soil_grids(var="phh2o", depth=5)
}
}
