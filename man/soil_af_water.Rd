\name{soil_af_water}

\alias{soil_af_water}

\title{Soil data for water balance computation (Africa only)}

\description{
Download physical soil properties data for Africa that can be used in water balance computation. The values are for a soil depth of 0 to 30 cm. The spatial resolution is 30 arc-seconds (about 1 km2), aggregated from the original 250m resolution.

For other properties see \code{\link{soil_af}}, \code{\link{soil_af_elements}}, \code{\link{soil_af_isda}}. 

For more info, see https://www.isric.org/projects/soil-property-maps-africa-250-m-resolution

The data have a CC-BY 4.0 NC license
}

\usage{
soil_af_water(var, depth = "30cm", path, ...)
}

\arguments{
  \item{var}{character. Variables name such as "awcpf23" or "pwp". See Details}
  
   \item{depth}{character. Either "30cm" or "erzd" (the effective rooting zone depth of maize)}
     
  \item{path}{character. Path for storing the downloaded data. See \code{\link{geodata_path}}}
  
  \item{...}{additional arguments passed to \code{\link{download.file}}}  
} 

\value{
SpatRaster
}



\details{
\tabular{lllr}{
\tab \bold{var} \tab \bold{description} \tab \bold{unit} \cr

\tab awcpf23 \tab Available water capacity of the fine earth with field capacity defined at pF 2.3 \tab volumetric \% \cr

\tab pwp \tab  Moisture content of the fine earth at permanent wilting point, with PWP defined at pF 4.2 \tab volumetric \% \cr

\tab tetas \tab Moisture content of the fine earth at saturation \tab volumetric \% \cr

\tab tawcpf23 \tab Absolute total available water capacity \tab cm? \cr

\tab tawcpf23mm \tab Absolute total available water capacity in mm \tab mm \cr

\tab erzd \tab Effective root zone depth (for maize) \tab cm \cr
}}


\seealso{
\code{\link{soil_af_elements}}, \code{\link{soil_af_isda}}, \code{\link{soil_world}}
}



\examples{
\donttest{
tetaS <- soil_af_water(var="tetas", depth="erzd", path=tempdir())
}
}
