\name{crop_monfreda}

\alias{crop_monfreda}
\alias{monfredaCrops}


\title{Monfreda crop data}

\description{
Monfreda global crop data (area, yield) for 175 crops.

Data may be freely used for research, study, or teaching, but must be cited appropriately (see below). Re-release of the data, or incorporation of the data into a commercial product, is allowed only with explicit permission.
}

\usage{
monfredaCrops()
crop_monfreda(crop="", var="area_ha", path, ...)
}

\arguments{
 \item{crop}{character. Crop name(s). See \code{monfredaCrops} for valid names}
 \item{var}{character. The variable(s) of interest. Choose from "area_ha" (crop area in ha per cell), "area_f" (crop area as a fraction of each cell), "area_q" (quality of the crop area data), "yield" (crop yield in Mg/ha), "yield_q" (quality of the yield data), "prod" (production per grid cell in Mg), or "all"}
 \item{path}{character. Path for storing the downloaded data. See \code{\link{geodata_path}}}
 \item{...}{additional arguments passed to \code{\link{download.file}}}
}

\value{
SpatRaster
}

\references{
Monfreda, C., N. Ramankutty, and J. A. Foley (2008), Farming the planet: 2. Geographic distribution of crop areas, yields, physiological types, and net primary production in the year 2000, Global Biogeochem. Cycles, 22, GB1022, doi:10.1029/2007GB002947.
}

\seealso{
\url{http://www.earthstat.org/harvested-area-yield-175-crops/}
}


\examples{
mcas <- crop_monfreda("cassava", path=tempdir())
mcas
names(mcas)
}
