\name{bio_oracle}

\alias{bio_oracle}

\title{Marine data}

\description{
Marine data from Bio-Oracle
}

\usage{
bio_oracle(path, var, stat, benthic=FALSE, depth="Mean", time="Present", rcp, ...)
}

\arguments{
  \item{path}{character. Path for storing the downloaded data. See \code{\link{geodata_path}}}
  \item{var}{character. Variable of interest. One of 'Calcite', 'Chlorophyll', 'Cloud.cover', 'Current.Velocity', 'Diffuse.attenuation', 'Dissolved.oxygen', 'Ice.cover', 'Ice.thickness', 'Iron', 'Light.bottom', 'Nitrate', 'Par', 'pH', 'Phosphate', 'Phytoplankton', 'Primary.productivity', 'Salinity', 'Silicate', 'Temperature'}
  \item{stat}{character. Statistic of interest. One of 'Lt.max', 'Lt.min', 'Max', 'Mean', 'Min', 'Range'}
  \item{benthic}{logical. If \code{FALSE} surface data are returned}
  \item{depth}{character. Either "Min", "Mean", or "Max". Only relevant if \code{benthic} is \code{TRUE}}
  \item{time}{character. Either "Present", "2150" or "2100"}
  \item{rcp}{character. Either "26", "45", "60", or "85"}
  \item{...}{additional arguments passed to \code{\link{download.file}}}  
} 

\value{
SpatRaster
}

\references{
Assis, J., Tyberghein, L., Bosh, S., Verbruggen, H., Serrão, E.A., & De Clerck, O. (2017). Bio-ORACLE v2.0: Extending marine data layers for bioclimatic modelling. Global Ecology and Biogeography 27: 277-284.
}

\seealso{
\url{https://bio-oracle.org/}
}


\examples{
\donttest{
x <- bio_oracle(path=tempdir(), "Salinity", "Max", benthic=TRUE, depth="Mean", time="Present")

y <- bio_oracle(path=tempdir(), "Temperature", "Mean", benthic=FALSE, time=2100, rcp=45)
}
}
