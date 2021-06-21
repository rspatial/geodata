\name{af_soil_isda}

\alias{af_soil_isda}

\title{iSDA soil data for Africa}

\description{
Download iSDA soil data for Africa. 

The spatial resolution is 30 arc-seconds (about 1 km2), aggregated from the original 30m resolution.

For more info see:

\url{https://envirometrix.nl/isdasoil-open-soil-data-for-africa/} 

\url{https://zenodo.org/search?page=1&size=20&q=iSDAsoil}
}

\usage{
af_soil_isda(var, depth=20, path) 
}

\arguments{
  \item{var}{character. Variables name. One of: "al", "bdr", "clay", "c.tot", "ca", "db.od", "ecec.f", "fe", "k", "mg", "n.tot", "oc", "p", "ph", "sand", "silt", "s", "texture", "wpg2", "zn" see Details}
  \item{depth}{numeric. One of 20 (for 0-20 cm) and 50 (for 20-50 cm)}
  \item{version}{integer. Only version 0.13 is currently available} 
  \item{path}{character. Path name where the data should be downloaded to}  
}

\value{
SpatRaster
}

\details{
\tabular{lllr}{
\tab \bold{name} \tab bold{description} \tab bold{unit} \cr
\tab Al \tab extractable aluminum \tab mg kg-1 \cr
\tab bdr \tab bed rock depth \tab cm \cr
\tab clay \tab clay content \tab \% \cr
\tab C.tot \tab total carbon \tab kg-1 \cr
\tab Ca \tab extractable calcium \tab mg kg-1 \cr
\tab db.od \tab bulk density 100 g m3 \tab \cr
\tab eCEC.f \tab effective cation exchange capacity \tab cmol(+) kg-1 \cr
\tab Fe \tab extractable iron \tab \cr
\tab K \tab extractable potassium \tab mg kg-1 \cr
\tab Mg \tab extractable magnesium \tab mg kg-1 \cr
\tab N.tot \tab total organic nitrogen \tab g kg-1 \cr
\tab OC \tab Organic Carbon \tab  g kg-1  \cr
\tab P \tab Phosphorus content \tab mg kg-1 \cr
\tab pH \tab pH (H2O) \tab - \cr
\tab sand \tab Sand content \tab \% \cr
\tab silt \tab Silt content \tab \% \cr
\tab S \tab Extractable sulfer \tab mg kg-1\cr
\tab texture \tab texture class \tab - \cr
\tab wpg2 \tab stone content \tab \% \cr
\tab Zn \tab Extractable zinc \tab mg kg-1\cr
}
}


\references{
Tomislav Hengl, Matthew A. E. Miller, Josip Križan, Keith D. Shepherd, Andrew Sila, Milan Kilibarda, Ognjen Antonijević, Luka Glušica, Achim Dobermann, Stephan M. Haefele, Steve P. McGrath, Gifty E. Acquah, Jamie Collinson, Leandro Parente, Mohammadreza Sheykhmousa, Kazuki Saito, Jean-Martial Johnson, Jordan Chamberlin, Francis B. T. Silatsa, Martin Yemefack, John Wendt, Robert A. MacMillan, Ichsani Wheeler & Jonathan Crouch, 2021. African soil properties and nutrients mapped at 30 m spatial resolution using two-scale ensemble machine learning. Scientific Reports 11: 6130. 
}

\seealso{
\code{\link{af_elements}}, \code{\link{af_soil}}, \code{\link{soil_grids_vrt}}
}


\examples{
\donttest{
ph <- af_soil_isda("ph", path=tempdir())
}
}
