\name{footprint}

\alias{footprint}

\title{Human footprint}

\description{
The "human footprint" is an estimate of the direct and indirect human pressures on the environment. The human pressure is measured using eight variables including built-up environments, population density, electric power infrastructure, crop lands, pasture lands, roads, railways, and navigable waterways. It is expressed on a scale of 0 (low) to 50 (high footprint). See https://www.nature.com/articles/sdata201667 for the details. And the original data is available here: https://sedac.ciesin.columbia.edu/data/collection/wildareas-v3
 
Data are available for two years: 1993 and 2009, for all terrestrial areas except Antarctica. The footprint of seas and oceans was set to zero. The original data was in the Mollweide projection at a 1000 m spatial resolution. The data available through this function was transformed to a longitude/latitude grid at 30-seconds resolution.

Users are free to use, copy, distribute, transmit, and adapt the work for commercial and non-commercial purposes, without restriction, as long as clear attribution of the source is provided.
}

\usage{
footprint(year=2009, path, ...) 
}

\arguments{
  \item{year}{character. "1993" or "2009"} 
  \item{path}{character. Path name where the data should be downloaded to} 
  \item{...}{additional arguments passed to \code{\link{download.file}}}  
}

\value{
SpatRaster
}

\seealso{
\code{\link{landcover}}
}

\references{
Venter, O., E. W. Sanderson, A. Magrach, J. R. Allan, J. Beher, K. R. Jones, H. P. Possingham, W. F. Laurance, P. Wood, B. M. Fekete, M. A. Levy, and J. E. Watson. 2016. Sixteen Years of Change in the Global Terrestrial Human Footprint and Implications for Biodiversity Conservation. Nature Communications 7:12558. https://doi.org/10.1038/ncomms12558.
}