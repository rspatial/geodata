\name{climate}
\alias{climate}

\title{Climate data}

\usage{
worldclim(var, res, path, ...)
worldclim30s(var, path, ...)
}

\arguments{
  \item{var}{Character. Valid variables names are 'tmin', 'tmax', 'prec' and 'bio'}
  \item{res}{Numeric. Valid resolutions are 2.5, 5, and 10 (minutes of a degree)}
  \item{path}{Character. Path name indicating where to store the data} 
  \item{...}{Additional arguments (none implemented)}  
}

\value{
SpatRaster
}


\examples{
\dontrun{
# To get current conditions:
worldclim(var='tmin', res=10)
}
}

\seealso{
\url{http://www.worldclim.org/} 
}

