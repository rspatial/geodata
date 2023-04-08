\name{rice_calendar}

\alias{rice_calendar}


\title{crop calendar for rice}

\description{
Get crop calendar and production data for rice 
}

\usage{
rice_calendar(path, ...)
}

\arguments{
 \item{path}{character. Path for storing the downloaded data. See \code{\link{geodata_path}}}  
  \item{...}{additional arguments passed to \code{\link{download.file}}} 
}

\value{
SpatVectorCollection
}

\references{
Laborte, A.G.; Gutierrez, M.A.; Balanza, J.G.; Saito, K.; Zwart, S.J.; Boschetti, M.; Murty, MVR; Villano, L.; Aunario, J.K.; Reinke, R.; Koo, J.; Hijmans, R.J.; Nelson, A., 2017. RiceAtlas, a spatial database of global rice calendars and production. Scientific Data 4: 170074. https://doi.org/10.1038/sdata.2017.74
}


\examples{
\donttest{
rice <- rice_calendar(path=tempdir())
cal <- rice[1]
}
}
