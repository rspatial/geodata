\name{travel_time}

\alias{travel_time}

\title{Travel time to a city or port}

\description{
Download travel time to a city or port on rasters at a 30 arc-seconds (about 1 km2) resolution. 

\url{https://www.nature.com/articles/s41597-019-0265-5} 
}

\usage{
travel_time(to="city", size=1, path, ...) 
}

\arguments{
  \item{to}{character. "city" or "port"}
  \item{size}{positive integer indicating the size of the city or port. can be between 1 and 9 if \code{to="city"} or between 1 and 5 if \code{to="port"}. See Details} 
  \item{path}{character. Path name where the data should be downloaded to}  
  \item{...}{additional arguments passed to \code{\link{download.file}}}  
}

\value{
SpatRaster
}

\details{
Description of the the \code{size} argument

\tabular{llr}{
\tab \bold{size}\tab \bold{inhabitants} \cr
\tab 1\tab 5,000,000 to 50,000,000\cr
\tab 2\tab 1,000,000 to 5,000,000\cr
\tab 3\tab 500,000 to 1,000,000\cr
\tab 4\tab 200,000 to 500,000\cr
\tab 5\tab 100,000 to 200,000\cr
\tab 6\tab 50,000 to 100,000\cr
\tab 7\tab 20,000 to 50,000\cr
\tab 8\tab 10,000 to 20,000\cr
\tab 9\tab 5,000 to 10,000\cr
}

\tabular{lllr}{
\tab \bold{size}\tab \bold{description} \tab \bold{number of ports} \cr
\tab 1 \tab Large \tab 160 \cr
\tab 2 \tab Medium \tab 361 \cr
\tab 3 \tab Small \tab 990 \cr
\tab 4 \tab Very small \tab 2,153 \cr
\tab 5 \tab Any \tab 3,778 \cr
}
}


\references{
Nelson, A., Weiss, D.J., van Etten, J. et al. A suite of global accessibility indicators. Sci Data 6, 266 (2019). https://doi.org/10.1038/s41597-019-0265-5

Version 3 (2019-05-15) from \url{https://figshare.com/articles/dataset/Travel_time_to_cities_and_ports_in_the_year_2015/7638134/3}
}


\examples{
\donttest{
ttime <- travel_time("city", 2, path=tempdir())
}
}

