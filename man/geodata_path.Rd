\name{geodata_path}

\alias{geodata_path}

\title{Set the data path}

\description{
This function allows you set or get the default download path for the geodata package. By setting this path you can avoid downloading the same data many times over. This also guards against service interruptions.

The default path is ignored if you use the path variable in a function.

To save the default path across sessions, you can add a line like this \code{options(geodata_path="c:/your/geodata/path")} to the file returned by this command  \code{file.path(R.home(), "etc/Rprofile.site")}
}

\usage{
geodata_path(path)
}


\arguments{
  \item{path}{character. Path name where the data should be downloaded to. If missing, the current default path is returned} 
}

\value{
character
}

\examples{
geodata_path()
}
