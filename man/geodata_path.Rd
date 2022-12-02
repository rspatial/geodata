\name{geodata_path}

\alias{geodata_path}

\title{geodata_path}

\description{
Set or get the default download path for the geodata package.
}

\usage{
geodata_path(path)
}

\note{
To save the default path across sessions, you can add a line like this 

\code{options(geodata_geodata_path="c:/your/geodata/path")}

to this filename returned by this command 

\code{file.path(R.home(), "etc/Rprofile.site")}
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