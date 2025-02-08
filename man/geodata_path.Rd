\name{geodata_path}

\alias{geodata_path}

\title{Set the data path}

\description{

This function allows you set or get the default download cache path for the geodata package. The default path is ignored if you set the \code{path} argument in a geodata function to another value.

The "factory-fresh" default location for downloads is returned by \code{file.path(rappdirs::user_data_dir(), ".geodata")}

You can set it to another value with this function. To save your own default path across sessions, you can add a line like this:

\code{options( geodata_default_path = "c:/your/geodata/path")} to the file returned by \code{file.path( R.home(), "etc/Rprofile.site")}

Alternatively, you can also set a system variable "GEODATA_PATH" to the desired path. 
}

\usage{
geodata_path(path)
}


\arguments{
  \item{path}{character. Path name where geodata should be download and cache data. If missing, the current default path is returned. Use \code{NA} to restore the original value} 
}

\value{
character
}

\examples{
geodata_path()
}

