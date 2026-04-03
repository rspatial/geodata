\name{geodata_path}

\alias{geodata_path}

\title{Set the data path}

\description{

This function allows you set or get the default download cache path for the geodata package. The default path is ignored if you set the \code{path} argument in a geodata function to another value.

A special value is "user_data_dir". This sets it to the location returned by \code{file.path(rappdirs::user_data_dir(), "geodata")}

Alternatively, you can also set a system variable "GEODATA_PATH" to the desired path. 
}

\usage{
geodata_path(path, persistent=TRUE)
}


\arguments{
  \item{path}{character. Path name where geodata should be download and cache data. If missing, the current default path is returned. Use \code{NA} to restore the original value} 
  \item{persistent}{logical. Should the path be persistent (automatically used in future R sessions)?}
}

\value{
character
}

\examples{
geodata_path()
}

