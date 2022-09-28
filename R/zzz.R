
.onLoad <- function(libname, pkgname) {
	options(timeout = max(900, getOption("timeout")))
}

