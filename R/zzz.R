
.onLoad <- function(libname, pkgname) {
	options(timeout = max(6000, getOption("timeout")))
}

