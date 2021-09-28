
.onLoad <- function(libname, pkgname) {
	options(timeout = max(600, getOption("timeout")))
}

