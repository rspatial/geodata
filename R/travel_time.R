

travel_time <- function(to="city", size=1, up=FALSE, path, ...) {
	path <- .get_path(path)

	to <- tolower(to)
	vars <- c("city", "port")
	if (!(to %in% vars)) {
		stop(paste("to is not valid. Use one of:", vars))
	}
	size <- round(size)
	if (to == "city") {
		stopifnot(size %in% 1:9)
		f <- "travel_time_to_cities_"
	} else if (to == "port") {
		stopifnot(size %in% 1:5)
		f <- "travel_time_to_ports_"
	}
	if (up && (size != 1)) {
		f <- paste0(f, "u")
	}
	filename <- paste0(f, size, ".tif")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0(.data_url(), "travel/", filename)
		if (!.downloadDirect(url, filepath, ...)) return(NULL)
	} 
	
	r <- rast(filepath)
	if (!up) {
		NAflag(r) <- 65535
	}
	r
}



