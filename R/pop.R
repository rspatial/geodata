

population <- function(year, res="5", path, ...) {
	stopifnot(dir.exists(path))
	stopifnot(as.numeric(year) %in% c(2000, 2005, 2010, 2015, 2020))
	stopifnot(as.numeric(res) %in% c(10, 5, 2.5, 0.5))
	res <- ifelse(res==0.5, "30s", paste0(res, "m"))

	filename <- paste0("gpw_v4_population_density_rev11_", year, "_", res, ".tif")
	filepath <- file.path(path, "pop", filename)

	if (!(file.exists(filepath))) {
		url <- paste0("https://biogeo.ucdavis.edu/data/geodata/pop/", filename)
		dir.create(dirname(filepath), showWarnings=FALSE)
		.downloadDirect(url, filepath, ...)
	} 
	rast(filepath)
}


