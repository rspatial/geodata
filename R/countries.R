

country_codes <- function() {
	path <- system.file(package="geodata")
	#d <- utils::read.csv(paste(path, "/ex/countries.csv", sep=""), stringsAsFactors=FALSE, encoding="UTF-8")
	readRDS(file.path(path, "ex/countries.rds"))
}


.getCountryISO <- function(country) {

	country <- toupper(trim(country[1]))
	cs <- country_codes()
	cs <- sapply(cs, toupper)
	cs <- data.frame(cs, stringsAsFactors=FALSE)
	nc <- nchar(country)

	if (nc == 3) {
		if (country %in% cs$ISO3) {
			return(country)
		} else {
			stop('unknown country')
		}
	} else if (nc == 2) {
		if (country %in% cs$ISO2) {
			i <- which(country==cs$ISO2)
			return( cs$ISO3[i] )
		} else {
			stop('unknown country')
		}
	} else if (country %in% cs[,1]) {
		i <- which(country==cs[,1])
		return( cs$ISO3[i] )
	} else if (country %in% cs[,4]) {
		i <- which(country==cs[,4])
		return( cs$ISO3[i] )
	} else if (country %in% cs[,5]) {
		i <- which(country==cs[,5])
		return( cs$ISO3[i] )
	} else {
		stop('provide a valid name name or 3 letter ISO country code; you can get a list with "country_codes()"')
	}
}




.countries <- function(download, path, type='sp', ...) {

	if (type == 'sf') {
		f <- "countries_gadm36_sf.rds"
	} else {
		f <- "countries_gadm36_sp.rds"	
	}
	filename <- file.path(path, f, sep="")
	
	if (!file.exists(filename)) {
		if (download) {
			theurl <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/", f)
			.download(theurl, filename)
			if (!file.exists(filename)) {
				message("\nCould not download file -- perhaps it does not exist") 
			}
		} else {
			message("File not available locally. Use 'download = TRUE'")
		}
	}	
	if (file.exists(filename)) {
		#thisenvir = new.env()
		#data <- get(load(filename, thisenvir), thisenvir)
		data <- readRDS(filename)
		return(data)
	} 
}
