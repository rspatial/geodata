

country_codes <- function() {
	path <- system.file(package="geodata")
	#d <- utils::read.csv(paste(path, "/ex/countries.csv", sep=""), stringsAsFactors=FALSE, encoding="UTF-8")
	readRDS(file.path(path, "ex/countries.rds"))
}



country_codes <- function(query = NULL) {
	path <- system.file(package="geodata")
	res <- readRDS(file.path(path, "ex/countries.rds"))
    if ((!is.null(query)) && (!is.na(query[1]))) {
		query <- query[1]
        hits <- apply(res, 1, function(x) any(grepl(query, x, ignore.case = TRUE)))
        res[hits, ]
    } else {
		res
	}
}


.getCountryISO <- function(country) {

	country <- toupper(trimws(country[1]))
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
		stop('provide a valid name or 3 letter ISO country code; you can get a list with "country_codes()"')
	}
}



