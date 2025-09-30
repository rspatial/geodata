

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

	country <- toupper(trimws(country))
	cs <- country_codes()
	cs <- sapply(cs, toupper)
	cs <- data.frame(cs, stringsAsFactors=FALSE)
	nc <- nchar(country)
	
	out <- rep(NA, length(nc))

	iso3 <- nc == 3	
	if (any(iso3)) {
		j <- country %in% cs$ISO3
		out[j] <- country[j]
	} 
	if (!any(is.na(out))) return(out)

	iso2 <- nc == 2
	if (any(iso2)) {
		j <- country %in% cs$ISO2
		if (any(j)) {
			j <- which(j)
			i <- match(country[j], cs$ISO2)
			out[j] <- cs$ISO3[i]
		}
	}
	if (!any(is.na(out))) return(out)

	for (nc in c(1, 4, 5, 6)) {
		j <- (country %in% cs[,nc])
		if (any(j)) {
			j <- which(j)
			i <- match(country[j], cs[,nc])
			out[j] <- cs$ISO3[i]
		}
		if (!any(is.na(out))) return(out)
	}
	stop(paste("unknown country:", paste(country[is.na(out)], collapse=", ")), '\n  Provide a valid name or 3 letter ISO country code; see "?country_codes()"')
}

