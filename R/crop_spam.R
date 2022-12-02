
spamCrops <- function() {
	x <- matrix(c("wheat", "whea", "rice", "rice", "maize", "maiz", "barley", "barl", "pearl millet", "pmil", "small millet", "smil", "sorghum", "sorg", "other cereals", "ocer", "potato", "pota", "sweet potato", "swpo", "yams", "yams", "cassava", "cass", "other roots", "orts", "bean", "bean", "chickpea", "chic", "cowpea", "cowp", "pigeonpea", "pige", "lentil", "lent", "other pulses", "opul", "soybean", "soyb", "groundnut", "grou", "coconut", "cnut", "oilpalm", "oilp", "sunflower", "sunf", "rapeseed", "rape", "sesameseed", "sesa", "other oil crops", "ooil", "sugarcane", "sugc", "sugarbeet", "sugb", "cotton", "cott", "other fibre crops", "ofib", "arabica coffee", "acof", "robusta coffee", "rcof", "cocoa", "coco", "tea", "teas", "tobacco", "toba", "banana", "bana", "plantain", "plnt", "tropical fruit", "trof", "temperate fruit", "temf", "vegetables", "vege", "rest of crops", "rest"), ncol=2, byrow=TRUE)
	colnames(x) <- c("crop", "code")
	x
}



crop_spam <- function(crop="", var="area", path=".", africa=FALSE, ...) {
	folder <- file.path(path, "spam")
	# area is allowed for backwards compatibility
	stopifnot(var %in% c("area", "yield", "harv_area", "phys_area", "val_prod", "prod"))

	path <- .get_path(path)
	dir.create(folder, FALSE, FALSE)
	crp <- tolower(trimws(crop))
	crops <- spamCrops()
	if (!(crp %in% crops)) { stop("crop not in SPAM; see spamCrops()") }
	i <- which(crp == crops)[1]
	if (i > nrow(crops)) i = i - nrow(crops)
	crp <- toupper(crops[i,2])
	if (africa) {
		urlbase <- "https://s3.amazonaws.com/mapspam/2017/ssa/v1.1/geotiff/"	
	} else {
		urlbase <- "https://s3.amazonaws.com/mapspam/2010/v1.1/geotiff/"
	}
	if (var == "area" || var == "harv_area") {
		url <- paste0(urlbase, "spam2010v1r1_global_harv_area.geotiff.zip")
	} else if (var == "phys_area") {
		if (!africa) {
			url <- "https://s3.amazonaws.com/mapspam/2010/v2.0/geotiff/spam2010v2r0_global_phys_area.geotiff.zip"
		} else {
			url <- "https://s3.amazonaws.com/mapspam/2017/ssa/v2.1/geotiff/spam2017v2r1_ssa_phys_area.geotiff.zip"
		}
	} else if (var == "prod") {
		url <- paste0(urlbase, "spam2010v1r1_global_prod.geotiff.zip")
	} else if (var == "val_prod") {
		url <- paste0(urlbase, "spam2010v1r1_global_val_prod.geotiff.zip")
	} else {
		url <- paste0(urlbase, "spam2010v1r1_global_yield.geotiff.zip")
	}

	pre <- "spam2010v1r1_global_"
	if (africa) {
		afpre <- "spam2017v1r1_ssa_"
		url <- gsub(pre, afpre, url)
		pre <- afpre
	}
	zipf <- file.path(folder, basename(url))
	if (!file.exists(zipf)) {
		if (!.downloadDirect(url, zipf, ...)) return(NULL)
	}
	ff <- utils::unzip(zipf, list=TRUE)
	fs <- grep(crp, ff$Name, value=TRUE)
	ffs <- file.path(folder, fs)
	if (all(!file.exists(ffs))) {
		utils::unzip(zipf, files=fs, junkpaths=TRUE, exdir=folder)
	}
	x <- terra::rast(ffs)

	#nicenms <- c("A", "all", "I", "irrigated", "H", "rainfed-highinput", "L", "rainfed-lowinput", "S", "rainfed-subsistence", "R", "rainfed")
	#nicenms <- matrix(nicenms, ncol=2, byrow=TRUE)
	#nicenms <- nicenms[order(nicenms[,1]), ]
	#if (africa) {
	#	nicenms <- nicenms[nicenms[,1] %in% c("A", "I", "R"), ]
	#}

	#n <- sort(names(x))
	#n <- substr(n, nchar(n[1]), nchar(n[1]))
	#i <- match(n, nicenms[,1])
	names(x) <- gsub(pre, "", names(x), ignore.case=TRUE)
	names(x) <- gsub("gr_", "", names(x), ignore.case=TRUE)
	
	terra::ext(x) <- c(-180, 180, -90, 90)
	if (africa) {
		x <- crop(x, ext(-26, 58, -35, 26))
	}
	if (var=="yield") {
		x <- classify(x, cbind(0, NA))
	}
	x
}


