
spamCrops <- function() {
	x <- matrix(c("wheat", "whea", "rice", "rice", "maize", "maiz", "barley", "barl", "pearl millet", "pmil", "small millet", "smil", "sorghum", "sorg", "other cereals", "ocer", "potato", "pota", "sweet potato", "swpo", "yams", "yams", "cassava", "cass", "other roots", "orts", "bean", "bean", "chickpea", "chic", "cowpea", "cowp", "pigeonpea", "pige", "lentil", "lent", "other pulses", "opul", "soybean", "soyb", "groundnut", "grou", "coconut", "cnut", "oilpalm", "oilp", "sunflower", "sunf", "rapeseed", "rape", "sesameseed", "sesa", "other oil crops", "ooil", "sugarcane", "sugc", "sugarbeet", "sugb", "cotton", "cott", "other fibre crops", "ofib", "arabica coffee", "acof", "robusta coffee", "rcof", "cocoa", "coco", "tea", "teas", "tobacco", "toba", "banana", "bana", "plantain", "plnt", "tropical fruit", "trof", "temperate fruit", "temf", "vegetables", "vege", "rest of crops", "rest"), ncol=2, byrow=TRUE)
	colnames(x) <- c("crop", "code")
	x
}



crop_spam <- function(crop="", var="area", path, africa=FALSE, ...) {
	
	# area is allowed for backwards compatibility
	stopifnot(var %in% c("area", "yield", "harv_area", "phys_area", "val_prod", "prod"))

	path <- .get_path(path, add="spam")
	crp <- tolower(trimws(crop))
	crops <- spamCrops()
	if (!(crp %in% crops)) { stop("crop not in SPAM; see spamCrops()") }
	i <- which(crp == crops)[1]
	if (i > nrow(crops)) i = i - nrow(crops)
	crp <- toupper(crops[i,2])
	
	urlbase <- "https://dataverse.harvard.edu/api/access/datafile/"
	#4271688?format=original"

	if (africa) {
		pre <- "spam2017v2r1_ssa_"
		if (var %in% c("area", "harv_area")) {
			id = 4271688
		} else if (var == "phys_area") {
			id = 4271676
		} else if (var == "prod") {
			id = 4271677
		} else if (var == "val_prod") {
			id = 4271679
		} else if (var == "yield") {
			id = 4271678
		}
	} else {
		pre <- "spam2010v2r0_global_"
		if (var %in% c("area", "harv_area")) {
			id = 3985008
		} else if (var == "phys_area") {
			id = 3985010
		} else if (var == "prod") {
			id = 3985009
		} else if (var == "val_prod") {
			id = 3985011
		} else if (var == "yield") {
			id = 3985012
		}
	}
	url <- paste0(urlbase, id, "?format=original") 
	zipf <- file.path(path, paste0(pre, var, ".zip"))
	if (!file.exists(zipf)) {
		if (!.downloadDirect(url, zipf, ...)) return(NULL)
	}
	ff <- utils::unzip(zipf, list=TRUE)
	fs <- grep(crp, ff$Name, value=TRUE)
	ffs <- file.path(path, fs)
	if (all(!file.exists(ffs))) {
		# utils::unzip(zipf, fails for yield !?
		utils::unzip(zipf, files=fs, junkpaths=TRUE, exdir=path)
		#if (length(files) == 0) file.remove(zipf)
	}
	x <- terra::rast(ffs)
	
	
	syst <- c("A", "all", "I", "irrigated", "H", "rainfed-highinput", "L", "rainfed-lowinput", "S", "rainfed-subsistence", "R", "rainfed")
	syst <- matrix(syst, ncol=2, byrow=TRUE)
	nms <- names(x)
	i <- match(syst[,1], substr(nms, nchar(nms), nchar(nms)))
	names(x) <- paste0(crop, "_", var, "_", syst[i,2])

	terra::ext(x) <- c(-180, 180, -90, 90)
	if (africa) {
		x <- crop(x, ext(-26, 58, -35, 26))
	}
	if (var=="yield") {
		x <- classify(x, cbind(0, NA))
	}
	x
}


