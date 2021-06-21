
soil_grids_vrt <- function(var, depth, stat="mean", name="") {
	sg_url="/vsicurl/https://files.isric.org/soilgrids/latest/data/"
	var <- var[1]
	stopifnot(var %in% c("bdod", "cfvo", "clay", "nitrogen", "ocd", "phh2o", "sand", "silt", "soc", "wrb"))
	if (var == "wrb") {
		#h <- readLines("https://files.isric.org/soilgrids/latest/data/wrb/")
		#h <- grep("vrt<", h, value=TRUE)
		#h <- gsub("<li><a href=\"",  "", h)
		#h <- strsplit(h, ".vrt\"")
		#h <- sapply(h, function(i) i[1])
		h <- c('Acrisols', 'Albeluvisols', 'Alisols', 'Andosols', 'Arenosols', 'Calcisols', 'Cambisols', 'Chernozems', 'Cryosols', 'Durisols', 'Ferralsols', 'Fluvisols', 'Gleysols', 'Gypsisols', 'Histosols', 'Kastanozems', 'Leptosols', 'Lixisols', 'Luvisols', 'Nitisols', 'Phaeozems', 'Planosols', 'Plinthosols', 'Podzols', 'Regosols', 'Solonchaks', 'Solonetz', 'Stagnosols', 'Umbrisols', 'Vertisols') 
		name <- name[1]
		if (!(name %in% h)) {
			stop(paste("choose one of:", paste(h, collapse=", "))) 
		}
		u <- file.path(sg_url, var, paste0(name, ".vrt"))
	} else {
		depth <- as.character(round(depth[1]))
		if (var == "ocs") {
			stopifnot(depth == "30")	
			dd <- "0-30"
		} else {
			dpts <- c("5", "15", "30", "60", "100", "200")
			if (!(depth %in% dpts)) {
				stop(paste("depth must be one of:", paste(dpts, collapse=", ")))
			}
			dd <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")[depth == dpts]
		}
		stat <- stat[1]
		stopifnot(stat %in% c("mean", "uncertainty", "Q0.05", "Q0.5", "Q0.95"))
		u <- file.path(sg_url, var, paste0(var, "_", dd, "cm_", stat, ".vrt"))
	}
	rast(u)
}


