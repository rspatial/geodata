
crop_calendar_sacks <- function(crop="", path, ...) {

	.check_path(path)
	folder <- file.path(path, "sachs")
	dir.create(folder, FALSE, FALSE)
	
	m <- c("Barley.Winter.crop.calendar.nc.gz", "Barley (winter)", "Barley.crop.calendar.nc.gz", "Barley (spring)", "Cassava.crop.calendar.nc.gz", "Cassava", "Cotton.crop.calendar.nc.gz", "Cotton", "Groundnuts.crop.calendar.nc.gz", "Groundnut", "Maize.crop.calendar.nc.gz", "Maize (main season)", "Maize.2.crop.calendar.nc.gz", "Maize (2nd season)", "Millet.crop.calendar.nc.gz", "Millet", "Oats.Winter.crop.calendar.nc.gz", "Oat (winter)", "Oats.crop.calendar.nc.gz", "Oat (spring)", "Potatoes.crop.calendar.nc.gz", "Potato", "Pulses.crop.calendar.nc.gz", "Pulses", "Rapeseed.Winter.crop.calendar.nc.gz", "Rapeseed", "Rice.crop.calendar.nc.gz", "Rice (main season)", "Rice.2.crop.calendar.nc.gz", "Rice (2nd season)", "Rye.Winter.crop.calendar.nc.gz", "Rye", "Sorghum.crop.calendar.nc.gz", "Sorghum (main season)", "Sorghum.2.crop.calendar.nc.gz", "Sorghum (2nd season)", "Soybeans.crop.calendar.nc.gz", "soybean", "Sugarbeets.crop.calendar.nc.gz", "sugarbeet", "Sunflower.crop.calendar.nc.gz", "Sunflower", "Sweet.Potatoes.crop.calendar.nc.gz", "Sweetpotato", "Wheat.Winter.crop.calendar.nc.gz", "Wheat (winter)", "Wheat.crop.calendar.nc.gz", "Wheat (spring)", "Yams.crop.calendar.nc.gz", "Yam")
	m <- matrix(m, ncol=2, byrow=TRUE)

	if (!(crop %in% m[,2])) {
		cat("Choose one of:\n")
		print(m[,2])
	} else {
		i <- which(m[,2] == crop)
		fout <- file.path(folder, m[i,1])
		fout2 <- gsub(".gz$", "", fout)
		if (!file.exists(fout2)) {
			baseurl <- "https://biogeo.ucdavis.edu/data/geodata/crops/sacks/"
			url <- paste0(baseurl, m[i,1])
			.downloadDirect(url, fout, ...)
			R.utils::gunzip(fout)
		}
		r <- terra::rast(fout2)
		return(r)
	}
}


