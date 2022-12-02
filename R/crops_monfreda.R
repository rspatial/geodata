monfredaCrops <- function() {
m <- matrix(c("abaca","Manila Fibre (Abaca)","Fiber","agave","Agave Fibres Nes","Fiber","alfalfa","alfalfa","Forage","almond","Almonds,with shell","Treenuts","aniseetc","Anise,badian,fennel,corian.","OtherCrops","apple","Apples","Fruit","apricot","Apricots","Fruit","areca","Arecanuts","OtherCrops","artichoke","Artichokes","Vegetables&Melons","asparagus","Asparagus","Vegetables&Melons","avocado","Avocados","Fruit","bambara","Bambara beans","Pulses","banana","Bananas","Fruit","barley","Barley","Cereals","bean","Beans,dry","Pulses","beetfor","beetfor","Forage","berrynes","Berries Nes","Fruit","blueberry","Blueberries","Fruit","brazil","Brazil nuts,with shell","Treenuts","broadbean","Broad beans,horse beans,dry","Pulses","buckwheat","Buckwheat","Cereals","cabbage","Cabbages and other brassicas","Vegetables&Melons","cabbagefor","cabbagefor","Forage","canaryseed","Canary seed","Cereals","carob","Carobs","Fruit","carrot","Carrots and turnips","Vegetables&Melons","carrotfor","carrotfor","Forage","cashew","Cashew nuts,with shell","Treenuts","cashewapple","Cashewapple","Fruit","cassava","Cassava","Roots&Tubers","castor","Castor oil seed","Oilcrops","cauliflower","Cauliflowers and broccoli","Vegetables&Melons","cerealnes","Cereals,nes","Cereals","cherry","Cherries","Fruit","chestnut","Chestnuts","Treenuts","chickpea","Chick peas","Pulses","chicory","Chicory roots","OtherCrops","chilleetc","Chillies and peppers,green","Vegetables&Melons","cinnamon","Cinnamon (canella)","OtherCrops","citrusnes","Citrus fruit,nes","Fruit","clove","Cloves","OtherCrops","clover","clover","Forage","cocoa","Cocoa beans","OtherCrops","coconut","Coconuts","Oilcrops","coffee","Coffee,green","OtherCrops","cotton","Seed cotton","Fiber","cowpea","Cow peas,dry","Pulses","cranberry","Cranberries","Fruit","cucumberetc","Cucumbers and gherkins","Vegetables&Melons","currant","Currants","Fruit","date","Dates","Fruit","eggplant","Eggplants (aubergines)","Vegetables&Melons","fibrenes","Fibre Crops Nes","Fiber","fig","Figs","Fruit","flax","Flax fibre and tow","Fiber","fonio","Fonio","Cereals","fornes","fornes","Forage","fruitnes","Fruit Fresh Nes","Fruit","garlic","Garlic","Vegetables&Melons","ginger","Ginger","OtherCrops","gooseberry","Gooseberries","Fruit","grape","Grapes","Fruit","grapefruitetc","Grapefruit (inc. pomelos)","Fruit","grassnes","grassnes","Forage","greenbean","Beans,green","Vegetables&Melons","greenbroadbean","Leguminous vegetables,nes","Vegetables&Melons","greencorn","Maize,green","Vegetables&Melons","greenonion","Onions (inc. shallots),green","Vegetables&Melons","greenpea","Peas,green","Vegetables&Melons","groundnut","Groundnuts,with shell","Oilcrops","hazelnut","Hazelnuts,with shell","Treenuts","hemp","Hemp Tow Waste","Fiber","hempseed","Hempseed","Oilcrops","hop","Hops","OtherCrops","jute","Jute","Fiber","jutelikefiber","Other Bastfibres","Fiber","kapokfiber","Kapok Fibre","Fiber","kapokseed","Kapokseed in Shell","Fiber","karite","Karite Nuts (Sheanuts)","Oilcrops","kiwi","Kiwi fruit","Fruit","kolanut","Kolanuts","OtherCrops","legumenes","legumenes","Forage","lemonlime","Lemons and limes","Fruit","lentil","Lentils","Pulses","lettuce","Lettuce and chicory","Vegetables&Melons","linseed","Linseed","Oilcrops","lupin","Lupins","Pulses","maize","Maize","Cereals","maizefor","maizefor","Forage","mango","Mangoes,mangosteens,guavas","Fruit","mate","Mate","OtherCrops","melonetc","Other melons (inc.cantaloupes)","Vegetables&Melons","melonseed","Melonseed","Oilcrops","millet","Millet","Cereals","mixedgrain","Mixed grain","Cereals","mixedgrass","mixedgrass","Forage","mushroom","Mushrooms and truffles","Vegetables&Melons","mustard","Mustard seed","Oilcrops","nutmeg","Nutmeg,mace and cardamoms","OtherCrops","nutnes","Nuts,nes","Treenuts","oats","Oats","Cereals","oilpalm","Oil palm fruit","Oilcrops","oilseedfor","oilseedfor","Forage","oilseednes","Oilseeds,Nes","Oilcrops","okra","Okra","Vegetables&Melons","olive","Olives","Oilcrops","onion","Onions,dry","Vegetables&Melons","orange","Oranges","Fruit","papaya","Papayas","Fruit","pea","Peas,dry","Pulses","peachetc","Peaches and nectarines","Fruit","pear","Pears","Fruit","pepper","Pepper (Piper spp.)","OtherCrops","peppermint","Peppermint","OtherCrops","persimmon","Persimmons","Fruit","pigeonpea","Pigeon peas","Pulses","pimento","Chillies and peppers,dry","OtherCrops","pineapple","Pineapples","Fruit","pistachio","Pistachios","Treenuts","plantain","Plantains","Fruit","plum","Plums and sloes","Fruit","poppy","Poppy seed","Oilcrops","potato","Potatoes","Roots&Tubers","pulsenes","Pulses,nes","Pulses","pumpkinetc","Pumpkins,squash and gourds","Vegetables&Melons","pyrethrum","Pyrethrum,Dried","OtherCrops","quince","Quinces","Fruit","quinoa","Quinoa","Cereals","ramie","Ramie","Fiber","rapeseed","Rapeseed","Oilcrops","rasberry","Raspberries","Fruit","rice","Rice,paddy","Cereals","rootnes","Roots and Tubers,nes","Roots&Tubers","rubber","Natural rubber","OtherCrops","rye","Rye","Cereals","ryefor","ryefor","Forage","safflower","Safflower seed","Oilcrops","sesame","Sesame seed","Oilcrops","sisal","Sisal","Fiber","sorghum","Sorghum","Cereals","sorghumfor","sorghumfor","Forage","sourcherry","Sour cherries","Fruit","soybean","Soybeans","Oilcrops","spicenes","Spices,nes","OtherCrops","spinach","Spinach","Vegetables&Melons","stonefruitnes","Stone fruit,nes","Fruit","strawberry","Strawberries","Fruit","stringbean","String beans","Vegetables&Melons","sugarbeet","Sugar beet","SugarCrops","sugarcane","Sugar cane","SugarCrops","sugarnes","Sugar crops,nes","SugarCrops","sunflower","Sunflower seed","Oilcrops","swedefor","swedefor","Forage","sweetpotato","Sweet potatoes","Roots&Tubers","tangetc","Tangerines,mandarins,clem.","Fruit","taro","Taro (cocoyam)","Roots&Tubers","tea","Tea","OtherCrops","tobacco","Tobacco,unmanufactured","OtherCrops","tomato","Tomatoes","Vegetables&Melons","triticale","Triticale","Cereals","tropicalnes","Fruit,tropical fresh nes","Fruit","tung","Tung Nuts","Oilcrops","turnipfor","turnipfor","Forage","vanilla","Vanilla","OtherCrops","vegetablenes","Vegetables fresh nes","Vegetables&Melons","vegfor","vegfor","Forage","vetch","Vetches","Pulses","walnut","Walnuts,with shell","Treenuts","watermelon","Watermelons","Vegetables&Melons","wheat","Wheat","Cereals","yam","Yams","Roots&Tubers","yautia","Yautia (cocoyam)","Roots&Tubers"),ncol=3,byrow=TRUE)

colnames(m) <- c("name","FAO_name","group")
data.frame(m)
}



crop_monfreda <- function(crop="", var="area_ha", path, ...) {
#	stopifnot(var %in% c("areaf", "areah", "yield", "prod"))
	path <- .get_path(path)
	
	folder <- file.path(path, "monfreda")
	dir.create(folder, FALSE, FALSE)
	crp <- tolower(trimws(crop))
	vars <- c("area_ha", "area_f", "area_q", "yield", "yield_q", "prod", "all")
	subds <- c("_HarvestedAreaHectares.tif", "_HarvestedAreaFraction.tif", "_DataQuality_HarvestedArea.tif", "_YieldPerHectare.tif", "_DataQuality_Yield.tif", "_Production.tif")
	
	var <- tolower(var)
	good <- var %in% vars
	if (!all(good)) {
		var <- paste(var[!good], collapse=", ")
		stop(paste(var, "is not a valid variable name"))
	}	 
	if (any(var %in% c("", "all"))) {
		ss <- subds	
		var <- vars[1:6]
	} else {
		ss <- subds[match(var, vars)]
	}
	
	if (crp[1] == "all") {
		x <- vector(mode="list", length=length(ss))
		for (i in seq_along(ss)) {
			url <- paste0("https://geodata.ucdavis.edu/geodata/crops/monfreda/Monfreda", gsub(".tif$", ".zip", ss[i]))
			zipf <- file.path(folder, basename(url))
			if (!file.exists(zipf)) {
				if (!.downloadDirect(url, zipf)) return(NULL)
			}
			ff <- utils::unzip(zipf, list=TRUE)
			ff <- grep(".tif$", ff$Name, value=TRUE)
			fs <- file.path(folder, basename(ff))
			if (!all(file.exists(fs))) {
				utils::unzip(zipf, files=ff, junkpaths=TRUE, exdir=folder)
			}
			x[[i]] <- rast(fs)
		}
		if (length(x) > 1) {
			names(x) <- var
			sds(x)
		} else {
			x[[1]]
		}
	} else {
		crops <- monfredaCrops()$name
		j <- crp %in% crops
		if (!all(j)) { 
			crp <- paste(crp[j], collapse=", ")
			stop(paste(crp, "is not available; see monfredaCrops()"))
		}
		x <- vector(mode="list", length=length(ss))	
		for (j in seq_along(ss)) {
			s <- ss[j]
			y <- vector(mode="list", length=length(crp))
			for (i in 1:length(crp)) {
				ff <- file.path(folder, paste0(crp[i], s))
				if (!all(file.exists(ff))) {
					urlbase <- "https://s3.us-east-2.amazonaws.com/earthstatdata/HarvestedAreaYield175Crops_Indvidual_Geotiff/"
					url <- paste0(urlbase, crp[i], "_HarvAreaYield_Geotiff.zip")
					zipf <- file.path(folder, basename(url))
					if (!file.exists(zipf)) {
						if(!.downloadDirect(url, zipf, ...)) return(NULL)
					}
					zf <- utils::unzip(zipf, list=TRUE)
					zf <- grep(".tif$", zf$Name, value=TRUE)
					zf <- grep(s, zf, value=TRUE)
					utils::unzip(zipf, files=zf, junkpaths=TRUE, exdir=folder)
				}
				y[[i]] <- terra::rast(ff)			
			}
			if (length(y) > 1) {
				x[[j]] <- rast(y)
			} else {
				x[[j]] <- y[[1]]
			}
		}
		if (length(x) > 1) {
			names(x) <- var
			sds(x)
		} else {
			x[[1]]
		}
	}
}

