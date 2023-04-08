# Author: Robert J. Hijmans
# Sept 2019
# version 1
# license GPL3


.get_simple_URI <- function(uri, reverse=FALSE) {
  
  if (reverse) {
    return(gsub("_", "/", sub("_", ":", uri))	)
  }
  
  ur <- .removeprotocol(uri)
  if (grepl("dx.doi.org/", ur)) {
    u <- gsub("dx.doi.org/", "", ur)
    u <- paste0("doi_", u)
  } else if (grepl("doi.org/", ur)) {
    u <- gsub("doi.org/", "", ur)
    u <- paste0("doi_", u)
  } else if (grepl("persistentId=doi:", ur)) {
    u <- unlist(strsplit(ur, "persistentId=doi:"))[2]
    u <- paste0("doi_", u)
  } else if (grepl("^doi:", ur)) {
    u <- gsub("^doi:", "doi_", ur)		
  } else if (grepl("persistentId=hdl:", ur)) {
    u <- unlist(strsplit(ur, "persistentId=hdl:"))[2]
    u <- paste0("hdl_", u)
  } else if (grepl("^hdl:", ur)) {
    u <- gsub("^hdl:", "hdl_", ur)		
  } else if (grepl("hdl.handle.net/", ur)) {
    u <- gsub("hdl.handle.net/", "", ur)
    u <- paste0("hdl_", u)
  } else {
    stop(paste0("Not valid unique object identifier (DOI or HDL)"))
  }
  gsub("/", "_", u)
}



.dataverse_unzip <- function(zipf, path, unzip) {
  allzf <- NULL
  for (z in zipf) {
    zf <- unzip(z, list=TRUE)
    zf <- zf$Name[zf$Name != "MANIFEST.TXT"]
    allzf <- c(allzf, zf)
    if (unzip) {
      ff <- list.files(path)
      there <- (zf %in% ff)
      if (!all(there)) {
        unzip(z, zf[!there], exdir = path)
      }	
    }
  }
  zf <- grep("\\.pdf$", allzf, value=TRUE, invert=TRUE)
  zf <- file.path(path, zf)
  return(zf)
}


.download_dataverse_files <- function(u, baseu, path, uname, domain, protocol, unzip, zipf) {
  pid <- unlist(strsplit(u, "\\?"))[2]
  uu <- paste0(baseu, "/api/datasets/:persistentId?", pid)
  
  # the nice way
  #r <- httr::GET(uu)
  #httr::stop_for_status(r)
  #js <- httr::content(r, as = "text", encoding = "UTF-8")
  # but for cimmyt...
  tmpf <- tempfile()
  utils::download.file(uu, tmpf, quiet=TRUE)
  js <- readLines(tmpf, encoding = "UTF-8", warn=FALSE)
  js <- jsonlite::fromJSON(js)
  fjs <- js$data$latestVersion$files
  jsp <- jsonlite::toJSON(js, pretty=TRUE)
  writeLines(jsp, file.path(path, paste0(uname, ".json")))
  f <- if(is.null(fjs$dataFile)) {fjs$datafile} else {fjs$dataFile}
  f$checksum <- NULL
  f$tabularTags <- NULL
  fn <- file.path(path, paste0(uname, "_files.txt"))
  try(utils::write.csv(f, fn))
  
  rest <- f$restricted
  if (!is.null(rest)) {
    f <- f[!rest, ]
    if (nrow(f) == 0) {
      stop("access to the files is restricted")
    }
    warning("access to some files is restricted")
  }
  if (nrow(f) == 0) {
    stop("no files!")
  }
  
  if (sum(f$originalFileSize, na.rm=TRUE) < 10000000) {
    files <- paste0(f$id, collapse = ",")
    fu <- paste0(protocol, domain, "/api/access/datafiles/", files, "?format=original")
    utils::download.file(fu, zipf, mode="wb", quiet=TRUE)
  } else {
    f$originalFileSize[is.na(f$originalFileSize)] <- 0
    i <- 1
    zipf <- NULL
    while(TRUE) {
      print(paste("part", i)); utils::flush.console()
      cs <- cumsum(f$originalFileSize)
      k <- which (cs < 9000000)
      if (length(k) == 0) k <- 1
      files <- paste0(f$id[k], collapse = ",")
      fu <- paste0(protocol, domain, "/api/access/datafiles/", files, "?format=original")
      zipi <- file.path(path, paste0(uname, "_", i, ".zip"))
      utils::download.file(fu, zipi, mode="wb", quiet=TRUE)
      f <- f[-k,]
      zipf <- c(zipf, zipi)
      if (nrow(f) == 0) break
      i <- i + 1
    }
  }	
  .dataverse_unzip(zipf, path, unzip)
}


.download_ckan_files <- function(u, baseu, path, uname) {
  pid <- unlist(strsplit(u, "dataset/"))[2]
  uu <- paste0(baseu, "/api/3/action/package_show?id=", pid)
  y  <- httr::GET(uu)
  if (y$status_code != 200) {
    return(NULL)
  }
  
  ry <- httr::content(y, as="raw")
  meta <- rawToChar(ry)
  writeLines(meta, file.path(path, paste0(uname, ".json")))
  js  <- jsonlite::fromJSON(meta)
  d <- js$result$resources
  done <- TRUE
  files <- ""[0]
  for (i in 1:nrow(d)) {
    u <- file.path(baseu, "dataset", d$package_id[i], "resource", d$id[i], "download", d$name[i])
    #if (d$available[i] == "yes") { "active" ?
    outf <- file.path(path, d$name[i])
    ok <- try(utils::download.file(d$url[i], outf, mode="wb", quiet=TRUE) )
    if (inherits(ok, "try-error")) {
      print("cannot download", d$name[i])
      done <- FALSE
    } else {
      files <- c(files, outf)
    }
  }
  writeLines("ok", file.path(path, "ok.txt"))
  files
}

.download_dryad_files <- function(u, baseu, path, uname){
  pid <- gsub(":", "%3A", gsub("/", "%2F", unlist(strsplit(u, "dataset/"))[2]))
  uu <- paste0(baseu, "/api/v2/datasets/", pid)
  y  <- httr::GET(uu)
  if (y$status_code != 200) {
    return(NULL)
  }
  
  ry <- httr::content(y, as="raw")
  meta <- rawToChar(ry)
  writeLines(meta, file.path(path, paste0(uname, ".json")))
  js  <- jsonlite::fromJSON(meta)
  d <- js$id
  done <- TRUE
  files <- ""[0]
  outf <- file.path(path, paste0(uname, ".zip"))
  ok <- try(utils::download.file(file.path(uu,"download"), outf, mode="wb", quiet=TRUE) )
  if (inherits(ok, "try-error")) {
    print("cannot download ", uname)
    done <- FALSE
  } else {
    files <- c(files, outf)
  }
  utils::unzip(outf, exdir = path)
  writeLines("ok", file.path(path, "ok.txt"))
  files
}


.getdomain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
.getprotocol <- function(x) paste0(strsplit(x, "/")[[c(1, 1)]], "//")
.removeprotocol <- function(x) gsub("http://|https://|www\\.", "", x)


.get_from_uri <- function(uri, path, uripath=FALSE, overwrite=FALSE) {
  
  unzip=TRUE
  uname <- .get_simple_URI(uri)
  if (uripath) path <- file.path(path, uname)
  
  #ckan 
  if (!(overwrite) && (file.exists(file.path(path, "ok.txt")))) {
    ff <- list.files(path, full.names=TRUE)
    ff <- ff[!grepl(".json$", ff)]
    ff <- ff[basename(ff) != "ok.txt"]
    return(ff)
  }
  zipf0 <- file.path(path, paste0(uname, ".zip"))
  zipf1 <- file.path(path, paste0(uname, "_1.zip"))
  if ((file.exists(zipf0) || file.exists(zipf1))) {
    zipf <- list.files(path, paste0(uname, ".*zip$"), full.names=TRUE)		
    return(.dataverse_unzip(zipf, path, unzip))
  }
  
  if (grepl("^doi:", uri)) {
    uri <- gsub("^doi:", "https://dx.doi.org/", uri)
  } else if (grepl("^hdl:", uri)) {
    uri <- gsub("^hdl:", "https://hdl.handle.net/", uri)
  }
  dir.create(path, FALSE, TRUE)
  if (!file.exists(path)) {
    stop(paste("cannot create path:", path))
  }
  x <- httr::GET(uri)
  stopifnot("Dataset or resource not reachable. Server returned an error message. \n" = x$status_code == 200)
  u <- x$url
  domain <- .getdomain(u)
  protocol <- .getprotocol(u)
  baseu <- paste0(protocol, domain)
  if (grepl("/stash/", u)) {	
    .download_dryad_files(u, baseu, path, uname)
  } else if (grepl("/dataset/", u)) {	
    .download_ckan_files(u, baseu, path, uname)
  } else {
    .download_dataverse_files(u, baseu, path, uname, domain, protocol, unzip, zipf1)
  }
}



# uri <- "https://doi.org/10.5061/dryad.pj76g30"
# ff <- get_data_from_uri(uri, ".")

