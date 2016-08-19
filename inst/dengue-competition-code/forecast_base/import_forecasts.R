require(stringr)

import.forecasts <- function(this.dir) {
  file.names <- list.files(this.dir)
  if (length(file.names) == 0) stop("No files found; check directory.")
  these.files <- list()
  for (this.file.name in file.names) {
	  if (str_detect(this.file.name, "\\.csv")) {
		  this.file <- process.file(this.dir, this.file.name)
		  these.files[[this.file$name]][[this.file$target]] <- this.file$data
	  }
  }
  return(these.files)
}

process.file <- function(this.dir, this.file.name) {
	# import file
	this.file <- read.csv(paste(this.dir, this.file.name, sep="/"))
	
	# remove any empty rows
	this.file <- this.file[as.character(this.file[ , 1]) != "", ]
	rownames(this.file) <- this.file[ , 1]
	this.file <- this.file[ , -1]
	
	# remove any empty columns
	this.file <- this.file[ , str_detect(names(this.file), "X20")]
	
	# get team name or "template" label
	this.name <- tolower(str_extract(this.file.name, "[^\\_]*"))
	
	# get target name
	this.target <- tolower(str_replace_all(this.file.name, "_", "."))
	this.target <- paste(str_split(this.target, "\\.")[[1]][2:4], collapse='.')
	
	return(list(
			name=this.name,
			target=this.target,
			data=this.file))
}
