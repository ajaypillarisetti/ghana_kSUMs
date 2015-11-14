library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(lubridate)
library(data.table)
library(xts)
library(shinydashboard)
library(scales)
library(devtools)
library(dygraphs)

### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0N

# install missing packages.
list.of.packages <- c("shiny","ggplot2","reshape2","plyr","lubridate","data.table","dygraphs","xts","devtools","shinydashboard","scales",'dygraphs','readxl')
	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))(print(paste("The following packages are not installed: ", new.packages, sep="")))else(print("All packages installed"))
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 

#global functions
alt.diff <- function (x, n = 1, na.pad = TRUE) {
  NAs <- NULL
  if (na.pad) {NAs <- rep(NA, n)}
  diffs <- c(NAs, diff(x, n))
}

round.minutes <- function(x, noOfMinutes=5){
	tz <- tz(x[1])
	class <- class(x[1])
	structure((noOfMinutes*60) * (as.numeric(x + (noOfMinutes*60*0.5)) %/% (noOfMinutes*60)), class=class,tz=tz)
}

read.kSUM <- function(x, toMemory=F, saveFile=F, tzone="Africa/Accra"){
	fileCheck <- file.info(x)$size>0
	if(fileCheck){
		#read in without regard for delimiters
		raw <- read.delim(x)
			#deal with the header
			# datastart <- as.numeric(sapply(raw, function(x) grep("<PatsData>", x)))
			# header <- read.table(x,nrow=datastart, stringsAsFactors=F)
			# fn.header <- tempfile()
			# header <- rbind(header, "</PatsFile>")
			# write.table(header, file=fn.header, row.names=F, col.names=F, quote=F)
			# rawHeader <- xmlParse(fn.header)
			# unlink(fn.header)
			# xmlToList(rawHeader)
		#use a regular expression to identify lines that are data, denote the line number
		kLines <- as.numeric(sapply(raw, function(x) grep('[0-9]{4}/[0-9]{2}/[0-9]{2,} [0-9:]{6,},[0-9.,]{3,}',x)))
		#convert to character
		rare <- as.character(raw[kLines,])
		rare <- rare[2:length(rare)]
		fn <- tempfile()
		write(rare, file=fn)
		mediumwell <- fread(fn)
		unlink(fn)
		setnames(mediumwell, c('datetime','batt','t0','t1','t2','t3'))
		mediumwell[,datetime:=ymd_hms(datetime, tz='Africa/Accra')]
		if(saveFile){write.csv(mediumwell, file=paste(x,'.csv',sep=''))}
		if(toMemory){assign(strsplit(x,'/')[[1]][3],mediumwell,envir=.GlobalEnv)}
		mediumwell
	}
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}