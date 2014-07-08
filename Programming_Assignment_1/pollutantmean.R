## Write a function named 'pollutantmean' that calculates the mean of a pollutant
## (sulfate or nitrate) across a specified list of monitors.
## The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
##Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter 
##data from the directory specified in the 'directory' argument and returns the mean of the
## pollutant across all of the monitors, ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id = 1:332) {
	filepath = file.path("/Users", "lezoujonathan", "R", directory)
	myFiles <- list.files(path = directory, pattern = '*.csv')
	myData <- data.frame()
	for (i in id){
		myData <- rbind(myData, read.csv(paste(filepath,'/',myFiles[i], sep="")))
		
	}
	mean(myData[,pollutant], na.rm = T)
	
	}