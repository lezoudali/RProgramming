##Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between 
##sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the
## threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no 
##monitors meet the threshold requirement, then the function should return a numeric vector of length 0
corr <- function(directory, threshold = 0){

	filepath = filepath = file.path("/Users", "lezoujonathan", "R", directory)
	myFiles <- list.files(path= directory, pattern=".csv")
	
	
	myData <- data.frame()
	myComplete = complete(directory)
	x <-numeric()
	
	for (i in 1:length(myFiles)){
		if (myComplete[,'nobs'][i] > threshold){
			myData <- read.csv(paste(filepath, '/',myFiles[i], sep=""))
			myData <- na.omit(myData)
			y <-cor(myData$sulfate, myData$nitrate)	
			x<-append(x, y)
		}
	}
	x
}