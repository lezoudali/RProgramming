##Write a function that reads a directory full of files and 
##reports the number of completely observed cases in each data file. 
##The function should return a data frame where the first column is the name of the file
##and the second column is the number of complete cases.

complete <- function (directory, id = 1:332) {
	filepath = file.path("/Users", "lezoujonathan", "R", directory)
	myFiles <- list.files(path = directory, pattern=".csv")
	nobs <- numeric()
	for (i in id){
		tempframe <- data.frame(read.csv(paste(filepath, '/',myFiles[i], sep="")))
		nobs <- rbind(nobs, sum(complete.cases(tempframe)))
	}
	colnames(nobs) <- 'nobs'
	#cbind(id, nobs)
	data.frame(id,nobs)
}
