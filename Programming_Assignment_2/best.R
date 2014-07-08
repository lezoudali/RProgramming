##Write a function called best that take two arguments: the 2-character abbreviated name of a state and 
##outcome name. The function reads the outcome-of-care-measures.csv ﬁle and returns a character vector
##with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the speciﬁed outcome
##in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
##be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
##outcome should be excluded from the set of hospitals when deciding the rankings.

best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  new_data <- subset(data, subset = data$State == state)
  
  if(!state %in% data$State){
    stop("invalid state")
  }
  if ( outcome == "heart attack"){
    new_data[,11] = as.numeric(new_data[,11])
    lowest = min(new_data[,11], na.rm = T)
    result = subset(new_data, subset = new_data[, 11]==lowest)
  }
  else if (outcome == "heart failure"){
    new_data[,17] = as.numeric(new_data[,17])
    lowest = min(new_data[,17], na.rm = T)
    result = subset(new_data, subset = new_data[, 17]==lowest)
  }
  else if (outcome == "pneumonia"){
    new_data[,23] = as.numeric(new_data[,23])
    lowest = min(new_data[,23], na.rm = T)
    result = subset(new_data, subset = new_data[,23]==lowest)
  }
  else {
    stop("invalid outcome")
  }
  result$Hospital.Name
}

