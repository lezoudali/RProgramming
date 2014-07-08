# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv ﬁle and returns a character vector with the name
# of the hospital that has the ranking speciﬁed by the num argument. 

rankhospital <- function(state, outcome, num = "best" ){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!state %in% data$State){
    stop("invalid state")
  }
  if (outcome == 'heart attack'){
    n <- 11
  }else if (outcome == 'heart failure'){
    n <- 17
  }else if (outcome == 'pneumonia'){
    n <- 23
  } else {
    stop("invalid outcome")
  }
  
  new_data <- subset(data, subset = data$State == state)
  new_data[,n] <- as.numeric(new_data[,n])
  new_data <- new_data[order(new_data[,n]),]
  new_data <- new_data[complete.cases(new_data),]
  
  if (num == "best"){
    num <- 1
  }else if (num == "worst"){
    num <- nrow(new_data)
  }

  new_data <- subset(new_data, subset = new_data[,n] == new_data[num,n])  
  new_data[1,2]
}