rankhospital <- function(state, outcome, num = "best") 
{
  all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!is.element(state,unique(all_data$State)))
    stop("invalid state")
  
  if(!is.element(outcome,c("heart attack","heart failure","pneumonia")))
    stop("invalid outcome")
  
  state_data <- all_data[all_data$State==state,]
  
  outcome_data = data.frame();
  
  if(outcome == "heart attack")
    outcome_data <- state_data[,c(2,11)]
  else if (outcome == "heart failure")
    outcome_data <- state_data[,c(2,17)]
  else if (outcome == "pneumonia")
    outcome_data <- state_data[,c(2,23)]
  
  outcome_data <- outcome_data[order(as.numeric(outcome_data[, 2]), outcome_data[, 1], na.last = NA),]

  if (num == "best")
    rank <- 1
  else if (num == "worst")
    rank <- nrow(outcome_data)
  else
    rank <- num
  
 if (rank <= nrow(outcome_data))
    result <- outcome_data[rank,1]
 else
    result <- "NA"
}