rankall <- function(outcome, num = "best")
{
  all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!is.element(outcome,c("heart attack","heart failure","pneumonia")))
    stop("invalid outcome")

  unique_states <- sort(unique(all_data$State))
  
  hospital <- rep("", length(unique_states))
  
  for (counter in 1:length(unique_states))
  {
    state_data <- all_data[all_data$State==unique_states[counter],]
    
    if (outcome == "heart attack")
    {
      outcome_data <- state_data[,c(2,11)]
    }
    else if (outcome == "heart failure")
    {
      outcome_data <- state_data[,c(2,17)]
    }
    else if (outcome == "pneumonia")
    {
      outcome_data <- state_data[,c(2,23)]
    }

    ordered_data <- outcome_data[order(as.numeric(outcome_data[, 2]), outcome_data[, 1], na.last = NA),]
    
    if (num == "best")
    {
      rank <- 1
    }
    else if (num == "worst")
    {
      rank <- nrow(ordered_data)
    }
    else if (num > length(hospital))
    {
      rank = "NA"
    }
    else 
    {
      rank <- num
    }

    if(!is.na(rank))
      hospital[counter] <- ordered_data[rank,1]
    else
      hospital[counter] <- "N/A"
  }
  
  data.frame(hospital=hospital, state=unique_states)
}