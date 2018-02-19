corr <- function(directory, threshold=0)
{
  files = list.files(directory, full.names = TRUE)
  
  all_data <- data.frame()

  correlations <- integer();
  idx <- 1
  for(counter in seq(1, length(files), by=1))
  {
    monitor_data <- read.csv(files[counter])
    monitor_data <- monitor_data[complete.cases(monitor_data),]
   
    if (nrow(monitor_data) >= threshold)
    {
      correlations[idx] <- cor(monitor_data$sulfate, monitor_data$nitrate)
      idx <- idx +1
    }
  }
  correlations
}