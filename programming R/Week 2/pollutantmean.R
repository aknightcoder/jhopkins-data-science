pollutantmean <- function(directory, pollutant, id = 1:332)
{
  files = list.files(directory, full.names = TRUE)
  
  df <- data.frame()
  
  for(counter in id)
  {
    df <- rbind(df, read.csv(files[counter]))
  }
  
  mean(df[,pollutant], na.rm = TRUE)
}