get_data <- function (fileName) {

  data <- read.table(fileName, comment.char="#", header = FALSE, sep = "|", na.strings = "")

  cnames <- readLines(fileName, 1)

  cnames <- strsplit(cnames, "|", fixed = TRUE)

  names(data) <- make.names(cnames[[1]])

  data
}

get_state_data <- function(stateCode, data)
{
    state_data <- unique(subset(data, State.Code == stateCode, c(County.Code, Site.ID)))
    
    state_data
}

get_paste_data <- function(stateCode, fileName, separator)
{
  state_data <- get_state_data(stateCode, fileName)
  
  paste_data <- paste(state_data[,1],state_data[,2], sep=separator)
}

get_data_with_site_column <- function(fileName){
  
  data <- get_data(fileName)
  
  data <- add_site_column(data)
}

add_site_column <- function(data)
{
  data$county.site = with(data, paste(County.Code, Site.ID, sep="."))
  
  data
}

get_intersect <- function(data1, data2)
{
  intersect_data <- intersect(data1, data2)
  
  intersect_data
}

get_state_data_by_intersect <- function(data, intersect_data){
  
  state_data <- subset(data,State.Code == 36 & county.site %in% intersect_data)
  
}

get_plot <- function(data, rng)
{
  dates <- as.Date(as.character(data$Date), "%Y%m%d")
  
  sampleValues <- data$Sample.Value
  
  plot(dates, sampleValues, pch=20, ylim=rng)
  
  abline(h = median(sampleValues, na.rm=T))
}

get_full_plot <- function(data1, data2)
{
  par(mfrow=c(1,2), mar=c(4,4,2,1))
  rng <- range(data1$Sample.Value, data2$Sample.Value, na.rm=T)
  get_plot(data1,rng)
  get_plot(data2,rng)
}

get_mean_plot_data <- function(fileName)
{
  data <- get_data(fileName)
  
  mean_data <- with(data, tapply(Sample.Value, State.Code, mean, na.rm=T))

  data_frame <- data.frame(state=names(mean_data), mean=mean_data)

  data_frame
}

show_mean_plot <- function()
{
  par(mfrow=c(1,1))
  
  df_1999 <- get_mean_plot_data("RD_501_88101_1999-0.txt")
  
  df_2012 <- get_mean_plot_data("RD_501_88101_2012-0.txt")
  
  merged_data <- merge(df_1999, df_2012, by="state")
  m 
  with(merged_data, plot(rep(1999,52), merged_data[,2], xlim=c(1998, 2012)))
  
  with(merged_data, points(rep(2012,52), merged_data[,3]))
  
  segments(rep(1999,52), merged_data[,2], rep(2012, 52), merged_data[,3])
}

run_analysis <- function()
{
  data_1999 <- get_data_with_site_column("RD_501_88101_1999-0.txt")
  
  data_2012 <- get_data_with_site_column("RD_501_88101_2012-0.txt")
  
  ny_data_1999 <- get_state_data(36, data_1999)
  
  ny_data_1999 <- paste(ny_data_1999[,1], ny_data_1999[,2], sep=".")
  
  ny_data_2012 <- get_state_data(36, data_2012)
  
  ny_data_2012 <- paste(ny_data_2012[,1], ny_data_2012[,2], sep=".")
  
  intersect_data <- intersect(ny_data_1999, ny_data_2012)
  
  county_data_1 <- get_state_data_by_intersect(data_1999, intersect_data)
  
  county_data_2 <- get_state_data_by_intersect(data_2012, intersect_data)
  
  sapply(split(county_data_1, county_data_1$county.site),nrow)
  
  sapply(split(county_data_2, county_data_2$county.site),nrow)
  
  data_1999_1 <- subset(data_1999, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
  
  data_2012_1 <- subset(data_2012, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
}