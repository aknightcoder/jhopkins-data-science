download_data <- function(url, file_name)
{
    if (!file.exists(file_name))
    {
        download.file(url, file_name)
    }
}

get_data <- function()
{
    file_name <- "StormData.csv.bz2"
    
    download_data("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",file_name)
  
    data <- read.csv(bzfile(file_name))
}

get_relevant_data <- function(raw_data)
{
  raw_data$BGN_DATE <- as.Date(as.character(raw_data$BGN_DATE), format = "%m/%d/%Y")
  
  relevant_data <- subset(raw_data, format.Date(BGN_DATE, "%Y") >= "1996")
  
  relevant_data <- relevant_data[,c(2,8,23,24,25,27)]
}

process_data <- function(raw_data)
{

  relevant_data$PROPDMG <- mapply(convert_cost, relevant_data$PROPDMG,relevant_data$PROPDMGEXP)
 
  relevant_data$CROPDMG <- mapply(convert_cost, relevant_data$CROPDMG,relevant_data$CROPDMGEXP)

  relevant_data$ECONOMYIMPACT <- relevant_data$PROPDMG+relevant_data$CROPDMG
  
  relevant_data
}

clean_evtype <- function(value)
{
  value <- toupper(value)

  value <- gsub("TSTM", "THUNDERSTORM",value)
  
  value <- gsub("THUNDERSTORM WIND.*", "THUNDERSTORM WIND", value)
  
  value <- gsub("*HIGH SURF.*", "HIGH SURF", value)
  
  value <- gsub("  ", " ",value)
  
  value <- gsub("SUMMARY.*", "OTHER", value)

  value <- gsub("CSTL", "COASTAL", value)
  
  value <- sub("^\\s+","",value)
  
  value
}

convert_cost <- function(value, exp)
{
  exp <- toupper(exp)
  
  if (exp == 'K')
  {
      value <- value * 1000
  } 
  else if (exp == 'M'){
      value <- value * 1000000
  }
  else if (exp == 'B'){
      value <- value * 10000000
  }
  else
  {
    value <- 0
  }
}

get_health_data <- function(data)
{
  health_data <- aggregate(HEALTHIMPACT ~ EVTYPE, data, FUN=sum)
  
  health_data <- health_data[health_data[,2] >0,]
  
  health_data <- health_data[order(-health_data$HEALTHIMPACT),]
  
  health_data <- health_data[c(1:10),]
}

show_health_plot <- function(data)
{
  png("health_plot.png")
  
  library(ggplot2)

  health_plot <- ggplot(data=data, aes(x=EVTYPE,y=HEALTHIMPACT)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle = 45, hjust = 1))
  
  print(health_plot)
  
  dev.off()
}

show_economy_plot <- function(data)
{
  png("economy_plot.png")
  
  library(ggplot2)
  
  economy_plot <- qplot(EVTYPE, ECONOMYIMPACT, data = data, color=c(1,3), xlab="Events", 
               ylab="Economy Impact",
               geom=c("point","smooth")) + ggtitle("Economy impacts by Event Type") + 
  theme(plot.title = element_text(hjust = 0.5))

  print(economy_plot)

  dev.off()
}

get_economy_data <- function(data)
{
  economy_data <- aggregate(ECONOMYIMPACT ~ EVTYPE, data, FUN=sum)
  
  economy_data <- economy_data[economy_data[,2] >0,]
}
