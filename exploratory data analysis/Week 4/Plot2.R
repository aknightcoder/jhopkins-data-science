get_rds_data <- function(fileName)
{
  data <- readRDS(fileName)
  
  data
}

get_data <- function()
{
  data <- get_rds_data("summarySCC_PM25.rds")
  
  data <- subset(data, fips == "24510")
  
  emission_totals <- with(data, tapply(Emissions, year, sum, na.rm=T))
}

show_plot <- function()
{
  png("Plot2.png")
  
  bar_plot_data <- get_data()
  
  barplot(bar_plot_data, xlab="Year", ylab="Total Emissions (ton)", main="Total Esourmissions By Year For Baltimore")
  
  dev.off()
}