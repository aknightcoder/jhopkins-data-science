get_rds_data <- function(fileName)
{
  data <- readRDS(fileName)
  
  data
}

get_data <- function()
{
  data <- get_rds_data("summarySCC_PM25.rds")
  
  data <- subset(data, fips == "24510")
  
  emission_totals_by_year_type <- aggregate(Emissions ~ year + type, data, sum)
}

show_plot <- function()
{
  png("Plot3.png")
  
  library(ggplot2)

  gg_plot_data <- get_data()
  
  plot3 <- qplot(year, Emissions, data = gg_plot_data, color=type, xlab="Year", 
        ylab="Total PM [2.5] Emissions",
        geom=c("point","smooth")) + ggtitle("Emission Totals By Year And Type") + 
        theme(plot.title = element_text(hjust = 0.5))

  print(plot3)
  
  dev.off()
}