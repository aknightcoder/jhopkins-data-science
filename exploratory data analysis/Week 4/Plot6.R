get_rds_data <- function(fileName)
{
  data <- readRDS(fileName)
  
  data
}

get_data <- function()
{
  data <- get_rds_data("summarySCC_PM25.rds")
  
  data <- subset(data, fips == "24510" | fips == "06037")
  
  scc_data <- get_rds_data("Source_Classification_Code.rds")
  
  motor_vehicle_sources <- grepl("vehicle", scc_data$SCC.Level.Two, ignore.case=TRUE)
  
  scc_data <- scc_data[motor_vehicle_sources,]
  
  data <- merge(data, scc_data, by="SCC")
  
  emission_totals_by_year_fips <- aggregate(Emissions ~ year + fips, data, sum)
}

show_plot <- function()
{
  library(ggplot2)
  
  png("Plot6.png")
  
  gg_plot_data <- get_data()

  plot6 <- ggplot(gg_plot_data, aes(year, Emissions, color = fips)) + geom_line() + xlab("Year") + ylab("Total PM [2.5] Emissions") + ggtitle("Emissions By Year For Baltimore And LA") + theme(plot.title = element_text(hjust = 0.5))

  print(plot6)
  
  dev.off()
}