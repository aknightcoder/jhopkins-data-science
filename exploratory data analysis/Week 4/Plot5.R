get_rds_data <- function(fileName)
{
  data <- readRDS(fileName)
  
  data
}

get_data <- function()
{
  data <- get_rds_data("summarySCC_PM25.rds")
  
  data <- subset(data, fips == "24510")
  
  scc_data <- get_rds_data("Source_Classification_Code.rds")
  
  motor_vehicle_sources <- grepl("vehicle", scc_data$SCC.Level.Two, ignore.case=TRUE)
  
  scc_data <- scc_data[motor_vehicle_sources,]

  data <- merge(data, scc_data, by="SCC")

  emission_totals <- with(data, tapply(Emissions, year, sum, na.rm=T))
}

show_plot <- function()
{
  png("Plot5.png")
  
  data <- get_data()
  
  barplot(data, xlab="Year", ylab="Total PM [2.5] Emissions", main="Emission Totals By Vehicle Sources in Baltimore Per Year") 
  
  dev.off()
}