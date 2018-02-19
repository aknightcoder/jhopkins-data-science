get_rds_data <- function(fileName)
{
  data <- readRDS(fileName)
  
  data
}

get_data <- function()
{
  scc_data <- get_rds_data("Source_Classification_Code.rds")
  
  coal_sources  <- grepl("coal", scc_data$Short.Name, ignore.case=TRUE)
  
  scc_data <- scc_data[coal_sources,]
  
  data <- get_rds_data("summarySCC_PM25.rds")

  data <- merge(data, scc_data, by="SCC")
  
  emission_totals <- with(data, tapply(Emissions, year, sum, na.rm=T))
}

show_plot <- function()
{
  png("Plot4.png")
  
  data <- get_data()
  
  barplot(data, xlab="Year", ylab="Total PM [2.5] Emissions", main="Emission Totals By Coal Sources Per Year") 

  dev.off()
}