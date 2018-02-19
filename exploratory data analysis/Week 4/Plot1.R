#Source_Classification_Code.rds
#summarySCC_PM25.rds

get_rds_data <- function(fileName)
{
  data <- readRDS(fileName)
  
  data
}

get_data <- function()
{
  data <- get_rds_data("summarySCC_PM25.rds")
  
  emission_totals <- with(data, tapply(Emissions, year, sum, na.rm=T))
}

get_plot_data <- function()
{
    data <- get_data()

    data_frame <- data.frame(year=names(emission_totals), totals=emission_totals)

    data_frame$year = as.character(data_frame$year)

    data_frame
}

show_plot <- function()
{
  png("Plot1.png")
  
  bar_plot_data <- get_data()
  
  barplot(bar_plot_data, xlab="Year", ylab="Total Emissions (ton)", main="Total Emissions By Year")

  dev.off()
}
