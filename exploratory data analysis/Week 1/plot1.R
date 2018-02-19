display_active_power_hist <- function()
{
	source("get_data.R")
	
	data <- get_data("household_power_consumption.txt")
	
	png("plot1.png", width=480, height=480)
	
	hist(data$Global_active_power, col="Red", main="Global Active Power", xlab="Global Active Power(kilowatts)")

	dev.off()
}