display_active_power_plot <- function()
{
	source("get_data.R")
	
	data <- get_data("household_power_consumption.txt")
	
	png("plot2.png", width=480, height=480)

	plot(data$Time, data$Global_active_power, xlab="",ylab="Global Active Power (kilowatts)", type="l")

	dev.off()
}