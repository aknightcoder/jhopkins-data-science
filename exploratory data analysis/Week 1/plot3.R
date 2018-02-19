display_energy_sub_metering_plot <- function()
{
	source("get_data.R")
	
	data <- get_data("household_power_consumption.txt")
	
	png("plot3.png", width=480, height=480)

	plot(data$Time, data$Sub_metering_1, xlab="",ylab="Energy sub metering", type="l")
	
	lines(data$Time, data$Sub_metering_2,col="Red")
	
	lines(data$Time, data$Sub_metering_3,col="Blue")
	
	legend("topright", col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), lty=c(1,1,1))

	dev.off()
}