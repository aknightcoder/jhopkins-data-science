display_all <- function()
{
	source("get_data.R")
	
	data <- get_data("household_power_consumption.txt")
	
	png("plot4.png", width=480, height=480)
	
	par(mfrow=c(2,2))
	
	display_active_power_plot(data)

	display_voltage_plot(data)
	
	display_energy_sub_metering_plot(data)
	
	display_inactive_power_plot(data)
	
	dev.off()
}

display_active_power_plot <- function (data)
{
	plot(data$Time, data$Global_active_power, xlab="",ylab="Global Active Power (kilowatts)", type="l")
}

display_inactive_power_plot <- function(data)
{
	plot(data$Time, data$Global_reactive_power, xlab="datetime", ylab="Global_reactive_power", type="l")
}

display_voltage_plot <- function (data)
{
	plot(data$Time, data$Voltage, xlab="datetime",ylab="Voltage", type="l")
}

display_energy_sub_metering_plot <- function (data)
{
	plot(data$Time, data$Sub_metering_1, xlab="",ylab="Energy sub metering", type="l")
	
	lines(data$Time, data$Sub_metering_2,col="Red")
	
	lines(data$Time, data$Sub_metering_3,col="Blue")
	
	legend("topright", col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), lty=c(1,1,1))
}