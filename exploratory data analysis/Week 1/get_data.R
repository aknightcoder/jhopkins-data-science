get_data_with_sub_set <- function(file_name)
{
	if(file.exists(file_name))
	{
		table <- read.table(file_name, header=TRUE, sep=";", skip=66636, stringsAsFactors=FALSE, dec=".")
        
        table$Time = strptime(paste(table$Date, table$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
        
        table$Date = as.Date(table$Date,format="%d/%m/%Y")
        
        table <- subset(table, Date >= "2007-02-01" & Date <="2007-02-02")
        
        table$Global_active_power <- as.numeric(table$Global_active_power)
        
        table
	}
}

get_data <- function(file_name)
{
	if (file.exists(file_name))
	{
		column_names <- c("Date", "Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
	
		table <- read.table(file_name, header=FALSE, sep=";", skip=66636, nrows=2880,stringsAsFactors=FALSE, dec=".", col.names=column_names)
		
		table$Time = strptime(paste(table$Date, table$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
        
        table$Date = as.Date(table$Date,format="%d/%m/%Y")
        
        table
	}
}
