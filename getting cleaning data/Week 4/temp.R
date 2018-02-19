file_name <- "quiz4_q4a.csv"
if(!file.exists(file_name))
{
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", file_name)
}

dat1 <- read.csv(file_name, header = F, skip = 5, nrows=190);

file_name <- "quiz4_q4b.csv"
if(!file.exists(file_name))
{
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", file_name)
}

dat2 <- read.csv(file_name)

dat3 <- merge(dat1, dat2, by.x = "V1", by.y = "CountryCode")