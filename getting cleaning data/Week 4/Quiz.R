Question1 <- function()
{
  file_name <- "quiz4.csv"
  if (!file.exists(file_name))
  {
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", file_name)
  }
  
  dat <- read.csv(file_name)
  
  split_names <- strsplit(names(dat), "wgtp")
}

Question2 <- function()
{
  file_name <- "quiz4_q1.csv"
  if(!file.exists(file_name))
  {
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", file_name)
  }
  
  dat <- read.csv(file_name, header = F, skip = 5, nrows=190);
  
  millions <- gsub(",","",dat$V5)
  
  millions <- millions[!is.na(as.numeric(millions))]
  
  millions <- as.numeric(millions)
}

Question3 <- function()
{
  file_name <- "quiz4_q1.csv"
  if(!file.exists(file_name))
  {
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", file_name)
  }
  
  dat <- read.csv(file_name, header = F, skip = 5, nrows=190);
  
  dat <- grep("^United",dat$V4, value = TRUE)
}

Question4 <- function()
{
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
  
  dat4 <- grep("Fiscal year end: June 30;*",dat3$Special.Notes, value = TRUE)
  
  length(dat4)
}

Question5 <- function()
{
  library(quantmod)
  amzn = getSymbols("AMZN",auto.assign=FALSE)
  sampleTimes = index(amzn)
  
  dat <- grep("^2012",dat)
  
  length(dat)
}

download <- function(url, name)
{
  download.file(url, name)
}
