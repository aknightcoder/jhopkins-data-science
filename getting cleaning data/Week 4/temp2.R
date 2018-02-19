stopifnot(require(stringr))

# Download and unzip the data
getData <- function(localSource = "UCI_HAR_Dataset.zip") {
  remoteSource <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  if (!file.exists(localSource)) {
    download.file(remoteSource, localSource)
  }
  unzip(localSource)
}

# Get a single column of string values
getStringColumn <- function(path, index) {
  stringData <- read.table(path, stringsAsFactors = F)
  stringData[[index]]
}

# Return a tidy dataframe
# Arguments:
# directory: path to the data
# name: (`test` or `train`) - subdirectory in the data
# yFactors: labels of the activity factors
# features: logical vector of the features to be kept
# fetureLabels: the clean names of the features required.
prepareData <- function(dir, name, yFactors, features, featureLabels) {
  activityData <- read.table(file.path(dir, name,
                                       str_c("y_", name, ".txt")),
                             col.names = "activity")
  activity <- factor(activityData$activity, labels = yFactors)
  subject <- read.table(file.path(dir, name,
                                  str_c("subject_", name, ".txt")),
                        col.names = "subject")
  observations <- read.table(file.path(dir, name,
                                       str_c("X_", name, ".txt")),
                             col.names = featureLabels)
  
  return(data.frame(subject = subject,
                    activity = activity,
                    observations[,desiredFeatures]))
}

getData() #Download and unzip
dir <- "UCI HAR Dataset" #The root directory

activityLabels <- getStringColumn(
  file.path(dir, "activity_labels.txt"), 2) #get the vector of activity labels
featureLabels <- getStringColumn(
  file.path(dir, "features.txt"), 2) #get the vector of column names

desiredFeatures <- grepl( #decide which features to keep
  str_c(
    "-mean\\(\\)", #containing `-mean()`
    "-std\\(\\)",  #containing `-std()`
    sep="|"),
  featureLabels)

featureLabels <- str_replace_all(featureLabels, c( #clean up the column (feature) names
  "\\(" = "",   #drop `(`
  "\\)" = "",   #drop `)`
  "-" = "_",    #replace `-` with `_`
  "," = "_"     #replace `,` with `_`
))

#get train data
testData <- prepareData(dir = dir, name = "test",
                        yFactors = activityLabels, 
                        features = desiredFeatures, 
                        featureLabels = featureLabels)

#get test data
trainData <- prepareData(dir = dir, name = "train",
                         yFactors = activityLabels, 
                         features = desiredFeatures, 
                         featureLabels = featureLabels)

#merge data
allData <- rbind(trainData, testData)

#write data
write.table(allData, file = "tidyData.txt", row.names = F)

#Question 5
#Means per subject per activity
summarizedData <- aggregate(allData[, -c(1, 2)], #ignore name columns
                            by = list(
                              subject = allData$subject,    #aggregate using subject
                              activity = allData$activity), #aggregate using activity
                            mean)                           #get mean for each group

#write data
write.table(summarizedData, file = "meanObservations.txt", row.names = F)