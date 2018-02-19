run_analysis <- function()
{
  library(dplyr)
  
  # load features into a data frame; features are effectively column names
  features <- read.table("features.txt", stringsAsFactors = FALSE)
  
  # filter 'features' to only include column headings containing 'mean' OR 'std'
  mean_std_features <- grep(".*mean.*|.*std.*", features$V2)
  
  #load train data; x_train filters out only rows containing 'mean or std'
  x_train <- read.table("train/X_train.txt")[mean_std_features]
  y_train <- read.table("train/Y_train.txt")
  subject_train <- read.table("train/subject_train.txt")
  
  #create train_data data frame by combining columns from the source data frames loaded above
  train_data <- cbind(subject_train, y_train, x_train)

  #load test data; x_test filters out only rows containing 'mean or std'
  x_test <- read.table("test/X_test.txt")[mean_std_features]
  y_test <- read.table("test/Y_test.txt")
  subject_test <- read.table("test/subject_test.txt")
  
  #create test_data data frame by combining columns from the source data frames loaded above
  test_data <- cbind(subject_test, y_test, x_test)

  # append train data to test data; rows from following data frame are appended to previous data frame
  data <- rbind(train_data, test_data)

  # add more descriptive columns names to main data frame; need to include column names represented by filter list 'mean_std_features'
  colnames(data) <- c("subject","activity", features[mean_std_features,2])
 
  # load the activity labels
  activity_labels <- read.table("activity_labels.txt")
  
  # iterate each activity in df$Activity, replacing numeric code with string description; activity labels is used as a lookup
  data$activity <- as.factor(activity_labels$V2[data$activity])
  
  # now we groud the data by subject AND activity and summarise each calculating mean
  summarised_data <- data %>% group_by(subject, activity) %>% summarise_each(funs(mean))
}

save_data <- function()
{
  tidy_data <- run_analysis();
  
  write.table(tidy_data, "tidy_data.txt", row.name=FALSE)
}