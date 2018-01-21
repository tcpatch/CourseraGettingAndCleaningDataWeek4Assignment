# -The Github repo contains the required scripts.
# -GitHub contains a code book that modifies and updates the available codebooks 
#  with the data to indicate all the variables and summaries calculated, along 
#  with units, and any other relevant information.
# -The README that explains the analysis files is clear and understandable.

##main_driver runs through the entire script
main_driver <- function() {
  get_data()
  setwd('UCI HAR Dataset')
  df <- merge_training_and_test_sets()
  df2 <- extract_mean_and_stdev(df)
  df3 <- name_activities_in_data_set(df2)
  df4 <- rename_variable_names(df3)
  create_tidy_data_set(df4)
}

## 1) Merge the training and the test sets to create one data set.
merge_training_and_test_sets <- function() {
  ## read column names of dataset and store as features
  features <- read.table('features.txt')
  ## read all datasets to be merged
  X_test <- read.table('test/X_test.txt')
  y_test <- read.table('test/y_test.txt')
  X_train <- read.table('train/X_train.txt')
  y_train <- read.table('train/y_train.txt')
  test_subject <- read.table('test/subject_test.txt')
  train_subject <- read.table('train/subject_train.txt')
  ## rename column names to values from features
  colnames(X_test) <- features$V2
  colnames(X_train) <- features$V2
  ## convert datasets to data frames
  X_test <- as.data.frame(X_test)
  y_test <- as.data.frame(y_test)
  X_train <- as.data.frame(X_train)
  y_train <- as.data.frame(y_train)
  ## add column named 'labels' to both test and train data set
  X_test$labels <- y_test$V1
  X_train$labels <- y_train$V1
  ## add column named 'subject' containing subject id to both data sets
  X_test$subject <- test_subject$V1
  X_train$subject <- train_subject$V1
  ## combine both data sets into one dataset
  complete_data <- rbind(X_test, X_train, make.row.names=TRUE)
  ## return data set as a data frame
  return(as.data.frame(complete_data))
}


## 2) Extract only the measurements on the mean and standard deviation for each
##    measurement.
extract_mean_and_stdev <- function(df) {
  ## get each column name and coerce into a character vector
  variables <- colnames(df)
  variables <- as.character(variables)
  ## store each column index name containing mean or std measurements
  mean_columns <- grepl('mean\\(\\)', variables)
  std_columns <- grepl('std\\(\\)', variables)
  ## extract each column name from the grepl function
  mean_names <- variables[mean_columns]
  std_names <- variables[std_columns]
  ## combine all relavent labels for the data set
  mean_std_names <- c(mean_names, std_names, 'labels', 'subject')
  ## return the subsetted column names
  df[, mean_std_names]
}


## 3) Use descriptive activity names to name the activities in the data set
name_activities_in_data_set <- function(df) {
  ## obtain activitiy names from the activity_labels.txt file
  activity_labels <- read.table('activity_labels.txt')
  ## rename each level to the corresponding label from 'activity_labels.txt'
  df$labels <- factor(df$labels, levels=activity_labels$V1, labels=activity_labels$V2)
  ## return the relabeled dataframe
  df
}


## 4) Appropriately label the data set with descriptive variable names.
rename_variable_names <- function(df) {
  ## extract the column names of the dataset (named in merge_training_and_test_sets)
  original_names <- colnames(df)
  ## usees the helper function original_names to rename each column name and
  ## set new names as the column names of the df
  colnames(df) <- lapply(original_names, name_parser)
  ## return the dataframe
  df
}


## 5) From the data set in step 4, create a second, independent tidy data set 
##   with the average of each variable for each activity and each subject.
create_tidy_data_set <- function(df) {
  library(dplyr)
  ## group dataframe by subject, then by activity
  ## take avg of each variable
  tidydf <- df %>% group_by(subject, labels) %>% summarize_all(mean)
  ## table is written to tidydf.txt for submission
  write.table(tidydf, file='tidydf.txt', row.name=FALSE)
  ## data set is returned as a matrix if further processing is desired
  as.data.set(tidydf)
}


## get_data is a helper function to download data if necessary
get_data <- function() {
  filename <- 'UCI_HAR_Dataset.zip'
  ## Check if file exists and download the dataset if it hasn't:
  if (!file.exists('UCI HAR Dataset/test/X_test.txt')){
    file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(file_url, filename, method="auto")
  }
  ## Unzip the dataset if it hasn't been done so already
  if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
  }
}


## name_parse is a helper function to rename datasets variable names
name_parser <- function(s) {
  ## Logic of code is to check whether the signal is time or frequency
  ## then go through and see which variable(s) are being measured:
  ##    mean/std, body/gravity, accelerometer/gyroscope, jerk/mag, and direction
  ## then all descriptive names (saved into variables) are pasted together
  if (grepl('^t', s)) {
    signal = 'time'
    if (grepl('std', s)) {
      mean_std = 'std'
    }
    else {
      mean_std = 'mean'
    }
    if (grepl('Body', s)) {
      aspect_measured = 'body'
    }
    else {
      aspect_measured = 'gravity'
    }
    if (grepl('Acc', s)) {
      measurement_device = 'accelerometer'
    }
    else{
      measurement_device = 'gyroscope'
    }
    if (grepl('Jerk', s)) {
      jerk = 'jerk'
    }
    else {
      jerk = ''
    }
    if (grepl('Mag', s)) {
      mag = 'magnitude'
    }
    else {
      mag = ''
    }
    if (grepl('-X', s)) {
      direction = 'x'
    }
    else if (grepl('-Y', s)) {
      direction = 'xy'
    }
    else if (grepl('-Z', s)) {
      direction = 'z'
    }
    else {
      direction = ''
    }
    if (jerk=='' && mag == '') {
      n <- paste(signal, aspect_measured, measurement_device, direction, mean_std, sep='-')
    }
    else if (jerk == '' && direction == '') {
      n <- paste(signal, aspect_measured, measurement_device, mean_std, sep='-')
    }
    else if (mag == '' && direction == '') {
      n <- paste(signal, aspect_measured, measurement_device, jerk, mean_std, sep='-')
    }
    else if (jerk == '') {
      n <- paste(signal, aspect_measured, measurement_device, mag, direction, mean_std, sep='-')
    }
    else if (mag == '') {
      n <- paste(signal, aspect_measured, measurement_device, jerk, direction, mean_std, sep='-')
    }
    else if (direction == '') {
      n <- paste(signal, aspect_measured, measurement_device, jerk, mag, mean_std, sep='-')
    }
    else {
      n <- paste(signal, aspect_measured, measurement_device, jerk, mag, direction, mean_std, sep='-')
    }
  }
  else if (grepl('^f', s)) {
    signal = 'frequency'
    aspect_measured = 'body'
    if (grepl('std', s)) {
      mean_std = 'std'
    }
    else {
      mean_std = 'mean'
    }
    if (grepl('Acc', s)) {
      measurement_device = 'accelerometer'
    }
    else{
      measurement_device = 'gyroscope'
    }
    if (grepl('Jerk', s)) {
      jerk = 'jerk'
    }
    else {
      jerk = ''
    }
    if (grepl('Mag', s)) {
      mag = 'magnitude'
    }
    else {
      mag = ''
    }
    if (grepl('-X', s)) {
      direction = 'x'
    }
    else if (grepl('-Y', s)) {
      direction = 'y'
    }
    else if (grepl('-Z', s)) {
      direction = 'z'
    }
    else {
      direction = ''
    }
    if (jerk=='' && mag == '') {
      n <- paste(signal, aspect_measured, measurement_device, direction, mean_std, sep='-')
    }
    else if (jerk == '' && direction == '') {
      n <- paste(signal, aspect_measured, measurement_device, mean_std, sep='-')
    }
    else if (mag == '' && direction == '') {
      n <- paste(signal, aspect_measured, measurement_device, jerk, mean_std, sep='-')
    }
    else if (jerk == '') {
      n <- paste(signal, aspect_measured, measurement_device, mag, direction, mean_std, sep='-')
    }
    else if (mag == '') {
      n <- paste(signal, aspect_measured, measurement_device, jerk, direction, mean_std, sep='-')
    }
    else if (direction == '') {
      n <- paste(signal, aspect_measured, measurement_device, jerk, mag, mean_std, sep='-')
    }
    else {
      n <- paste(signal, aspect_measured, measurement_device, jerk, mag, direction, mean_std, sep='-')
    }
  }
  else if (grepl('labels', s)) {
    n <- 'labels'
  }
  else if (grepl('subject', s)) {
    n <- 'subject'
  }
}
