library(dplyr)
library(tidyr)
library(data.table)

readAndMergeData <- function() {
  train_x <- tbl_df(fread("UCI HAR Dataset/train/X_train.txt"))
  train_y <- tbl_df(fread("UCI HAR Dataset/train/y_train.txt"))
  train_sub <- tbl_df(fread("UCI HAR Dataset/train/subject_train.txt"))
  test_x <- tbl_df(fread("UCI HAR Dataset/test/X_test.txt"))
  test_y <- tbl_df(fread("UCI HAR Dataset/test/y_test.txt"))
  test_sub <- tbl_df(fread("UCI HAR Dataset/test/subject_test.txt"))
  features <- tbl_df(fread("UCI HAR Dataset/features.txt"))
  
  x <- train_x %>% bind_rows(test_x)
  y <- train_y %>% bind_rows(test_y)
  subject <- train_sub %>% bind_rows(test_sub)
  
  colnames(subject) <- c("Subject")
  
  list(x=x, y=y, subject=subject, features=features)
}

extractMeanAndStd <- function(data) {
  meanStd <- grepl("mean|std", unlist(data$features[,2]))
  data$x <- data$x[,meanStd]
  colnames(data$x) <- unlist(data$features[meanStd,2])
  data$x
}

labelActivities <- function(data) {
  labels <- tbl_df(fread("UCI HAR Dataset/activity_labels.txt"))
  colnames(labels) <- c("Activity")
  colnames(data$y) <- c("Activity")
  data$y <- merge(data$y, labels, by="Activity")
  data$y <- data$y[,2]
  data$y <- tbl_df(data.table(data$y))
  colnames(data$y) <- c("Activity")
  data$y
}

prepareAll <- function() {
  out <- readAndMergeData()
  out$x <- extractMeanAndStd(out)
  out$y <- labelActivities(out)
  out$combined <- out$x %>% bind_cols(out$y) %>% bind_cols(out$subject)
  out$tidy <- out$combined %>% group_by(Subject, Activity) %>% 
    summarise_each(funs(mean)) %>% 
    gather(Measurement, Mean, -Activity, -Subject)
  write.table(out$tidy, file="tidy_data.txt", row.name = FALSE)
  out
}