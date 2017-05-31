# Step 1 - Merges the training and the test sets to create one data set

library(dplyr)
# make sure to set working directory to proper location
# if you don't have this directory, you can specify different one
setwd('~/workspace/coursera/GCDCP')

# download file
temp <- tempfile(pattern="inputs", tmpdir="~/workspace/coursera/GCDCP")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
files <- unzip(zipfile=temp, exdir=paste(temp, ".dir", sep=''))

files_test <- grep("/X_test.txt|/y_test.txt|/subject_test.txt", files)
files_train <- grep("/X_train.txt|/y_train.txt|/subject_train.txt", files)

# we will loop over files and create data frame for each pair
# my_list will contain data for test/train
my_list <- rep(NA, length(files_test))

for( i in 1:length(files_test)) {
  cat("Merging (rbind):\n", sprintf("%s",files[files_test[i]]), "\n", sprintf("%s", files[files_train[i]]), "\n")
  df_test <- read.table(files[files_test[i]])
  df_train <- read.table(files[files_train[i]])
  my_list[[i]] <- rbind(df_test, df_train)
}

# Now we can cbind data frames
# we are joining subject (1), labels (3) and set (2)
merged_data <- cbind(my_list[[1]], my_list[[3]], my_list[[2]])

# columns' names are stored inside festures.txt
# second column is the name of the column. Additionally we have
# to set names for activity and subject id 
column_mames <- read.table(files[[2]])
names(merged_data) <- c(c("subject", "activity"), as.character(column_mames$V2))

# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement

# filter data and remove everything apart subject, activity and columns that refer to std or mean
names_filtered <- grep("subject|activity|mean|std",names(merged_data), ignore.case = TRUE)
reduced_data <- merged_data[,names_filtered]

# Step 3 - Uses descriptive activity names to name the activities in the data set

# set activity names
labels <- read.table(files[[1]])
reduced_data$activity <- labels$V2[reduced_data$activity]

# Step 4 - Appropriately labels the data set with descriptive variable names

# set descriptive names for columns
# we can make them more descriptive by replacing some abbreviations
names(reduced_data)<-gsub("^t", " Time ", names(reduced_data))
names(reduced_data)<-gsub("\\(t", "\\( Time ", names(reduced_data))
names(reduced_data)<-gsub("^f|Fre", " Freqency ", names(reduced_data))
names(reduced_data)<-gsub("Acc", " Accelerometer ", names(reduced_data))
names(reduced_data)<-gsub("Gyro", " Gyroscope ", names(reduced_data))
names(reduced_data)<-gsub("Mag", " Magnitude ", names(reduced_data))
names(reduced_data)<-gsub("BodyBody", " Body ", names(reduced_data))
names(reduced_data)<-gsub("-mean()-", " -mean() ", names(reduced_data))
names(reduced_data)<-gsub("-std()-", " -std() ", names(reduced_data))
names(reduced_data)<-gsub("JerkMean", "Jerk Mean", names(reduced_data))

# Step 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

second_set <- reduced_data %>% group_by(activity, subject) %>% summarise_each(funs(mean))
write.table(second_set, file = "second_set.txt", row.names = FALSE)

# clean up
# WARNING! Make sure you know what you are doing. This might be risky if you set your
# input locations incorrectly or in case you modify them during script execution

# unlink(temp)
# unlink(paste(temp, ".dir", sep=''), recursive = TRUE, force=TRUE)
