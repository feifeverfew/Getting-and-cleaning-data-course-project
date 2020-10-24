library(tidyverse)
# check if file exists; if not, download the file
filename <- "course3_data.zip"
if (!file.exists (filename)) {
    fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileurl, destfile =filename, method ='curl')
}
#check if the folder exists; if not, unzip the folder
if (!file.exists('UCI HAR Dataset')) {
    unzip(filename)
}
# read data frames
features <- read_table2("UCI HAR Dataset/features.txt", col_names = c('n', 'functions'))
activities <- read_table2("UCI HAR Dataset/activity_labels.txt", col_names = c('code', 'activity'))
subject_test <- read_table2("UCI HAR Dataset/test/subject_test.txt", col_names = "subject")
x_test <- read_table2("UCI HAR Dataset/test/X_test.txt", col_names = features$functions)
y_test <- read_table2("UCI HAR Dataset/test/Y_test.txt", col_names = 'code')
subject_train <- read_table2("UCI HAR Dataset/train/subject_train.txt", col_names = "subject")
x_train <- read_table2("UCI HAR Dataset/train/X_train.txt", col_names = features$functions)
y_train <- read_table2("UCI HAR Dataset/train/Y_train.txt", col_names = 'code')
# question 1: merge the training and test sets to create one data set
x <- rbind (x_train, x_test)
y <- rbind (y_train, y_test)
subject <- rbind (subject_train, subject_test)
all_data <- cbind (subject, y, x)
# question 2: Extracts only the measurements on the mean and standard deviation for each measurement
extracted_data <- all_data %>% select (subject, code, contains('mean'), contains ('std'))
# question 3: uses descriptive activity names to name the activities in the data set; 
extracted_data <- extracted_data %>% left_join (activities) %>% select (-code)
# question 4: Appropriately labels the data set with descriptive variable names.
names(extracted_data)<-gsub("Acc", "Accelerometer", names(extracted_data))
names(extracted_data)<-gsub("Gyro", "Gyroscope", names(extracted_data))
names(extracted_data)<-gsub("BodyBody", "Body", names(extracted_data))
names(extracted_data)<-gsub("Mag", "Magnitude", names(extracted_data))
names(extracted_data)<-gsub("^t", "Time", names(extracted_data))
names(extracted_data)<-gsub("^f", "Frequency", names(extracted_data))
names(extracted_data)<-gsub("tBody", "TimeBody", names(extracted_data))
names(extracted_data)<-gsub("-mean()", "Mean", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("-std()", "STD", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("-freq()", "Frequency", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("angle", "Angle", names(extracted_data))
names(extracted_data)<-gsub("gravity", "Gravity", names(extracted_data))
#question 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
data2nd <- extracted_data %>% group_by(subject, activity) %>% summarise_all(funs(mean))
write_csv (data2nd, 'data2nd.csv')