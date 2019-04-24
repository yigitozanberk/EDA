#load Required Library
library(dplyr)
#Download Dataset if not exists and ziped it 
filename <- "getdata_projectfiles_UCI HAR Dataset.zip"
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, filename, method="curl")
}

#read file from Data Set
table_file <- c("UCI HAR Dataset/features.txt", 
                "UCI HAR Dataset/activity_labels.txt", 
                "UCI HAR Dataset/test/subject_test.txt", 
                "UCI HAR Dataset/test/X_test.txt", 
                "UCI HAR Dataset/test/y_test.txt", 
                "UCI HAR Dataset/train/subject_train.txt", 
                "UCI HAR Dataset/train/X_train.txt", 
                "UCI HAR Dataset/train/y_train.txt")
list_table <- lapply(table_file, read.table)

#name the file in list 
names(list_table) = c("features", "activities", "subject_test", 
                      "X_test", "y_test", "subject_train", "X_train", 
                      "y_train")
# bind X_train and X_test rows
X<- rbind(list_table$X_train, list_table$X_test)
# bind Y_train and Y_test rows
Y<- rbind(list_table$y_train, list_table$y_test)
# bind subject train and test rows
Subject <- rbind(list_table$subject_train, list_table$subject_test)
#marage Subject, X, Y columes in single Dataset
Data <- cbind(Subject, X, Y)
# rename columes in Marage dataset 
names(Data) <- c("Subject", as.character(list_table[["features"]][,2]), 
                 "Activity")
# creat index for select columes

index = which(grepl("Subject|Activity|mean|std",  names(Data), ignore.case = TRUE))
# subsetting colume by index from main Datafram
SubData <- Data[, index]
# change code number in Activiy colume with acivity corssbond with it form activites list
SubData$Activity <- list_table$activities[SubData$Activity, 2]
# change absorbation to full name to make tidy data set 
names(SubData)<-gsub("Acc", "Accelerometer", names(SubData))
names(SubData)<-gsub("Gyro", "Gyroscope", names(SubData))
names(SubData)<-gsub("BodyBody", "Body", names(SubData))
names(SubData)<-gsub("Mag", "Magnitude", names(SubData))
names(SubData)<-gsub("^t", "Time", names(SubData))
names(SubData)<-gsub("^f", "Frequency", names(SubData))
names(SubData)<-gsub("tBody", "TimeBody", names(SubData))
names(SubData)<-gsub("-mean()", "Mean", names(SubData), ignore.case = TRUE)
names(SubData)<-gsub("-std()", "STD", names(SubData), ignore.case = TRUE)
names(SubData)<-gsub("-freq()", "Frequency", names(SubData), ignore.case = TRUE)
names(SubData)<-gsub("angle", "Angle", names(SubData))
names(SubData)<-gsub("gravity", "Gravity", names(SubData))
# make tidy data 
TidyData <- SubData %>%
        group_by(Subject, Activity) %>%
        summarise_all(funs(mean))
write.table(TidyData, "TidyData.txt", row.name=FALSE)