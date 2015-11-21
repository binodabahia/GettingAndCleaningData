#21/11/2015
#Coursera - Getting and Cleaning Data - Course code


#_____OVERALL INSTRUCTIONS____________
# You should create one R script called run_analysis.R that does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#_____________________________________

#Loading library dplyr
library(dplyr)

#Reads train data"
subject_train <- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt",header=FALSE)
y_train <- read.table(".\\UCI HAR Dataset\\train\\y_train.txt",header=FALSE)
X_train <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt",header=FALSE)

#Reads test data
subject_test <- read.table(".\\UCI HAR Dataset\\test\\subject_test.txt",header=FALSE)
y_test <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt",header=FALSE)
X_test <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt",header=FALSE)


#Reads labels
features <- read.table(".\\UCI HAR Dataset\\features.txt",header=FALSE)
activity_labels <- read.table(".\\UCI HAR Dataset\\activity_labels.txt",header=FALSE)
activity_labels<-activity_labels[,2]

# Merges the training and the test sets to create one data set.
subject <- rbind(subject_train, subject_test)
Y <- rbind(y_train, y_test)
X <- rbind(X_train, X_test)

# Appropriately labels the data set with descriptive variable names.
colnames(subject)[1]<- "subject"
colnames(Y)[1]<- "activity"
colnames(X)<- features[,2]


# Extracts only the measurements on the mean and standard deviation for each measurement. 
X_No_Duplicates<- X[ !duplicated(names(X)) ]
mean_observations <- select(X_No_Duplicates , contains("mean"))
std_observations <- select(X_No_Duplicates , contains("std"))


# Appropriately labels the data set with descriptive variable names.
Y_labeled <- mutate(Y,activities = factor(activity,labels=activity_labels))
Y_labeled <- Y_labeled[,2]


# Merges to create one data set.
Merged_Data <- data.frame(subject,Y_labeled,mean_observations,std_observations)
colnames(Merged_Data)[2]<- "activity"

# From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.
MD <- group_by(Merged_Data, subject,activity)
MD_Summary <- MD %>% summarise_each(funs(mean))
write.table(MD_Summary, file = "MD_Summary.txt",row.name=FALSE)
