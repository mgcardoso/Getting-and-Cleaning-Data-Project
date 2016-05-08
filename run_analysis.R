setwd("C:/Users/miria/Desktop/Coursera/UCI HAR Dataset")
library(plyr)
library(dplyr)
library(data.table)
YTest <- read.table("C:/Users/miria/Desktop/Coursera/UCI HAR Dataset/test/y_test.txt")
XTest <- read.table("C:/Users/miria/Desktop/Coursera/UCI HAR Dataset/test/X_test.txt")
SubjectTest <- read.table("C:/Users/miria/Desktop/Coursera/UCI HAR Dataset/test/subject_test.txt")
YTrain <- read.table("C:/Users/miria/Desktop/Coursera/UCI HAR Dataset/train/y_train.txt")
XTrain <- read.table("C:/Users/miria/Desktop/Coursera/UCI HAR Dataset/train/X_train.txt")
SubjectTrain <- read.table("C:/Users/miria/Desktop/Coursera/UCI HAR Dataset/train/subject_train.txt")
Features <- read.table("C:/Users/miria/Desktop/Coursera/UCI HAR Dataset/features.txt")
# 1-Fix column names
colnames(XTrain) <- t(Features[2])
colnames(XTest) <- t(Features[2])

XTrain$activities <- YTrain[, 1]
XTrain$participants <- SubjectTrain[, 1]
XTest$activities <- YTest[, 1]
XTest$participants <- SubjectTest[, 1]
#1-Merges the training and the test sets to create one data set
Master <- rbind(XTrain, XTest)
duplicated(colnames(Master))
Master <- Master[, !duplicated(colnames(Master))]
str(Master)
#2-Extracts only the measurements on the mean and standard deviation for each measurement
Mean <- grep("mean()", names(Master), value = FALSE, fixed = TRUE)
Mean <- append(Mean, 471:477)
InstrumentMeanMatrix <- Master[Mean]
names(InstrumentMeanMatrix)
str(InstrumentMeanMatrix)
STD <- grep("std()", names(Master), value = FALSE)
InstrumentSTDMatrix <- Master[STD]
str(InstrumentSTDMatrix)
#3-Uses descriptive activity names to name the activities in the data set
Master$activities <- as.character(Master$activities)
Master$activities[Master$activities == 1] <- "Walking"
Master$activities[Master$activities == 2] <- "Walking Upstairs"
Master$activities[Master$activities == 3] <- "Walking Downstairs"
Master$activities[Master$activities == 4] <- "Sitting"
Master$activities[Master$activities == 5] <- "Standing"
Master$activities[Master$activities == 6] <- "Laying"
Master$activities <- as.factor(Master$activities)
str(Master$activities)
#4-Appropriately labels the data set with descriptive variable names
names(Master)
names(Master) <- gsub("Acc", "Accelerator", names(Master))
names(Master) <- gsub("Mag", "Magnitude", names(Master))
names(Master) <- gsub("Gyro", "Gyroscope", names(Master))
names(Master) <- gsub("^t", "time", names(Master))
names(Master) <- gsub("^f", "frequency", names(Master))
names(Master) <- gsub("BodyBody", "Body", names(Master))
names(Master)
#5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Master.dt <- data.table(Master)
TidyData <- Master.dt[, lapply(.SD, mean), by = 'participants,activities']
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)
str(TidyData)