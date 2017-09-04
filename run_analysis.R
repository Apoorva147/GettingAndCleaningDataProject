#1. Merges the training and test data sets to create  one data set
#Need to merge train and test data to create one single data set. 
#Where we have total six datasets

#- 'train/X_train.txt': Training set.
#- 'train/y_train.txt': Training labels.
#-Train/subject_train.txt'
#- 'test/X_test.txt': Test set.
#- 'test/y_test.txt': Test labels.
#-Test/subject_test.txt'

#As x_train.txt and x_test.txt are similar data we need to do row 
#bind which as same number columns As y_train.txt and y_test.txt are 
#similar data we need to do row bind which as same number columns
#subject_train.txt'and Test/subject_test.txt' are similar data we 
#need to do row bind which as same number columnsonce step 1 ,2 ,3 
#are done now we have to do column bind as we have different data 
#sets that give single data set comprising all


subjecttraindata <- read.table("train/subject_train.txt")
xtraindata <- read.table("train/X_train.txt")
ytraindata <- read.table("train/y_train.txt")
subjecttestdata <- read.table("test/subject_test.txt")
xtestdata <- read.table("test/X_test.txt")
ytestdata <- read.table("test/y_test.txt")

completedataset <- rbind(cbind(subjecttraindata, xtraindata, ytraindata),cbind(subjecttestdata, xtestdata, ytestdata))
#2. Extracts only the measurements on the mean and standard deviation
#for each measurement
#As we got complete data set we need the assign column names from 
#features text file which  v2 column contain 561 names where
#as complete set got 563 where 562 and 563 are labels and subject data

# read features
features <- read.table("features.txt", as.is = TRUE)

colnames(completedataset) <- c("subject", features[, 2], "activity")

# determine columns of data set to keep 
stdmeancolumns<- grepl("mean|std", colnames(completedataset))

# data in these columns only
stdmeandataset <- completedataset[, stdmeancolumns]

#3. Uses descriptive activity names to name the activities in the data set
# read activity labels
#use descriptive activity name to activities in the data set ( in our data
#set its labels column).Descriptive activity name are stored in activity_labels.txt

activities <- read.table("activity_labels.txt")
completedataset$activity <- factor(completedataset$activity, 
                          levels = activities[, 1], labels = activities[, 2])


#4. Appropriately  labels the data set with descriptive variable names\
# for this we need to get all the column names from the complete dataset and find 
#probable name with meaningful description

# get column names
completedatasetcolumns <- colnames(completedataset)

completedatasetcolumns <- gsub("mean", "Mean", completedatasetcolumns)
completedatasetcolumns <- gsub("std", "StandardDeviation", completedatasetcolumns)
completedatasetcolumns <- gsub("Acc", "Accelerometer", completedatasetcolumns)
completedatasetcolumns <- gsub("Gyro", "Gyroscope", completedatasetcolumns)
completedatasetcolumns <- gsub("Mag", "Magnitude", completedatasetcolumns)
completedatasetcolumns <- gsub("Freq", "Frequency", completedatasetcolumns)
 


# 5. Create second independent tidy data set with average  variable for each activity
#subject

# group by subject and activity and summarise using mean

completedatasetcolumns <- gsub("[\\(\\)-]", "", completedatasetcolumns)
library(dplyr)
completedatasettidydata<-group_by(completedataset,subject,activity)%>%
summarise_all(funs(mean))

write.table(completedatasettidydata, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)