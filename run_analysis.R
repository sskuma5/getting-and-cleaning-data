library(reshape2)
# 1.Merge the training and test sets to create one data set
# Read the data from files
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")

# Load activity labels + features
activityLabels[,2] <- as.character(read.table("UCI HAR Dataset/activity_labels.txt")[,2])
featuresData[,2] <- as.character(read.table("UCI HAR Dataset/features.txt")[,2])

# 2. Extract only the measurements on the mean and standard deviation for each measurement.
features <- grep(".*mean.*|.*std.*", featuresData[,2])
features.names <- featuresData[features,2]

# Load the datasets and bind the data
train <- read.table("UCI HAR Dataset/train/X_train.txt")[features]
train <- cbind(trainSubjects, trainActivities, train)

test <- read.table("UCI HAR Dataset/test/X_test.txt")[features]
test <- cbind(testSubjects, testActivities, test)

# 3. Use descriptive activity names to name the activities in the data set.
# Merge the finalData set with the acitivityType table to include descriptive activity names
allData <- rbind(train, test)
colnames(allData) <- c("subject", "activity", features.names)

# 4. Appropriately label the data set with descriptive activity names. 
# Cleaning up the variable names
features.names <- gsub('[-()]', '', features.names)
features.names <- gsub("-std$","StdDev",features.names)
features.names <- gsub("-mean","Mean",features.names)
features.names <- gsub("^(t)","time",features.names)
features.names <- gsub("^(f)","freq",features.names)
features.names <- gsub("([Gg]ravity)","Gravity",features.names)
features.names <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",features.names)
features.names <- gsub("[Gg]yro","Gyro",features.names)
features.names <- gsub("AccMag","AccMagnitude",features.names)
features.names <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",features.names)
features.names <- gsub("JerkMag","JerkMagnitude",features.names)
features.names <- gsub("GyroMag","GyroMagnitude",features.names)

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
# turn activities & subjects into factors
allData$activity <- factor(allData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
allData$subject <- as.factor(allData$subject)

allData.melted <- melt(allData, id = c("subject", "activity"))
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)

# Export the tidyData set
write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)