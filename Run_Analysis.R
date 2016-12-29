# PART 1: Merge the training and the test sets to create one tidy data set.
# This step also completes 
# -------------------------------------------------------------------
# Get the activity and feature names and clean them
ActivityNames <- read.table("UCI HAR Dataset/activity_labels.txt")
FeatureNames <- read.table("UCI HAR Dataset/features.txt")

# remove unclean characters from the feature name list
ClearFeatureNames <- clean_names(FeatureNames$V2)
ClearFeatureNames <- gsub( "mean_", "mean", ClearFeatureNames)

# create dummy column for dynamically loading the data into data frame
DataTypeList_train <- get_tidydat("UCI HAR Dataset/train/subject_train.txt")

# rename the dummy varibale to "data_type" and set values to "train" for training data
names(DataTypeList_train) <- "data_type"
DataTypeList_train[,] <- 'train'

# Get the subject data
subjdat_train <- get_tidydat("UCI HAR Dataset/train/subject_train.txt")
names(subjdat_train) <- "subject"

# Get the subject data
activitydat_train <- get_tidydat("UCI HAR Dataset/train//y_train.txt")
names(activitydat_train) <- "activity"

# Get the measurement data
xdat_train <- get_tidydat("UCI HAR Dataset/train//x_train.txt")
names(xdat_train) <- ClearFeatureNames

TrainData <- cbind.data.frame(DataTypeList_train,subjdat_train,activitydat_train,xdat_train)

# do the same for the test data

# create dummy column for dynamically loading the data into data frame
DataTypeList_test <- get_tidydat("UCI HAR Dataset/test/subject_test.txt")

# rename the dummy varibale to "data_type" and set values to "train" for training data
names(DataTypeList_test) <- "data_type"
DataTypeList_test[,] <- 'test'

# Get the subject data
subjdat_test <- get_tidydat("UCI HAR Dataset/test/subject_test.txt")
names(subjdat_test) <- "subject"

# Get the subject data
activitydat_test <- get_tidydat("UCI HAR Dataset/test//y_test.txt")
names(activitydat_test) <- "activity"

# Get the measurement data
xdat_test <- get_tidydat("UCI HAR Dataset/test//x_test.txt")
names(xdat_test) <- ClearFeatureNames

TestData <- cbind.data.frame(DataTypeList_test,subjdat_test,activitydat_test,xdat_test)

# append the test data and training data
TheData <- rbind(TrainData,TestData);

# -------------------END PART 1 -------------------------------------
# -------------------------------------------------------------------

# PART 2: Extract only the measurements on the mean and standard deviation for each measurement.
# -------------------------------------------------------------------
# Extract only varibales for mean and std
VarNames <- names(TheData);
# get the variables for mean and std
VarList <- grepl('mean|std',VarNames)
# add the type, subject, and acticity labels back to the set of included variables
VarList[1:3] <-TRUE

VarNames[VarList]
ReducedData <- TheData[,VarList]

# -------------------END PART 2 -------------------------------------
# -------------------------------------------------------------------

# PART 3: Use descriptive activity names to name the activities in the data set
# -------------------------------------------------------------------
# name the activities in the dataset
ReducedData$activity <- tolower(ActivityNames$V2[ReducedData$activity])


# write data to file:
write.table(ReducedData, "DataFile1.txt", sep="\t",row.names = FALSE)

# -------------------END PART 3 -------------------------------------
# -------------------------------------------------------------------

# PART 4: Appropriately labels the data set with descriptive variable names.
# -------------------------------------------------------------------
# Unclear about this instruction, as I dont know what many of these variables are.  
# So I just made the variable names "tidy"

# -------------------END PART 4 -------------------------------------
# -------------------------------------------------------------------


# PART 5: creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# -------------------------------------------------------------------
# create factor varaibles for subject and activity
dim$activity <- factor(ReducedData$activity)
ReducedData$subject <- factor(ReducedData$subject)

# subset the data for mean computation
XData <- ReducedData[,4:89];

# compute mean by factors
SummaryData <- aggregate(x = XData, by = list(TheData$activity, TheData$subject), FUN = "mean")

# label cators in new dataset
names(SummaryData)[1] <- 'activity'
names(SummaryData)[2] <- 'subject'

# write the data to file
write.table(SummaryData, "DataFile2_means.txt", sep="\t",row.names = FALSE)

# write the feature names for summary file
VarNames <- names(XData)
write.table(VarNames, "feature_names.txt", sep="\t",row.names = FALSE,col.names = FALSE)



# function reads data from specificed file path and default names the variables using the name of the file
get_tidydat <- function(filepath) {
  X <- gsub( ".txt", "", as.character(basename(filepath)))
  tempdat = read.table(filepath)
  
  ifelse(length(tempdat) == 1,
         names(tempdat) <- tolower(X),
         names(tempdat) <- tolower(paste0(X,'_',names(tempdat)))
         # names(tempdat) <- tolower(names(tempdat))
         )
  tempdat
}

# function removes special characters from the variable in x
clean_names <- function(namelist) {
  X <- gsub( "()", "", namelist, fixed="TRUE")
  X <- gsub( "-", "_", X, fixed="TRUE")
  X <- gsub( ",", "to", X, fixed="TRUE")
  X <- gsub( "(", "_", X, fixed="TRUE")
  X <- gsub( ")", "_", X, fixed="TRUE")
  X <- gsub( "mean_", "mean", X, fixed="TRUE")
  X <- tolower(X)
}