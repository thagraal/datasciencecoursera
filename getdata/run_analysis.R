library(plyr)

stopifnot(file.exists("./Samsung Data"))
#if (!file.exists("./Samsung Data")) {
#    print "please, locate the data in the directory named 'Samsung Data'"
#    break
#}

dataDir <- file.path("./Samsung Data")

write_tidy <- function() {
# Merges the training and the test sets to create one data set.
    X_train <- read.table(file.path(dataDir,"train", "X_train.txt"), header=FALSE, stringsAsFactors=FALSE )
    X_test <- read.table(file.path(dataDir,"test", "X_test.txt"), header=FALSE, stringsAsFactors=FALSE )
    Y_train <- read.table(file.path(dataDir,"train", "Y_train.txt"), header=FALSE, stringsAsFactors=FALSE, col.names=c("activity") )
    Y_test <- read.table(file.path(dataDir,"test", "Y_test.txt"), header=FALSE, stringsAsFactors=FALSE, col.names=c("activity") )
    features <- read.table(file.path(dataDir,"features.txt"), header=FALSE, stringsAsFactors=FALSE )
    names <- grep(".*mean|std.*", features$V2, value=TRUE)
    activity_labels <- read.table( file.path( dataDir,"activity_labels.txt"), header=FALSE, stringsAsFactors=FALSE )
    subject_train <- read.table(file.path(dataDir,"train", "subject_train.txt"), header=FALSE, stringsAsFactors=FALSE, col.names=c("subjectid") )
    subject_test <- read.table(file.path(dataDir,"test", "subject_test.txt"), header=FALSE, stringsAsFactors=FALSE, col.names=c("subjectid") )
    
    train <- cbind( Y_train, subject_train, X_train )
    test <- cbind( Y_test, subject_test, X_test)
    data <- rbind( train, test)

# Extracts only the measurements on the mean and standard deviation for each measurement. 
    measure <- data[, c(c(1:2), grep(".*mean|std.*", features$V2)+2)]
    measure$activity <- as.factor(measure$activity)

# Uses descriptive activity names to name the activities in the data set
    levels(measure$activity) <- activity_labels$V2
    names(measure)<-c("activity","subjectid", names)
    activitynames <- rep( names(measure[,3:(length(names(measure)))]),nrow(activity)*length(c(unique(subject_train$subjectid),unique(subject_test$subjectid))))

# Appropriately labels the data set with descriptive variable names. 
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    tidy <- ddply(measure, c(.(activity),.(subjectid)), summarize, means=colMeans( measure[3:(length(names(measure)))]))
    tidy$measures <- activitynames
    write.table(tidy, "tidy-data.txt",row.names=FALSE)
}

write_tidy()
