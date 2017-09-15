# **************************************** About this Script **************************************** #


# **************************************** Setup Section **************************************** #
# Introduction:  This script constitutes my answer to the "Peer-graded Assignment", that is
#   part of the Coursera MOOC "Getting and Cleaning Data".

# Goal:  To integrate, structure and format the input data to generate a tidy dataset that is 
#   appropriate for it's further analysis.

# Inputs:  
#  1) The dataset of "data collected from the accelerometers from the Samsung Galaxy S smartphone",  
#     provided by the MOOC instructors, located in the directory given by the input parameter
#     "working_directory_name".

# Outputs:
#  1) A file named "tidy_samsung_data.txt" that contains the tidy data set, located in the 
#     directory working directory given by the input variable <working_directory_name>.

# Usage:
#  1) Locate the input files and directories in the base directory.
#  2) From RStudio, call this script's main function run_analysis(), setting the input parameter with the base directory.
#  3) Wait a couple of seconds until the script ends and generates the output file.

# Assumptions:
#  1) The input data has the exact directories and files as provided by the MOOC instructors.

# Author:  Pablo Rosales
# *********************************************************************************************** #

run_analysis <- function (working_directory_name) {
     
     # **************************************** Setup Section **************************************** #
     
     # ---------------------------------- Import Librariess ---------------------------------- #
     library(dplyr)
     
     # ---------------------------------- Set input directories ---------------------------------- #
     base_dir  <- working_directory_name
     test_dir  <- paste0(base_dir, 'test/')
     train_dir <- paste0(base_dir, 'train/')
     
     # ---------------------------------- Set output directory and file ---------------------------------- #
     tidy_data_filename <- paste0(base_dir, "tidy_samsung_data.txt")
     
     # **************************************** Step 1:  Merge Input Files **************************************** #
     
     # ---------------------------------- Read input files ---------------------------------- #
     #test
     test_subject <- read.table(paste0(test_dir, 'subject_test.txt'))
     test_x <- read.table(paste0(test_dir, 'X_test.txt'))
     test_y <- read.table(paste0(test_dir, 'y_test.txt'))
     
     #training
     train_subject <- read.table(paste0(train_dir, 'subject_train.txt'))
     train_x <- read.table(paste0(train_dir, 'X_train.txt'))
     train_y <- read.table(paste0(train_dir, 'y_train.txt'))
     
     #metadata for column names
     columns_names <- read.table(paste0(base_dir, 'features.txt'))
     
     #metadata for activities codes and labels(names)
     activities_lookup_table <- read.table(paste0(base_dir, 'activity_labels.txt'))
     colnames(activities_lookup_table)[1] <- "activity_code"
     colnames(activities_lookup_table)[2] <- "activity_name"
     
     # ---------------------------------- Merge inputs into a consolidated file ----------------------------------#
     consolidated_test <- cbind(test_subject, test_y) %>% cbind(test_x)
     consolidated_train <- cbind(train_subject, train_y) %>% cbind(train_x)
     consolidated_all <- rbind(consolidated_test, consolidated_train)

     # **************************************** Step 2: Extract Mean & Std. Deviation **************************************** #
     # indices of:  subject and label (i.e. activity)
     subject_and_label_indices <- c(1,2)

     # the indices of the columns
     columns_offset <- 2  # the number of columns inserted to the left in the consolidated dataset(i.e. subject and label columns)
     mean_headers_indices <- grep("(mean)", columns_names$V2, ignore.case = T) + columns_offset
     std_headers_indices  <- grep("(std)", columns_names$V2, ignore.case = T) + columns_offset
     all_headers_indices  <- c(subject_and_label_indices, c(mean_headers_indices, std_headers_indices))
     
     
     # select the subject, label, means and stdvs columns from the consolidated dataset
     consolidated_mean_and_std_data <- consolidated_all[all_headers_indices]
     
     colnames(consolidated_mean_and_std_data)[1] <- "subject_id"
     colnames(consolidated_mean_and_std_data)[2] <- "activity_code"
     
     
     # **************************************** Step 3: Use activity "labels" to describe activity "codes" **************************************** #
     consolidated_mean_and_std_data_labeled <- merge(consolidated_mean_and_std_data, activities_lookup_table, by.x='activity_code' , by.y='activity_code')
     consolidated_mean_and_std_data_labeled <- mutate(consolidated_mean_and_std_data_labeled, activity_name = as.character(activity_name))
     consolidated_mean_and_std_data_labeled <- mutate(consolidated_mean_and_std_data_labeled, activity_code = activity_name)
     consolidated_mean_and_std_data_labeled <- subset(consolidated_mean_and_std_data_labeled, select = -c(activity_name))
     
     colnames(consolidated_mean_and_std_data_labeled)[1] <- "activity_name"
     
     # **************************************** Step 4: Label variables with descriptive names **************************************** #
     # extract column names from features file
     mean_headers_indices_in_features_file <- grep("(mean)", columns_names$V2, ignore.case = T) 
     std_headers_indices_in_features_file  <- grep("(std)", columns_names$V2, ignore.case = T) 
     all_headers_indices_in_features_file  <- c(mean_headers_indices_in_features_file, std_headers_indices_in_features_file)
     column_names_vec <- columns_names$V2
     selected_column_names_vec <- sapply(column_names_vec, as.character)
     selected_column_names_with_labels_vec <- selected_column_names_vec[all_headers_indices_in_features_file]
     
     # Set descriptive column names
     tidy_column_names_vec <- sub("^t", "Time ", selected_column_names_with_labels_vec)
     tidy_column_names_vec <- sub("^f", "Frequency ", tidy_column_names_vec)
     tidy_column_names_vec <- sub("\\(\\)", "", tidy_column_names_vec)
     tidy_column_names_vec <- sub("-", " ", tidy_column_names_vec)
     tidy_column_names_vec <- c(c("activity_name", "subject_id"), tidy_column_names_vec)
     
     # rename columns in consolidated dataset
     consolidated_mean_and_std_data_labeled <- setNames(consolidated_mean_and_std_data_labeled, tidy_column_names_vec)
     
     
     # **************************************** Step 5: Group by Subjects and activity **************************************** #
     summarized_df <- consolidated_mean_and_std_data_labeled %>% group_by(subject_id, activity_name) %>% summarize_all(funs(mean))
     write.table(summarized_df, file=tidy_data_filename, row.names=FALSE)
}
