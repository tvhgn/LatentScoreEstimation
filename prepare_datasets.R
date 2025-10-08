#' Prepare Participant Behavioral Datasets for JAGS Modeling
#'
#' This script organizes, cleans, and summarizes behavioral data from multiple experimental subjects
#' for use in Bayesian modeling and subsequent analysis. It processes individual participant directories containing raw 
#' behavioral CSV files, splits each subject's data into halves (for possible reliability checks), removes incomplete trials, 
#' and stores cleaned datasets in a structured list. It also imports preprocessed ranking and rating files, storing ranking data per subject
#' and extracting pre-MRI liking score ratings.
#'
#' For each subject, the script calculates the number of unique stimulus pairs presented, and aggregates this information to compute
#' the median and theoretical maximum pairwise comparisons across the sample. Results and intermediate datasets are stored in a dedicated 'varstore' folder.
#'
#' Required libraries: dplyr, readxl
#'
#' Workflow summary:
#' - Creates a "varstore" directory for data storage
#' - Locates and indexes participant behavioral file directories
#' - Reads, cleans, and splits each subject’s behavioral data (excluding incomplete/jittered trials)
#' - Reads participant-specific ranking and ratings files; extracts and stores ranking vectors and pre-experiment ratings
#' - Calculates number of unique stimulus pairs tested for each subject and percentage coverage of possible pairs
#' - Exports participant-wise data as an RData file for downstream modeling/analysis
#'
#' @author [Tom van Hogen]
#' @date [2025-10-08]


# Necessary imports
library(dplyr)
library(readxl)

# Create folder called varstore to store any important variables and datasets
ifelse(!dir.exists(file.path("varstore")),
       dir.create(file.path("varstore")),
       "Directory Exists")

# Path to subjects data, which includes subject folders containing behavioral files
subjects_path <- file.path("..", "subject_behave_files") # Change according to directory structure

# Get directories from subjects_path
subject_dirs <- list.dirs(subjects_path, recursive=FALSE)

# Initialize variables
n_subjects <- length(subject_dirs)
unique_pairs <- matrix(NA, n_subjects, 1)
nChains <- 3
true_ranks_subs <- list() # Initialize list that contains 'true' ranking data for all subjects (from ranking task)
datasets <- list()
ratings <- list()

# Loop across participants and create behavioral datasets fit for JAGS
for (i in 1:n_subjects){
  # Initialize list for containing datafiles per participant
  behavioral_data <- list()
  # Initialize empty lists per half of the subject dataset (for potential reliability checks)
  first_half <- list()
  second_half <- list()
  
  # List folder containing data
  data_dir <- subject_dirs[i]
  # Within folder list all datafiles (csv files)
  csv_files <- list.files(path = data_dir, pattern = "^ART.*\\.csv$", full.names = TRUE)
  runs <- length(csv_files)
 
  # Loop through each csv file (loop trough all runs)
  for (j in 1:length(csv_files)){
    # Read the CSV file
    data <- read.csv(csv_files[j])
    
    if (j<=(runs/2)){
      # Append the dataframe to the list
      first_half <- append(first_half, list(data))
    }else{
      second_half <- append(second_half, list(data))
    }
    
  }
  
  # Combine all dataframes into one single dataframe
  raw_data1 <- do.call(rbind, first_half)
  raw_data2 <- do.call(rbind, second_half)
  # Remove jittered trials (that are missing values in cat1 or cat2)
  last_col <- ncol(raw_data1)
  raw_data1 <- raw_data1[raw_data1[, last_col]!= "" & raw_data1[, (last_col-1)]!="", ]
  raw_data2 <- raw_data2[raw_data2[, last_col]!= "" & raw_data2[, (last_col-1)]!="", ]
  
  # Store halves in participant-specific list
  new_entries <- list(first_half=raw_data1, second_half=raw_data2)
  behavioral_data <- c(behavioral_data, new_entries)
  # Store in list for all participants
  new_subject <- paste("subject", i, sep="_") # Numbering will be different than actual subject numbering
  datasets[[new_subject]] <- behavioral_data
  
  # True ranks data
  # Read datafile
  ranking_file <- file.path(data_dir, "preprocessed_final_rating.xlsx")
  ranking_data <- read_excel(ranking_file)
  # Extract ranking vector
  true_ranks_vec <- ranking_data$MRI_code 
  
  # Drop all columns but MRI_code and rank, sort by MRI_code, rename columns to ID and Ranking, and convert to data.frame
  ranking_data <- ranking_data %>%
    select(MRI_code, rank) %>%
    arrange(MRI_code) %>%
    rename(ID=MRI_code, Ranking=rank) %>%
    as.data.frame()
  
  ranking_entry <- list(data=ranking_data, ranking_vector=true_ranks_vec)
  true_ranks_subs[[new_subject]] <- append(true_ranks_subs[[new_subject]], ranking_entry)
  
  # Get pre-mri ratings
  rating_file <- file.path(data_dir, "personal_rating_5_variables.xlsx")
  rating_data <- read_excel(rating_file) %>%
    select(Label, liking_scores) %>%
    rename(MRI_code=Label)
  
  ratings[[new_subject]] <- rating_data
  
  # Get unique pairs
  # first combine halves
  all_runs <- rbind(behavioral_data$first_half, behavioral_data$second_half)
  unique_trials <- unique(t(apply(all_runs[c("stim1","stim2")], 1, sort)))
  unique_pairs[i, 1] <- nrow(unique_trials)
}
# Store datasets
save(datasets, file="varstore/datasets.RData")

# Get median value of number of unique pairs
unique_pairs_med <- median(unique_pairs[,1])
# Calculate theoretical maximum 
max_unique_pairs <- 36 * 35 / 2
# Percentage
percentage_assessed <- round(unique_pairs_med/max_unique_pairs * 100, 2)