#' Perform k-fold cross-validation with JAGS model on multiple subject datasets
#'
#' This script performs k-fold cross-validation to evaluate a Bayesian logistic regression model
#' using JAGS on a collection of subject datasets. It allows loading previous cross-validation results 
#' or re-running the full k-fold procedure on the provided datasets.
#' 
#' The processing includes:
#' - Loading necessary libraries and user-defined JAGS utility functions.
#' - Setting model parameters (number of chains, samples, burn-in, adaptation) and k-fold parameters.
#' - Loading datasets or previous cross-validation results.
#' - For each subject dataset, running k-fold cross-validation by splitting the data 
#'   into training and testing sets.
#' - Creating matrices for JAGS input, initializing chains, running the model in parallel.
#' - Predicting responses and evaluating accuracy with confusion matrices.
#' - Storing and saving accuracy results in a dataframe for later analysis.
#' 
#' @author [Tom van Hogen]
#' @date [2025-10-08]
#' 
#' @details
#' The JAGS model file used is "models/ranking_logregres_bin_reparam.txt". The number of paintings (stimuli) is fixed at 36.
#' Parameters monitored in JAGS include "liking". The model by default uses 3 chains, 10,000 samples, 1,000 burn-in iterations, and 1,000 adaptation steps.
#' Cross-validation uses 10 folds by default.

#' @usage
#' Run the script interactively. When prompted, specify whether to load previous results by typing "Y" or "N".
#' If "N", the script runs the full k-fold validation process and saves the results.
#'
#' @return
#' Saves a dataframe \code{df_acc} containing the accuracy results per subject and fold in "varstore/k_fold_val_df.RData".
#' The dataframe contains columns: Subject (factor), Fold (factor), and Accuracy (numeric).
#' 
#' @note
#' Requires external utility functions defined in "utils/jags_functions.R" for data preparation and model interaction.

library(dplyr)
library(ggplot2)
library(scales)
library(caret)
library(rstatix)
library(runjags)

# Set parameters
# Model parameters
pars <- c("liking")
nChains <- 3
samples <- 10000
burnin <- 1000
nAdapt <- 1000
# k-fold validation parameters
k <- 10 # number of folds

# Load functions
source(file.path("utils","jags_functions.R"))

# Ask if previous results should be loaded
load_request <- readline(prompt = "Do you want to load previous k-fold validation results (Y/N)?: ")
load_request <- tolower(load_request)
load_results <- ifelse(load_request == "y", TRUE, FALSE)

if (load_results == TRUE){
  load("varstore/k_fold_val_df.RData")
} else {
  # Load datasets
  load(file.path("varstore", "datasets.RData"))
  
  # initialize parameters and matrices
  
  acc_mat <- matrix(NA, nrow=length(datasets)*k, ncol = 3)
  
  for (S_i in 1:length(datasets)){
    # Get subject data
    subject_string <- paste("subject", S_i, sep="_")
    pick <- datasets[[subject_string]]
    raw_data <- rbind(pick$first_half, pick$second_half)
    for (fold in 1:k){
      # Create train and test datasets
      data <- k_fold_dataloader(raw_data, fold, k)
      trainset <- data$train_data
      testset <- data$test_data
      # Prepare matrices for JAGS
      mats <- create_matrices(trainset)
      M_pref <- mats$pref_mat
      M_n <- mats$all_mat
      # Initialize chains
      set.seed(1234)
      inits <- generate_initial_values(3)
      # Train JAGS model
      # Prepare the data
      dataList <- list(y = M_pref,
                       n = M_n,
                       nPaintings = 36)
      
      # Print message to console
      cat(paste("Running fold", fold, "for", subject_string, "\n\n"))
      # Run JAGS
      output <- run.jags(model="models/ranking_logregres_bin_reparam.txt",
                         monitor=pars,
                         data=dataList,
                         n.chains = nChains,
                         inits=inits,
                         burnin=burnin,
                         sample=samples,
                         adapt=nAdapt,
                         silent.jags = TRUE,
                         method="rjparallel"
      )
      # Use JAGS model to predict
      # Get liking scores
      latent_liking <- as.numeric(output$summary$statistics[,'Mean'])
      # then predict
      preds_df <- predict_response(testset, latent_liking) # Get predictions
      # Evaluate and store results
      conf_mat <- confusionMatrix(preds_df$pred, preds_df$preferredStim) # Get accuracy and such
      mat_index_start <- S_i * k - (k-1)
      mat_index_current <- mat_index_start + (fold-1)
      acc_mat[mat_index_current,] <- c(subject_string,
                                       fold,
                                       as.numeric(conf_mat$overall["Accuracy"]))
      
    }
  }
  
  # Convert to dataframe and save
  df_acc <- data.frame(acc_mat)
  colnames(df_acc) <- c("Subject", "Fold", "Accuracy")
  # Factorize variables
  df_acc <- df_acc %>%
    mutate(Subject = factor(Subject),
           Fold = factor(Fold),
           Accuracy = as.numeric(Accuracy))
  
  
  # Store results
  save(df_acc, file=file.path("varstore", "k_fold_val_df.RData"))
}

