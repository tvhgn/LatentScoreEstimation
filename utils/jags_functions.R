library(dplyr)

# Function for creating comparison matrices
# iterate over subject
create_matrices <- function(data){
  
  # Generate comparison matrix
  M_pref <- matrix(0, 36, 36) # Matrix with specific response if given
  M_n <- matrix(0, 36, 36) # Matrix that contains any response other than 0

  for (trial in 1:nrow(data)){
    stim1 <- data[trial, 'stim1']
    stim2 <- data[trial, 'stim2']
    preferred <- data[trial, 'preferredStim']
    # If response was given add to M_n (trial occurred for specific combination and reversed)
    if (preferred!=0){
      M_n[stim2, stim1] <- M_n[stim2, stim1] + 1
      M_n[stim1, stim2] <- M_n[stim1, stim2] + 1
    }

    if (preferred==1){
      M_pref[stim1, stim2] <- M_pref[stim1, stim2] + 1
    } else if (preferred==2){
      M_pref[stim2, stim1] <- M_pref[stim2, stim1] + 1
    }
  }
  return (
    list(
      pref_mat = M_pref, 
      all_mat = M_n)
    )
}

k_fold_dataloader <- function(input_data, test_index, k){
  n_trials <- nrow(input_data)
  n_fold <- n_trials %/% k # k-fold cross validation
  
  # split the data into train, and test sets based on test_index
  start_fold <- test_index*n_fold - (n_fold-1)
  split_indices <- rep(TRUE, n_trials)
  split_indices[start_fold:(start_fold+(n_fold-1))] <- FALSE
  # Create training and testing sets
  return(list(
    train_data= input_data[split_indices, ],
    test_data = input_data[!split_indices, ]
  ))
}

generate_initial_values <- function(nChains) {
  # Function to generate initial values for a single chain
  generate_chain_values <- function() {
    # Initial values for mu
    mu <- rbeta(36, 1, 1)  # random values from a beta(1,1) distribution
    mu[1] <- NA
    # Initial value for kappa
    kappa <- runif(1, 0.001, 100)  # random value between 0.001 and 100
    # Assemble initial values list for JAGS
    initial_values <- list(
      mu = mu,
      kappa = kappa
    )
    return(initial_values)
  }
  
  # Generate initial values for each chain
  initial_values_list <- lapply(1:nChains, function(x) generate_chain_values())
  return(initial_values_list)
}

predict_response <- function(input_data, liking_data){
  output_data <- input_data %>%
    filter(preferredStim!=0) %>%
    mutate(liking1 = liking_data[stim1],
           liking2 = liking_data[stim2]) %>%
    mutate(pred = ifelse(liking1 > liking2, 1, 2)) %>%
    mutate(pred = factor(pred),
           preferredStim = factor(preferredStim))
  
  return (output_data)
}
