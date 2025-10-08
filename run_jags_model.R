#' Fit and Manage Hierarchical Bradley-Terry Based Model in JAGS for Ranking Data
#'
#' This script sets up, fits, and stores a hierarchical Bradley-Terry type model for estimating latent "liking" scores for 36 paintings
#' using paired comparison data in R via JAGS. It allows interactive loading of previous model outputs 
#' or full model estimation across all available subjects. Participant-level comparison matrices are built from trial-wise behavioral data,
#' and Bayesian estimation is performed using custom-initialized chains and parallel computation.
#'
#' Workflow summary:
#' - Imports R2jags, runjags, and bayesplot libraries for Bayesian inference and visualization
#' - Sets model sampling, adaptation, and burn-in parameters, plus parameters to monitor
#' - Asks user whether to load saved JAGS results or re-run model estimation
#' - Initializes functions for random starting values for MCMC chains and creates necessary model storage folders
#' - If running JAGS :
#'     - Assembles subject × stimulus comparison matrices from raw behavioral data
#'     - Writes JAGS model file (logistic regression with reparameterization and hierarchical priors)
#'     - Runs the Bayesian rank estimation model in parallel for each subject, summarizing and saving all outputs
#' - Model results and subjectwise JAGS output objects are saved in the "varstore" folder for later analysis
#'
#' Dependencies: R2jags, runjags, bayesplot
#'
#' @author [Tom van Hogen]
#' @date [2025-10-08]
#'
#' @details
#' - Customizable parameters for number of samples, burn-in, adaptation steps, number of chains, and model priors
#' - Behavioral data matrices assume structure from preceding preprocessing scripts (see datasets and true_ranks_subs variables)
#' - User-interactive load option prevents unnecessary recomputation
#' - Model uses uninformative priors with reparameterization for identifiability and confidence estimation
#' - Parallel execution recommended for computational efficiency


# Necessary imports
library(R2jags)
library(runjags)
library(bayesplot)

# Load external functions
source(file.path("utils", "jags_functions.R"))

# Set parameters
pars <- c("liking") # parameters to be tracked
samples <- 10000
burnin <- 1000
nAdapt <- 1000

# Ask if previous JAGS results should be loaded
load_request <- readline(prompt = "Do you want to load previous JAGS results (Y/N)?: ")
load_request <- tolower(load_request)
load_jags <- ifelse(load_request == "y", TRUE, FALSE)

# Create folder containing JAGS model if not yet existent
ifelse(!dir.exists(file.path("models")),
       dir.create(file.path("models")),
       "Directory Exists")


# # Create function for generating initial values for nChains number of chains
# generate_initial_values <- function(nChains) {
#   # Function to generate initial values for a single chain
#   generate_chain_values <- function() {
#     
#     # Initial values for mu
#     mu <- rbeta(36, 1, 1)  # random values from a beta(1,1) distribution
#     mu[1] <- NA
#     
#     # Initial value for kappa
#     kappa <- runif(1, 0.001, 100)  # random value between 0.001 and 100
#     
#     # Assemble initial values list for JAGS
#     initial_values <- list(
#       mu = mu,
#       kappa = kappa
#     )
#     
#     return(initial_values)
#   }
#   
#   # Generate initial values for each chain
#   initial_values_list <- lapply(1:nChains, function(x) generate_chain_values())
#   
#   return(initial_values_list)
# }



# Load results if already executed
if (load_jags == TRUE){
  load(file.path("varstore", "jags_output_all_subs.RData"))
# Otherwise, run model
} else{
  # string variable containing JAGS script
  modelString <- "
       model{
        sf <- 10 # scale factor
        
        for(i in 1:nPaintings){
          for(j in 1:nPaintings){
            # Likelihood
            y[i,j] ~ dbin(theta[i,j], n[i,j])
            logit(theta[i,j]) <- liking[i] - liking[j]
          }
        }
        # priors
        # Uninformative priors for all paintings except the first.
        for (k in 2:nPaintings){
          liking[k] <- liking_[k] * sf # scaled score
          liking_[k] ~ dbeta(a[k], b[k])
          a[k] <- ifelse((mu[k] * kappa) < 1, 1, mu[k] * kappa)
          b[k] <- ifelse((kappa - a[k]) < 1, 1, kappa - a[k])
          
          # Priors over reparameterizations
          mu[k] ~ dbeta(1,1)
        }
        liking[1] <- sf/2 # fix one value in middle of scale to make model identifiable
        # Hyperprior over kappa
        kappa ~ dunif(0.001, 100) # We assume similar confidence for all comparisons
      }"
  
  # Write model to local textfile that JAGS model can load
  writeLines(modelString, con = "models/ranking_logregres_bin_reparam.txt")
  
  ## Initialization
  # Set seed
  set.seed(1234)
  
  # Generate initial values list for three chains
  inits <- generate_initial_values(3)
  
  ## Run JAGS model
  # Initialize list variable which will contain outputs across subjects
  output_subs <- list()

  # iterate over subject
  for (sub in 1:n_subjects){
    # Pick subject and create string object
    subject <- paste("subject", sub, sep="_")
    # # Generate comparison matrix
    # M_pref <- matrix(0, 36, 36) # Matrix with specific response if given
    # M_n <- matrix(0, 36, 36) # Matrix that contains any response other than 0
  
    # Combine halves to estimate using full dataset
    raw_data <- rbind(datasets[[subject]]$first_half, datasets[[subject]]$second_half)
    # Extract true ranking data
    true_ranks_vec <- true_ranks_subs[[subject]]$ranking_vector
    true_ranks_data <- true_ranks_subs[[subject]]$data
    
    # Prepare data matrices for JAGS model
    mats <- create_matrices(raw_data)
    M_pref <- mats$pref_mat
    M_n <- mats$all_mat
    
    # # Prepare data matrix that can be used by the JAGS model
    # for (trial in 1:nrow(raw_data)){
    #   stim1 <- raw_data[trial, 'stim1']
    #   stim2 <- raw_data[trial, 'stim2']
    #   preferred <- raw_data[trial, 'preferredStim']
    #   # If response was given add to M_n (trial occurred for specific combination and reversed)
    #   if (preferred!=0){
    #     M_n[stim2, stim1] <- M_n[stim2, stim1] + 1
    #     M_n[stim1, stim2] <- M_n[stim1, stim2] + 1
    #   }
    # 
    #   if (preferred==1){
    #     M_pref[stim1, stim2] <- M_pref[stim1, stim2] + 1
    #   } else if (preferred==2){
    #     M_pref[stim2, stim1] <- M_pref[stim2, stim1] + 1
    #   }
    # }

    #### Run JAGS Model ####
    # set seed
    set.seed(1234)

    # Prepare the data
    dataList <- list(y = M_pref,
                     n = M_n,
                     nPaintings = 36)

    

    output <- run.jags(model="models/ranking_logregres_bin_reparam.txt",
                       monitor=pars,
                       data=dataList,
                       n.chains = nChains,
                       inits=inits,
                       burnin=burnin,
                       sample=samples,
                       adapt=nAdapt,
                       method="rjparallel"
    )

    # Store outputs
    output_subs[[subject]] <- output
    # Summary
    print(output)
  }

  # Save the data to local variable
  save(output_subs, file=file.path("varstore", "jags_output_all_subs.RData"))
}


