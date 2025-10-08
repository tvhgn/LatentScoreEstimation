# Due to readline() commands, it is necessary to run below scripts one by one, instead of all at once.

# Prepare behavioral datasets for JAGS
source("prepare_datasets.R")

# Run JAGS model
source("run_jags_model.R")

# k-Fold validation
source("k_fold_validation.R")

# Run analysis
source("model_analysis.R")

