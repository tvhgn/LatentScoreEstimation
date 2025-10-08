#' Analyze and Visualize Liking Task Model Validity and Prediction
#'
#' This script processes and evaluates the validity of latent ranking models applied to multi-stage liking task data.
#' It conducts subject-wise extraction of MCMC samples, computes the correlation between model estimates and true rankings,
#' summarizes validity statistics, and exports subject-level summary Excel files.
#'
#' After organizing and transforming key dataframes, the script calculates pairwise Spearman correlations among rating, latent, 
#' and ranking data. It filters outliers as needed, reshapes the dataset for visualization, and generates violin and density plots 
#' to illustrate score distributions and concurrent validity.
#'
#' The script also runs K-fold cross-validation to estimate prediction accuracy and visualizes these results with histograms.
#' Output includes summary dataframes, Excel files per subject, and publication-quality plots.
#'
#' Required libraries: papaja, writexl, ggplot2, bayestestR, scales, ggpubr, effectsize, stringr, tidyr
#'
#' @author [Tom van Hogen]
#' @date [2025-10-08]
#'
#' @details
#' - Initializes variables and creates folder structure for saving results
#' - Iterates over all subjects, computing and storing subject-specific summaries and correlations
#' - Exports per-subject rating and model output to Excel files
#' - Aggregates and visualizes correlations and score distributions (violin and density plots)
#' - Filters known outlier subjects from main correlation analyses
#' - Loads cross-validation accuracy results, computes mean accuracy, significance, and visualizes results
#'
#' Directory structure and save paths for results are automatically handled within the script.


# Necessary imports
library(papaja)
library(writexl)
library(ggplot2)
library(bayestestR)
library(scales)
library(ggpubr)
library(effectsize)
library(stringr)
library(tidyr)

# initialize variables
subs_validity <- list()
#subs_validity_test <- list()
n_samples <- output_subs$subject_1$sample * nChains
cor_samples_df <- data.frame("S1"=rep(NA, n_samples)) # contains correlations across model samples
liking_stages_df <- matrix(NA, 
                           nrow = n_subjects*36,
                           ncol = 5) # Contains liking scores from all three stages

# Create folder for data and figure storage
ifelse(!dir.exists(file.path("analysis")),
       dir.create(file.path("analysis")),
       "Directory Exists")

ifelse(!dir.exists(file.path("analysis", "figures")),
       dir.create(file.path("analysis", "figures")),
       "Directory Exists")

# Loop across subjects
for (i in 1:n_subjects){
  # string object for picking subject
  subject_string <- paste("subject", i, sep="_")
  # Extract MCMC samples for latent ranks
  liking_samples <- combine.mcmc(output_subs[[subject_string]])
  # Calculate correlation for each MCMC sample with the ranking task data
  cor_samples <- apply(liking_samples, 1, 
                       function(x) cor(rank(-x),
                                       true_ranks_subs[[subject_string]]$data$Ranking, 
                                       method='spearman')) # Reverse rank (higher liking scores correspond to higher ranks. In other words, they are closer to rank 1)
  # start and end indices depends on subject number
  i_start <- (i-1)*36 + 1
  i_end <- i_start + 35
  # Enter subject number and painting ID
  liking_stages_df[i_start:i_end, 1] <- paste('S',i, sep="")
  liking_stages_df[i_start:i_end, 2] <- 1:36
  # Extract and store ratings
  subject_rating <- ratings[[subject_string]]$liking_scores
  liking_stages_df[i_start:i_end, 3] <- subject_rating
  # Extract latent liking scores and store
  subject_latent <- as.numeric(output_subs[[subject_string]]$summary$statistics[,'Mean'])
  liking_stages_df[i_start:i_end, 4] <- subject_latent
  # Extract rankings and store
  subject_ranking <- true_ranks_subs[[subject_string]]$data$Ranking
  liking_stages_df[i_start:i_end, 5] <- subject_ranking
  
  # Create csv file with data on subject
  df_subject <- data.frame(
    label = 1:36,
    rating = subject_rating,
    latent_liking = subject_latent,
    ranking = subject_ranking
  )
  save_to_dir <- str_extract(subject_dirs[i],".{7}$")
  save_path <- file.path("analysis", save_to_dir, "liking_three_stages.xlsx")
  
  # Create directory if necessary
  ifelse(!dir.exists(file.path("analysis", save_to_dir)),
         dir.create(file.path("analysis", save_to_dir)),
         "Directory Exists")
  # Write csv file
  write_xlsx(df_subject, path=save_path)
  
  # Store in dataframe
  cor_samples_df[paste('S',i, sep="")] <- cor_samples
  
  # Summarize the posterior distribution of the correlations
  # First get median values
  median_r <- median(cor_samples)
  # Get highest density interval
  HDI <- hdi(cor_samples)
  # store into list
  HDI_data <- list(median=median_r,
                   HDI.L=as.numeric(HDI)[2],
                   HDI.H=as.numeric(HDI)[3])
  
  
  # Store data to list variable
  subs_validity[[subject_string]] <- HDI_data
}

# Transform liking scores matrix to dataframe
liking_stages_df <- as.data.frame(liking_stages_df)
colnames(liking_stages_df) <- c("Subject", "PaintID", "Rating_Score", "Latent_Score", "Ranking_Score")
liking_stages_df <- liking_stages_df %>%
  mutate(Subject = factor(Subject),
         PaintID = factor(PaintID),
         Rating_Score = as.numeric(Rating_Score),
         Latent_Score = as.numeric(Latent_Score),
         Ranking_Score = as.numeric(Ranking_Score))


# Correlations across stages
df_ranked <- liking_stages_df %>%
  filter(Subject != "S42" & Subject != "S33") %>% # filter outliers
  group_by(Subject) %>%
  mutate(Rating_Score=rank(-Rating_Score),
         Latent_Score=rank(-Latent_Score))

cor_rat_ranked <- cor.test(df_ranked$Rating_Score, df_ranked$Ranking_Score,
                           method = 'spearman')
cor_rat_latent <- cor.test(df_ranked$Rating_Score, df_ranked$Latent_Score,
                           method = 'spearman')
cor_lat_ranked <- cor.test(df_ranked$Latent_Score, df_ranked$Ranking_Score,
                           method = 'spearman')


# Convert to long format
liking_stages_long <- liking_stages_df %>%
  pivot_longer(cols=3:5, names_to = "liking_stage", values_to = "score") %>%
  mutate(
    liking_stage = case_match(liking_stage,
                              "Rating_Score"~"Rating",
                              "Latent_Score"~"Latent",
                              "Ranking_Score"~"Ranking")
  ) %>%
  mutate(liking_stage = factor(liking_stage,
                               levels=c("Rating", "Latent", "Ranking")))



# violin plot
liking_stages_long %>% filter(liking_stage!="Ranking") %>%
  ggplot(aes(x=liking_stage, y = score, fill=liking_stage)) +
  geom_violin() +
  geom_jitter(width=0.05, height=0.2, alpha=0.1) +
  #scale_x_discrete(limits = c("Rating", "Latent", "Ranking")) +
  #geom_boxplot() +
  theme_classic()

# Create long format of dataframe
cor_samples_long <- cor_samples_df %>%
  pivot_longer(cols=starts_with("S"),
               names_to="Subject",
               values_to="Sample_r") %>%
  mutate(Subject= factor(Subject))

mean_corr <- mean(cor_samples_long$Sample_r)
# Get the average correlation
cor_samples_filtered <- 
  cor_samples_long %>% filter(Subject!= "S33" & Subject!= "S42")

mean(cor_samples_filtered$Sample_r)

# Create plot using ggplot
conc_val_plot <- ggplot(cor_samples_long, aes(x=Sample_r, colour=Subject, fill=Subject)) +
  geom_density(alpha = 0.4, color=NA) +
  #geom_vline(aes(xintercept = mean_corr), color='red', linewidth = 1, linetype = 'dashed') +
  xlab("Correlation with Final Ranking") +
  ylab("Density") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    axis.title = element_text(size = 16),     # Change axis title size
    axis.text = element_text(size = 14)) 

ggsave(filename = "concurrent_validity.png", path = "figures", units = "px", dpi = 300)
# Summary df
cor_summary <- cor_samples_long %>%
  group_by(Subject) %>%
  summarise(mean = mean(Sample_r),
            sd = sd(Sample_r))

# Inspect outliers
liking_stages_outliers <- df_ranked %>%
  filter(Subject == "S33" | Subject == "S42") %>%
  group_by(Subject) %>%
  summarize(cor_rat_lat = cor(Rating_Score, Latent_Score),
            cor_lat_rank = cor(Latent_Score, Ranking_Score),
            cor_rat_rank = cor(Rating_Score, Ranking_Score))

#### K-fold validation ####
#load("varstore/k_fold_val_df.RData")

# Test mean accuracy for significance
# Get the mean
mean_acc <- mean(df_acc$Accuracy)
# check for significance
t.test(df_acc$Accuracy, mu=0.5)
# Get effect size
cohens_d(df_acc, Accuracy~1, mu=0.5)

# Plot histogram of accuracies 
k_fold_plot <- df_acc %>% ggplot(aes(x=Accuracy)) +
  geom_histogram(binwidth = 0.025, color='black', fill='cyan') +
  #geom_density(aes(y=after_stat(density) * nrow(df_acc) * 0.025), color='red') +
  geom_vline(aes(xintercept = mean_acc), color='red', linetype = 'dashed', linewidth = 1) +
  theme_classic() +
  ylab("Number of Observations") +
  xlab("Prediction Accuracy") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    axis.title = element_text(size = 16),     # Change axis title size
    axis.text = element_text(size = 14)) 

# Store figures
ggsave(filename = "BT_model_pred_accuracy.png", path="figures", units="px", dpi=300)
fig_arranged <- ggarrange(conc_val_plot, k_fold_plot,
          labels=c("A", "B"), font.label= list(size=16))
ggsave(plot = fig_arranged, filename = "BT_model_arranged_accuracies.png", path="figures", units="px", dpi=300)