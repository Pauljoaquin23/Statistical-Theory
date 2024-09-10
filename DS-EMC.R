# Load necessary library
library(dplyr)

# Define the population
population <- c('Marcos', 'Marcos', 'Marcos', 'Marcos', 'Robredo', 'Robredo')

# Parameters
sample_size <- 4
num_samples <- 1000  # Number of samples to generate

# Function to compute proportions
compute_proportions <- function(sample) {
  total <- length(sample)
  robredo_count <- sum(sample == 'Robredo')
  marcos_count <- sum(sample == 'Marcos')
  prop_robredo <- robredo_count / total
  prop_marcos <- marcos_count / total
  return(c(prop_robredo, prop_marcos))
}

# Initialize lists to store proportions
prop_robredo_list <- numeric(num_samples)
prop_marcos_list <- numeric(num_samples)

# Generate samples and compute proportions
set.seed(123)  # For reproducibility
for (i in 1:num_samples) {
  sample <- sample(population, sample_size, replace = FALSE)
  proportions <- compute_proportions(sample)
  prop_robredo_list[i] <- proportions[1]
  prop_marcos_list[i] <- proportions[2]
}

# Convert lists to data frame for easier statistical calculations
proportions_df <- data.frame(
  PropRobredo = prop_robredo_list,
  PropMarcos = prop_marcos_list
)

# Calculate and display averages
avg_prop_robredo <- mean(proportions_df$PropRobredo)
avg_prop_marcos <- mean(proportions_df$PropMarcos)

cat(sprintf("Average proportion of 'Robredo': %.2f\n", avg_prop_robredo))
cat(sprintf("Average proportion of 'Marcos': %.2f\n", avg_prop_marcos))

# Display the first few samples and their proportions for verification
cat("Sample proportions (first few samples):\n")
for (i in 1:min(5, num_samples)) {
  cat(sprintf("Sample %d:\n", i))
  cat(sprintf("  Proportion of 'Robredo': %.2f\n", prop_robredo_list[i]))
  cat(sprintf("  Proportion of 'Marcos': %.2f\n", prop_marcos_list[i]))
}