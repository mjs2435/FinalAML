# Load the necessary library
library(data.table)
# Read the CSV file
data <- read.csv("application_data.csv")

# Select the first 5000 entries
subset_data <- data[1:100, ]

# Save this subset to a new CSV file
write.csv(subset_data, "subset_application_data.csv", row.names = FALSE)

