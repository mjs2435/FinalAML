# Load the necessary library
library(data.table)

# Read the CSV file
data <- fread("application_data.csv")

# Convert data to a data table
setDT(data)

# Select 300 entries for each category of TARGET
balanced_data <- data[, .SD[sample(.N, min(.N, 300))], by = TARGET]

ordered_balanced_data <- balanced_data[order(SK_ID_CURR)]
# Save this balanced dataset to a new CSV file
fwrite(ordered_balanced_data, "subset_application_data.csv", row.names = FALSE)
