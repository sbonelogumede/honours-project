# Load the dataset.
load(file = "../objects/extracted_data_storage.RData")

# Find the rows with complete observations.
complete_rows <- apply(X = F_mat[, 2:5], MARGIN = 1, FUN = function(row) all(!is.na(row)))

# Find the runs of TRUEs.
rle_rows <- rle(x = complete_rows)

# Find the longest run.
longest_run_index <- which.max(x = rle_rows$lengths * rle_rows$values)
longest_run <- rle_rows$lengths[longest_run_index]

# Find the starting row of that run.
end_row <- cumsum(x = rle_rows$lengths)[longest_run_index]
start_row <- end_row - longest_run + 1

# Extract the dataset with the longest sub-sequence of no missing values.
X_mat <- F_mat[start_row:end_row, ]

# Save the extracted dataset.
save(X_mat, file = "../objects/subset_data_storage.RData")
