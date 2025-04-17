# This R script separates 'Force' and 'Stroke' data for material 1 and 2 from sample files
# in the 'data' directory and saves them to 'data/separated/material1' and 'data/separated/material2'.

# --- Set up Directories ---
data_directory <- "./data/"
material1_directory <- "./data/separated/material1/"
material2_directory <- "./data/separated/material2/"

# Create directories if they don't exist
create_dir_if_not_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat(paste("Created directory:", dir_path, "\n"))
  } else {
    cat(paste("Directory already exists:", dir_path, "\n"))
  }
}

create_dir_if_not_exists(material1_directory)
create_dir_if_not_exists(material2_directory)

# --- Get List of Sample Files ---
sample_files <- list.files(path = data_directory, pattern = "^Sample\\d+\\.csv$", full.names = TRUE)

if (length(sample_files) == 0) {
  stop(paste("No sample files found in:", data_directory, "\nPlease ensure CSV files are named like 'Sample1.csv'."))
}

cat(paste("Found", length(sample_files), "sample files.\n"))

# --- Process Each Sample File ---
for (file_path in sample_files) {
  file_name_base <- gsub(".csv", "", basename(file_path))
  cat(paste("Processing:", basename(file_path), "\n"))

  data <- read.csv(file_path)

  # --- Separate and Save Material 1 Data ---
  material1_data <- data[, c("Force_1", "Stroke_1")]
  output_file_material1 <- file.path(material1_directory, paste0(file_name_base, "_material1.csv"))
  write.csv(material1_data, file = output_file_material1, row.names = FALSE)
  cat(paste("  Saved Material 1 data to:", output_file_material1, "\n"))

  # --- Separate and Save Material 2 Data ---
  material2_data <- data[, c("Force_2", "Stroke_2")]
  output_file_material2 <- file.path(material2_directory, paste0(file_name_base, "_material2.csv"))
  write.csv(material2_data, file = output_file_material2, row.names = FALSE)
  cat(paste("  Saved Material 2 data to:", output_file_material2, "\n"))
}

cat("\nSuccessfully separated Force and Stroke data from all sample files.\n")