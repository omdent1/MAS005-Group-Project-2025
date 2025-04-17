# This R script plots Force vs. Stroke data for all samples of each material on separate graphs,
# saving them as images in the 'exports/init_comparisons' folder.
# Samples for each material are distinguished by color with a legend.

# --- Define Graph Titles ---
material1_plot_title <- "Force vs. Stroke for All Samples of Material 1"
material2_plot_title <- "Force vs. Stroke for All Samples of Material 2"

# --- Set up Directories ---
data_directory <- "./data/"
material1_directory <- file.path(data_directory, "separated/material1/")
material2_directory <- file.path(data_directory, "separated/material2/")
exports_directory <- "./exports/init_comparisons/"

# --- Clear the Exports Directory ---
if (dir.exists(exports_directory)) {
  unlink(list.files(path = exports_directory, full.names = TRUE), recursive = FALSE, force = FALSE)
  cat(paste("Cleared directory:", exports_directory, "\n"))
} else {
  dir.create(exports_directory, recursive = TRUE)
  cat(paste("Created directory:", exports_directory, "\n"))
}

# --- Function to Process and Plot Data for a Material ---
process_and_plot_material <- function(material_directory, material_name, plot_title, output_filename) {
  material_files <- list.files(path = material_directory, pattern = "*.csv", full.names = TRUE)

  if (length(material_files) == 0) {
    warning(paste("No", material_name, "files found in:", material_directory))
    return()
  }

  cat(paste("Found", length(material_files), material_name, "files to plot.\n"))

  all_data <- data.frame()
  sample_numbers <- numeric()
  material_num <- gsub("material", "", material_name)

  for (file_path in material_files) {
    file_name <- basename(file_path)
    sample_number <- as.numeric(gsub("^Sample(\\d+)_material.+?\\.csv$", "\\1", file_name))
    sample_numbers <- c(sample_numbers, sample_number)

    data <- read.csv(file_path)
    force_col <- paste0("Force_", material_num)
    stroke_col <- paste0("Stroke_", material_num)

    if (all(c(force_col, stroke_col) %in% colnames(data))) {
      data$Sample <- sample_number
      names(data)[names(data) == stroke_col] <- "Stroke"
      names(data)[names(data) == force_col] <- "Force"
      all_data <- rbind(all_data, data[, c("Stroke", "Force", "Sample")])
    } else {
      warning(paste("Columns not found in", file_name))
    }
  }

  output_path <- file.path(exports_directory, output_filename)
  png(filename = output_path, width = 1200, height = 900, units = "px", res = 200) # Reduced image size for simplicity

  plot(all_data$Stroke, all_data$Force,
       xlab = "Stroke/mm",
       ylab = "Force/N",
       main = plot_title,
       type = "n")

  unique_samples <- sort(unique(all_data$Sample))
  num_samples <- length(unique_samples)
  colors <- rainbow(num_samples) # Using rainbow for a range of colors

  for (i in seq_along(unique_samples)) {
    sample_data <- subset(all_data, Sample == unique_samples[i])
    points(sample_data$Stroke, sample_data$Force, col = colors[i], pch = 1, cex = 0.7)
  }

  legend("bottomright", legend = paste("Sample", unique_samples), col = colors, pch = 1, cex = 0.7)

  dev.off()
  cat(paste("\nSaved combined scatter plot for", material_name, "to:", output_path, "\n"))
}

# --- Process and Plot Data ---
process_and_plot_material(material1_directory, "material1", material1_plot_title, "material1_all_samples.png")
process_and_plot_material(material2_directory, "material2", material2_plot_title, "material2_all_samples.png")

cat("\nFinished processing and saving combined scatter plots to exports/init_comparisons.\n")