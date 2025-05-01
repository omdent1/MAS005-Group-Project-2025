# This R script reads Young's Modulus (YM) values from CSV files,
# generates a boxplot with color and legend, exports it as a PNG,
# and prints the five-number summary to the console.

# --- Define File Paths ---
output_dir <- "exports"
file_mat1 <- file.path(output_dir, "mat1_ym.csv")
file_mat2 <- file.path(output_dir, "mat2_ym.csv")
output_plot_file <- file.path(output_dir, "boxplot_ym.png")

# --- Function to Read YM Data ---
read_ym_data <- function(file_path) {
  if (file.exists(file_path)) {
    ym_df <- read.csv(file_path, header = TRUE)
    return(ym_df[[1]]) # Safely extract the first column
  } else {
    cat(paste("Error: File not found:", file_path, "\n"))
    return(NULL)
  }
}

# --- Read Data ---
YM_material1_GPa <- read_ym_data(file_mat1)
YM_material2_GPa <- read_ym_data(file_mat2)

# --- Perform Analysis if Data is Available ---
if (!is.null(YM_material1_GPa) && !is.null(YM_material2_GPa)) {
  cat("\n--- Analysis of Young's Modulus Data ---\n")

  # Five-Number Summary
  cat("\nFive-Number Summary for Material 1:\n")
  print(fivenum(YM_material1_GPa))

  cat("\nFive-Number Summary for Material 2:\n")
  print(fivenum(YM_material2_GPa))

  # --- Export Boxplot as PNG Image ---
  png(filename = output_plot_file, width = 800, height = 600) # Open PNG device

  # Boxplot with Color and Legend
  boxplot(YM_material1_GPa, YM_material2_GPa,
          names = c("Material 1", "Material 2"),
          ylab = "Young's Modulus (GPa)",
          main = "Comparison of Young's Modulus",
          col = c("lightblue", "lightgreen"),
          yaxt = 's',
          axes = TRUE)

  # Add Legend
  legend("bottomright",
         legend = c("Material 1", "Material 2"),
         fill = c("lightblue", "lightgreen"),
         title = "Material")

  dev.off() # Close PNG device

  cat(paste("\nAnalysis complete. Boxplot saved as:", output_plot_file, "\n"))

} else {
  cat("\nError: Could not perform analysis. Please ensure both CSV files are present in the 'exports' directory.\n")
}
