# This R script calculates Young's Modulus (YM) for material 1 and material 2 samples.
# It uses gradient values and material dimensions.
# The results are saved to two separate CSV files in the 'exports' directory.

# --- Define Variables ---

# Gradient values (N/mm) for material 1 samples
material1_gradients <- c(1373.8, 886.6, 912.07, 1822.02, 1622.2,
                         1097.02, 901.94, 1206.17, 1840.22, 854.55)

# Gradient values (N/mm) for material 2 samples
material2_gradients <- c(3182.97, 3355.54, 3033.79, 3549.48, 1240.05,
                         3122.67, 2955.21, 3297.18, 3530.97, 3149.15)

# Material properties (mm)
diameter <- 3.40
length_initial <- 68.4

# --- Calculate Cross-sectional Area ---
radius <- diameter / 2
area_mm2 <- pi * (radius^2)

# --- Function to Calculate Young's Modulus ---
calculate_ym <- function(gradients, length, area) {
  YM_GPa <- numeric(length(gradients))
  for (i in seq_along(gradients)) {
    YM_mm2 <- (gradients[i] * length) / area
    YM_GPa[i] <- YM_mm2 / 1000
  }
  return(YM_GPa)
}

# --- Calculate Young's Modulus (YM) for each material ---
YM_material1_GPa <- calculate_ym(material1_gradients, length_initial, area_mm2)
YM_material2_GPa <- calculate_ym(material2_gradients, length_initial, area_mm2)

# --- Output Results to CSV ---

# Function to round to significant figures
round_sig <- function(x, significance) {
  order <- floor(log10(abs(x)))
  return(round(x, significance - 1 - order))
}

# Round the YM values
YM_material1_GPa_rounded <- round_sig(YM_material1_GPa, 3)
YM_material2_GPa_rounded <- round_sig(YM_material2_GPa, 3)

# Create 'exports' directory if needed
output_dir <- "exports"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Define output file paths
output_file_mat1 <- file.path(output_dir, "mat1_ym.csv")
output_file_mat2 <- file.path(output_dir, "mat2_ym.csv")

# Write to CSV files
write.csv(data.frame(Youngs_Modulus_GPa = YM_material1_GPa_rounded),
          file = output_file_mat1, row.names = FALSE, col.names = FALSE)
write.csv(data.frame(Youngs_Modulus_GPa = YM_material2_GPa_rounded),
          file = output_file_mat2, row.names = FALSE, col.names = FALSE)

cat(paste("Calculated Young's Modulus values for Material 1 saved to:", output_file_mat1, "\n"))
cat(paste("Calculated Young's Modulus values for Material 2 saved to:", output_file_mat2, "\n"))

# Optional: Print the results to the console
cat("\nCalculated Young's Modulus for Material 1 samples (in GPa):\n")
print(YM_material1_GPa_rounded)

cat("\nCalculated Young's Modulus for Material 2 samples (in GPa):\n")
print(YM_material2_GPa_rounded)