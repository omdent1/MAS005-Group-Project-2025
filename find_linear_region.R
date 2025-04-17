# This R script plots Force vs. Stroke data with a line of best fit for selected samples.
# Equations and R² values are displayed next to the graph.
# The plot is saved as a high-resolution PNG file.

# --- Adjustable Variables ---
material_to_plot <- "material2"
samples_to_plot <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
min_force <- 10
max_force <- 40
graph_title <- "Force vs Stroke with Linear Regions for Material 2 (10N to 40N)"
output_filename_base <- "m2"

# --- Directory Path ---
data_directory <- "./data/separated/"
material_directory <- file.path(data_directory, material_to_plot)

# --- Specify Files to Plot ---
material_files <- file.path(material_directory, paste0("Sample", samples_to_plot, "_", material_to_plot, ".csv"))
if (!all(file.exists(material_files))) {
  stop(paste("Error: One or more sample files not found:", paste(material_files[!file.exists(material_files)], collapse = ", ")))
}

cat(paste("Plotting data for", gsub("material", "Material ", material_to_plot), "Samples:", paste(samples_to_plot, collapse = ", "), "\n"))

# --- Read, Filter, and Model Data ---
process_sample <- function(file_path, material_num) {
  data <- read.csv(file_path)
  force_col <- paste0("Force_", material_num)
  stroke_col <- paste0("Stroke_", material_num)

  if (all(c(force_col, stroke_col) %in% colnames(data))) {
    filtered_data <- subset(data, get(force_col) >= min_force & get(force_col) <= max_force)
    colnames(filtered_data)[colnames(filtered_data) == stroke_col] <- "Stroke"
    colnames(filtered_data)[colnames(filtered_data) == force_col] <- "Force"
    if (nrow(filtered_data) > 0) {
      model <- lm(Force ~ Stroke, data = filtered_data)
      return(list(data = filtered_data, model = model))
    } else {
      warning(paste("No data in the force range for", basename(file_path)))
      return(NULL)
    }
  } else {
    warning(paste("Missing 'Force' or 'Stroke' column in", basename(file_path)))
    return(NULL)
  }
}

all_data_processed <- lapply(material_files, process_sample, material_num = gsub("material", "", material_to_plot))
names(all_data_processed) <- paste("Sample", samples_to_plot)
all_data_processed <- Filter(Negate(is.null), all_data_processed) # Remove NULL entries

if (length(all_data_processed) == 0) {
  stop("No valid data to plot.")
}

combined_data_for_plot <- do.call(rbind, lapply(all_data_processed, function(x) x$data))
x_min <- min(combined_data_for_plot$Stroke)
x_max <- max(combined_data_for_plot$Stroke)

# --- Create and Save the Plot ---
output_directory <- "exports/final_graphs"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)
output_filename <- file.path(output_directory, paste0(output_filename_base, ".png"))

png(filename = output_filename, width = 2000, height = 1200, units = "px", res = 200) # Adjusted size and resolution
layout(matrix(c(1, 2), ncol = 2), widths = c(3, 1))
par(mar = c(5, 5, 4, 2))

plot(combined_data_for_plot$Stroke, combined_data_for_plot$Force,
     xlab = "Stroke/mm", ylab = "Force/N", main = graph_title,
     xlim = c(x_min, x_max), ylim = c(min_force, max_force), type = "n")

sample_names <- names(all_data_processed)
num_samples <- length(sample_names)
colors <- rainbow(num_samples)
symbols <- seq_along(sample_names)

for (i in seq_along(sample_names)) {
  sample_name <- sample_names[i]
  data <- all_data_processed[[sample_name]]$data
  model <- all_data_processed[[sample_name]]$model
  points(data$Stroke, data$Force, pch = symbols[i], col = colors[i], cex = 0.7)
  abline(model, col = colors[i], lwd = 2)
}

legend("bottomright", legend = sample_names, pch = symbols, col = colors, cex = 0.8)

# --- Right Panel for Equations ---
par(mar = c(5, 1, 4, 2))
plot.new()
text_x <- 0
text_y <- 1
line_spacing <- 1 / (num_samples * 2 + 1)
text_cex <- min(0.8, 1 / (num_samples / 5))

for (i in seq_along(sample_names)) {
  sample_name <- sample_names[i]
  model <- all_data_processed[[sample_name]]$model
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  r_squared <- summary(model)$r.squared

  eq_text <- paste0(sample_name, ": y = ", round(slope, 2), "x + ", round(intercept, 2))
  r2_text <- bquote(R^2 == .(round(r_squared, 3)))

  text(text_x, text_y, eq_text, pos = 4, col = colors[i], cex = text_cex)
  text_y <- text_y - line_spacing
  text(text_x, text_y, r2_text, pos = 4, col = colors[i], cex = text_cex)
  text_y <- text_y - line_spacing
}

dev.off()
cat(paste("High-resolution plot saved to:", output_filename, "\n"))