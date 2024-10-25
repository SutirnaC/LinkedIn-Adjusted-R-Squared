# Load libraries
library(ggplot2)     # For plotting
library(gganimate)   # For creating animations
library(magick)      # For saving the GIF
library(dplyr)       # For data manipulation

# Load the mtcars dataset
data(mtcars)

# Create a function to calculate adjusted R-squared for different models
calculate_adjusted_r_squared <- function(data, predictors) {
  formula <- as.formula(paste("mpg ~", paste(predictors, collapse = "+")))
  model <- lm(formula, data = data)
  summary(model)$adj.r.squared
}

# Generate all possible combinations of predictors
predictor_combinations <- unlist(lapply(1:length(names(mtcars)[-1]), 
                                        function(x) combn(names(mtcars)[-1], x, simplify = FALSE)), 
                                        recursive = FALSE)

# Calculate adjusted R-squared for each combination
results <- sapply(predictor_combinations, function(predictors) {
  calculate_adjusted_r_squared(mtcars, predictors)
})

# Prepare data for plotting
results_df <- data.frame(
  predictors = sapply(predictor_combinations, paste, collapse = ", "),
  adjusted_r_squared = results,
  iteration = seq_along(results)  # Create an iteration column
)

# Create a line plot and animate using gganimate
p <- ggplot(results_df, aes(x = iteration, y = adjusted_r_squared, group = 1)) +
  geom_line(color = "blue", size = 1) +        # Line plot joining each point
  geom_point(color = "blue", size = 2) +       # Add points to the line
  labs(x = "Iteration", y = "Adjusted R-squared") +
  ggtitle("Adjusted R-squared Values for Different Predictor Combinations") +
  theme_minimal() +
  transition_reveal(iteration) +  # Reveals the line progressively
  ease_aes('linear') +               # Smooth transition effect
  geom_text(aes(label = round(adjusted_r_squared, 3)), 
            x = nrow(results_df), y = max(results_df$adjusted_r_squared), 
            hjust = 1.1, vjust = 1.5, size = 5, color = "red", 
            check_overlap = TRUE)  # Add dynamic adjusted R-squared as a text label

# Animate and save the GIF
anim <- animate(p, nframes = 200, fps = 10, width = 600, height = 400, renderer = magick_renderer())

# Save the animation as a GIF
anim_save("adjusted_r_squared_animation.gif", animation = anim)

