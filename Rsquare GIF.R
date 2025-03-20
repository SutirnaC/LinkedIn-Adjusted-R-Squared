library(ggplot2)
library(gganimate)

# Simulated dataset
set.seed(123)
n <- 100
x <- runif(n, 1, 10)
y <- 2 + 3*x + rnorm(n, sd=3)

data <- data.frame(x, y)

# Store R-squared values for different polynomial degrees
rsq_values <- data.frame(degree = integer(), rsq = numeric())

for (d in 1:5) {  # Loop over model complexity
  model <- lm(y ~ poly(x, d), data = data)
  rsq <- summary(model)$r.squared
  rsq_values <- rbind(rsq_values, data.frame(degree = d, rsq = rsq))
}

# Create the plot
p <- ggplot(rsq_values, aes(x = degree, y = rsq)) +
  geom_line(color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "How R² Changes with Model Complexity",
       x = "Polynomial Degree",
       y = "R-squared") +
  theme_minimal()

# Animate the plot
animated_plot <- p + transition_reveal(degree)

# Save as GIF
anim_save("r_squared_animation.gif", animation = animate(animated_plot, fps = 5, width = 600, height = 400))
