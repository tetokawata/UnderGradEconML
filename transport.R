# Load the library
library(transport)

# Define two simple distributions
a <- c(0.2, 0.5, 0.3)
b <- c(0.4, 0.4, 0.2)

# Define cost matrix (e.g., Euclidean distances)
costm <- matrix(c(
  0, 1, 2,
  1, 0, 1,
  2, 1, 0
), nrow = 3, byrow = TRUE)

# Compute optimal transport plan
result <- transport(a, b, costm)

# View the result
print(result)

emoji::emoji_name
