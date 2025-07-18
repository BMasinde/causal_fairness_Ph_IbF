# DEMO OF HOW CONFOUNDERS AFFECT REGRESSION
# Set seed for reproducibility
set.seed(123)

# Sample size
n <- 200

# Confounder: age (Z)
Z <- rnorm(n, mean = 50, sd = 10)  # Age between ~30 to 70

# X: Exercise, which is negatively correlated with age
X <- 100 - Z + rnorm(n, sd = 5)

# Y: Blood pressure, positively correlated with age
Y <- 0.5 * Z + rnorm(n, sd = 5)

# Create a data frame
df <- data.frame(X = X, Y = Y, Z = Z)

# Check naive correlation between X and Y
cor(df$X, df$Y)

# Plot without controlling for Z
plot(df$X, df$Y, main = "Naive Correlation between X and Y",
     xlab = "Exercise (X)", ylab = "Blood Pressure (Y)")
abline(lm(Y ~ X, data = df), col = "red")

# Multiple regression controlling for Z
model_with_confounder <- lm(Y ~ X + Z, data = df)
summary(model_with_confounder)

# Compare with model without controlling for Z
model_without_confounder <- lm(Y ~ X, data = df)
summary(model_without_confounder)


# DEMO WHEN CONFOUNDER IS A CATEGORICAL VARIABLE
set.seed(123)

# Sample size
n <- 200

# Generate gender variable (categorical confounder)
gender <- sample(c("Male", "Female"), n, replace = TRUE)

# Assign different means for X and Y based on gender
X <- ifelse(gender == "Male", rnorm(n, mean = 4, sd = 1),
            rnorm(n, mean = 6, sd = 1))  # Hours studied

Y <- ifelse(gender == "Male", rnorm(n, mean = 65, sd = 5),
            rnorm(n, mean = 75, sd = 5))  # Exam score

df <- data.frame(X = X, Y = Y, gender = factor(gender))

# Naive correlation between study hours and exam score
cor(df$X, df$Y)

# Scatter plot without adjusting for gender
plot(df$X, df$Y, main = "Study Hours vs Exam Score (Naive)",
     xlab = "Study Hours", ylab = "Exam Score")
abline(lm(Y ~ X, data = df), col = "red")

# Linear model ignoring gender
summary(lm(Y ~ X, data = df))

# Linear model controlling for gender (categorical confounder)
summary(lm(Y ~ X + gender, data = df))

# Interaction model (optional)
summary(lm(Y ~ X * gender, data = df))
