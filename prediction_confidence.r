library(ggplot2)
library(dplyr)

set.seed(42)

# First dataset
X1 <- runif(100, -7, 10)
Y1 <- - 4 + 2 * X1 + rnorm(100, 0, 2)

# Second dataset with the same model
X2 <- runif(100, -7, 10)
Y2 <- - 4 + 2 * X2 + rnorm(100, 0, 2)


# Combine both datasets into a single data frame
df <- rbind(
  data.frame(X = X1, Y = Y1, Sample = "Sample 1"),
  data.frame(X = X2, Y = Y2, Sample = "Sample 2")
)

mod1 <- lm(Y ~ X, data = subset(df, Sample == "Sample 1"))
mod2 <- lm(Y ~ X, data = subset(df, Sample == "Sample 2"))

ggplot(data = df, aes(x = X, y = Y, color = Sample)) +
  geom_point() +
  geom_abline(slope = coef(mod1)["X"], intercept = coef(mod1)["(Intercept)"], colour = 'magenta') +
  geom_abline(slope = coef(mod2)["X"], intercept = coef(mod2)["(Intercept)"], colour = 'darkblue') +
  scale_color_manual(values = c("Sample 1" = "red", "Sample 2" = "blue")) +
  labs(title = "Two samples from the same model",
       x = "X", y = "Y", colour = "Sample") +
  theme_minimal()



predConfidence <- predict(mod1, interval = c("confidence"))
predPrediction <- predict(mod1, interval = c("prediction"))
head(predConfidence)
head(predPrediction)

X_seq <- df %>% filter(Sample == "Sample 1") %>% select(X) %>% arrange(X)
