library(tidyverse)

data <- read.csv("../../Data/mushroom_growth.csv")
data$Humidity <- as.factor(data$Humidity)
str(data)
head(data)

ggplot(data, aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("GrowthRate vs Temperature")

ggplot(data, aes(x = Humidity, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("GrowthRate vs Humidity")

ggplot(data, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("GrowthRate vs Light")

pairs(data[sapply(data, is.numeric)])

mod1 <- lm(GrowthRate ~ Temperature, data = data)

mod2 <- lm(GrowthRate ~ Temperature + Humidity, data = data)

mod3 <- lm(GrowthRate ~ Temperature + Humidity + Light, data = data)

mod4 <- lm(GrowthRate ~ Temperature * Humidity + Light, data = data)

mse <- function(model) {
  mean(residuals(model)^2)
}

mse1 <- mse(mod1)
mse2 <- mse(mod2)
mse3 <- mse(mod3)
mse4 <- mse(mod4)

mse1
mse2
mse3
mse4

model_results <- data.frame(
  Model = c("mod1", "mod2", "mod3", "mod4"),
  MSE = c(mse1, mse2, mse3, mse4)
)

print(model_results)

best_model <- mod4  

data$pred <- predict(best_model, newdata = data)

new_data <- expand.grid(
  Temperature = c(20, 25),
  Humidity = factor(c("Low", "High"), levels = levels(data$Humidity)),
  Light = c(0, 10, 20)
)


new_data$pred <- predict(best_model, newdata = new_data)
str(data)

print(new_data)


ggplot(data, aes(x = Temperature, y = GrowthRate)) +
  geom_point(color = "black") +
  geom_line(aes(y = pred), color = "blue") +
  geom_point(data = new_data, aes(y = pred), color = "red", size = 3) +
  ggtitle("Model Predictions vs Actual Data") +
  ylab("Growth Rate")



