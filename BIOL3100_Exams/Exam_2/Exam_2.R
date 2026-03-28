library(tidyverse)
unicef <- read_csv("unicef-u5mr.csv")
glimpse(unicef)

unicef_tidy <- unicef %>%
  pivot_longer(
    cols = starts_with("U5MR"),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  mutate(
    Year = as.numeric(gsub("U5MR\\.", "", Year))
  ) 

library(ggplot2)

plot1 <- ggplot(unicef_tidy, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line(color = "black", alpha = 0.7) +
  facet_wrap(~ Continent) +
  labs(
    title = "Under-5 Mortality Rate (U5MR) Over Time",
    x = "Year",
    y = "U5MR (deaths per 1000 live births)"
  ) +
  theme_minimal()

ggsave("Wheeler_Plot_1.png", plot = plot1, width = 10, height = 6, dpi = 300)

library(dplyr)

unicef_mean <- unicef_tidy %>%
  group_by(Continent, Year) %>%
  summarize(
    mean_U5MR = mean(U5MR, na.rm = TRUE)
  )

library(ggplot2)

plot2 <- ggplot(unicef_mean, aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line(size = 1) +
  labs(
    title = "Mean Under-5 Mortality Rate by Continent Over Time",
    x = "Year",
    y = "Mean U5MR (deaths per 1000 live births)"
  ) +
  theme_minimal()

ggsave("LASTNAME_Plot_2.png", plot = plot2, width = 10, height = 6, dpi = 300)

mod1 <- lm(U5MR ~ Year, data = unicef_tidy, na.action = na.exclude)
mod2 <- lm(U5MR ~ Year + Continent, data = unicef_tidy, na.action = na.exclude)
mod3 <- lm(U5MR ~ Year * Continent, data = unicef_tidy, na.action = na.exclude)

AIC(mod1, mod2, mod3)
##      df      AIC
## mod1  3       117100.7
## mod2  7       110281.1
## mod3 11       109215.1
## model 3 has the lowest AIC value meaning that it is
## the model that most accurately fits the data

predict_df <- unicef_tidy %>%
  select(CountryName, Continent, Year) %>%
  distinct() %>%
  mutate(
    pred_mod1 = predict(mod1, newdata = .),
    pred_mod2 = predict(mod2, newdata = .),
    pred_mod3 = predict(mod3, newdata = .)
  )

pred_long <- pred_summary %>%
  pivot_longer(
    cols = starts_with("pred"),
    names_to = "Model",
    values_to = "Predicted_U5MR"
  )

ggplot(pred_long, aes(x = Year, y = Predicted_U5MR, color = Continent)) +
  geom_line(size = 1) +
  facet_wrap(~ Model) +
  labs(
    title = "Model Predictions",
    x = "Year",
    y = "Predicted U5MR"
  ) +
  theme_minimal()

#### Bonus ####

mod4 <- lm(log(U5MR) ~ Year * Continent, data = unicef_tidy, na.action = na.exclude)

ecuador_2020 <- data.frame(
  CountryName = "Ecuador",
  Continent = "Americas",
  Year = 2020
)

log_pred <- predict(mod4, newdata = ecuador_2020)
pred_mod4 <- exp(log_pred)

real_value <- 13
diff_mod4 <- pred_mod4 - real_value

data.frame(
  Model = "mod4",
  Prediction = pred_mod4,
  Reality = real_value,
  Difference = diff_mod4
)
