library(tidyverse)
dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

dat_long <- dat %>%
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    values_to = "Absorbance"
  )
dat_long <- dat_long %>%
  mutate(Time = parse_number(Time))

dat_long <- dat_long %>%
  mutate(Environment = case_when(
    `Sample ID` %in% c("Soil_1", "Soil_2") ~ "Soil",
    `Sample ID` %in% c("Clear_Creek", "Waste_Water") ~ "Water"
  ))

plot_data <- dat_long %>%
  filter(Dilution == 0.1)
ggplot(plot_data, aes(x = Time, y = Absorbance, color = Environment)) +
  stat_summary(fun = mean, geom = "line") +
  facet_wrap(~Substrate) +
  labs(
    x = "Time (hours)",
    y = "Absorbance",
    color = "Environment"
  ) +
  theme_classic()

library(tidyverse)
library(gganimate)

dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

dat_long <- dat %>%
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    values_to = "Absorbance"
  ) %>%
  mutate(Time = parse_number(Time))

itaconic <- dat_long %>%
  filter(Substrate == "Itaconic Acid")

itaconic_mean <- itaconic %>%
  group_by(`Sample ID`, Dilution, Time) %>%
  summarise(
    Mean_absorbance = mean(Absorbance, na.rm = TRUE),
    .groups = "drop"
  )

p <- ggplot(itaconic_mean,
            aes(x = Time,
                y = Mean_absorbance,
                color = `Sample ID`,
                group = `Sample ID`)) +
  geom_line(size = 1) +
  facet_wrap(~Dilution) +
  labs(
    x = "Time",
    y = "Mean_absorbance",
    color = "Sample ID"
  ) +
  transition_reveal(Time)

animate(p)

