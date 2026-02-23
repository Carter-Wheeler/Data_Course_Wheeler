covid_data <- read.csv("cleaned_covid_data.csv", stringsAsFactors = FALSE)
head(covid_data)
library(tidyverse)
A_states <- covid_data %>% filter(str_starts(Province_State, "A"))

library(tidyverse)
ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point() +                                
  geom_smooth(method = "loess", se = FALSE) +  
  facet_wrap(~ Province_State, scales = "free") +  
  labs(title = "COVID-19 Deaths Over Time in States/Provinces Starting with 'A'",
       x = "Time",
       y = "Deaths") +
  theme_minimal()

state_max_fatality_rate <- covid_data %>%
  group_by(Province_State) %>%                             
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%  
  arrange(desc(Maximum_Fatality_Ratio))  

state_max_fatality_rate <- state_max_fatality_rate %>%
  mutate(Province_State = factor(Province_State, levels = Province_State))

library(ggplot2)

ggplot(state_max_fatality_rate, aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col(fill = "steelblue") +              
  labs(title = "Maximum Case Fatality Ratio by Province/State",
       x = "Province_State",
       y = "Maximum_Fatality_Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


us_cumulative <- covid_data %>%
  group_by(Last_Update) %>%                     
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE)) %>%  
  arrange(Last_Update)                          

ggplot(us_cumulative, aes(x = Last_Update, y = Total_Deaths)) +
  geom_line(color = "red", linewidth = 1) +   # updated: use linewidth instead of size
  geom_point(size = 1) +                       # size is still valid for points
  labs(title = "Cumulative COVID-19 Deaths in the US Over Time",
       x = "Time",
       y = "Cumulative Deaths") +
  theme_minimal()
