library(tidyverse)

utah_religions <- read_csv("Utah_Religions_by_County.csv")

utah_religions_tidy <- utah_religions %>%
  pivot_longer(
    cols = -c(County, Pop_2010, Religious),
    names_to = "Religion",
    values_to = "Count"
  )


#### Exploring the Data ####

utah_religions_tidy %>%
  ggplot(aes(x = reorder(County, Religious), y = Religious)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Religious Adherents by County",
    x = "County",
    y = "Total Religious Population"
  )

utah_religions_tidy %>%
  ggplot(aes(x = County, y = Count, fill = Religion)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Religious Composition by County",
    x = "County",
    y = "Number of Adherents"
  )

utah_religions_tidy %>%
  group_by(Religion) %>%
  summarize(total = sum(Count, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Religion, total), y = total)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Adherents by Religion in Utah",
    x = "Religion",
    y = "Total Adherents"
  )

### Does population of a county correlate with the proportion  of any specific 
### religious group in that county?

# I originally thought that I had to make a new object for the proportion of
# each religion but later realized that the Count collumn is already a
# percentage. I just kept the new object = to count so I didn't have to
# change the other code.

utah_prop <- utah_religions_tidy %>%
  mutate(religion_prop = Count)

# This section creates a single correlation number for each religion by
# correlating county population with each religions proportion giving a
# single number showing whether the religion becomes more or less common
# as population increases.

correlations <- utah_prop %>%
  group_by(Religion) %>%
  summarize(
    correlation = cor(Pop_2010, religion_prop, use = "complete.obs")
  )

correlations

# This sections uses that correlation number to show how much each religion's
# proportion changes as population increases

ggplot(correlations, aes(x = reorder(Religion, correlation), y = correlation)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Correlation Between Population and Religion Proportion",
    x = "Religion",
    y = "Correlation"
  ) +
  theme_minimal()

### Does proportion of any specific religion in a given county correlate with 
### the proportion of non-religious people?

# First I made a new table showing the proportion of non-religious people in
# each county.

non_religious <- utah_religions_tidy %>%
  filter(Religion == "Non-Religious") %>%
  select(County, non_rel_prop = Count)

# Similar to the first question, I joined the non-religious proportion to all
# other religions by county and calculated the correlation for each religion, 
# showing whether its proportion tends to increase or decrease as the 
# non-religious proportion changes.

utah_corr <- utah_religions_tidy %>%
  filter(Religion != "Non-Religious") %>%
  left_join(non_religious, by = "County") %>%
  group_by(Religion) %>%
  summarize(correlation_with_non_religious = cor(Count, non_rel_prop, use = "complete.obs"))

# Finally I created a bar chart to demonstrate those correlations for each
# county.

ggplot(utah_corr, aes(x = reorder(Religion, correlation_with_non_religious), 
                      y = correlation_with_non_religious)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Correlation of Religion and Non-Religious Proportion",
    x = "Religion",
    y = "Correlation with Non-Religious Proportion"
  ) +
  theme_minimal()
