library(tidyverse)
surveys <- read_csv("data_raw/portal_data_joined.csv")
view(surveys)
glimpse(surveys)

# Last time: select, filter, mutate
## Select columns (using names)
## Filter rows (using some criterion)
## Mutate adds new columns base on old ones.

# Q: I am confused when you put a "-" in front and when 
# you dont

leftPart <- surveys %>%
  select(record_id:species_id)
glimpse(leftPart)
rightPart <- surveys %>%
  select(-(record_id:species_id))
glimpse(rightPart)

unique(surveys$plot_type)
unique(surveys$plot_id)

## Create new columns with mutate

surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2) %>%
  glimpse()

## Challenge:
## Create a new data frame from the surveys data that 
## meets the following criteria: 
## ## The data frame contains only the species_id column 
##    and a new column called hindfoot_cm containing 
##    the hindfoot_length values (currently in mm) 
##    converted to centimeters. 
## ## In this hindfoot_cm column, there are no NAs and 
##    all values are less than 3.
##
## ##  Hint: think about how the commands should be ordered 
##     to produce this data frame!

newdata <- surveys %>%
  mutate(hindfoot_cm = hindfoot_length / 10) %>%
  select(species_id, hindfoot_cm) %>%
  filter(!is.na(species_id))%>%
  filter(!is.na(hindfoot_cm))%>%
  filter(hindfoot_cm < 3)
view(newdata)

newdata2 <- surveys %>%
  mutate(hindfoot_cm = hindfoot_length / 10) %>%
  select(species_id, hindfoot_cm) %>%
  filter(!is.na(species_id),
         !is.na(hindfoot_cm),
         hindfoot_cm < 3)
view(newdata2)

## Check that it worked
max(newdata$hindfoot_cm)
summary(newdata$hindfoot_cm)
newdata %>% summary()

## Split-apply-combine with summarize()

surveys %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>% 
  view()

# Build a summary gradually:
surveys %>% 
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight),
            group_size = n(),
            gs2 = length(weight)) %>%
  arrange(desc(group_size)) %>% # sort (in descending order)
  view()
  
# Counting observations

surveys %>%
  group_by(sex) %>%
  summarize(groupSize = n() )

surveys %>%
  count(sex) # shorthand for the above

surveys %>%
  count(sex, sort = TRUE)
?count # if you forget how things work

## Challenge

## ##  1. How many animals were caught in each 
##        `plot_type` surveyed?
surveys%>%
  count(plot_type) 
## 
## ##  2. Use `group_by()` and `summarize()` to 
##        find the mean, min, and max
## ##     hindfoot length for each species 
##        (using `species_id`). Also add the number of
## ##     observations (hint: see `?n`).
surveys%>%
  group_by(species_id)%>%
  filter(!is.na(hindfoot_length))%>%
  summarize(mean_hind=mean(hindfoot_length),
            min_hind=min(hindfoot_length),
            max_hind=max(hindfoot_length),
            count_hind=n())
## 
## ##  3. What was the heaviest animal measured 
##        in each year? Return the
## ##     columns `year`, `genus`, `species_id`, 
##        and `weight`.

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>%  # tricky (applies to groups)
  select(year, genus, species_id, weight) %>%
  arrange(year) %>%
  view()

surveys %>% 
  filter(!is.na(weight)) %>% 
  select(year, genus, species_id, weight) %>% 
  group_by(year) %>% 
  summarize(max_weight = max(weight)) %>% 
  arrange(year) 
# ^^^ Notice the other info (genus, species_id)
# goes away here because of the summarize()

## Clean data for the graphics episode

surveys_complete <- surveys %>%
  filter(!is.na(weight),
         !is.na(hindfoot_length),
         !is.na(sex))
species_counts <- surveys_complete %>%
  count(species_id) %>%
  filter(n >= 50) # get rid of rare species
glimpse(species_counts) 

surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)

write_csv(surveys_complete, file = "data/surveys_complete.csv")
