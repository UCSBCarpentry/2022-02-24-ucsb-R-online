library(tidyverse)
surveys <- read_csv("data_raw/portal_data_joined.csv")

# Data Wrangling in R

## select, filter, mutate (group_by and summarize)

## Selecting columns

select(surveys, plot_id, species_id, weight)

select(surveys, -record_id, -species_id)

## Filtering Rows

filter(surveys, year == 1995)

## Pipes

## Problem: we want to do several operations in sequence:

surveys2 <- filter(surveys, weight < 5)
select(surveys2, species_id, sex, weight)

# Alternatively:
select(filter(surveys, weight < 5), species_id, sex, weight)

## Better: Pipes

## DATA %>%
##    Do Something %>%
##    Do Something Else

surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

## Challenge:
## Using pipes, subset the surveys data to include animals 
## collected before 1995 and retain only the columns year, 
## sex, and weight.

surveys %>%
  filter(year < 1995) %>%
  select(year, sex, weight)

## Creating new columns with mutate

surveys %>% 
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight/1000) %>%
  view()

thingtosave <- surveys %>% 
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight/1000)
  
surveys %>% 
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight/1000) ->
  otherthing

### End of Thursday Section

