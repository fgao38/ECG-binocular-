library(tidyverse)
library(ggplot2)
library(dplyr)
## Create your goal tibble to replicate

# Run this line to see what your end product should look like
sw.wrangled.goal <- read_csv("class_data/sw-wrangled.csv") %>% 
  mutate(across(c(hair, gender, species, homeworld), factor)) # this is a quick-and-dirty fix to account for odd importing behavior

# View in console
sw.wrangled.goal 

# Examine the structure of the df and take note of data types
# Look closely at factors (you may need another function to do so) to see their levels
str(sw.wrangled.goal) 



## Use the built-in starwars dataset to replicate the tibble above in a tbl called sw.wrangled
# If you get stuck, use comments to "hold space" for where you know code needs to go to achieve a goal you're not sure how to execute


sw.wranged <- data.frame(starwars) %>% 
# split the "name" column into first name and last name, the extra are added to last name
# arrange the last name based on alph order
  separate(name, into = c("first_name", "last_name"), sep = " ", extra = "merge") %>% 
  arrange(last_name) %>% 
# adding a new column initials by extracting the first letters from first and last name  
  mutate(initials = paste0(substr(first_name, 1, 1), substr(last_name, 1, 1))) %>% 
# rearrange plus getting rid of unwanted columns  
  select(first_name, last_name, initials, height, 
         mass, hair_color, gender, species,
         homeworld) %>% 
# creating new height columns
  rename(height_cm = height) %>% 
  mutate(height_in = round(height_cm / 2.54000508, 4)) %>% 
  rename(hair = hair_color) %>% 
# filling the NAs with "bald"  
  mutate(hair = ifelse(is.na(hair), "bald", hair)) %>% 
# cutting the long names into initials  
  mutate(gender = ifelse(gender == "masculine", "m", "f")) %>% 
# upper case all species  
  mutate(species = toupper(species)) %>% 
# any hair column contains brown are returned TRUE  
  mutate(brown_hair = grepl("brown", hair)) %>%
# rearrange  
  select(first_name, last_name, initials, height_in,
         height_cm, mass, hair, gender, species,
         homeworld, brown_hair) %>% 
  filter(!is.na(height_in)) %>% 
  arrange((is.na(last_name)), last_name, first_name) %>% 
# transforming data structure  
  mutate(
    hair = factor(hair),
    gender = factor(gender),
    species = factor(species),
    homeworld = factor(homeworld)
  ) %>% 
  mutate(height_cm = as.numeric(height_cm)) %>% 
  as_tibble()

# checking and comparing data structure  
str(sw.wranged)
str(sw.wrangled.goal) 
## Check that your sw.wrangled df is identical to the goal df
# Use any returned information about mismatches to adjust your code as needed
all.equal(sw.wrangled.goal, sw.wranged)

# Graphing
## Histogram
ggplot(sw.wranged, aes(x = height_cm)) +
  geom_histogram() +
  labs(title = "Count of height(cm) in starwars",
       x = "height_cm",
       y = "count") +
  theme_minimal()

## Bar Graph
# reorder first
sw.wranged_table <- table(sw.wranged $ hair)
sw.wranged_table_df <- data.frame(sw.wranged_table)
sw.wranged_table_df$Var1 <- reorder(sw.wranged_table_df$Var1,-sw.wranged_table_df$Freq)

ggplot(sw.wranged_table_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of hair in starwars",
       x = "hair",
       y = "count") +
  theme_minimal()

## Dot plot

ggplot(sw.wranged, aes(x = height_in, y = mass)) +
  geom_point(shape = "triangle") +
  labs(title = "Dot Plot of Height vs Mass",
       x = "Height",
       y = "Mass") +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 170)) +
  scale_x_continuous(breaks = c(40, 60, 80)) +
  scale_y_continuous(breaks = c(40, 80, 120, 160))


