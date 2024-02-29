library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggsci)
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



# Assignment 12 Part 1

# Box Plot 
ggplot(sw.wranged, aes(x = hair, y = mass, fill = hair)) +
  geom_boxplot() +
  labs(title = "Hair vs Mass",
       x = "Hair Color(s)",
       y = "Mass(kg)",
       fill = "Corlorful hair")+
  coord_cartesian(ylim = c(0, 170)) +
  scale_x_discrete(limits = levels(sw.wranged_table_df$Var1)) +
  scale_fill_discrete(limits = levels(sw.wranged_table_df$Var1)) +
  scale_y_continuous(breaks = c(40, 80, 120, 160))




# Smooth point plot 
# Get rid of outlier
filtered_data <- sw.wranged %>%
  filter(mass < 1000)
# Creating the labels
filtered_data$brown_hair <- factor(filtered_data$brown_hair, levels = c("TRUE", "FALSE"))
custom_labels <- c("TRUE" = "Has brown hair",
                   "FALSE" = "No brown hair")
# Creating the plot
ggplot(filtered_data, aes(x = mass, y = height_in)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_point() +
  labs(title = "Mass vs. height by brown-hair havingness",
       x = "mass",
       y = "height_in",
       subtitle = "A critically important analysis")+
  facet_wrap(~brown_hair, labeller = as_labeller(custom_labels)) +
  coord_cartesian(xlim = c(0, 200), ylim = c(-4, 200)) +
  theme_minimal()

# Bar plot 
# Creating a subcategory
sp_first_name <- sw.wranged %>%
  mutate(species_first_name = substr(species, 1, 1)) %>% 
  drop_na(species_first_name) %>% 
  mutate(species_first_name = factor(species_first_name, 
                                     levels = rev(sort(unique(species_first_name)))))
  
# Creating plot
ggplot(sp_first_name, aes(y = species_first_name, fill = gender)) +
  geom_bar() +
  labs(x = "count",
       y = "species_first_name",
       caption = "A clear male human bias") +
  theme_minimal()

# Assignment part 2

# Did you use the best kind of plot to tell the story you’re hoping to tell?
# 1. Yes, I believed I have created the plot that I want. A plot for the substracted mean difference and a plot
# for average comparison between syn and asyn

# Did you have to do some wonky wrangling to plot what you wanted?
# I did some wrangling to the plot that I wanted, espically I have to readjust my data frames. 

# Are there adjustments you could make to scale, coord, or labs layers that would make things easier to read?
# I think the adjustments that I currently have is easier enough to read, I have shorten my x-axis to be 
# just right. 

#Are you effectively making use of groups? Could you communicate your point better by grouping in a different way (e.g., color, shape, fill, facet)? Add code to one chunk created in Assignment (10)
# Yes, in my histogram, I created a new dataframe named average_diff that grouped subject and calculate the mean duration
# that the participant spent on syn and asyn. 

# Assignment 12

transformed_data <- sw.wranged %>%
  mutate('Gender Presentation' = case_when(
    gender == "f" ~ "Female",
    gender == "m" ~ "Male",
    is.na(gender) ~ "Other",
    TRUE ~ as.character(gender)
  ))
transformed_data$`Gender Presentation` <- factor(transformed_data$`Gender Presentation`, 
                                              levels = c("Female", "Male", "Other"))

ggplot(transformed_data, aes(x = height_cm, y = mass, color = `Gender Presentation`)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_point() +
  facet_wrap(vars(`Gender Presentation`), scales = "free_y") +
  labs(title = "Height and weight across gender presentation",
       x = "Height(cm)",
       y = "Mass(kg)",
       subtitle = "A cautionary tale in misleading 'free' axis scales & bad design choices",
       caption = "Color hint: use the ggsci pacakge!")+
  coord_cartesian(xlim = c(50, 250)) +
  theme(plot.caption = element_text(angle = 180, vjust = 0, hjust = 0, color = "red"),
        panel.background = element_rect(fill = 'mistyrose'),
        panel.grid.major = element_line(linetype = 'dashed', color="grey"),
        panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
        axis.text = element_text(colour = "black", 
                                 face = "italic", 
                                 family = "Times New Roman"),
        strip.background = element_rect(fill = "darkgreen"),
        strip.text = element_text(color = "white", hjust = 0),
        text = element_text(family = "Times New Roman"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "mediumpurple"),
        legend.title = element_text(family = "Comic Sans MS", face = "bold", size = 12, color = "black"))


