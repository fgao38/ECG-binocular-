# Importing necessary packages 
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)

# NOTE: MY mock datas are all located inside the folder "bids_dataset"

# Combining 54 participants' data into one single big data
# Reading in 54 participants' data
# creating a loop to extra all of the participants' information
folder_path <- "bids_dataset/Binocular_rivalry"
  participant_data <- c()
  for(id in 1:58){
    for(session_id in 1:2){
      file_name <- sprintf("sub-%02d_task-rivalry_run-%02d_events", id, session_id)
      file_path <- file.path(folder_path, paste0(file_name, ".tsv"))
      # debugging    
      #    cat("Checking file:", file_path, "\n")
      if (file.exists(file_path)){
        participant_session_data <- read_tsv(file_path, show_col_types = FALSE)
        # adding participant id and session id      
        participant_session_data$subject <- id
        participant_session_data$session <- session_id
        # append all of participants information into a single data
        participant_data <- append(participant_data, list(participant_session_data))
        
      }
      #debugging
      else {
        cat("File not found:", file_path, "\n")
        if (id %in% c(43, 53, 54, 56)) {
          cat("Skipping missing participant:", id, "\n")
        }
        next
      }
    }}
  final <- bind_rows(participant_data)

# Write out my "final" data as a csv file
  write.csv(final, "bids_dataset/compiled_part.csv",row.names = FALSE)

# My compiled data has 4 important columns, they are "subject id", "session",
# "duration", and "dominant".The duration records the time that the participant 
# perceived on asynchronous and synchronous stimulus. 
# I write out in this way so I can conduct t-test and ANOVAS to calculate 
# whether there is significant difference between the duration among 
# synchronous and asynchronous.

# Example of the t-test
  
  # Clean the data: remove rows with 'n/a' in 'duration', and convert 'duration' to numeric
  clean_data <- final[!final$duration == "n/a", ]
  clean_data$duration <- as.numeric(as.character(clean_data$duration))
  
  # Perform a t-test comparing 'duration' between two groups of 'dominant'
  t_test_result <- t.test(duration ~ dominant, data = clean_data)
  
  # Display the t-test results
  print(t_test_result)

# t = -2.5272(p<0.05)  
  
# Using dplyr functions and write out my grouped file as a csv file
  sub_data <- final %>% 
    mutate(duration = as.numeric(duration)) %>% 
    drop_na(duration) %>% 
    group_by(subject,dominant) %>% 
    summarize(total_duration = sum(duration, na.rm = TRUE))
  write.csv(sub_data, "bids_dataset/total_durations.csv",row.names = FALSE)

# My sub_data grouped the total duration of each participant in each session. 
# I am planning to create a dot plot with dominant as the x-axis and total_time
# as y-axis to closely examine the pattern
# Below is my ongoing plan for the dot plot:

#dot_plot <- ggplot(filtered_data, aes(x = dominant, y = duration)) +
#  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5, binwidth = 0.1) +
#  labs(x = "dominant", y = "duration") +
#  theme_minimal()
#dot_plot

  