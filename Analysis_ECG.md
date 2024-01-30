data_cleaning
================
Fan Gao (Scott)
2024-01-17

# Importing packages

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
library(tidyverse)
```

    ## Warning: package 'stringr' was built under R version 4.2.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggplot2)
```

# Counting the number of datas inside BIDS

``` r
folder_path <- "bids_dataset/Binocular_rivalry"
files <- list.files(folder_path)
print(length(files))
```

    ## [1] 108

# Reading in 54 participants’ data

``` r
# creating a loop to extra all of the participants' information
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
```

    ## File not found: bids_dataset/Binocular_rivalry/sub-43_task-rivalry_run-01_events.tsv 
    ## Skipping missing participant: 43 
    ## File not found: bids_dataset/Binocular_rivalry/sub-43_task-rivalry_run-02_events.tsv 
    ## Skipping missing participant: 43 
    ## File not found: bids_dataset/Binocular_rivalry/sub-53_task-rivalry_run-01_events.tsv 
    ## Skipping missing participant: 53 
    ## File not found: bids_dataset/Binocular_rivalry/sub-53_task-rivalry_run-02_events.tsv 
    ## Skipping missing participant: 53 
    ## File not found: bids_dataset/Binocular_rivalry/sub-54_task-rivalry_run-01_events.tsv 
    ## Skipping missing participant: 54 
    ## File not found: bids_dataset/Binocular_rivalry/sub-54_task-rivalry_run-02_events.tsv 
    ## Skipping missing participant: 54 
    ## File not found: bids_dataset/Binocular_rivalry/sub-56_task-rivalry_run-01_events.tsv 
    ## Skipping missing participant: 56 
    ## File not found: bids_dataset/Binocular_rivalry/sub-56_task-rivalry_run-02_events.tsv 
    ## Skipping missing participant: 56

``` r
final <- bind_rows(participant_data)
```

# Counting the number of participants in my new data to double check

``` r
num_participants <- final %>% 
  distinct(subject) %>% 
  n_distinct()

print(num_participants)
```

    ## [1] 54

# Examine your dataset in RStudio. Answer these questions with comments in your read-in chunk

- Tidy?

- Yes, each column is a single variable, and they are “onset”,
  “duration”, “trail”, “sync_side”, “key”, “dominant”, “subject”,
  “session”

- Yes, each row is a single observation, for example each row is
  corrsponding to each participant at each moment, wether he/she
  pressing the key responding to sychronized and unsychronized
  heartrate.

- Yes, each cell is a single measurment, for example under “dominant”,
  each cell is corrsponding to whether the participant pressed the key
  under sycrhonized or unsychronized heartrate condition.

- It is tidy enough, I just need to drop some NAs.

- The dominant, duration, key, participant, sessions are useful variable
  for this analysis; onset and trails are not useful variables.

``` r
write.csv(final, "bids_dataset/compiled_part.csv",row.names = FALSE)
```

# Using dplyr functions

``` r
sub_data <- final %>% 
  mutate(duration = as.numeric(duration)) %>% 
  drop_na(duration) %>% 
  group_by(subject,dominant) %>% 
  summarize(total_duration = sum(duration, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'subject'. You can override using the
    ## `.groups` argument.

``` r
write.csv(sub_data, "bids_dataset/total_durations.csv",row.names = FALSE)
```

# Average duration

``` r
average_duration <- final %>%
  filter(!(duration == "n/a" | dominant == "n/a")) %>% 
  group_by(subject, dominant) %>%
  summarize(average_duration = mean(as.numeric(duration), na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'subject'. You can override using the
    ## `.groups` argument.

# Total duration for each participant

``` r
filtered_data <- final %>% 
  filter(!(duration == "n/a" | dominant == "n/a")) %>% 
  mutate(duration = as.numeric(duration)) %>%
  data.frame()
# debugging
# has_nas <- any(is.na(filtered_data$duration))
# has_nas
```

### Conducting analysis

``` r
# Example of the t-test
  
  # Perform a t-test comparing 'duration' between two groups of 'dominant'
  t_test_result <- t.test(average_duration ~ dominant, data = average_duration, paired = TRUE)
  
  # Display the t-test results
  print(t_test_result)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  average_duration by dominant
    ## t = -1.8347, df = 53, p-value = 0.07217
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.22701307  0.01011334
    ## sample estimates:
    ## mean difference 
    ##      -0.1084499

``` r
# t = -2.5272(p<0.05)  lme4/glmer
```

``` r
# Assuming your data frame is named df
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

``` r
# Fit a linear mixed-effects model
lmer_model <- lmer(average_duration ~ dominant + (1 | subject), data = average_duration)

# Print the summary of the model
summary(lmer_model)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: average_duration ~ dominant + (1 | subject)
    ##    Data: average_duration
    ## 
    ## REML criterion at convergence: 224.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2746 -0.2165 -0.0573  0.2086  4.8760 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  subject  (Intercept) 1.03172  1.0157  
    ##  Residual             0.09434  0.3072  
    ## Number of obs: 108, groups:  subject, 54
    ## 
    ## Fixed effects:
    ##                     Estimate Std. Error t value
    ## (Intercept)          2.64021    0.14441  18.283
    ## dominantsynchronous  0.10845    0.05911   1.835
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## dmnntsynchr -0.205

# Conducting paired t-test

``` r
average_duration[average_duration$dominant == 'synchronous',]
```

    ## # A tibble: 54 × 3
    ## # Groups:   subject [54]
    ##    subject dominant    average_duration
    ##      <int> <chr>                  <dbl>
    ##  1       1 synchronous             1.74
    ##  2       2 synchronous             3.20
    ##  3       3 synchronous             3.91
    ##  4       4 synchronous             3.73
    ##  5       5 synchronous             2.83
    ##  6       6 synchronous             1.21
    ##  7       7 synchronous             1.62
    ##  8       8 synchronous             4.21
    ##  9       9 synchronous             1.89
    ## 10      10 synchronous             9.63
    ## # ℹ 44 more rows

``` r
average_duration[average_duration$dominant == 'synchronous',]$average_duration
```

    ##  [1] 1.743880 3.201954 3.909915 3.730280 2.831163 1.208105 1.622574 4.213632
    ##  [9] 1.894324 9.625352 2.934931 2.086315 1.251889 2.117697 2.013234 2.440120
    ## [17] 3.349135 2.824185 2.017624 1.632808 2.603979 3.285624 3.430582 1.525600
    ## [25] 2.680354 2.876360 3.527354 2.180540 2.795232 3.485889 3.243082 2.397631
    ## [33] 3.776834 2.113516 2.193779 2.983784 2.577060 1.877041 3.408991 2.354018
    ## [41] 2.439674 1.938565 3.112015 3.278496 2.374210 2.624420 2.184329 2.083668
    ## [49] 3.157358 1.676587 3.088676 3.706757 2.647031 2.149433

``` r
sync <- average_duration[average_duration$dominant == 'synchronous',]$average_duration
async <- average_duration[average_duration$dominant == 'asynchronous',]$average_duration
t.test(sync, async, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  sync and async
    ## t = 1.8347, df = 53, p-value = 0.07217
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.01011334  0.22701307
    ## sample estimates:
    ## mean difference 
    ##       0.1084499

# Conducting one way t-test

``` r
t.test(sync, async, paired = TRUE, alternative = 'greater')
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  sync and async
    ## t = 1.8347, df = 53, p-value = 0.03609
    ## alternative hypothesis: true mean difference is greater than 0
    ## 95 percent confidence interval:
    ##  0.009489886         Inf
    ## sample estimates:
    ## mean difference 
    ##       0.1084499

# conducting Generalized Linear Mixed-Effects Models dominant + subject

``` r
glmer(duration ~ dominant + (1 + dominant|subject), 
      family = Gamma(link = 'log'), 
      data = filtered_data)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: Gamma  ( log )
    ## Formula: duration ~ dominant + (1 + dominant | subject)
    ##    Data: filtered_data
    ##       AIC       BIC    logLik  deviance  df.resid 
    ##  84544.07  84593.15 -42266.04  84532.07     26361 
    ## Random effects:
    ##  Groups   Name                Std.Dev. Corr
    ##  subject  (Intercept)         0.22353      
    ##           dominantsynchronous 0.07758  0.05
    ##  Residual                     0.69029      
    ## Number of obs: 26367, groups:  subject, 54
    ## Fixed Effects:
    ##         (Intercept)  dominantsynchronous  
    ##             0.92288              0.02574

``` r
fit <- glmer(duration ~ dominant + (1 + dominant|subject), 
             family = Gamma(link = 'log'), 
             data = filtered_data)
summary(fit)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: Gamma  ( log )
    ## Formula: duration ~ dominant + (1 + dominant | subject)
    ##    Data: filtered_data
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  84544.1  84593.2 -42266.0  84532.1    26361 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4156 -0.5947 -0.2137  0.3059 22.6403 
    ## 
    ## Random effects:
    ##  Groups   Name                Variance Std.Dev. Corr
    ##  subject  (Intercept)         0.049968 0.22353      
    ##           dominantsynchronous 0.006019 0.07758  0.05
    ##  Residual                     0.476495 0.69029      
    ## Number of obs: 26367, groups:  subject, 54
    ## 
    ## Fixed effects:
    ##                     Estimate Std. Error t value Pr(>|z|)    
    ## (Intercept)          0.92288    0.04369  21.123   <2e-16 ***
    ## dominantsynchronous  0.02574    0.01509   1.705   0.0881 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## dmnntsynchr 0.056

# conducting Generalized Linear Mixed-Effects Models dominant + subject

``` r
fit1 <- glmer(duration ~ dominant + (1|subject), 
              family = Gamma(link = 'log'), 
              data = filtered_data)
summary(fit1)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: Gamma  ( log )
    ## Formula: duration ~ dominant + (1 | subject)
    ##    Data: filtered_data
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  84599.8  84632.6 -42295.9  84591.8    26363 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4094 -0.5926 -0.2122  0.3085 22.7225 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  subject  (Intercept) 0.05314  0.2305  
    ##  Residual             0.48057  0.6932  
    ## Number of obs: 26367, groups:  subject, 54
    ## 
    ## Fixed effects:
    ##                     Estimate Std. Error t value Pr(>|z|)    
    ## (Intercept)         0.927540   0.045024  20.601  < 2e-16 ***
    ## dominantsynchronous 0.019123   0.007245   2.639  0.00831 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## dmnntsynchr -0.081

# conducting Generalized Linear Mixed-Effects Models key + subject

``` r
fit2 <- glmer(duration ~ dominant +  key + (1 + key|subject), 
             family = Gamma(link = 'log'), 
             data = filtered_data)
summary(fit2)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: Gamma  ( log )
    ## Formula: duration ~ dominant + key + (1 + key | subject)
    ##    Data: filtered_data
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  84140.0  84197.3 -42063.0  84126.0    26360 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4193 -0.5876 -0.2078  0.3101 22.3087 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  subject  (Intercept) 0.05704  0.2388        
    ##           keyright    0.02121  0.1456   -0.29
    ##  Residual             0.47277  0.6876        
    ## Number of obs: 26367, groups:  subject, 54
    ## 
    ## Fixed effects:
    ##                     Estimate Std. Error t value Pr(>|z|)    
    ## (Intercept)         0.906608   0.046824  19.362  < 2e-16 ***
    ## dominantsynchronous 0.019277   0.007174   2.687  0.00721 ** 
    ## keyright            0.032863   0.027494   1.195  0.23198    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) dmnnts
    ## dmnntsynchr -0.077       
    ## keyright    -0.282  0.000

# Graphing

### line plot

- In this plot, syn and asyn are on the axis.
- This is a point plot with line plot connect the points
- I am comparing the dominance(syn + asyn) across 54 participants
- I am expecting that the line should overall going up because
  participants should overall spend more time on syn stimulus

``` r
ggplot(average_duration, aes(x = dominant, 
                             y = average_duration,
                             group = subject,
                             color = factor(subject)))+
  geom_line(aes(linetype = factor(subject)), size = 1) +
  geom_point(size = 3) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", linetype = "dashed", color = "black", size = 1) +
  labs(title = "Average duration for syn and asyn",
       x = "Dominance",
       y = "Average Duration") +
  theme_minimal()
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Analysis_ECG_files/figure-gfm/line%20plot%20average%20syn%20vs%20asyn-1.png)<!-- -->

### Histogram

- The substracted average duration Syn - Asyn are on x-axis, the y axis
  represents count
- This is a histogram
- I am comparing the difference across 54 participants
- I am anticipate to see more results lies on \>0 because when Syn -
  Asyn should be positive values

``` r
average_diff <- average_duration %>%
  group_by(subject) %>%
  summarise(diff_duration = mean(average_duration[dominant == "synchronous"]) - mean(average_duration[dominant == "asynchronous"]))

ggplot(average_diff, aes(x = diff_duration)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Difference in Average Duration (Syn - Asyn)",
       x = "Difference in Duration",
       y = "Count") +
  theme_minimal()
```

![](Analysis_ECG_files/figure-gfm/Histogram%20substracted%20difference%20syn%20-%20asyn-1.png)<!-- -->
\### Below are my experimenting graphs…

``` r
# Still working on graphing
#dot_plot <- ggplot(average_duration, aes(x = dominant, y = average_duration)) +
#  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5, binwidth = 0.1, position = "jitter") +
#  labs(x = "dominant", y = "duration") +
#  theme_minimal()
#dot_plot
```

``` r
#ggplot(average_duration, aes(x = dominant, y = average_duration)) +
#  geom_boxplot() +
#  labs(x = "dominant", y = "duration") +
#  theme_minimal()
```

``` r
#ggplot(average_duration, aes(x = dominant, y = average_duration)) +
#  geom_violin() +
#  labs(x = "dominant", y = "duration") +
#  theme_minimal()
```

``` r
#ggplot(average_duration, aes(x = dominant, y = average_duration)) +
#  geom_point(position = position_jitter(width = 0.2)) +
#  labs(x = "dominant", y = "duration") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
