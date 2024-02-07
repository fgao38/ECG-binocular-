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
  group_by(subject, sync_side, dominant) %>%
  summarize(average_duration = mean(as.numeric(duration), na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'subject', 'sync_side'. You can override
    ## using the `.groups` argument.

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
    ## t = -1.7111, df = 107, p-value = 0.08997
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.25273600  0.01856601
    ## sample estimates:
    ## mean difference 
    ##       -0.117085

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
    ## REML criterion at convergence: 477.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.6456 -0.3954 -0.0449  0.3498  3.0874 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  subject  (Intercept) 1.0261   1.013   
    ##  Residual             0.2591   0.509   
    ## Number of obs: 216, groups:  subject, 54
    ## 
    ## Fixed effects:
    ##                     Estimate Std. Error t value
    ## (Intercept)          2.65511    0.14629   18.15
    ## dominantsynchronous  0.11708    0.06927    1.69
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## dmnntsynchr -0.237

# Conducting paired t-test

``` r
average_duration[average_duration$dominant == 'synchronous',]
```

    ## # A tibble: 108 × 4
    ## # Groups:   subject, sync_side [108]
    ##    subject sync_side dominant    average_duration
    ##      <int> <chr>     <chr>                  <dbl>
    ##  1       1 left      synchronous             1.54
    ##  2       1 right     synchronous             2.04
    ##  3       2 left      synchronous             3.22
    ##  4       2 right     synchronous             3.19
    ##  5       3 left      synchronous             4.45
    ##  6       3 right     synchronous             3.40
    ##  7       4 left      synchronous             4.85
    ##  8       4 right     synchronous             2.84
    ##  9       5 left      synchronous             3.18
    ## 10       5 right     synchronous             2.54
    ## # ℹ 98 more rows

``` r
average_duration[average_duration$dominant == 'synchronous',]$average_duration
```

    ##   [1] 1.5381747 2.0408492 3.2204919 3.1850272 4.4514676 3.4017917 4.8506552
    ##   [8] 2.8405700 3.1788764 2.5374043 1.5669746 0.9558916 1.3458174 1.9246347
    ##  [15] 4.0288723 4.3904742 1.7714777 2.0256165 9.5714462 9.6705168 3.2625604
    ##  [22] 2.6489501 2.1240639 2.0469255 1.3510059 1.1711681 2.1362545 2.0999013
    ##  [29] 2.0662479 1.9612535 2.4456740 2.4342911 2.6635223 4.2484459 2.5605051
    ##  [36] 3.1142330 2.1455448 1.9104911 1.3568758 1.9318725 2.7010978 2.5068611
    ##  [43] 3.4209738 3.1640848 3.5305408 3.3248775 1.3456866 1.7584931 2.1389284
    ##  [50] 3.4488301 2.4371686 3.4253482 3.3569464 3.7313941 2.4876966 1.8755009
    ##  [57] 2.8590758 2.7361171 2.9254591 4.1643049 3.0878120 3.3932337 2.1778271
    ##  [64] 2.6439639 3.6703511 3.9003531 2.4664165 1.7883432 1.7605971 2.7407622
    ##  [71] 2.8004603 3.1746675 2.2277481 3.0630598 1.5832928 2.2509029 3.3293866
    ##  [78] 3.4913722 2.4428135 2.2652230 2.3625592 2.5137992 1.4868848 2.5909928
    ##  [85] 3.1837988 3.0241852 3.7446250 2.8173249 2.2501946 2.4943194 1.5512955
    ##  [92] 3.8479834 2.1046538 2.2514526 1.9905369 2.1788844 2.6446685 3.6543530
    ##  [99] 1.6248060 1.7305125 3.0733311 3.1033745 4.7708524 3.0321966 2.8552472
    ## [106] 2.4388150 2.1283307 2.1719119

``` r
sync <- average_duration[average_duration$dominant == 'synchronous',]$average_duration
async <- average_duration[average_duration$dominant == 'asynchronous',]$average_duration
t.test(sync, async, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  sync and async
    ## t = 1.7111, df = 107, p-value = 0.08997
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.01856601  0.25273600
    ## sample estimates:
    ## mean difference 
    ##        0.117085

# Conducting one way t-test

``` r
t.test(sync, async, paired = TRUE, alternative = 'greater')
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  sync and async
    ## t = 1.7111, df = 107, p-value = 0.04498
    ## alternative hypothesis: true mean difference is greater than 0
    ## 95 percent confidence interval:
    ##  0.003547533         Inf
    ## sample estimates:
    ## mean difference 
    ##        0.117085

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
  stat_summary(aes(group = 1), fun = mean, geom = "line", linetype = "solid", color = "black", size = 1) +
  labs(title = "Average duration for syn and asyn",
       x = "Dominance",
       y = "Average Duration",
       subtitle = "n=54") +
  facet_wrap(~sync_side, scales = "free") +
  theme(panel.background = element_rect(fill = 'mistyrose'),
        panel.grid.major = element_line(linetype = 'dashed', color="grey"),
        panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
        axis.text = element_text(colour = "black", 
                                 face = "italic", 
                                 family = "Times New Roman"),
        strip.background = element_rect(fill = "darkgreen"),
        strip.text = element_text(color = "white", hjust = 0),
        text = element_text(family = "Times New Roman"))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![Average Duration for Syn and
Asyn](Analysis_ECG_files/figure-gfm/line%20plot%20average%20syn%20vs%20asyn-1.png)

### Histogram

- The substracted average duration Syn - Asyn are on x-axis, the y axis
  represents count
- This is a histogram
- I am comparing the difference across 54 participants
- I am anticipate to see more results lies on \>0 because when Syn -
  Asyn should be positive values

``` r
average_diff <- average_duration %>%
  group_by(subject, sync_side) %>%
  summarise(diff_duration = mean(average_duration[dominant == "synchronous"]) - mean(average_duration[dominant == "asynchronous"]))
```

    ## `summarise()` has grouped output by 'subject'. You can override using the
    ## `.groups` argument.

``` r
ggplot(average_diff, aes(x = diff_duration)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Difference in Average Duration (Syn - Asyn)",
       x = "Difference in Duration",
       y = "Count",
       subtitle = "n=54") +
  facet_wrap(~sync_side, scales = "free") +
  theme(panel.background = element_rect(fill = 'mistyrose'),
        panel.grid.major = element_line(linetype = 'dashed', color="grey"),
        panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
        axis.text = element_text(colour = "black", 
                                 face = "italic", 
                                 family = "Times New Roman"),
        strip.background = element_rect(fill = "darkgreen"),
        strip.text = element_text(color = "white", hjust = 0),
        text = element_text(family = "Times New Roman"))
```

![Histogram for substracted
difference](Analysis_ECG_files/figure-gfm/Histogram%20substracted%20difference%20syn%20-%20asyn-1.png)

``` r
#ggplot(average_duration, aes(x = sync_side, y = average_duration, fill = dominant)) +
#  geom_boxplot() +
#  labs(title = "Syn vs. Average Duration",
#       x = "Syn",
#       y = "Average Duration",
#       fill = "Sync_side") +
# coord_cartesian(ylim = c(0, 6))
```

### Below are my experimenting graphs…

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
