Project
================

## Libraries

``` r
library(ggraph)
library(igraph)

library(arrow)
library(tidyverse)
library(gender)
library(wru)
library(lubridate)

library(ggplot2)
library(gridExtra)
library(grid)

library(stargazer)
```

## ——————————————————————

# Data cleaning & Preprocessing section

## Data

``` r
data_path <- "Data/"
applications <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
edges <- read_csv(paste0(data_path,"edges_sample.csv"))
```

## Add gender

``` r
# get a list of first names without repetitions
examiner_names <- applications %>% 
  distinct(examiner_name_first)

# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
# remove extra columns from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
applications <- applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4708538 251.5    8239330 440.1  4728084 252.6
    ## Vcells 49741747 379.5   95701116 730.2 80057306 610.8

## Add race

``` r
# get list of distinct last names
examiner_surnames <- applications %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

``` r
# infer racial probabilities from surname tibble
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

# removing extra columns and merge into application data
examiner_race <- examiner_race %>% 
  select(surname,race)

applications <- applications %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
# cleanup
rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  5048551 269.7    8239330 440.1  5687703 303.8
    ## Vcells 53428306 407.7   95701116 730.2 94215858 718.9

## Add tenure

``` r
# get all application filing dates
examiner_dates <- applications %>% 
  select(examiner_id, filing_date, appl_status_date) 

# calculate start and end date from filing / status date respectively
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))

# for each examiner, get earliest and latest days, then interval between them as tenure in days
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    ) %>% 
  filter(year(latest_date)<2018)

# merge and clean
applications <- applications %>% 
  left_join(examiner_dates, by = "examiner_id")

rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  5062601 270.4   14709752  785.6  14709752  785.6
    ## Vcells 65807066 502.1  137985606 1052.8 137865166 1051.9

## Add application duration

``` r
# Since an application can only be issued or abandoned, one or the other will always be NA, therefore I will combine the columns

applications$appl_end_date <- paste(applications$patent_issue_date, applications$abandon_date, sep=',')

# Then I will clean up the column by removing instances of commas and NA's
applications$appl_end_date <- gsub('NA', "", as.character(applications$appl_end_date))
applications$appl_end_date <- gsub(',', "", as.character(applications$appl_end_date))

# Ensure date format is consistent for both columns
applications$appl_end_date <- as.Date(applications$appl_end_date, format="%Y-%m-%d")
applications$filing_date <- as.Date(applications$filing_date, format="%Y-%m-%d")

# Finding the difference in days between the application end date and the filing date
applications$appl_proc_days <- as.numeric(difftime(applications$appl_end_date, applications$filing_date, units=c("days")))

# Remove instances where the filing date happens after the issue or abandon dates (these must be mistakes as this shouldnt be possible
applications <- applications %>% filter(appl_proc_days >=0 & !is.na(appl_proc_days))

gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  4733176 252.8   14709752  785.6  14709752  785.6
    ## Vcells 61599944 470.0  138687648 1058.2 138687648 1058.2

Check completeness of the dataset to this point

``` r
library(skimr)
applications %>% skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 1688681    |
| Number of columns                                | 23         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 11         |
| Date                                             | 6          |
| numeric                                          | 6          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim\_variable         | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:-----------------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| application\_number    |          0 |           1.00 |   8 |   8 |     0 |   1688681 |          0 |
| examiner\_name\_last   |          0 |           1.00 |   2 |  17 |     0 |      3747 |          0 |
| examiner\_name\_first  |          0 |           1.00 |   1 |  12 |     0 |      2549 |          0 |
| examiner\_name\_middle |     390396 |           0.77 |   1 |  12 |     0 |       512 |          0 |
| uspc\_class            |          4 |           1.00 |   3 |   3 |     0 |       413 |          0 |
| uspc\_subclass         |       1555 |           1.00 |   6 |   6 |     0 |      6093 |          0 |
| patent\_number         |     601857 |           0.64 |   4 |   7 |     0 |   1086823 |          0 |
| disposal\_type         |          0 |           1.00 |   3 |   3 |     0 |         2 |          0 |
| appl\_status\_date     |        356 |           1.00 |  18 |  18 |     0 |      5680 |          0 |
| gender                 |     253871 |           0.85 |   4 |   6 |     0 |         2 |          0 |
| race                   |          0 |           1.00 |   5 |   8 |     0 |         5 |          0 |

**Variable type: Date**

| skim\_variable      | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:--------------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| filing\_date        |          0 |           1.00 | 2000-01-02 | 2017-03-24 | 2008-03-14 |      6045 |
| patent\_issue\_date |     601383 |           0.64 | 2000-06-06 | 2017-06-20 | 2012-05-22 |       890 |
| abandon\_date       |    1087295 |           0.36 | 2000-03-07 | 2050-06-30 | 2011-04-19 |      5040 |
| earliest\_date      |      18240 |           0.99 | 2000-01-02 | 2015-02-26 | 2000-05-12 |      2244 |
| latest\_date        |      18240 |           0.99 | 2000-09-14 | 2017-12-06 | 2017-05-20 |       865 |
| appl\_end\_date     |          0 |           1.00 | 2000-03-07 | 2050-06-30 | 2011-12-27 |      5053 |

**Variable type: numeric**

| skim\_variable      | n\_missing | complete\_rate |     mean |       sd |    p0 |   p25 |   p50 |   p75 |  p100 | hist  |
|:--------------------|-----------:|---------------:|---------:|---------:|------:|------:|------:|------:|------:|:------|
| examiner\_id        |       3746 |           1.00 | 78650.65 | 13611.68 | 59012 | 66481 | 75149 | 93760 | 99990 | ▇▆▃▂▇ |
| examiner\_art\_unit |          0 |           1.00 |  1918.68 |   300.06 |  1600 |  1657 |  1771 |  2166 |  2498 | ▇▂▁▂▂ |
| appl\_status\_code  |        355 |           1.00 |   164.38 |    30.73 |    16 |   150 |   150 |   161 |   854 | ▇▁▁▁▁ |
| tc                  |          0 |           1.00 |  1867.83 |   294.43 |  1600 |  1600 |  1700 |  2100 |  2400 | ▇▁▁▂▂ |
| tenure\_days        |      18240 |           0.99 |  5636.92 |   987.07 |   216 |  5128 |  6184 |  6337 |  6518 | ▁▁▁▂▇ |
| appl\_proc\_days    |          0 |           1.00 |  1190.28 |   620.63 |     0 |   765 |  1079 |  1481 | 17898 | ▇▁▁▁▁ |

Given that our goal is to measure the relationship between centrality
and application processing time, there are a few variables here that may
be worth imputing to remove NaNs.

-   Gender
-   tenure days
-   appl days

We will use R’s mice package which performs multiple imputation under
the assumption that any missing data is ‘Missing At Random’ ie the
probability that a value is missing depends only on the observed value
itself. Mice will impute data for each input variable by specifying a
unique imputation model per-variable. Ie if our feature set consists of
X1, X2, … Xn and X1 has missing values, it will be imputed based on the
patterns observed in X2….Xn.

Before we do this, we have to remove some variables which may be missing
not-at-random, or are deemed to be unhelpful for the later modelling
stage.

``` r
applications_subs = subset(applications, select=-c(examiner_name_middle,patent_number, appl_status_date,patent_issue_date,abandon_date,earliest_date,latest_date, filing_date))
# Removal explanations:

# some people might not have a middle name by choice (ie it was not just randomly forgotten to be entered into the data base)
# missing patent number means no patent issues, not missing at random
# appl_status_date for the same reason as patent number, and all of the related date-measurements arising from this

# we remove the remaining date columns since we already have the metrics we need from them (tenure and application processing time), # and mice does not play well with date columns

# we want examiner_id to remain unique which will not be the case if we allow mice to impute it, so we have no choice but to drop the missing examinerid rows
applications_subs = applications_subs %>% drop_na(examiner_id)
```

``` r
library(mice)
md.pattern(applications_subs)
```

![](Project_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ##         application_number examiner_name_last examiner_name_first examiner_id
    ## 1422502                  1                  1                   1           1
    ## 247595                   1                  1                   1           1
    ## 12017                    1                  1                   1           1
    ## 2471                     1                  1                   1           1
    ## 288                      1                  1                   1           1
    ## 56                       1                  1                   1           1
    ## 3                        1                  1                   1           1
    ## 3                        1                  1                   1           1
    ##                          0                  0                   0           0
    ##         examiner_art_unit uspc_class uspc_subclass disposal_type tc race
    ## 1422502                 1          1             1             1  1    1
    ## 247595                  1          1             1             1  1    1
    ## 12017                   1          1             1             1  1    1
    ## 2471                    1          1             1             1  1    1
    ## 288                     1          1             1             1  1    1
    ## 56                      1          1             1             1  1    1
    ## 3                       1          1             1             1  1    1
    ## 3                       1          1             1             1  1    1
    ##                         0          0             0             0  0    0
    ##         appl_end_date appl_proc_days appl_status_code tenure_days gender       
    ## 1422502             1              1                1           1      1      0
    ## 247595              1              1                1           1      0      1
    ## 12017               1              1                1           0      1      1
    ## 2471                1              1                1           0      0      2
    ## 288                 1              1                0           1      1      1
    ## 56                  1              1                0           1      0      2
    ## 3                   1              1                0           0      1      2
    ## 3                   1              1                0           0      0      3
    ##                     0              0              350       14494 250125 264969

``` r
# there are 1696847 observations with no missing values (84% of the dataset)
# another 14% has just one missing value (gender)
# the remaining 2% of missing values is composed of the other features

applications_subs$gender = as.factor(applications_subs$gender) # mice will only impute on categorically-defined columns and numericals

applications_full = complete(mice(applications_subs, m=3, maxit=3)) # impute using default mice imputation, 3 iterations (mice will decide the appropriate model for each column). This will be ~5-10 minutes
```

    ## 
    ##  iter imp variable
    ##   1   1  appl_status_code  gender  tenure_days
    ##   1   2  appl_status_code  gender  tenure_days
    ##   1   3  appl_status_code  gender  tenure_days
    ##   2   1  appl_status_code  gender  tenure_days
    ##   2   2  appl_status_code  gender  tenure_days
    ##   2   3  appl_status_code  gender  tenure_days
    ##   3   1  appl_status_code  gender  tenure_days
    ##   3   2  appl_status_code  gender  tenure_days
    ##   3   3  appl_status_code  gender  tenure_days

``` r
rm(applications_subs)

applications_full %>% skim() # all done
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 1684935    |
| Number of columns                                | 15         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 7          |
| Date                                             | 1          |
| factor                                           | 1          |
| numeric                                          | 6          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim\_variable        | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:----------------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| application\_number   |          0 |              1 |   8 |   8 |     0 |   1684935 |          0 |
| examiner\_name\_last  |          0 |              1 |   2 |  17 |     0 |      3746 |          0 |
| examiner\_name\_first |          0 |              1 |   1 |  12 |     0 |      2548 |          0 |
| uspc\_class           |          0 |              1 |   3 |   3 |     0 |       412 |          0 |
| uspc\_subclass        |          0 |              1 |   6 |   6 |     0 |      6090 |          0 |
| disposal\_type        |          0 |              1 |   3 |   3 |     0 |         2 |          0 |
| race                  |          0 |              1 |   5 |   8 |     0 |         5 |          0 |

**Variable type: Date**

| skim\_variable  | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:----------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| appl\_end\_date |          0 |              1 | 2000-04-07 | 2050-06-30 | 2011-12-27 |      5003 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts               |
|:---------------|-----------:|---------------:|:--------|----------:|:--------------------------|
| gender         |          0 |              1 | FALSE   |         2 | mal: 1133969, fem: 550966 |

**Variable type: numeric**

| skim\_variable      | n\_missing | complete\_rate |     mean |       sd |    p0 |   p25 |   p50 |   p75 |  p100 | hist  |
|:--------------------|-----------:|---------------:|---------:|---------:|------:|------:|------:|------:|------:|:------|
| examiner\_id        |          0 |              1 | 78650.65 | 13611.68 | 59012 | 66481 | 75149 | 93760 | 99990 | ▇▆▃▂▇ |
| examiner\_art\_unit |          0 |              1 |  1918.94 |   300.12 |  1600 |  1657 |  1771 |  2166 |  2498 | ▇▂▁▂▂ |
| appl\_status\_code  |          0 |              1 |   164.39 |    30.75 |    18 |   150 |   150 |   161 |   854 | ▇▁▁▁▁ |
| tc                  |          0 |              1 |  1868.08 |   294.48 |  1600 |  1600 |  1700 |  2100 |  2400 | ▇▁▁▂▂ |
| tenure\_days        |          0 |              1 |  5638.22 |   986.14 |   216 |  5131 |  6185 |  6337 |  6518 | ▁▁▁▂▇ |
| appl\_proc\_days    |          0 |              1 |  1192.41 |   619.59 |     0 |   768 |  1081 |  1482 | 17898 | ▇▁▁▁▁ |

## With our remaining values imputed, we can proceed with demographics & constructing our advice network and calculating centralities

## ——————————————————————————–

# Demographics Insights

``` r
# filter for unique examiners only 
final <- distinct(applications_full, examiner_id, .keep_all = TRUE)

# isolate for specific workgroups
final$wg = substr(final$examiner_art_unit, 1,3)

# create dataframes consisting of our specific workgroups 
WG_219 <- final[final$wg == 219, ] 
WG_162 <- final[final$wg == 162, ]
```

## Race

### Summarize Race Distribution by Working Group

``` r
# obtain raw percentage of race by working group
WG219_Race <- WG_219 %>%
  group_by(race) %>%
  summarise(WorkGroup = "Work Group 219", count = n()) %>%
  mutate(percentage  = round(count / sum(count), 2)) %>% 
  arrange(desc(percentage))
head(WG219_Race)
```

    ## # A tibble: 4 x 4
    ##   race     WorkGroup      count percentage
    ##   <chr>    <chr>          <int>      <dbl>
    ## 1 Asian    Work Group 219    86       0.49
    ## 2 white    Work Group 219    72       0.41
    ## 3 Hispanic Work Group 219    11       0.06
    ## 4 black    Work Group 219     5       0.03

``` r
WG162_Race <- WG_162 %>%
  group_by(race) %>%
  summarise(WorkGroup = "Work Group 162", count = n()) %>%
  mutate(percentage = round(count / sum(count), 2)) %>% 
  arrange(desc(percentage))
head(WG162_Race)
```

    ## # A tibble: 4 x 4
    ##   race     WorkGroup      count percentage
    ##   <chr>    <chr>          <int>      <dbl>
    ## 1 white    Work Group 162   138       0.72
    ## 2 Asian    Work Group 162    38       0.2 
    ## 3 black    Work Group 162    10       0.05
    ## 4 Hispanic Work Group 162     7       0.04

``` r
# join both together
comps_perc <- rbind(WG219_Race, WG162_Race)
```

### Visualization of Race by Working Group

``` r
# visualization of race by working group as a function of percentage
ggplot(comps_perc, aes(x=race, y=percentage, fill=WorkGroup)) + 
  geom_bar(stat="identity", position="dodge") + 
  # specify the color palette
  scale_fill_manual(values=c("#1a3260","#969fa7")) + 
  labs(title = "Race by Working Group", 
       subtitle= "Overview of Race Distribution by Organic Chemistry (162) and Interprocess Communication and Software Development (219) Working Groups",
       caption = "Data source: United States Patent and Trademark Office") +
  ylab("Percentage of Examiners") + 
  xlab("Race") + 
  # adjust title + subtitle formatting 
  theme(plot.title = element_text(color = "#1a3260", size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(color = "#585858", size = 8, hjust =0.5),
        plot.caption = element_text(color = "#585858", size = 6, face = "italic", hjust =0.9)) +
  # add labels
  geom_text(aes(label=paste("(Cnt:",count,", Pct:",percentage,")")),
            colour = "white", size = 2,
            vjust = 1.5, position = position_dodge(.9))
```

![](Project_files/figure-gfm/race-visualization-1.png)<!-- -->

## Gender

### Summarize Gender Distribution by Working Group

``` r
# obtain raw count and percentage of gender by working group
WG219_Gender <- WG_219 %>%
  group_by(gender) %>%
  summarise(WorkGroup = "Work Group 219", count = n()) %>%
  mutate(percentage = round(count / sum(count), 2)) %>% 
  arrange(desc(percentage))
head(WG219_Gender)
```

    ## # A tibble: 2 x 4
    ##   gender WorkGroup      count percentage
    ##   <fct>  <chr>          <int>      <dbl>
    ## 1 male   Work Group 219   146       0.84
    ## 2 female Work Group 219    28       0.16

``` r
WG162_Gender <- WG_162 %>%
  group_by(gender) %>%
  summarise(WorkGroup = "Work Group 162", count = n()) %>%
  mutate(percentage = round(count / sum(count), 2)) %>% 
  arrange(desc(percentage))
head(WG162_Gender)
```

    ## # A tibble: 2 x 4
    ##   gender WorkGroup      count percentage
    ##   <fct>  <chr>          <int>      <dbl>
    ## 1 male   Work Group 162   110       0.57
    ## 2 female Work Group 162    83       0.43

``` r
# join both together
comps_perc <- rbind(WG219_Gender, WG162_Gender)
```

### Visualization of Gender by Working Group

``` r
# visualization of race by working group as a function of percentage
ggplot(comps_perc, aes(x=gender, y=percentage, fill=WorkGroup)) + 
  geom_bar(stat="identity", position="dodge") +
  # specify the color palette 
  scale_fill_manual(values=c("#1a3260","#969fa7")) + 
  labs(title = "Gender by Working Group", 
       subtitle= "Overview of Gender Distribution by Organic Chemistry (162) and Interprocess Communication and Software Development (219) Working Groups",
       caption = "Data source: United States Patent and Trademark Office") +
  ylab("Percentage of Examiners") + 
  xlab("Gender") + 
  # adjust title + subtitle formatting 
  theme(plot.title = element_text(color = "#1a3260", size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(color = "#585858", size = 8, hjust =0.5),
        plot.caption = element_text(color = "#585858", size = 6, face = "italic", hjust =0.9)) +
  # add labels
  geom_text(aes(label=paste("(Cnt:",count,", Pct:",percentage,")")),
            colour = "white", size = 2,
            vjust = 1.5, position = position_dodge(.9))
```

![](Project_files/figure-gfm/gender-visualization-1.png)<!-- -->

## Tenure

## Summarize Examiner Tenure by Race and Gender

``` r
# generate new variable which looks at average tenure days by race and gender
WG219_GenderRace <- WG_219 %>%
   group_by(gender, race) %>% 
   summarise(tenure_days = mean(tenure_days), count = n())
  
WG162_GenderRace <- WG_162 %>%
   group_by(gender, race) %>% 
   summarise(tenure_days = mean(tenure_days), count = n()) 

# generate a new variable to describe the workgroups
WG219_GenderRace$WorkGroup <- "Work Group 219"
WG162_GenderRace$WorkGroup <- "Work Group 162"

# rename gender for more clear visualization
WG219_GenderRace <- WG219_GenderRace %>%
    mutate(gender = recode(gender, female = "FM", male = "ML"))
WG162_GenderRace <- WG162_GenderRace %>%
    mutate(gender = recode(gender, female = "FM", male = "ML"))

# create new variable that is a combination of both 
WG219_GenderRace$gender_race <- paste(WG219_GenderRace$gender, WG219_GenderRace$race)
WG162_GenderRace$gender_race <- paste(WG162_GenderRace$gender, WG162_GenderRace$race)

# round the average tenure days 
WG219_GenderRace$tenure_days <- round(WG219_GenderRace$tenure_days,digit=2) 
WG162_GenderRace$tenure_days <- round(WG162_GenderRace$tenure_days,digit=2) 

# add together
aggregate <- rbind(WG219_GenderRace, WG162_GenderRace)
```

### Visualization of Tenure by Race and Gender

``` r
ggplot(aggregate, aes(x=gender_race, y=tenure_days, fill=WorkGroup)) + 
  geom_bar(stat="identity", position="dodge") + 
  # specify the color palette
  scale_fill_manual(values=c("#1a3260","#969fa7")) + 
  labs(title = "Average Tenure for Gender + Race by Working Group", 
       subtitle= "Overview of Average tenure days by Gender and Race for Organic Chemistry (162) and Interprocess Communication and Software Development (219) Working Groups",
       caption = "Data source: United States Patent and Trademark Office") +
  ylab("Average Days of Tenure of Examiners") + 
  xlab("Gender + Race") + 
  # adjust title + subtitle formatting 
  theme(plot.title = element_text(color = "#1a3260", size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(color = "#585858", size = 8, hjust =0.5),
        plot.caption = element_text(color = "#585858", size = 6, face = "italic", hjust =0.9)) +
  # add labels
  geom_text(aes(label=paste("(",count,",",tenure_days,")")),
            colour = "white", size = 2,
            vjust = 1.5, position = position_dodge(.9))
```

![](Project_files/figure-gfm/tenure-race-gender-visualization-1.png)<!-- -->

## Processing Days

## Summarize Examiner Processing Days by Race and Gender

``` r
# generate new variable which looks at average processing days by race and gender
WG219_GenderRace <- WG_219 %>%
   group_by(gender, race) %>% 
   summarise_at(vars("appl_proc_days"), mean)
WG162_GenderRace <- WG_162 %>%
   group_by(gender, race) %>% 
   summarise_at(vars("appl_proc_days"), mean)

# generate a new variable to describe the workgroups
WG219_GenderRace$WorkGroup <- "Work Group 212"
WG162_GenderRace$WorkGroup <- "Work Group 162"

# rename gender for more clear visualization
WG219_GenderRace <- WG219_GenderRace %>%
    mutate(gender = recode(gender, female = "FM", male = "ML"))
WG162_GenderRace <- WG162_GenderRace %>%
    mutate(gender = recode(gender, female = "FM", male = "ML"))

# create new variable that is a combination of both 
WG219_GenderRace$gender_race <- paste(WG219_GenderRace$gender, WG219_GenderRace$race)
WG162_GenderRace$gender_race <- paste(WG162_GenderRace$gender, WG162_GenderRace$race)

# round the average tenure days 
WG219_GenderRace$appl_proc_days <- round(WG219_GenderRace$appl_proc_days,digit=2) 
WG162_GenderRace$appl_proc_days <- round(WG162_GenderRace$appl_proc_days,digit=2) 

# add together
aggregate <- rbind(WG219_GenderRace, WG162_GenderRace)
```

### Visualization of Processing by Working Group, Race and Gender

``` r
aggregate <- rbind(WG219_GenderRace, WG162_GenderRace)
ggplot(aggregate, aes(x=gender_race, y=appl_proc_days, fill=WorkGroup)) + geom_bar(stat="identity", position="dodge") +
  # specify color
  scale_fill_manual(values=c("#1a3260","#969fa7")) + 
  labs(title = "Average Processing Days for Patent Applications by Race and Gender",
       subtitle= "Overview of Average Processing Days for Patent Applications by Gender and Race for 
       Organic Chemistry (162) and Interprocess Communication and Software Development (219) Working Groups",
       caption = "Data source: United States Patent and Trademark Office") +
  ylab("Average Processing days for Patent Applications") + 
  xlab("Gender + Race") + 
  # adjust title + subtitle formatting 
  theme(plot.title = element_text(color = "#1a3260", size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(color = "#585858", size = 8, hjust =0.5),
        plot.caption = element_text(color = "#585858", size = 6, face = "italic", hjust =0.9)) +
  # add labels
  geom_text(aes(label = appl_proc_days),
            colour = "white", size = 2,
            vjust = 1.5, position = position_dodge(.9))
```

![](Project_files/figure-gfm/processsing-race-gender-visualization-1.png)<!-- -->

## Processing Days Overview

``` r
# isolate for working groups 
combined <- subset(final, wg==219 | wg==162)

# violin plot
ggplot(combined, aes(wg, appl_proc_days)) +
  geom_violin() + 
  geom_boxplot(width = .1, fill = "#969fa7", outlier.shape = NA) +
  stat_summary(fun.y = "median", geom = "point", col = "#1a3260") +
  labs(title = "Violin Plot of Processing Days for Patent Applications",
       subtitle= "Distribution and Density of Processing Days for Patent Applications for 
       Organic Chemistry (162) and Interprocess Communication and Software Development (219) Working Groups",
       caption = "Data source: United States Patent and Trademark Office") +
  ylab("Processing Days for Patent Applications") + 
  xlab("Work Group") + 
  # adjust title + subtitle formatting 
  theme(plot.title = element_text(color = "#1a3260", size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(color = "#585858", size = 8, hjust =0.5),
        plot.caption = element_text(color = "#585858", size = 6, face = "italic", hjust =0.9))
```

![](Project_files/figure-gfm/processing-violin-1.png)<!-- -->

## Processing Days Overview - Boxplot

``` r
# define function to return label for facet_wrap WG titles
label_facet <- function(original_var, custom_name){
  lev <- levels(as.factor(original_var))
  lab <- paste0(custom_name, ": ", lev)
  names(lab) <- lev
  return(lab)}

# box plot
combined %>%
  ggplot(aes(race, appl_proc_days, color = gender)) +
  geom_boxplot(width = .4, fill = "#36454F", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#36454F","#969fa7")) +
  scale_color_manual(values = c("#969fa7","#1a3260")) +
  geom_jitter(width=0.15, alpha=0.5) +
  labs(title = "Boxplot of Processing Days for Patent Applications",
       subtitle= "Boxplot of Processing Days by Gender and Race for Patent Applications for 
       Organic Chemistry (162) and Interprocess Communication and Software Development (219) Working Groups",
       caption = "Data source: United States Patent and Trademark Office") +
  ylab("Processing Days for Patent Applications") + 
  xlab("Race") + 
  # adjust title + subtitle formatting 
  theme(plot.title = element_text(color = "#1a3260", size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(color = "#585858", size = 8, hjust =0.5),
        plot.caption = element_text(color = "#585858", size = 6, face = "italic", hjust =0.9)) +
  facet_wrap( ~ wg, labeller = labeller(wg = label_facet(combined$wg, "Work Group")))
```

![](Project_files/figure-gfm/processing-box-1.png)<!-- -->

## ——————————————————————————–

------------------------------------------------------------------------

# Advice Networks

``` r
# first get work group for each examiner and limit to our two wgs of interest
examiner_aus = distinct(subset(applications_full, select=c(examiner_art_unit, examiner_id)))
# we eventually want to make a network with nodes colored by work group, so lets add that indicator
examiner_aus$wg = substr(examiner_aus$examiner_art_unit, 1,3)
# restrict down to our selected art units to reduce merging complexity later on
# examiner_aus = examiner_aus[examiner_aus$wg==163 | examiner_aus$wg==176,]

# now we will merge in the aus df on applications 
adviceNet = merge(x=edges, y=examiner_aus, by.x="ego_examiner_id", by.y="examiner_id", all.x=TRUE)
adviceNet = adviceNet %>% rename(ego_art_unit=examiner_art_unit, ego_wg=wg)

# drop edges which are missing ego or alter id
adviceNet = drop_na(adviceNet)

# now repeat for the alter examiners
adviceNet = merge(x=adviceNet, y=examiner_aus, by.x="alter_examiner_id", by.y="examiner_id", all.x=TRUE)
adviceNet = adviceNet %>% rename(alter_art_unit=examiner_art_unit, alter_wg=wg)
adviceNet = drop_na(adviceNet)

egoNodes = subset(adviceNet, select=c(ego_examiner_id,ego_art_unit, ego_wg)) %>%   rename(examiner_id=ego_examiner_id,art_unit=ego_art_unit,wg=ego_wg)
alterNodes = subset(adviceNet, select=c(alter_examiner_id,alter_art_unit, alter_wg))%>% rename(examiner_id=alter_examiner_id,art_unit=alter_art_unit,wg=alter_wg)
nodes = rbind(egoNodes, alterNodes)
nodes = distinct(nodes) #5412 examiners(but some are repeated because they move amongst art units)

# when we reduce to the list of distinct vertices, we actually have more than we should, since some examiners move amongst art units/wgs in this data subset
nodes = nodes %>% group_by(examiner_id) %>% summarise(examiner_id=first(examiner_id), art_unit=first(art_unit), wg=first(wg))
# we are left with just 2400 unique examiners
```

## Construct network and calculate centralities

``` r
adviceNet = graph_from_data_frame(d=adviceNet, vertices=nodes, directed=TRUE)
# centralities
Degree <- degree(adviceNet, v=V(adviceNet))
Betweenness <- betweenness(adviceNet)
Eigenvector <- evcent(adviceNet)$vector

V(adviceNet)$size = Degree
V(adviceNet)$eig = round(Eigenvector,2)
V(adviceNet)$bet = round(Betweenness,2)
```

## Model the relationship between centralities and app\_proc\_time

``` r
# first we'll need to merge the centrality measurements back into the imputed applications set
centralities <- cbind(Degree, Eigenvector, Betweenness)
centralities = round(centralities,2)
centralities = data.frame(centralities)
centralities <- cbind(examiner_id = rownames(centralities), centralities)
rownames(centralities) <- 1:nrow(centralities)

centralities %>% skim() # no missing values but very heavily skewed towards 0 for all centrality measures
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 2387       |
| Number of columns                                | 4          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 1          |
| numeric                                          | 3          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| examiner\_id   |          0 |              1 |   5 |   5 |     0 |      2387 |          0 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |   mean |      sd |  p0 | p25 | p50 | p75 |     p100 | hist  |
|:---------------|-----------:|---------------:|-------:|--------:|----:|----:|----:|----:|---------:|:------|
| Degree         |          0 |              1 |  91.68 |  190.96 |   1 |   9 |  28 |  89 |  2844.00 | ▇▁▁▁▁ |
| Eigenvector    |          0 |              1 |   0.00 |    0.03 |   0 |   0 |   0 |   0 |     1.00 | ▇▁▁▁▁ |
| Betweenness    |          0 |              1 | 291.07 | 2549.03 |   0 |   0 |   0 |   0 | 62399.84 | ▇▁▁▁▁ |

``` r
# now merge on examiner_id
applications_final = merge(x=applications_full, y=centralities, by="examiner_id", all.x=TRUE)
applications_final %>% skim() # we will have quite a few NaNs popping back up for those examiners who didnt ask any advice
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 1684935    |
| Number of columns                                | 18         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 7          |
| Date                                             | 1          |
| factor                                           | 1          |
| numeric                                          | 9          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim\_variable        | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:----------------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| application\_number   |          0 |              1 |   8 |   8 |     0 |   1684935 |          0 |
| examiner\_name\_last  |          0 |              1 |   2 |  17 |     0 |      3746 |          0 |
| examiner\_name\_first |          0 |              1 |   1 |  12 |     0 |      2548 |          0 |
| uspc\_class           |          0 |              1 |   3 |   3 |     0 |       412 |          0 |
| uspc\_subclass        |          0 |              1 |   6 |   6 |     0 |      6090 |          0 |
| disposal\_type        |          0 |              1 |   3 |   3 |     0 |         2 |          0 |
| race                  |          0 |              1 |   5 |   8 |     0 |         5 |          0 |

**Variable type: Date**

| skim\_variable  | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:----------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| appl\_end\_date |          0 |              1 | 2000-04-07 | 2050-06-30 | 2011-12-27 |      5003 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts               |
|:---------------|-----------:|---------------:|:--------|----------:|:--------------------------|
| gender         |          0 |              1 | FALSE   |         2 | mal: 1133969, fem: 550966 |

**Variable type: numeric**

| skim\_variable      | n\_missing | complete\_rate |     mean |       sd |    p0 |   p25 |   p50 |   p75 |     p100 | hist  |
|:--------------------|-----------:|---------------:|---------:|---------:|------:|------:|------:|------:|---------:|:------|
| examiner\_id        |          0 |           1.00 | 78650.65 | 13611.68 | 59012 | 66481 | 75149 | 93760 | 99990.00 | ▇▆▃▂▇ |
| examiner\_art\_unit |          0 |           1.00 |  1918.94 |   300.12 |  1600 |  1657 |  1771 |  2166 |  2498.00 | ▇▂▁▂▂ |
| appl\_status\_code  |          0 |           1.00 |   164.39 |    30.75 |    18 |   150 |   150 |   161 |   854.00 | ▇▁▁▁▁ |
| tc                  |          0 |           1.00 |  1868.08 |   294.48 |  1600 |  1600 |  1700 |  2100 |  2400.00 | ▇▁▁▂▂ |
| tenure\_days        |          0 |           1.00 |  5638.22 |   986.14 |   216 |  5131 |  6185 |  6337 |  6518.00 | ▁▁▁▂▇ |
| appl\_proc\_days    |          0 |           1.00 |  1192.41 |   619.59 |     0 |   768 |  1081 |  1482 | 17898.00 | ▇▁▁▁▁ |
| Degree              |     656339 |           0.61 |    97.77 |   177.10 |     1 |    12 |    35 |   102 |  2844.00 | ▇▁▁▁▁ |
| Eigenvector         |     656339 |           0.61 |     0.00 |     0.03 |     0 |     0 |     0 |     0 |     1.00 | ▇▁▁▁▁ |
| Betweenness         |     656339 |           0.61 |   317.50 |  2560.50 |     0 |     0 |     0 |     1 | 62399.84 | ▇▁▁▁▁ |

``` r
# nothing to do there but remove the missing values
applications_final = drop_na(applications_final)

# clean
rm(examiner_aus)
rm(egoNodes)
rm(alterNodes)
rm(nodes)
rm(adviceNet)
gc()
```

    ##             used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells   5001599 267.2   14709752  785.6  14709752  785.6
    ## Vcells 105803712 807.3  331930789 2532.5 414911925 3165.6

## ——————————————————————————–

------------------------------------------------------------------------

# Modelling

``` r
# we wish to model the relationship between various centralities and appl_days
# we will make our first model as a simplistic model assuming no interactions among predictors
lm1 = lm(appl_proc_days~Degree+Eigenvector+Betweenness+tenure_days+gender+race, data=applications_final)
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = appl_proc_days ~ Degree + Eigenvector + Betweenness + 
    ##     tenure_days + gender + race, data = applications_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1383.1  -429.3  -113.5   294.2  4962.1 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.531e+03  5.520e+00 277.391  < 2e-16 ***
    ## Degree        7.033e-02  3.725e-03  18.881  < 2e-16 ***
    ## Eigenvector  -1.606e+02  2.397e+01  -6.700 2.08e-11 ***
    ## Betweenness   6.806e-03  2.493e-04  27.303  < 2e-16 ***
    ## tenure_days  -4.896e-02  9.010e-04 -54.340  < 2e-16 ***
    ## gendermale    1.466e+01  1.356e+00  10.818  < 2e-16 ***
    ## raceblack    -3.051e+01  3.154e+00  -9.672  < 2e-16 ***
    ## raceHispanic  1.619e+01  4.446e+00   3.641 0.000272 ***
    ## raceother     4.186e+01  3.508e+01   1.193 0.232757    
    ## racewhite    -6.497e+01  1.352e+00 -48.044  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 628 on 1028586 degrees of freedom
    ## Multiple R-squared:  0.007136,   Adjusted R-squared:  0.007127 
    ## F-statistic: 821.4 on 9 and 1028586 DF,  p-value: < 2.2e-16

Interpretations: - The “baseline” expectation for application processing
time is 1500 - That would be for a female asian examiner who just
started, 0 tenure days, and has never asked any advice

-   Everytime an examiner asks advice to a new colleague examiner
    (increase degree by 1), we expect processing time to increase
    slightly (.07 days)

-   Increasing an examiner’s importance as measured by eigenvector
    centrality is expected to decrease processing time by 160 days

-   Increasing an examiner’s betweenness increases the processing time
    slightly (less than a day)

-   It is important to note that the centrality measurements are all
    coupled, so in a vacuum these interpretations are valid, but in
    practice we could not increase an examiner’s degree without also
    altering in some way their eigenvector and betweenness centralities

-   Longer tenured examiners process applications a bit faster with each
    additional day of tenure

-   Male examiners are expected to take roughly 2 weeks longer than
    their female counterparts

-   Black, Hispanic, and Other-raced examiners all take longer to
    process than asian

-   White examiners process applications much faster than Asian, by
    about 60 days

-   Important to note the out goodness of fit is very low, so these
    insights should be taken with a grain of salt

We can try to capture some of the more complex relationships among
predictors by adding interactions

``` r
lm2 = lm(appl_proc_days~Degree+Eigenvector+Betweenness+tenure_days+gender+race+Degree*gender+Eigenvector*gender+Betweenness*gender+Degree*race+Eigenvector*race+Betweenness*race, data=applications_final)
summary(lm2)
```

    ## 
    ## Call:
    ## lm(formula = appl_proc_days ~ Degree + Eigenvector + Betweenness + 
    ##     tenure_days + gender + race + Degree * gender + Eigenvector * 
    ##     gender + Betweenness * gender + Degree * race + Eigenvector * 
    ##     race + Betweenness * race, data = applications_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1416.2  -429.1  -113.4   294.2  4958.6 
    ## 
    ## Coefficients: (4 not defined because of singularities)
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               1.515e+03  5.597e+00 270.667  < 2e-16 ***
    ## Degree                    2.054e-01  8.485e-03  24.210  < 2e-16 ***
    ## Eigenvector              -4.533e+03  8.971e+02  -5.053 4.34e-07 ***
    ## Betweenness               1.534e-03  7.871e-04   1.949  0.05133 .  
    ## tenure_days              -4.812e-02  9.025e-04 -53.321  < 2e-16 ***
    ## gendermale                1.789e+01  1.537e+00  11.638  < 2e-16 ***
    ## raceblack                -3.178e+01  3.660e+00  -8.681  < 2e-16 ***
    ## raceHispanic              5.596e+01  5.425e+00  10.315  < 2e-16 ***
    ## raceother                 4.207e+01  3.507e+01   1.199  0.23034    
    ## racewhite                -5.041e+01  1.545e+00 -32.626  < 2e-16 ***
    ## Degree:gendermale        -4.846e-02  8.197e-03  -5.911 3.40e-09 ***
    ## Eigenvector:gendermale    1.909e+03  2.256e+02   8.462  < 2e-16 ***
    ## Betweenness:gendermale    5.200e-03  7.025e-04   7.402 1.35e-13 ***
    ## Degree:raceblack         -1.685e-02  2.622e-02  -0.643  0.52045    
    ## Degree:raceHispanic      -4.504e-01  3.870e-02 -11.639  < 2e-16 ***
    ## Degree:raceother                 NA         NA      NA       NA    
    ## Degree:racewhite         -1.528e-01  8.149e-03 -18.752  < 2e-16 ***
    ## Eigenvector:raceblack     3.055e+04  3.842e+03   7.952 1.83e-15 ***
    ## Eigenvector:raceHispanic         NA         NA      NA       NA    
    ## Eigenvector:raceother            NA         NA      NA       NA    
    ## Eigenvector:racewhite     2.587e+03  8.727e+02   2.965  0.00303 ** 
    ## Betweenness:raceblack     1.364e-02  2.594e-03   5.258 1.46e-07 ***
    ## Betweenness:raceHispanic -4.144e-02  9.257e-03  -4.476 7.61e-06 ***
    ## Betweenness:raceother            NA         NA      NA       NA    
    ## Betweenness:racewhite     1.461e-03  5.365e-04   2.723  0.00647 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 627.8 on 1028575 degrees of freedom
    ## Multiple R-squared:  0.007858,   Adjusted R-squared:  0.007839 
    ## F-statistic: 407.3 on 20 and 1028575 DF,  p-value: < 2.2e-16

-   The baseline expectation is roughly the same as it was before,
    around 1500 days

-   Increasing degree or betweenness centrality (in a vaccuum) is
    expected to increase processing time, while increasing eigenvector
    centrality decreases processing time quite significantly (4500 days)

-   This model expects Black examiners to process faster than asian
    examiners, and every other race to be slower

-   From interaction terms, we also know that increasing degree for male
    examiners decreases processing time significantly

-   Male examiners with higher betweenness centrality have roughly the
    same expected processing time (0.005 days added)

Disclaimer: While these models are providing theoretically meaningful
insights, we should note that the proportion of variance in the data
explained by both of these models is around 1%, ie they are not
particularly good models as far as goodness-of-fit is concerned.

## Workgroup-specific analysis

After completing the general USPTO analysis, we have chosen to zoom in
on two tech units: 1600 and 2100. We wanted to look at the STEM field
and specifically the differences between life-science related patents
(1600: Biotech and Organic Fields) and compute-science related patents
(2100: Computer Architecture and Information Security)

We will use workgroups 162 and 219 as the representative work groups for
these two tech units, and randomly sample from the larger workgroup to
get two approximately evenly sized workgroup data sets.

``` r
# first get work group for each examiner and limit to our two wgs of interest
examiner_aus = distinct(subset(applications_full, select=c(examiner_art_unit, examiner_id,gender)), examiner_id, .keep_all=TRUE)

# note we want distinct examiners, not just distinct art_unit+examiner combos, since examiners can move between units. In this case we just take the first art unit an examiner worked in to simplify our analysis

# we eventually want to make a network with nodes colored by work group, so lets add that indicator
examiner_aus$wg = substr(examiner_aus$examiner_art_unit, 1,3)
# restrict down to our selected art units to reduce merging complexity later on
examiner_aus = examiner_aus[examiner_aus$wg==162 | examiner_aus$wg==219,]

# now we will merge in the aus df on applications 
adviceNet = merge(x=edges, y=examiner_aus, by.x="ego_examiner_id", by.y="examiner_id", all.x=TRUE)
adviceNet = adviceNet %>% rename(ego_art_unit=examiner_art_unit, ego_wg=wg, ego_gender=gender)

# drop edges which are missing ego or alter id
adviceNet = drop_na(adviceNet)

# now repeat for the alter examiners
adviceNet = merge(x=adviceNet, y=examiner_aus, by.x="alter_examiner_id", by.y="examiner_id", all.x=TRUE)
adviceNet = adviceNet %>% rename(alter_art_unit=examiner_art_unit, alter_wg=wg, alter_gender=gender)
adviceNet = drop_na(adviceNet)

egoNodes = subset(adviceNet, select=c(ego_examiner_id,ego_art_unit, ego_wg, ego_gender)) %>%   rename(examiner_id=ego_examiner_id,art_unit=ego_art_unit,wg=ego_wg, gender=ego_gender)
alterNodes = subset(adviceNet, select=c(alter_examiner_id,alter_art_unit, alter_wg, alter_gender))%>% rename(examiner_id=alter_examiner_id,art_unit=alter_art_unit,wg=alter_wg, gender=alter_gender)
nodes = rbind(egoNodes, alterNodes)
nodes = distinct(nodes) 

# note we have fewer examiners than we started with due to some examiners never asking each other for advice


# when we reduce to the list of distinct vertices, we actually have more than we should, since some examiners move amongst art units/wgs in this data subset
#nodes = nodes %>% group_by(examiner_id) %>% summarise(examiner_id=first(examiner_id), art_unit=first(art_unit), wg=first(wg))
```

## Repeat centralities analysis

### Construct network and calculate centralities

``` r
adviceNet = graph_from_data_frame(d=adviceNet, vertices=nodes, directed=TRUE)
# centralities
Degree <- degree(adviceNet, v=V(adviceNet))
Betweenness <- betweenness(adviceNet)
Eigenvector <- evcent(adviceNet)$vector

V(adviceNet)$size = Degree
V(adviceNet)$eig = round(Eigenvector,2)
V(adviceNet)$bet = round(Betweenness,2)
V(adviceNet)$wg = nodes$wg
V(adviceNet)$gender = as.character(nodes$gender)
```

### Visualize

``` r
ggraph(adviceNet, layout="kk") +
  geom_edge_link(arrow=arrow(length=unit(2,'mm')), end_cap=circle(1.2,'mm'))+
  geom_node_point(aes(size=size, color=wg, shape=gender), show.legend=T) +
  scale_color_manual(values=c('#1a3260', '#969fa7'))
```

![](Project_files/figure-gfm/unnamed-chunk-17-1.png)<!-- --> We have a
much sparser network here with many components instead of one or two
large components. This is likely due to the restrictive size of our
analysis, however it is still interesting to note the existence of these
cliques, especially given that for some examiners we have 15-20
instances of advice asking. This shows a clear preference amongst the
examiners in both 162 and 219 to stick with their local friend group
when resolving issues.

``` r
unique <- applications_final[!duplicated(applications_final[, c('examiner_id')]), ]
unique$wg = substr(unique$examiner_art_unit,1,3)
summary_df <- applications_final %>% group_by(examiner_id) %>% summarise(Applications = length(application_number), Tenure = mean(tenure_days), Avg_Proc_Time = mean(appl_proc_days), Degree = mean(Degree), Eig = mean(Eigenvector))

# ggplot(unique, aes(x=Degree, y=tenure_days)) +
#   geom_point(aes(color=as.factor(wg)), show.legend=T) +
#   scale_color_manual(values=c('#1a3260', '#969fa7'))
# 
# ggplot(summary_df, aes(x=Degree, y=Avg_Proc_Time)) +
#   geom_point() +
#   scale_color_manual(values=c('#1a3260'))
```

### Model the relationship between centralities and app\_proc\_time

``` r
# first we'll need to merge the centrality measurements back into the imputed applications set
centralities <- cbind(Degree, Eigenvector, Betweenness)
centralities = round(centralities,2)
centralities = data.frame(centralities)
centralities <- cbind(examiner_id = rownames(centralities), centralities)
rownames(centralities) <- 1:nrow(centralities)

centralities = merge(centralities, subset(examiner_aus, select=-c(wg,gender)), by="examiner_id") # need art unit for the final merge with applications

# now merge on examiner_id
applications_final = merge(x=applications_full, y=centralities, by=c("examiner_id","examiner_art_unit"), all.y=TRUE)
applications_final %>% skim() # we will have quite a few NaNs popping back up for those examiners who didnt ask any advice
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 19502      |
| Number of columns                                | 18         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 7          |
| Date                                             | 1          |
| factor                                           | 1          |
| numeric                                          | 9          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim\_variable        | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:----------------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| application\_number   |          0 |              1 |   8 |   8 |     0 |     19502 |          0 |
| examiner\_name\_last  |          0 |              1 |   2 |  16 |     0 |        38 |          0 |
| examiner\_name\_first |          0 |              1 |   3 |  11 |     0 |        35 |          0 |
| uspc\_class           |          0 |              1 |   3 |   3 |     0 |        86 |          0 |
| uspc\_subclass        |          0 |              1 |   6 |   6 |     0 |      1257 |          0 |
| disposal\_type        |          0 |              1 |   3 |   3 |     0 |         2 |          0 |
| race                  |          0 |              1 |   5 |   8 |     0 |         3 |          0 |

**Variable type: Date**

| skim\_variable  | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:----------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| appl\_end\_date |          0 |              1 | 2000-08-22 | 2017-06-20 | 2010-08-03 |      2928 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts           |
|:---------------|-----------:|---------------:|:--------|----------:|:----------------------|
| gender         |          0 |              1 | FALSE   |         2 | fem: 12193, mal: 7309 |

**Variable type: numeric**

| skim\_variable      | n\_missing | complete\_rate |     mean |      sd |    p0 |   p25 |   p50 |   p75 |  p100 | hist  |
|:--------------------|-----------:|---------------:|---------:|--------:|------:|------:|------:|------:|------:|:------|
| examiner\_id        |          0 |              1 | 70237.43 | 8351.03 | 59491 | 63822 | 67753 | 75034 | 98717 | ▇▇▂▁▁ |
| examiner\_art\_unit |          0 |              1 |  1744.65 |  231.70 |  1621 |  1624 |  1626 |  1627 |  2199 | ▇▁▁▁▂ |
| appl\_status\_code  |          0 |              1 |   172.65 |   38.43 |    30 |   150 |   161 |   161 |   454 | ▁▇▂▁▁ |
| tc                  |          0 |              1 |  1705.45 |  203.98 |  1600 |  1600 |  1600 |  1600 |  2100 | ▇▁▁▁▂ |
| tenure\_days        |          0 |              1 |  6027.99 |  520.54 |  1526 |  5872 |  6311 |  6345 |  6346 | ▁▁▁▁▇ |
| appl\_proc\_days    |          0 |              1 |  1100.76 |  611.09 |    96 |   651 |   985 |  1428 |  4981 | ▇▅▁▁▁ |
| Degree              |          0 |              1 |     2.68 |    3.02 |     1 |     1 |     1 |     3 |    24 | ▇▁▁▁▁ |
| Eigenvector         |          0 |              1 |     0.02 |    0.11 |     0 |     0 |     0 |     0 |     1 | ▇▁▁▁▁ |
| Betweenness         |          0 |              1 |     0.00 |    0.00 |     0 |     0 |     0 |     0 |     0 | ▁▁▇▁▁ |

``` r
# nothing to do there but remove the missing values
applications_final = drop_na(applications_final)
```

### Modelling

``` r
applications_final$wg = substr(applications_final$examiner_art_unit,1,3)
applications_final$wg = as.factor(applications_final$wg)

# rename gender var to fix a knitting error


#unique(applications_final$race) # Just Asian, White, or Hispanic examiners present in this dataset



# for our first model we will once again cover no interactions and just look at base variables
# also, we have dropped betweenness because it is 0 for all examiners, probably due to the lack of connectivity between clusters
lm1 = lm(appl_proc_days~Degree+Eigenvector+tenure_days+race+gender+wg, data=applications_final)
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = appl_proc_days ~ Degree + Eigenvector + tenure_days + 
    ##     race + gender + wg, data = applications_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1513.3  -366.7   -65.7   280.2  3924.5 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.025e+03  5.539e+01  18.505  < 2e-16 ***
    ## Degree        1.911e+01  2.386e+00   8.012  1.2e-15 ***
    ## Eigenvector  -5.811e+02  6.640e+01  -8.752  < 2e-16 ***
    ## tenure_days  -1.583e-02  8.707e-03  -1.818  0.06910 .  
    ## raceHispanic -1.031e+02  3.161e+01  -3.262  0.00111 ** 
    ## racewhite    -2.200e+01  9.498e+00  -2.316  0.02057 *  
    ## gendermale    1.484e+01  8.679e+00   1.710  0.08719 .  
    ## wg219         6.661e+02  1.089e+01  61.185  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 547.3 on 19494 degrees of freedom
    ## Multiple R-squared:  0.1982, Adjusted R-squared:  0.198 
    ## F-statistic: 688.6 on 7 and 19494 DF,  p-value: < 2.2e-16

Our work-group specific analysis gives much different results from
before

First, our baseline estimate (Female, Asian, 0 tenure days and no prior
connections) for application time is 1038 days

In addition, we assume a further increase in processing time for each
advice-sought by about 20 days

One notable insight is that examiners from work group 219 are expected
to take significantly longer in processing applications than for those
in workgroup 162. This could potentially be due to the larger size of
workgroup 162, allowing for lower on-average workload. It is also
possible the discrepency is due to a simple difference in the
nature/complexity of Biotech vs CS -oriented patents.

We also expect Hispanic and White examiners to complete applications
faster than Asian examiners.

Based on this simplistic model, we would naively conclude that the USPTO
should focus on hiring more Hispanic and White female examiners, as we
expect them to process applications much faster than all male examiners,
and especially faster than male asian examiners.

Of course, we know this model is missing the whole picture and we ought
to increase its complexity before making conclusions…

``` r
# add interactions
lm2 = lm(appl_proc_days~Degree+Eigenvector+tenure_days+gender+race+wg
         +gender*Degree+race*Degree+gender*Eigenvector+race*Eigenvector, data=applications_final)
summary(lm2)
```

    ## 
    ## Call:
    ## lm(formula = appl_proc_days ~ Degree + Eigenvector + tenure_days + 
    ##     gender + race + wg + gender * Degree + race * Degree + gender * 
    ##     Eigenvector + race * Eigenvector, data = applications_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1472.4  -365.7   -61.2   280.3  3763.0 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               1.373e+03  5.840e+01  23.516  < 2e-16 ***
    ## Degree                   -1.039e+02  8.893e+00 -11.686  < 2e-16 ***
    ## Eigenvector               2.076e+03  1.976e+02  10.505  < 2e-16 ***
    ## tenure_days              -5.176e-02  8.845e-03  -5.852 4.94e-09 ***
    ## gendermale                1.200e+02  1.329e+01   9.025  < 2e-16 ***
    ## raceHispanic             -2.219e+01  3.172e+01  -0.700 0.484170    
    ## racewhite                -2.243e+02  1.581e+01 -14.188  < 2e-16 ***
    ## wg219                     7.494e+02  1.168e+01  64.188  < 2e-16 ***
    ## Degree:gendermale        -5.511e+01  4.603e+00 -11.972  < 2e-16 ***
    ## Degree:raceHispanic              NA         NA      NA       NA    
    ## Degree:racewhite          1.532e+02  9.187e+00  16.675  < 2e-16 ***
    ## Eigenvector:gendermale    7.795e+03  2.212e+03   3.524 0.000425 ***
    ## Eigenvector:raceHispanic         NA         NA      NA       NA    
    ## Eigenvector:racewhite     4.567e+03  5.425e+04   0.084 0.932914    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 541.5 on 19490 degrees of freedom
    ## Multiple R-squared:  0.2151, Adjusted R-squared:  0.2147 
    ## F-statistic: 485.6 on 11 and 19490 DF,  p-value: < 2.2e-16

``` r
# several interactions are omitted due to insufficient data/not statistically significant results:
# gender*race, all combinations are not statistically significant, probably due to having only 38 unique examiners in the dataset
# tenure*gender
# tenure*race
stargazer(lm1, lm2, 
          type="latex",
          dep.var.labels = "Application Processing Time",
          covariate.labels= c("Degree Centrality", "Eigenvector Centrality", "Tenure (days)", "Male", "Hispanic", "White", "Work Group 219", "Degree:Male","Degree:Hispanic","Degree:White","Eigenvector:Male","Eigenvector:Hispanic","Eigenvector:White"),
          digits = 2,
          font.size="LARGE")
```

    ## 
    ## % Table created by stargazer v.5.2.2 by Marek Hlavac, Harvard University. E-mail: hlavac at fas.harvard.edu
    ## % Date and time: Sun, Jun 05, 2022 - 12:52:19 PM
    ## \begin{table}[!htbp] \centering 
    ##   \caption{} 
    ##   \label{} 
    ## \LARGE 
    ## \begin{tabular}{@{\extracolsep{5pt}}lcc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{2}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-3} 
    ## \\[-1.8ex] & \multicolumn{2}{c}{Application Processing Time} \\ 
    ## \\[-1.8ex] & (1) & (2)\\ 
    ## \hline \\[-1.8ex] 
    ##  Degree Centrality & 19.11$^{***}$ & $-$103.92$^{***}$ \\ 
    ##   & (2.39) & (8.89) \\ 
    ##   & & \\ 
    ##  Eigenvector Centrality & $-$581.07$^{***}$ & 2,075.68$^{***}$ \\ 
    ##   & (66.40) & (197.60) \\ 
    ##   & & \\ 
    ##  Tenure (days) & $-$0.02$^{*}$ & $-$0.05$^{***}$ \\ 
    ##   & (0.01) & (0.01) \\ 
    ##   & & \\ 
    ##  Male & $-$103.11$^{***}$ & $-$22.19 \\ 
    ##   & (31.61) & (31.72) \\ 
    ##   & & \\ 
    ##  Hispanic & $-$22.00$^{**}$ & $-$224.30$^{***}$ \\ 
    ##   & (9.50) & (15.81) \\ 
    ##   & & \\ 
    ##  White & 14.84$^{*}$ & 119.95$^{***}$ \\ 
    ##   & (8.68) & (13.29) \\ 
    ##   & & \\ 
    ##  Work Group 219 & 666.13$^{***}$ & 749.41$^{***}$ \\ 
    ##   & (10.89) & (11.68) \\ 
    ##   & & \\ 
    ##  Degree:Male &  & $-$55.11$^{***}$ \\ 
    ##   &  & (4.60) \\ 
    ##   & & \\ 
    ##  Degree:Hispanic &  &  \\ 
    ##   &  &  \\ 
    ##   & & \\ 
    ##  Degree:White &  & 153.20$^{***}$ \\ 
    ##   &  & (9.19) \\ 
    ##   & & \\ 
    ##  Eigenvector:Male &  & 7,795.05$^{***}$ \\ 
    ##   &  & (2,211.74) \\ 
    ##   & & \\ 
    ##  Eigenvector:Hispanic &  &  \\ 
    ##   &  &  \\ 
    ##   & & \\ 
    ##  Eigenvector:White &  & 4,567.20 \\ 
    ##   &  & (54,254.70) \\ 
    ##   & & \\ 
    ##  Constant & 1,025.00$^{***}$ & 1,373.25$^{***}$ \\ 
    ##   & (55.39) & (58.40) \\ 
    ##   & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & 19,502 & 19,502 \\ 
    ## R$^{2}$ & 0.20 & 0.22 \\ 
    ## Adjusted R$^{2}$ & 0.20 & 0.21 \\ 
    ## Residual Std. Error & 547.28 (df = 19494) & 541.54 (df = 19490) \\ 
    ## F Statistic & 688.58$^{***}$ (df = 7; 19494) & 485.64$^{***}$ (df = 11; 19490) \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

Our baseline estimate is higher, around 1400 days.

Examiners are now expected to take less processing time with each
additional advice-seeking, by 106 days. Eigenvector centrality has a
negative (longer) impact on processing time by 2117 days.(?) With each
additional day of tenure, female examiners shave about 0.05 days off
their expected processing time. Male examiners are expected to take
about 110 days longer than their female counterparts. Both hispanic and
White examiners are expected to process faster than their asian
colleagues. As before, examiners from workgroup 219 appear to require
much longer to process applications.

Among interaction terms, we expect male examiners to remove about 53
days of processing time when seeking advice (inc degree by 1) compared
to women. - This would seem to imply that “importance” as measured by
degree centrality is more meaningful for male examiners than it is for
female examiners

Lets investigate that insight with some predictions:

``` r
baseline = predict(lm2, data.frame(Degree=0,Eigenvector=0,tenure_days=0,gender='female',race='Asian',wg='162'))
lowDegMale = predict(lm2, data.frame(Degree=0,Eigenvector=0,tenure_days=0,gender='male',race='Asian',wg='162'))
lowDegFemale = baseline
highDegMale = predict(lm2, data.frame(Degree=5,Eigenvector=0,tenure_days=0,gender='male',race='Asian',wg='162'))
highDegFemale = predict(lm2, data.frame(Degree=5,Eigenvector=0,tenure_days=0,gender='female', race='Asian',wg='162'))

data.frame(baseline=baseline, unimportant_male=lowDegMale, important_male=highDegMale, unimportant_female=lowDegFemale, important_female=highDegFemale)
```

    ##   baseline unimportant_male important_male unimportant_female important_female
    ## 1 1373.249         1493.204       698.0651           1373.249         853.6543

This affirms what we saw when examining the model summary: men seem to
gain more benefit (in terms of reducing processing time) from advice
seeking than women. We can’t deduce why that is from this model, but
conjecture might say that since this is a male-dominated organization,
men seem to benefit (at least in terms of reducing processing time) from
advice-seeking more than women.

We can additionally see from the model summary that
