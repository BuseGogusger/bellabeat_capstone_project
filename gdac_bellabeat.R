# PREPARE PHASE
# In order to move forward first we need to prepare our dataset. 
# We will use "install.packages()" and "library()" functions to install and load R packages.

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("here")
install.packages("hms")
install.packages("janitor")
install.packages("lubridate")
install.packages("skimr")
install.packages("gridExtra")
install.packages("paletteer")
install.packages("crayon")
install.packages("stringr")
install.packages("patchwork")
install.packages("ggthemes")
install.packages("ggforce")
install.packages("personograph")
install.packages("ggbeeswarm")
install.packages("knitr")
install.packages("ggpubr")
install.packages("ggrepel")
install.packages("reshape2")
install.packages("scales")

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(reshape2)
library(scales)
library(here)
library(hms)
library(janitor)
library(lubridate)
library(skimr)
library(gridExtra)
library(paletteer)
library(crayon)
library(stringr)
library(patchwork)
library(ggthemes)
library(ggforce)
library(personograph)
library(ggbeeswarm)
library(knitr)

# In order to import our data we will use following code.
# And we will create data frames while importing.
# We chose these ones to summarize our data set better.

daily_activity <- read_csv("fitbit_data/dailyActivity_merged.csv")
daily_calories <- read_csv("fitbit_data/dailyCalories_merged.csv")
daily_intensities <- read_csv("fitbit_data/dailyIntensities_merged.csv")
daily_steps <- read_csv("fitbit_data/dailySteps_merged.csv")
daily_sleep <- read_csv("fitbit_data/sleepDay_merged.csv")
weight_log <- read_csv("fitbit_data/weightLogInfo_merged.csv")
hourly_calories <- read_csv("fitbit_data/hourlyCalories_merged.csv")
hourly_intensities <- read_csv("fitbit_data/hourlyIntensities_merged.csv")
hourly_steps <- read_csv("fitbit_data/hourlySteps_merged.csv")
minute_sleep <- read_csv("fitbit_data/minuteSleep_merged.csv")
heart_rate <- read_csv("fitbit_data/heartrate_seconds_merged.csv")


#PROCESS PHASE

# In order to understand more about our dataset and take a closer look, 
# we will be using “head()”, “colnames()” and my favorite one “str()” functions.

# head() displays the first couple rows present in the input data frame

head(daily_activity)
head(daily_calories)
head(daily_intensities)
head(daily_steps)
head(daily_sleep)
head(weight_log)
head(hourly_calories)
head(hourly_intensities)
head(hourly_steps)
head(minute_sleep)
head(heart_rate)

# colnames() returns or sets the names of the columns in a data frame.

colnames(daily_activity)
colnames(daily_calories)
colnames(daily_intensities)
colnames(daily_steps)
colnames(daily_sleep)
colnames(weight_log)
colnames(hourly_calories)
colnames(hourly_intensities)
colnames(hourly_steps)
colnames(minute_sleep)
colnames(heart_rate)

# str() compactly displays the internal structure of a R object.
# Which I love using while I am trying to get a broader sense of the data.

str(daily_activity)
str(daily_calories)
str(daily_intensities)
str(daily_steps)
str(daily_sleep)
str(weight_log)
str(hourly_calories)
str(hourly_intensities)
str(hourly_steps)
str(minute_sleep)
str(heart_rate)

# While we were checking our data, we saw some inconsistencies across the data frames.
# We will be addressing them while cleaning our data.
# So that we won't be facing any problems down the line while analyzing our data.

# daily_activity

daily_activity <-
  daily_activity %>% 
  rename(
    activity_date = ActivityDate, 
    total_steps = TotalSteps, 
    total_distance = TotalDistance,
    tracker_distance = TrackerDistance,
    logged_activities_dist = LoggedActivitiesDistance,
    very_active_dist = VeryActiveDistance, 
    moderately_active_dist = ModeratelyActiveDistance, 
    light_active_dist = LightActiveDistance, 
    sedentary_dist = SedentaryActiveDistance, 
    very_active_min = VeryActiveMinutes, 
    fairly_active_min = FairlyActiveMinutes, 
    lightly_active_min = LightlyActiveMinutes, 
    sedentary_min = SedentaryMinutes, 
    calories = Calories
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%y"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend")
  )
# daily_calories

daily_calories <-
  daily_calories %>% 
  rename(
    activity_date = ActivityDay,
    calories = Calories
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend")
  )
# daily_intensities

daily_intensities <-
  daily_intensities %>% 
  rename(
    activity_date = ActivityDay,
    very_active_dist = VeryActiveDistance, 
    moderately_active_dist = ModeratelyActiveDistance, 
    light_active_dist = LightActiveDistance, 
    sedentary_dist = SedentaryActiveDistance, 
    very_active_min = VeryActiveMinutes, 
    fairly_active_min = FairlyActiveMinutes, 
    lightly_active_min = LightlyActiveMinutes, 
    sedentary_min = SedentaryMinutes, 
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend")
  )
# daily_steps

daily_steps <-
  daily_steps %>% 
  rename(
    activity_date = ActivityDay,
    step_total = StepTotal
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend")
  )
# daily_sleep

daily_sleep <-
  daily_sleep %>% 
  rename(
    activity_date = SleepDay,
    total_sleep_records = TotalSleepRecords,
    total_minutes_asleep = TotalMinutesAsleep,
    total_time_in_bed = TotalTimeInBed
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend")
  )
# weight_log

weight_log <-
  weight_log %>% 
  rename(
    activity_date = Date,
    weight_kg = WeightKg,
    weight_lb = WeightPounds,
    fat = Fat,
    bmi = BMI,
    is_manual_report = IsManualReport,
    log_id = LogId
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    activity_time = format(activity_date, format = "%I:%M:%S %p"), 
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend"),
    hour_of_day = as.POSIXct(activity_date, format = "%I:%M:%S %p")
  )
# hourly_calories

hourly_calories <-
  hourly_calories %>% 
  rename(
    activity_hour = ActivityHour,
    calories = Calories
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_hour = parse_date_time(activity_hour, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_hour, "%Y/%m/%d"), 
    activity_time = format(activity_hour, format = "%I:%M:%S %p"),
    day_of_week = weekdays(as.Date(activity_hour)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend"),
    hour_of_day = as.POSIXct(activity_time, format = "%I:%M:%S %p")
  )
# hourly_intensities

hourly_intensities <- 
  hourly_intensities %>% 
  rename(
    activity_hour = ActivityHour,
    total_intensity = TotalIntensity,
    average_intensity = AverageIntensity
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_hour = parse_date_time(activity_hour, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_hour, "%Y/%m/%d"), 
    activity_time = format(activity_hour, format = "%I:%M:%S %p"),
    day_of_week = weekdays(as.Date(activity_hour)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend"),
    hour_of_day = as.POSIXct(activity_time, format = "%I:%M:%S %p")
  )
# hourly_steps

hourly_steps <- 
  hourly_steps %>% 
  rename(
    activity_hour = ActivityHour,
    step_total = StepTotal
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_hour = parse_date_time(activity_hour, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_hour, "%Y/%m/%d"), 
    activity_time = format(activity_hour, format = "%I:%M:%S %p"),
    day_of_week = weekdays(as.Date(activity_hour)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend"),
    hour_of_day = as.POSIXct(activity_time, format = "%I:%M:%S %p")
  )
# minute_sleep

minute_sleep <-
  minute_sleep %>% 
  rename(
    activity_date = date,
    sleep_value = value,
    log_id = logId
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_date, "%y/%m/%d"),
    activity_time = format(activity_date, format = "%I:%M:00 %p"), 
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend"),
    hour_of_day = as.POSIXct(activity_time, format = "%I:%M:%S %p"),
    # lets create sleep_id which will help us to generate totals
    sleep_id = str_c(id, "-", log_id), 
    # now we will create variables for sleep values
    asleep = ifelse(sleep_value == 1, 1, 0),
    restless = ifelse(sleep_value == 2, 1, 0),
    awake = ifelse(sleep_value == 3, 1, 0)
  )

#Lets create another data frame for total sleep quality to help us at the analyze phase.

sleep_quality <-
  minute_sleep %>%
  mutate(
    id_date = str_c(id, "-", activity_date_ymd)
  ) %>% 
  group_by(sleep_id, activity_date_ymd, id_date, id) %>% 
  summarize(
    total_asleep = sum(sleep_value == "1"),
    total_restless = sum(sleep_value == "2"),
    total_awake = sum(sleep_value == "3")
  )
# Now we need to create a sleep summary

sleep_summary <-
  sleep_quality %>%
  mutate(
    activity_date = parse_date_time(activity_date_ymd, "%Y/%m/%d")
  ) %>%
  group_by(id_date, activity_date, id) %>%
  summarize(
    total_asleep_merged = sum(total_asleep),
    total_restless_merged = sum(total_restless),
    total_awake_merged = sum(total_awake)
  )

sleep_data <- merge(x = daily_sleep, y = sleep_summary, by = c("id", "activity_date"), all = TRUE)


# heart_rate
heart_rate <-
  heart_rate %>% 
  rename(
    time = Time,
    value = Value
  ) %>%
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_hour = parse_date_time(time, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(time, "%y/%m/%d"), 
    activity_time = format(time, format = "%I:%M:%S %p"),
    day_of_week = weekdays(as.Date(time)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend"),
    hour_of_day = as.POSIXct(activity_time, format = "%I:%M:%S %p")
  )

# ANALYZE PHASE
# While we are moving forward with our task, we are now at the analysis phase. 
# We will start with some summary statistics to summarize our data to better understan and analyze it.

# LETS COUNT OUR DATA
# In our data frames how many unique id's we have?

n_distinct(daily_activity$id)
n_distinct(daily_steps$id)
n_distinct(daily_sleep$id)
n_distinct(minute_sleep$id)
n_distinct(sleep_quality$id)
n_distinct(sleep_summary$id)
n_distinct(sleep_data$id)
n_distinct(weight_log$id)
n_distinct(hourly_calories$id)
n_distinct(hourly_intensities$id)
n_distinct(hourly_steps$id)
n_distinct(heart_rate$id)

# By this we can see that our data frames how related to one another


# In our data frames, how many observations we have?
# We will have them by calculating rows

nrow(daily_activity)
nrow(daily_sleep)
nrow(daily_steps)
nrow(minute_sleep)
nrow(sleep_quality)
nrow(sleep_summary)
nrow(sleep_data)
nrow(weight_log)
nrow(hourly_calories)
nrow(hourly_intensities)
nrow(hourly_steps)
nrow(heart_rate)

# Our hourly observations has same number of observations 
# while the daily observations, minute_sleep and heart_rate observations vary.

# Also heart_rate, weight_log and minute_sleep dfs has very low number of outputs
# So we have to keep in mind that we can't base our analysis solely on these data

# TIME FOR SUMMARY STATISTICS

# Now we will pull some key statistics for overview our data for each df

# Daily total steps, calories, distance, active minute levels etc.

daily_activity %>%
  select(
    total_steps, total_distance, very_active_min, fairly_active_min, 
    lightly_active_min, sedentary_min, calories
  ) %>%
   summary()

# NOTES ON DAILY STATISTICS

# In our summary we can see that users' average daily steps is 7.638 
# and this number is below CDC's recommended daily healthy steps amount  
# which is 10.000.

# When we look at the categorical data about daily activities sedentary time of the users are concerning.
# Average 991.2 minutes per day for sedentary time which means 16.52 hours is critically high 
# and can cause several health problems. The Fitbit users need to be warned about taking a break from sedentary time in every 30 minutes as recommended.

# Their combined daily average of very active and fairly active minutes is 34.72,
# which falls between the recommended amount of moderate-intensity physical activity 
# (150-300 minutes per week) by WHO.

# On the other hand average user is burning 2.304 calories a day according to our summaries
# which is between the scales of general studies' average (1.600-2.400)


# Sleep data summaries

daily_sleep %>%
  select(
    total_sleep_records, total_minutes_asleep, total_time_in_bed
  ) %>%
   summary()

minute_sleep %>%
  select(
    sleep_value, asleep, restless, awake
  ) %>%
  summary()

sleep_quality %>%
  select(
    total_asleep, total_restless, total_awake
  ) %>%
  summary()

# NOTES ON SLEEP DATA STATISTICS

# Average sleep duration of the users is 419.5 which is nearly 7 hours.
# According to several articles conducted by AASM (American Academy of Sleep Medicine) 
# a healthy adult needs between 7 and 7 and a half hours sleep duration 
# in order to get proper amount of rest. 

# On average, users spend 458.6 minutes in bed, which means they spend an average of 39.1 minutes before 
# falling asleep. The National Heart, Lung, and Blood Institute (NHLBI) advises that 
# National Sleep Foundation says that spending more time awake in bed before falling asleep can diminish your desire for sleep and 
# disrupt your circadian rhythm. This is because your body starts to associate the bed with being awake rather than being asleep. 
# If you find yourself tossing and turning in bed, it is better to get out of bed and do something relaxing until you feel tired. 
# This will help you to fall asleep more quickly and get a better night's sleep.


# Heart rate data...

heart_rate %>%
  select(
    value
    ) %>%
   summary()

# NOTES ON HEART RATE DATA STATISTICS

# In the heart rate data frame we face the challenge about the low number of users.
# But despite the insufficient data from the heart rate data frame 77.83 beats per minute 
# fits the average amount of "normal" range. But we have to remember that heart rate is highly
# divergent value and can't be normalized according to population. So it needs to have more
# data and needs to be monitored more broadly. 

# Weight data...

weight_log %>%
  select(
   weight_kg,
   bmi
  ) %>%
  summary()

# NOTES ON WEIGHT DATA STATISTICS

# We face the same challenge about the weight log data as well. But in order to analyze 
# what we have at hand gives us a glimpse. 25.19 BMI is considered as an overweight 
# by CDC. 

# If we check about the average weight statistics of our users which is 72.04, 
# shows us that this average falls below the statistical average of CDC researches.


# Hourly calories, steps, intensities summary statistics...

hourly_calories %>%
  select(
    calories
  )%>%
  summary()

hourly_steps %>%
  select(
    step_total
  )%>%
  summary()
    
hourly_intensities %>%
  select(
    total_intensity, average_intensity
  )%>%
  summary()

# NOTES ON HOURLY DATA STATISTICS
# Statistical data shows that average hourly calories burned 97.39 for Fitbit users with 12 minutes intensity and 0.2 minutes
# average intensity per hour. 
# General daily activities of Fitbit users around light or sedentary category and average hourly intensities are low.
# And according to researches while sleeping you burn 40-55 calories per hour and a bit more while watching TV
# or reading books. 

#Lets Explore Our Data Some More

# First lets start about our daily data frames and dig a little bit deeper
# We mentioned about daily steps and active minutes amount before. Now we will take a look at
# the relationship between those and calories burned.

ggplot(daily_activity, aes(x = total_steps, y = calories)) +
  geom_point(color="lightsalmon1", position = "jitter" ,alpha = 0.5) +
  geom_smooth(color="aquamarine3") +
  labs(title = "Daily Steps Total vs Calories Burned") +
  ylab("Calories Burned") +
  xlab("Daily Steps Total") 

ggplot(daily_activity, aes(x = very_active_min, y = calories)) +
  geom_point(color="lightsalmon1", position = "jitter" ,alpha = 0.5) +
  geom_smooth(color="aquamarine3") + 
  labs(title = "Very Active Minutes vs Calories Burned") +
  ylab("Calories Burned") +
  xlab("Very Active Minutes")

cor.test(daily_activity$very_active_min, daily_activity$calories)
cor.test(daily_activity$total_steps, daily_activity$calories)

# In these two plots trend line with a shadow represents %95 confidence interval 
# between the variables.And in both of these plots variables have positive correlation.
# This means the more steps you take, more calories are burned and same goes for very active people as well.
# If you are a very active person, you will be burning more calories. Since we mentioned 
# correlations, we also calculated correlation coefficients above. 

# Lightly active minutes average was much more higher than very active minutes average in the previous
# analysis, so lets see the relationship between the calories burned and lightly active minutes.
# In order to differentiate we will also use another color for the trend line.

ggplot(daily_activity, aes(x = lightly_active_min, y = calories)) +
  geom_point(color="lightsalmon1", position = "jitter" ,alpha = 0.5) +
  geom_smooth(color="aquamarine3") +
  labs(title = "Lightly Active Minutes vs Calories Burned") +
  ylab("Calories Burned") +
  xlab("Lightly Active Minutes")

cor.test(daily_activity$lightly_active_min, daily_activity$calories)

# The relationship is still positive but it is much more weaker than the others as expected.

# Lets take another look at the daily data and this time analyze the relationship between
# daily sleep and sleep quality. Does is differentiate between the weekdays and weekends?
# Is there any different trends?


labels_for_days <- c(
  "Monday" = "Monday", "Tuesday" = "Tuesday",
  "Wednesday" = "Wednesday", "Thursday" = "Thursday",
  "Friday" = "Friday", "Saturday" = "Saturday",
  "Sunday" = "Sunday"
)
limits_for_days <- c(
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
  "Saturday", "Sunday"
)

ggplot(sleep_data) +
  geom_point(
    aes(
      x = weekdays.POSIXt(activity_date), 
      y = total_asleep_merged,
      color = as.factor(time_of_week)
    ),
      alpha = 0.5
  ) +
  labs(title = "Weekly Total Asleep Minutes") +
  guides(color = "none") +
  ylab("Asleep Minutes") +
  scale_x_discrete(
    "Day of the Week",
    labels = labels_for_days, 
    limits = limits_for_days
  ) +
  stat_summary(
    aes(x = weekdays.POSIXt(activity_date), y = total_asleep_merged),
    fun = mean, 
    geom = "point", 
    color = "brown3", 
    size = 2,
    alpha = 1
)


ggplot(sleep_data) +
  geom_point(
    aes(
      x = weekdays.POSIXt(activity_date), 
      y = total_restless_merged,
      color = as.factor(time_of_week)
    ),
    alpha = 0.5
  ) +
  labs(title = "Weekly Total Restless Minutes") +
  guides(color = "none") +
  ylab("Restless Minutes") +
  scale_x_discrete(
    "Day of the Week",
    labels = labels_for_days, 
    limits = limits_for_days
  ) +
  stat_summary(
    aes(x = weekdays.POSIXt(activity_date), y = total_restless_merged),
    fun = mean, 
    geom = "point", 
    color = "brown3", 
    size = 2,
    alpha = 1
  ) 

# On our graphs averages are shown in dark green dots. According to those dots 
# average total asleep minutes are highest on Saturday, Sunday (as expected) and Wednesday.
# Also average restless minutes are on Saturday and Sunday as well. Even though people tend to sleep more
# on weekends, it seems that quality of sleep is not better according to the restless minutes.

# Lastly, user usage data is highly important for us so lets check daily usage of FitBit.
# We will use hourly intansities data frame for it.

usage <- hourly_intensities %>%
  group_by(activity_date_ymd) %>%
  summarize(hourly_usage= n()/33)

# Lets visualize it to better understand

ggplot(usage, aes(activity_date_ymd, hourly_usage))+
  geom_bar(stat="identity", fill="lightsalmon1")+
  labs(title ="Daily Usage of The Device", 
       x = "Dates",
       y = "Hours of the Day")+
  scale_x_date(date_breaks = ("1 day"),
               labels= date_format ("%d-%b"),
               guide = guide_axis(angle = 60))+
  scale_y_continuous(limits = c(0,24),
                     breaks = seq(0,max(usage$hourly_usage),by=2))

# When we check our graph we can see that after 17 days, users started to use the device lesser,
# and with 26th day usage starts to decrease dramatically.


# Activity Minutes Percentages
v_a_min <- sum(daily_activity$very_active_min)
f_a_min <- sum(daily_activity$fairly_active_min)
l_a_min <- sum(daily_activity$lightly_active_min)
s_min <- sum(daily_activity$sedentary_min)
total_min <- sum(v_a_min+f_a_min+l_a_min+s_min)

activity_min_perc <- c(v_a_min,l_a_min,f_a_min, s_min)
labels_min <- c("Very Active","Lightly Active","Fairly Active","Sedentary")
pct <- round(activity_min_perc/total_min*100)
labels_min <- paste(labels_min,pct)
labels_min <- paste(labels_min, "%")
pie(activity_min_perc,labels_min, srt=1 ,main="User Activity Percentages",col= c("coral4","coral3","coral","lightsalmon"))

