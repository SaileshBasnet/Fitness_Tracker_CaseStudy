# Setting up environment
# Loading library
library(dplyr)      # Data manipulation
library(janitor)    # Data cleaning
library(lubridate)  # Date handling
library(hms)        # Time management
library(skimr)      # Data summary
library(ggplot2)    # Data visualization
library(tidyverse)  # Unified ecosystem

# Loading csv files
daily_activity <- read.csv("D:\\Docs\\Google Data Analytics\\fitbit_fitness_tracker\\dailyActivity_merged.csv")
head(daily_activity)
names(daily_activity)
str(daily_activity)

# More data exploration
glimpse(daily_activity)
skim_without_charts(daily_activity)
as_tibble(daily_activity)
summary(daily_activity)
View(daily_activity)

# Data Cleaning
# Change id is in num so changing it into factor
daily_activity$Id <- as.factor(daily_activity$Id)

# Change chr activity date into date format
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate,format="%m/%d/%Y")

# Confirming the changes
str(daily_activity)

# Check for missing value
any(is.na(daily_activity))

# Check total missing value
sum(is.na(daily_activity))


# Check for duplicate value
daily_activity_V2 <- distinct(daily_activity)
View(daily_activity_V2)

# Check for values less than or equal to -1 in each column
apply(daily_activity_V2, 2, function(x) any(x <= -1))


# Analysis
# Basic stats
# Average of Steps, Distance & Calorie
daily_activity_V2  %>% 
  summarise(avg_TotalSteps=mean(TotalSteps),avg_TotalDistance=mean(TotalDistance),
            avg_Calories=mean(Calories))

# Maximum of Steps, Distance & Calorie
daily_activity_V2 %>% 
  summarise(max_TotalSteps=max(TotalSteps),max_TotalDistance=max(TotalDistance),
            max_Calories=max(Calories))

# Median of Steps, Distance & Calorie
daily_activity_V2 %>% 
  summarise(med_TotalSteps=median(TotalSteps),med_TotalDistance=median(TotalDistance),
            med_Calories=median(Calories))

# Standard deviation of Steps, Distance & Calorie
daily_activity_V2 %>% 
  summarise(sd_TotalSteps= sd(TotalSteps),sd_TotalDistance=sd(TotalDistance),
            sd_Calories=sd(Calories),sd_TrackerDistance=sd(TrackerDistance))

# User-Specific Analysis
daily_activity_V2 %>% group_by(Id) %>% 
  summarise(avg_steps=mean(TotalSteps),avg_distance=mean(TotalDistance),avg_calorie=mean(Calories))

# Classify users based on how many days they used their smart device during a 31-day survey period
# Active User
user_active_days <- daily_activity_V2 %>% group_by(Id) %>%
  summarise(active_days=n_distinct(ActivityDate))

# Classify users based on active days
user_classification <- user_active_days %>%
  mutate( usage_category = case_when(active_days >= 21 ~ "High user",active_days >= 10 ~ "Moderate user",
                                     active_days<=10 ~ "Low user")
  )

# Count users in each category
user_counts <- user_classification %>%group_by(usage_category) %>% 
  summarise(active_days=n())

## Calculate percentage
user_counts <- user_counts %>%
  mutate(percentage = active_days / sum(active_days) * 100)

# Trend analysis
# Highest no of step by date
daily_activity_V2 %>% select(ActivityDate,TotalSteps) %>% arrange(desc(TotalSteps))

# High no of step by date
daily_activity_V2 %>% select(ActivityDate, Calories) %>% arrange(desc(Calories))

# High no of distance covered by date
daily_activity_V2 %>% select(ActivityDate,TotalDistance) %>% arrange(desc(TotalDistance))

# Creating a col for week name
daily_activity_V2$week_name <-  weekdays(daily_activity_V2$ActivityDate)
head(daily_activity_V2)

#Arranging the data of day_of_week in sunday,monday,... format
daily_activity_V2$week_name <- ordered(daily_activity_V2$week_name, 
                                       levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
# Weekly Analysis 
# Calorie burnt by week 
daily_activity_V2 %>% group_by(week_name) %>% summarise(total_calorie=sum(Calories))

# Total Steps by week
daily_activity_V2 %>% group_by(week_name) %>% summarise(total_Steps=sum(TotalSteps))

# Total distance by week
daily_activity_V2 %>% group_by(week_name) %>% 
  summarise(total_distance=sum(TotalDistance)) 

#Weekday vs weekend 
daily_activity_V2 %>% 
  mutate(day_type=if_else(week_name %in% c("Sunday","Saturday"),"WeekEnd","WeekDay")) %>% 
  group_by(day_type) %>% 
  summarise(avg_steps=mean(TotalSteps),avg_distance=mean(TotalDistance),avg_calorie=mean(Calories))

# Summary 
summary(daily_activity_V2[, c("TrackerDistance", "LoggedActivitiesDistance", 
                              "VeryActiveDistance", "ModeratelyActiveDistance",
                              "LightActiveDistance", "SedentaryActiveDistance")])


# Correlation matrix to see how strongly each distance type is related to others
cor(daily_activity_V2[, c("TrackerDistance", "LoggedActivitiesDistance", 
                          "VeryActiveDistance","ModeratelyActiveDistance",
                          "LightActiveDistance", "SedentaryActiveDistance")])

# Correlation 
cor(daily_activity_V2[,c("TotalSteps","TotalDistance","Calories")])

# Variance
daily_activity_V2 %>% 
  summarise(var_TrackerDistance = var(TrackerDistance), var_LoggedActivitiesDistance = var(LoggedActivitiesDistance),
            var_VeryActiveDistance = var(VeryActiveDistance), var_ModeratelyActiveDistance = var(ModeratelyActiveDistance),
            var_LightActiveDistance = var(LightActiveDistance), var_SedentaryActiveDistance = var(SedentaryActiveDistance))


# Top 5 active users
daily_activity_V2 %>% group_by(Id) %>% summarise(active_days=n_distinct(ActivityDate)) %>% 
  arrange(desc(active_days)) %>% slice_head(n=5)

# Least 5 active users
daily_activity_V2 %>% group_by(Id) %>% summarise(active_days=n_distinct(ActivityDate)) %>% 
  arrange(active_days) %>% slice_head(n=5)


# Visualization
# Daily steps over time
daily_activity_V2 %>% 
  group_by(ActivityDate) %>% summarise(totalsteps=sum(TotalSteps)) %>% 
  ggplot(aes(x=ActivityDate,y=totalsteps))+geom_line(color = "#1f77b4")+
  labs(x="Date",y="Total Steps",title="Daily Steps Over Time")+theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust = 1))


# Calories vs Steps
daily_activity_V2 %>% 
  ggplot(aes(x=TotalSteps,y=Calories))+geom_jitter(color = "#ff7f0e")+geom_smooth(color = "#2ca02c")+
  labs(x= "Total Steps",y="Calories",title = "Comparison of Steps & Calories")+
  theme_minimal()

# Distance Vs calorie
daily_activity_V2 %>% 
  ggplot(aes(x= TotalDistance,y=Calories))+ geom_jitter(color = "#d62728") +
  geom_smooth(color = "#9467bd") +
  labs(x="Total Distance Covered",y="Calories",title = "Comparison of Distance & Calorie")+
  theme_minimal()

# Calorie burnt by week 
daily_activity_V2 %>% group_by(week_name) %>% summarise(total_calorie=sum(Calories)) %>% 
  ggplot(aes(x=week_name,y=total_calorie,fill = week_name))+geom_col()+
  labs(x="Name of Week",y="Total Calories",title="Calories usage by Week")+
  theme_minimal()+ scale_fill_discrete(name="Week Name")+
  theme(axis.text.x = element_text(angle = 10))+scale_fill_brewer(palette = "Blues", name = "Week Name") 

# Total Steps by week
daily_activity_V2 %>% group_by(week_name) %>% summarise(total_Steps=sum(TotalSteps)) %>% 
  ggplot(aes(x=week_name,y=total_Steps,fill = week_name))+geom_col()+
  labs(x="Name of Week",y="Total Steps",title = "Total Steps covered by Week")+
  theme_minimal()+scale_fill_discrete(name="Week Name")+scale_fill_brewer(palette = "Oranges", name = "Week Name")+
  theme(axis.text.x = element_text(angle = 10))

# Total distance by week
daily_activity_V2 %>% group_by(week_name) %>% summarise(total_distance=sum(TotalDistance)) %>% 
  ggplot(aes(x=week_name,y=total_distance,fill = week_name))+geom_col()+
  labs(x="Name of Week",y="Total Distance",title = "Total Distance Covered by Week")+
  theme_minimal()+scale_fill_discrete(name="Week Name")+scale_fill_brewer(palette = "Greens", name = "Week Name")+ theme(axis.text.x = element_text(angle = 10))

# Weekday vs weekend 
summary_week_dayEnd <- daily_activity_V2 %>% 
  mutate(day_type=if_else(week_name %in% c("Sunday","Saturday"),"WeekEnd","WeekDay")) %>% 
  group_by(day_type) %>% 
  summarise(avg_steps=mean(TotalSteps),avg_distance=mean(TotalDistance),avg_calorie=mean(Calories)) 

dayEnd_long <- summary_week_dayEnd %>% 
  pivot_longer(cols = c(avg_steps,avg_distance,avg_calorie),
               names_to = "Metric",values_to ="Average")
dayEnd_long %>% ggplot(aes(x=day_type,y=Average,fill = Metric))+geom_col(position = "dodge2")+
  labs(x="Day Type",y="Average",title = "Average Steps,Distance,Calories")+
  theme_minimal()+ scale_fill_manual(values = c("avg_steps" = "#1f77b4", "avg_distance" = "#ff7f0e", "avg_calorie" = "#2ca02c"))

# Top 5 active users
daily_activity_V2 %>% group_by(Id) %>% summarise(active_days=n_distinct(ActivityDate)) %>% 
  arrange(desc(active_days)) %>% slice_head(n=5) %>% 
  ggplot(aes(x=Id,y=active_days))+geom_col(fill="#d62728")+coord_flip()+
  labs(x="ID",y="Days of Active",title="Top 5 Active Users")+theme_minimal()


# Least 5 active users
daily_activity_V2 %>% group_by(Id) %>% summarise(active_days=n_distinct(ActivityDate)) %>% 
  arrange(active_days) %>% slice_head(n=5) %>% 
  ggplot(aes(x=Id,y=active_days))+geom_col(fill="#d62728")+coord_flip()+
  labs(x="ID",y="Days of Active",title="Least 5 Active Users")+theme_minimal()


# Users based on how many days they used their smart device during a 31-day survey period
user_counts %>% ggplot(aes(x=usage_category,y=percentage,fill = usage_category))+
  geom_bar(stat="identity")+
  labs(x="User Classification",y="Percentage",title = "Percentage of user by activity")+
  theme_minimal()+ scale_fill_discrete(name="User Type") +
  geom_text(aes(label = sprintf("%.1f%%", percentage),vjust = -0.199))+
  scale_fill_manual(values = c("High user" = "#1f77b4", "Moderate user" = "#ff7f0e", "Low user" = "#2ca02c"))


# Activity trend by user
daily_activity_V2 %>% group_by(Id,ActivityDate) %>% summarise(totalsteps=sum(TotalSteps)) %>% 
  ggplot(aes(x=ActivityDate,y=totalsteps,colour=Id))+geom_line()+
  labs(x="Date",y="Total Steps",title = "Activity Trends by User")+theme_minimal()

# Comparison between calories & distance by steps
daily_activity_V2 %>% ggplot(aes(x=TotalDistance,y=Calories,color=TotalSteps))+
  geom_line()+labs(x="Total Distance",y="Calories",title="Calories vs Distance by Steps")+
  theme_minimal()+scale_color_gradient(low = "#1f77b4", high = "#ff7f0e")

# Export the file
# Load the package to export
library(writexl)

# Export directly CSV file to local computer
write.csv(daily_activity_V2,"daily_activity_V2.csv",row.names = FALSE)
