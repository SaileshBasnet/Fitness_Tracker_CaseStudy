# Fitness Tracker Case Study 📊
## Company Overview 🏢
<div align="justify"> Bellabeat, founded in 2013 by Urška Sršen and Sando Mur, is a high-tech company specializing in health-focused smart products for women. With a mission to empower women through beautifully designed technology, Bellabeat collects data on activity, sleep, stress, and reproductive health to provide valuable insights. The company has experienced rapid growth, expanding globally with multiple product launches and a strong digital marketing presence. Bellabeat invests in various advertising channels, including Google Search, social media, and video ads. Sršen is interested in leveraging consumer data to refine marketing strategies and identify new growth opportunities.
</div>

### My role 👩‍💻
As a junior data analyst at Bellabeat, my role is to examine smart device fitness data to identify usage patterns and trends among consumers. I will then use these insights to provide actionable recommendations for optimizing the company's marketing strategy, which I will present to the executive team.

In order to organize the analysis, we follow these steps of the data analysis process: Ask, Prepare, Process, Analyze, Share, and Act.

### Stage 1: Ask ❓
Formulated key questions to explore trends in smart device usage, their relevance to Bellabeat customers, and their potential impact on marketing strategies.

### Stage 2: Prepare 🛠️
Prepared the Fitbit data by loading, inspecting, and cleaning it in R, addressing issues such as duplicates and missing values, and ensuring data types were suitable for analysis.

### Stage 3: Process 🔧
Conducted descriptive and trend analysis, including calculating basic statistics, identifying peak activity periods, and classifying users based on their activity levels.

### Stage 4: Analyze 🔍
Performed detailed analysis to uncover trends, correlations, and variance in user activity, and identified the top and least active users based on engagement levels.

### Stage 5: Share 📊
Created visualizations to illustrate key findings, such as daily and weekly activity patterns, relationships between steps, calories, and distance, and user engagement levels.

### Stage 6: Act 🚀
Summarized insights from the data, provided actionable recommendations for personalized notifications, social awareness campaigns, and promotional strategies, and prepared the cleaned data for further use.

### Detailed Steps 📝
## Stage 1: Ask❓
The final list of questions to analyze are:
-	What are some trends in smart device usage?
-	How could these trends apply to Bellabeat customers?
-	How could these trends help in infuence Bellabeat marketing strategy?

## Stage 2: Prepare 🛠️
We are using the Fitbit Fitness Tracker Data [link for dataset](https://www.kaggle.com/datasets/arashnic/fitbit) from Kaggle. This Kaggle data set contains personal fitness tracker from thirty Fitbit users. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes information about daily activity, steps, and hearth rate that can be used to explore users’ habits.
We move the files in a folder named as ‘fitbit_fitness_tracker’ to organize the files. Then, we extract the files. To avoid future accident, we also make copy of file. 

### Metadata Description 🗃️
The dataset includes the following fields: 
- Id: Unique identifier for each record.
- ActivityDate: Date of the recorded activities (YYYY-MM-DD).
- TotalSteps: Total number of steps taken.
- TotalDistance: Total distance covered (e.g., kilometres or miles).
- TrackerDistance: Distance recorded by the activity tracker.
- LoggedActivitiesDistance: Distance covered during logged activities.
- VeryActiveDistance: Distance covered during very high-intensity activity.
- ModeratelyActiveDistance: Distance covered during moderate-intensity activity.
- LightActiveDistance: Distance covered during light-intensity activity.
- SedentaryActiveDistance: Distance covered while sedentary.
- VeryActiveMinutes: Minutes spent in very high-intensity activities.
- FairlyActiveMinutes: Minutes spent in moderate-intensity activities.
- LightlyActiveMinutes: Minutes spent in light-intensity activities.
- SedentaryMinutes: Minutes spent being sedentary.
- Calories: Total calories burned.

### Data Reliability and Quality 📉

The dataset is considered a good data source based on the ROCCC method:
-	Reliability: The dataset includes different users’ data.
-	Originality: This dataset generated by respondents to a distributed survey via Amazon Mechanical Turk
-	Comprehensiveness: The dataset covers various aspects of health-related metrices, including steps, calories, distance etc.
-	Current: The dataset is from April 2016 to May 2016 and may lacks update.
-	Cited: The dataset is not properly cited.

### Limitation of Data ⚠️

- The size of the sampling is less than 30 which is inappropriate methods according to Central Limit Theorem (CLT).
- Most data are collected during Tuesday to Thursday.

*(Note: The data has been made available by Amazon Mechanical Turk. under this [license](https://creativecommons.org/publicdomain/zero/1.0/).)*

## Stage 3: Process 🔧

### Data Cleaning in R
The following steps were performed to clean and prepare the data for analysis:
1.	Loaded necessary libraries for data manipulation and visualization.
2.	Loading Data: The data was loaded for further analysis in R.
3.	Data Inspection: The structure and contents of each data frame were inspected to ensure consistency.
4.	Data Cleaning:
    -	Checked for Duplicates and Missing Values: We removed duplicate entries and addressed any missing data.
    -	Validated Data: We ensured all values were valid and verify if there are any negative values which may need correction.
5.	Data Transformation:
    -	 Converted Data Types: We changed ‘Id’ column to a factor and ‘ActivityDate’ column to Date format for analysis.

### Stage 4: Analyze	🔍
### Basic Statistics:
Calculate the average, maximum, median, and standard deviation for steps, distance, and calories. This helps in understanding the central tendencies and variability in user activity.

### User-Specific Analysis:
Summarize data by user to identify individual activity patterns, such as the average steps taken and calories burned by each user.

### User Classification:
Classify users based on their activity levels into categories: High, Moderate, and Low users. Calculate the percentage of users in each category to understand the distribution of user engagement.

### <ins> Trend Analysis </ins>
### Identify Peak Activity:
Find dates with the highest recorded values for steps, calories, and distance. This helps in identifying peak activity days.

### Weekly Patterns:
Analyze activity patterns across different days of the week. Create a new column to represent the day of the week and aggregate data to find total steps, distance, and calories burned by each day.

### <ins> Summary, Correlation & Variance </ins>
### Summary Statistics:
Provide summary statistics for various types of distances (e.g., VeryActiveDistance, LightActiveDistance).

### Correlation Analysis:
Calculate correlation matrices to understand the relationships between different activity metrics (e.g., steps, distance, and calories).

### Variance Analysis:
Determine the variance for different types of distances to understand the spread of data.

### <ins> Top and Least Active Users</ins>
### Identify Top Users:
Identify the top 5 users with the most active days and the least 5 users with the fewest active days. This helps in recognizing highly engaged users and those who are less active.

💎 Dive into the source code for this project and see how the analysis was conducted: [link to source code](https://github.com/SaileshBasnet/Fitness_Tracker_CaseStudy/blob/main/fitness_tracker_CaseStudy.R)

## Stage 5: Share 📊
### Visualizations 📈

### Daily Steps Over Time:
-	Plot daily steps over time to visualize trends and patterns in user activity.
  
### Calories vs. Steps:
-	Create scatter plots and trend lines to show the relationship between steps taken and calories burned.

### Distance vs. Calories:
-	Visualize the relationship between the distance covered and calories burned.
  
### Weekly Activity Patterns:
-  Use bar charts to show total steps, distance, and calories burned by each day of the week. Compare weekday and weekend activity levels.

### User Classification:
-	Visualize the percentage of users in each activity category using bar charts.

### Activity Trends by User:
-	Plot activity trends for individual users over time to identify consistent patterns and deviations.

Read the full documentation: [link to R Markdown](https://rpubs.com/Sailesh_Basnet/1209928)

## Stage 6: Act 🚀
### Summary of Data

### Daily Steps Over Time:

![image](https://github.com/user-attachments/assets/5c756fb6-a912-485e-bb42-62d9f4325660)

Findings from the plot:
- The plot shows cyclic pattern where there are fluctuations in daily step counts over time.
- The active period high steps count between April 12 to April 18, indicating a period of high physical activity.
- There is a drop-in physical activity starting from early May, with the lowest activity on May 12.

### Calories Vs Steps

![image](https://github.com/user-attachments/assets/c05b4ee5-2042-4931-aada-5d8a446c01ae)

Finding from the plot:
- There is a positive correlation between Total Steps and Calories burned. The plot shows number of calories burned tends to be higher for high step counts.
- The highest number of steps is less than 15000.
- The plot shows number of calories burned tends to be lower for low step counts.

### Distance Vs Calorie

![image](https://github.com/user-attachments/assets/50e8ce03-442c-49c8-875a-0fa7b90cad55)

Finding from the plot:
- The plot shows high number of calories burned with the increase of distance covered. By analyzing plot, the highest number of distances covered is less than 10 Miles.
- The plot shows low number of calories burned with the decrease of distance covered.

### Calories burnt by week

![image](https://github.com/user-attachments/assets/362bfb10-8cf7-4dd5-844d-4c812bd2dd83)

Finding from the plot:
- There are signification changes in calorie burnt over the week. 
-	Highest Calorie Burnt Day: Tuesday with 358,114 calories.
-	Lowest Calorie Burnt Day: Sunday with 273,823 calories.
- The difference between the highest day (Tuesday) and the lowest day (Sunday) is 84,291 calories, indicating a considerable variation in calorie expenditure over the week.

### Steps Covered by week

![image](https://github.com/user-attachments/assets/7b742047-a8e0-462b-8694-aa675f01e127)

Finding from the plot:
-	Highest Step Day: Tuesday with 1,235,001 steps.
-	Lowest Step Day: Sunday with 838,921 steps.
- Tuesday, Wednesday, and Thursday are the days with the highest step counts, suggesting more physical activity.
- Sunday has the lowest step count, suggesting low activity level.

### Distance Covered by week

![image](https://github.com/user-attachments/assets/decb21bf-df4e-48d9-8986-986384ceccc0)

Finding from the plot:
- Tuesday is the day with the highest distance covered, while Sunday is the lowest. 
- This pattern suggests that distance and activity levels vary throughout the week.

### Weekend vs Weekday

![image](https://github.com/user-attachments/assets/20b19432-e76b-4490-a9f3-c606ac30fb28)

Finding from the plot:
- The plot shows slightly high average distance and steps in weekday than weekend.
- Weekends shows marginal increase in average calorie burned.

### Top & Least Active Users

![image](https://github.com/user-attachments/assets/0e72d06f-3e9f-4f39-a5b4-a0d5d7482591)

![image](https://github.com/user-attachments/assets/3e340666-d198-48b5-ac67-8686f4777714)

Findings from both plot:
-The top 5 users have a constant engagement in daily activity.
- The least active users have a significantly lower number of active days compared to the most active users, with the range varying from 4 to 26 days.

### User Engagement Levels:

![image](https://github.com/user-attachments/assets/a4e5454c-763c-43bf-b62a-62e422c37589)

Finding from the plot: 
- There is a clear disparity in engagement levels: top users have consistent daily activity, while the least active users show significantly fewer active days. 
- Most users are categorized as high engagement (87.9%), with a smaller percentage being moderate (9.09%) or low users (3.03%).
 
## Insights from Data 🔍
These are the highlights we found from the data:
- Physical activity shows cyclical patterns, with noticeable drops and peaks over time.
- Higher steps and longer distance covered lead to more calorie burned.
- There are significant differences in activity levels throughout the week, with the highest activity on Tuesdays and the lowest on Sundays.
- Weekdays generally see higher step counts and distances covered compared to weekends, although calorie burn is slightly higher on weekends.
- The use of device is in high level.

## Recommendation & Suggestions 📈
After analyzing everything we conclude to this decision for further improvement:

### Implement Personalized Notifications:
Send personalized notifications to every user in certain period of time, including summary of major data to encourage them to increase their activity level. 

### Social Awareness:
Provide educational social awareness to every user by describing benefits of daily activity in life.

### Targeted Campaigns:
Launch campaigns to target user by offering rewards when the certain objective met to increase their daily steps count and distance covered.

### Start Weekly Promotions:
Create a motivational campaign or offers on Tuesday and Wednesday which are the highest active day in week to boost activity in lower active day. 

### Social Features:
Use social power by encouraging user to participate with their friends, family in a challenge or offers on lowers activity days. 

### Feedback Mechanisms:
Address user concerns and adapt the strategy based on real user experiences.

### Next Steps 📅
- Implement Recommendations: Begin with the most feasible recommendations and monitor their impact on user engagement and activity levels.
- Track Performance: Use key metrics to track the effectiveness of the new strategies. Adjust tactics based on performance data.
- Continuous Improvement: Regularly review and refine marketing strategies based on ongoing data analysis and user feedback.


## Export Data  📂
### Export Cleaned Data:
-	Export the cleaned and processed data to a CSV file for further analysis and sharing.

