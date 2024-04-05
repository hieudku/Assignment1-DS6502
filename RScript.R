# frances branch new new 1234567


# frances branch new
# this is my branch 202403280


################################################################################

# Dont worry if you overwrite these codes by accident when pushing main, 
# I ll keep them on a backup branch as we go (Hieu)


getwd()

# do we even need to set wd lol? maybe not if it works on your R studio? (Hieu)

# Install package - to be added more if needed
install.packages("ggplot2")
install.packages("dplyr") # this will install the package in case you haven't got it thus the error (hieu)

# Import package - to be added more if needed
library(ggplot2)
library(dplyr) #when I run this line, given me error that there is no package called 'dplyr'(mia)

# Import dataset
dataset.df <- read.csv("./Excel File/TimeOnSocialMedia.csv")

# Test a few first line of imported file
head(dataset.df)
tail(dataset.df)

########################### Q2 - Basic descriptive Statistics #######################################

#### Time spent column ####

#Mean for time spent value
Time_spent_Mean <- mean(dataset.df$time_spent)
Time_spent_Mean

#Median for time spent value
Time_spent_Median <- median(dataset.df$time_spent)
Time_spent_Median

#Standard deviation for time spent value 
Time_spent_Standard_deviation <- sd(dataset.df$time_spent)
Time_spent_Standard_deviation

#range for time spent value
Time_spent_range <- range(dataset.df$time_spent)
Time_spent_range

# Min Max
min_time_spent <- min(dataset.df$time_spent)
max_time_spent <- max(dataset.df$time_spent)
                      
#quartiles for time spent value
Time_spent_quartiles <- quantile(dataset.df$time_spent)
Time_spent_quartiles

#use summary() function to double check the result
summary(dataset.df$time_spent)



######################

#Havent found out any missing or inconsistencies values, so skip Q3 (mia)

############################### Q4 - Plot Graphs ########################################################################


###### Average Time Spent vs Platform ########## 

# Had go for the average time of each platform index of the total time of each platform
# Good the analysis in the futher report rather than to analysis the total time spent
# The plot shows the average of each platform near 5, which is near the mean of the time spent (5.029)
# The plot also shows which platform is used more on average
### Mia

#Calculate the average time of each platform
platform_time_spent <- aggregate(time_spent ~ platform, data = dataset.df, FUN = mean)

#Bar parplot vs platform
Average_time_barplot <- ggplot(platform_time_spent, aes(x = platform, y = time_spent, fill = platform)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Time Spent on Social Media by Platform",
       x = "Platform",
       y = "Time Spent (hours)") + theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), #center the title, front size to 20 and bold
                                         axis.text = element_text(colour = "brown", size = 10, face = "bold"), #color the x and y label, front size to 10 and bold 
                                         axis.title.x = element_text(size = 15, face = "bold"), #front size 15 and bold the x title
                                         axis.title.y = element_text(size = 15, face = "bold")) #front size 15 and bold the y title

Average_time_barplot 
#keep^
##########################################################

# Other graphs ideas: 
# - Total time spent vs gender? 
# - Age vs Time Spent as Scatterplot? both are numerical, so we can find the relationship between the 2 
# - Income vs Time Spent? same as above

# Should we plot at least once for each column? One type of graph for each relation? let me know your thoughts
# (hieu)


##### Age vs Time Boxplot #####

# **Note on Boxplot - Why i think it works:
# - Can view median line (middle line), first and third Quartiles (bottom and top edges) and min, max values (end of each whiskers)
# - Can be translated into pretty interesting analysis (median line = 5 for all groups, almost same as mean age (5.029) indicating a symmetrical distribution visually)
# - closely resemble a normal distribution without simulating our dataset multiple times, etc..)
# - maybe more
# (hieu)

# Divide age data into 5 groups for cleaner graph
dataset.df$age_group <- cut(dataset.df$age, breaks = c(18, 28, 38, 48, 58, 64), include.lowest = TRUE, right = FALSE,
                            labels = c("18-27", "28-37", "38-47", "48-57", "58-64")) 

# Boxplot age vs time spent
ggplot(dataset.df, aes(x = factor(dataset.df$age_group), y = time_spent)) +
  geom_boxplot(aes(fill = factor(dataset.df$age_group))) + # Fill color by age
  scale_fill_viridis_d() + 
  theme_minimal() + 
  labs(
    title = "Boxplot of Time Spent per Day by Age",
    x = "Age",
    y = "Time Spent",
    fill = "Age Group"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the plot title
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom" # Position the legend at the bottom
  )
# keep^
# make text appear on top of the bars
####################################
#(Frances) bar plot of average time spent by gender
# to analyze if there are significant differences in social media usage patterns between different gender identities
#count the number of each time spent  value
time_spent_count <- table(dataset.df$time_spent)

#basic bar plot
barplot(time_spent_count)

#bar plot to show the average time spent by different gender identities

average_plot <- ggplot(data= dataset.df, aes( x = gender,y = time_spent)) +
  stat_summary(fun = "mean", geom = "bar", fill = c("blue","red","orange"), color = "black", alpha = 0.6) +
  labs(title = "Average Time Spent by Gender", x = "Gender", y = "Average Time Spent")
average_plot
# make text appear on top of the bars
#(Frances) stacked bar plot of average time spent by location and Platform
# to show the relationship among average time, location (3 countries), and platform.
# got a problem here, the number of y axis seems wrong!!!!!!!!!!! 

# Calculate total time spent by location and platform (mia)
total_time_spent <- aggregate(time_spent ~ location + platform, data = dataset.df, sum)

# Create stacked bar plot
stacked_bar_plot <- ggplot(data = total_time_spent, aes(x = location, y = time_spent, fill = platform)) + 
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Time Spent by Location and Platform on Social Media",
       x = "Location", y = "Total Time Spent") +
  # Add text labels for total time spent on each bar
  geom_text(aes(label = paste(time_spent, "hours")), position = position_stack(vjust = 0.5))

stacked_bar_plot


# keep^ but pls add text on top of bar
########################## Time 2024-03-30-0:31 Frances################



######################################## 
#### Adya Q4- heatmaps ####

# Installing the required package
install.packages("reshape2")

# Loading from the library
library(reshape2)

# Preparing the dataset
dataset.melt <- melt(dataset.df)


# Calculating average time spent by gender
avg_time_spent <- aggregate(time_spent ~ gender, dataset.df, mean)


# Convert the data to long format for the heatmap
avg_time_spent_long <- reshape2::melt(avg_time_spent, id.vars = "gender")

# Creating a heatmap
ggplot(avg_time_spent_long, aes(x = gender, y = variable, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "blue") +
  labs(title = "Heatmap of Average Time Spent by Gender",
       x = "Gender",
       y = "Time Spent",
       fill = "Average Time Spent")

# Creating a histogram for each numerical column in the dataset
for(col in names(dataset.df)) {
  # Check if the column is numerical
  if(is.numeric(dataset.df[[col]])) {
    # Create a histogram
    p <- ggplot(dataset.df, aes_string(col)) + 
      geom_histogram(binwidth = 1000, fill = "green", color = "black") +
      scale_x_continuous(breaks = seq(min(dataset.df$income), max(dataset.df$income), by = 1000)) +
      labs(title = paste("Histogram of", col),
           x = col,
           y = "Frequency")
    print(p)
  }
}

######################################
# Pls add more group in x axis

#(Frances)average time spent in different income groups by prefession
# group the income
dataset.df$income_group <- cut(dataset.df$income, breaks = c(10000, 12500, 15000, 17500, 20000), include.lowest = TRUE, right = FALSE,
                            labels = c("10000-12500", "12500-15000", "15000-17500", "17500-20000")) 

#group a new dataset, %>% is a chain operator to chain together multiple operations
#The .groups = "drop" argument ensures that the grouping is dropped after summarization to avoid issues with plotting.
dataset1 <- dataset.df %>% group_by(profession, income_group) %>% summarize(avg_time = mean(time_spent), .groups = 'drop')

ggplot(data = dataset1, mapping = aes(x=income_group, y=avg_time, color=profession, group = profession)) + 
      geom_line() + geom_point() + 
      labs(title = "Average Time Spent on Social Media by Income Group, by Profession",  x = "Income Group", y = "Average Time Spent")

########################## Time 2024-03-31-1:13 Frances ￥################



#### Heatmap of timespent by Gender and Age ####

# How to read this heatmap (I talked to Hoang about heatmap at the end of a lab)
# - Darker or more intense colors typically represent higher values, while lighter colors represent lower values
# - Clusters of similar colors can indicate a relationship or trend between the variables.
# - For example, age 28 has similar trend across all 3(?) genders, this is horizontal cluster
# - And another example, non-binary aged 33-40 has almost similar shade, this is vertical cluster, and so on.
# - To be added more into the report when I have time.

# of course we can do something similar with income or something?
#(hieu)

dataset_subset <- subset(dataset.df, gender %in% c("male", "female", "non-binary"))

# Single heatmap #
ggplot(data = dataset_subset, aes(x = gender , y = age)) +
  geom_tile(aes(fill = time_spent))+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Heatmap of Time spent by Gender and Age between homeowners (true) and renters (false)",
       x = "Gender",
       y = "Age (years)",
       fill = "Time spent")

# Heatmaps by platforms #
ggplot(data = dataset_subset, aes(x = gender , y = age)) +
  geom_tile(aes(fill = time_spent))+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Heatmap of Time spent by Gender and Age between homeowners (true) and renters (false)",
      x = "Gender",
      y = "Age (years)",
      fill = "Time spent") +
  facet_wrap(~platform)



# keep ^
###############################################
# use time_spent instead of time_spent_mean

#### Frances's problem - stacked bar chart for location, platform vs time spent & y axis value issue ####

# I think since we know there are more participants in Australia > UK > US from Frances's chart, as explained in the group chat, 
# there is a bias toward the total percentage of the time spent for each countries.(Australia has the most participants so they will also likely to have the most time spent % out of 3 countries)
# Therefore, it makes sense to try to find the percentage of each platform inside EACH country instead.

# Below is a representation of number of particiants in each country, which looks pretty similar to Frances's stack bar chart as I anticipated:

######### Participants chart
# Count the number of participants from each country
participant_count <- table(dataset.df$location)

# Convert the participant count to a data frame
participant_count_df <- data.frame(
  country = names(participant_count),
  count = as.numeric(participant_count)
)

# Create a bar chart
ggplot(participant_count_df, aes(x = country, y = count, fill = country)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Number of Participants by Country", x = "Country", y = "Number of Participants") +
  theme(legend.position = "none")



#### My (possible) solution: #####

# Below are the pie charts to show percentages of time spent for each country.
# - I think these 3 pie charts will complement Frances's stacked bar chart in some way, as it show clear percentages
# of each platform across 3 countries. Because Frances's chart shows total values, while percentage imo is better represented 
# by pie charts, without bias from the difference in number of participants in our dataset. From here, we can see which platform is more or less popular in each country, which may help us to conclude
# our findings later in the report. Therefore we might want to keep both graphs on the visual and written report to complement eachothers.

######## Pie charts
# Calculate percentage of time spent for each location and platform
pie.df <- dataset.df %>%
  group_by(location, platform) %>%
  summarize(total_time = sum(time_spent)) %>%
  mutate(percentage = total_time / sum(total_time) * 100)

# Create pie charts
Time_Platform_Location_piechart <- ggplot(pie.df, aes(x = "", y = percentage, fill = platform)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar("y") +
  facet_wrap(~location) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Percentage of Time Spent on Platforms by Location", fill = "Platform") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), position = position_stack(vjust = 0.5))

Time_Platform_Location_piechart

###############################################


# Create a normal distribution plot
ggplot(dataset.df, aes(x = time_spent)) +
  stat_function(fun = dnorm, args = list(mean = Time_spent_Mean, sd = Time_spent_Standard_deviation), 
                color = "blue", size = 1.5) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", fill = "lightgray", alpha = 0.5) +
  scale_x_continuous(breaks = seq(min(dataset.df$time_spent), max(dataset.df$time_spent), by = 1)) + # show all time spent values on x (1-9)
  labs(x = "Time Spent", y = "Density") +
  ggtitle("Normal Distribution Plot for Time Spent")

# No clear sign of normal distribution > lack of trend and evenly distributed in time spent. Maybe include reasons for why we DONT add this to the repot instead of including it?

# Use this instead of Standard Deviation? - Histogram and density curve of Time Spent
# Note:  the density values on the y-axis of a density plot is the probabilty of observing data points at different values along the time spent x-axis

# Histogram by location #
ggplot(dataset.df, aes(x = time_spent)) +
  geom_histogram(binwidth = 1, color = "black", aes(y = ..density.., fill = ..count..), alpha = 0.5) +
  geom_density(alpha = 1, color = "blue") + # Density curve, look at peak and tails
  labs(x = "Time Spent", y = "Density") +
  scale_x_continuous(breaks = seq(min(dataset.df$time_spent), max(dataset.df$time_spent), by = 1)) + # show all time spent values on x (1-9)
  scale_fill_gradient(low = "green", high = "red") +
  ggtitle("Histogram with Density Curve for Time Spent") +
  
  facet_wrap(~ gender) # Solution to lack of trends - we compare between countries!

# Single histogram #
ggplot(dataset.df, aes(x = time_spent)) +
  geom_histogram(binwidth = 1, color = "black", aes(y = ..density.., fill = ..count..), alpha = 0.5) +
  geom_density(alpha = 1, color = "blue") + # Density curve, look at peak and tails
  labs(x = "Time Spent", y = "Density") +
  scale_x_continuous(breaks = seq(min(dataset.df$time_spent), max(dataset.df$time_spent), by = 1)) + # show all time spent values on x (1-9)
  scale_fill_gradient(low = "green", high = "red") +
  ggtitle("Histogram with Density Curve for Time Spent") +
  
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, # Add descriptive statistics: mean, median etc to the histogram
           label = paste("Mean:", round(Time_spent_Mean, 2), "\n",
                         "Median:", round(Time_spent_Median, 2), "\n",
                         "SD:", round(Time_spent_Standard_deviation, 2), "\n",
                         "Min:", min_time_spent, "\n",
                         "Max:", max_time_spent))

# This one not much better, but at lease we include SD in it to make predictions:
# 


## Simple boxplot ##
timeSpent_boxplot <- ggplot(dataset.df, aes(y = time_spent)) +
  geom_boxplot() + 
  geom_point(aes(x = 0),color = "red", size = 3) +
  geom_text(aes(x = 0.05),label = dataset.df$time_spent, vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::comma) +
  ylab("Time Spent") +
  ggtitle("Boxplot of time spent on social media") +
  
  annotate("text", x = 0.2, y = Inf, hjust = 0, vjust = 1,
           label = paste("Mean:", round(mean(dataset.df$time_spent), 2), "\n",
                         "Median:", round(median(dataset.df$time_spent), 2), "\n",
                         "SD:", round(sd(dataset.df$time_spent), 2), "\n",
                         "Min:", min(dataset.df$time_spent), "\n",
                         "Max:", max(dataset.df$time_spent)))


print(timeSpent_boxplot)
