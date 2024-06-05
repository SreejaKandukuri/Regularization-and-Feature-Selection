# Given data
observed = c(12, 8, 24, 6)
total_sample = sum(observed)
general_pop_percent = c(0.20, 0.28, 0.36, 0.16)

# Calculate expected frequencies
expected = total_sample * general_pop_percent

# Calculate test statistic
chisquared = sum((observed - expected)^2 / expected)
chisquared

# Critical value
criticalvalue <- qchisq(0.10, df = 3)

# Compare test statistic to critical value
if (chisquared > criticalvalue) {
  decision <- "Reject the null hypothesis."
} else {
  decision <- "Fail to reject the null hypothesis."
}

decision



# Given data
observed = c(125, 10, 40, 200 - 125 - 10 - 40) # On time, National Aviation System delay, Weather delay, Late arrival
total_sample = sum(observed)
govt_stats_percent = c(0.708, 0.082, 0.120, 0.090)

# Calculate expected frequencies
expected = total_sample * govt_stats_percent

# Calculate test statistic
chisquared = sum((observed - expected)^2 / expected)
chisquared


# Critical value
critical_value = qchisq(0.05, df = 3)

# Compare test statistic to critical value
if (chisquared > critical_value) {
  decision = "Reject the null hypothesis."
} else {
  decision = "Fail to reject the null hypothesis."
}

decision


# Given data
year2013 = c(724, 335, 174, 107)
year2014 = c(370, 292, 152, 140)

# Combine the data into a matrix (2x4 contingency table)
admissionsdata = rbind(year2013, year2014)

admissionsdata
# Perform the chi-square test
chisquaretest = chisq.test(admissionsdata)

chisquaretest


#SECTION: 11-2 10. WOMEN IN THE MILITARY


# Given data
army = c(10791, 62491)
navy = c(7816, 42750)
marine = c(932, 9525)
airforce = c(11819, 54344)

# Combine the data into a matrix (2x4 contingency table)
militarydata = rbind(army, navy, marine, airforce)

militarydata

# Perform the chi-square test
chiSquaretest = chisq.test(militarydata)

chisquaretest


#SECTION: 12-1
# Degrees of freedom for the numerator (k - 1)
numeratordf = 3 - 1

# Degrees of freedom for the denominator (total sample size - k)
denominatordf = 8 * 3 - 3

# Critical value
criticalvalue = qf(1 - 0.05, df1 = numeratordf, df2 = denominatordf)
criticalvalue

# Data for the three groups (condiments, cereals, and desserts)
condiments = c(270, 130, 230, 180, 80, 70, 200)
cereals = c(260, 220, 290, 290, 200, 320, 140)
desserts = c(100, 180, 250, 250, 300, 360, 300, 160)

# Combine the data into a single vector
all_data = c(condiments, cereals, desserts)

# Create corresponding labels for the groups
group_labels = c(rep("Condiments", length(condiments)), 
                  rep("Cereals", length(cereals)), 
                  rep("Desserts", length(desserts)))

# Create a data frame with the stacked data and labels
data_df = data.frame(Data = all_data, Group = group_labels)

# Perform ANOVA test
result = aov(Data ~ Group, data = data_df)

# Print the ANOVA result
summary(result)

Fvalue = summary(result)[[1]]$`F value`[1]

Fvalue

if (Fvalue > criticalvalue) {
  decision = "Reject the null hypothesis."
} else {
  decision = "Fail to reject the null hypothesis."
}

decision

#SECTION : 12-2 : SALES OF A COMPANY 

# Given data
cereal = c(578, 320, 264, 249, 237)
chocolate_candy <- c(311, 106, 109, 125, 173)
coffee = c(261, 185, 302, 689)

# Combine the data into a single vector
all_sales = c(cereal, chocolate_candy, coffee)

# Create corresponding factor variable to represent the groups
groups= factor(rep(c("Cereal", "Chocolate Candy", "Coffee"), times = c(5, 5, 4)))

# Perform one-way ANOVA
result_anova = aov(all_sales ~ groups)

# Print the ANOVA result
summary(result_anova)

#PER PUPIL EXPENDITURE
# Given data
eastern_third = c(4946, 5953, 6202, 7243, 6113)
middle_third = c(6149, 7451, 6000, 6479)
western_third = c(5282, 8605, 6528, 6911)

# Combine the data into a single vector
all_expenditures = c(eastern_third, middle_third, western_third)

# Create corresponding factor variable to represent the groups
groups = factor(rep(c("Eastern third", "Middle third", "Western third"), times = c(5, 4, 4)))

# Perform one-way ANOVA
result_anova = aov(all_expenditures ~ groups)

# Print the ANOVA result
summary(result_anova)

##### INCREASING PLANT GROWTH

# Given data
grow_light = factor(rep(c("Grow-light 1", "Grow-light 2"), each = 6))
plant_food = factor(rep(c("Plant food A", "Plant food B"), times = 6))

all_growth = c(9.2, 9.4, 8.9, 8.5, 9.2, 8.9, 7.1, 7.2, 8.5, 5.5, 5.8, 7.6)

# Create a data frame
data = data.frame(Grow_light = grow_light, Plant_food = plant_food, Growth = all_growth)

# Perform two-way ANOVA
result_anova = aov(Growth ~ Grow_light * Plant_food, data = data)

# Print the ANOVA table
summary(result_anova)

#### baseball data 
df= read.csv("C:\\Users\\sreej\\OneDrive\\Documents\\Cloud Computing\\baseballdata.csv", header=TRUE, stringsAsFactors=FALSE)
df
# structure of the dataset
str(df)
#summary statistics 
summary(df)

#DATA EXPLORATION PLOTS 
# histogram
hist(df$W, main = "Distribution of Matches Won", col="pink", xlab="Wins")
#boxplot
boxplot(df$RS, main="Boxplot of Run Scored" , col="brown" , horizontal = T)

# Scatter plot of RS vs. W
library(ggplot2)
ggplot(data = df, aes(x = RS, y = W)) +
  geom_point() +
  labs(title = "Scatter Plot: Runs Scored vs. Wins",
       x = "Runs Scored (RS)", y = "Wins (W)")

#correlation plot
library(corrplot)

numeric_vars = df[, c("RS", "RA", "W", "OBP", "SLG", "BA", "G", "OOBP", "OSLG")]
correlation_matrix = cor(numeric_vars)
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.8)


#COMPARING AL AND NL TEAMS 
# Load required libraries
library(ggplot2)

metric = "RS"  

# Create histograms for AL and NL teams
ggplot(data = df, aes(x = df[[metric]], fill = League)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 20) +
  labs(title = paste("Histogram of", metric),
       x = metric, y = "Frequency",
       fill = "League") +
  theme_minimal()

#chi square test 
observed = table(df$W)

# Perform Chi-Square Goodness-of-Fit test
chi_sq_test = chisq.test(observed)

# Display the test results
print(chi_sq_test)


#crop data 
cropdata= read.csv("C:\\Users\\sreej\\OneDrive\\Documents\\Cloud Computing\\cropdata.csv", header=TRUE, stringsAsFactors=FALSE)
cropdata

#Two-way ANOVA

cropdata$density = factor(cropdata$density)
cropdata$fertilizer = factor(cropdata$fertilizer)
cropdata$block = factor(cropdata$block)

anovaresult = aov(yield ~ fertilizer + density + fertilizer:density + Error(block), data = cropdata)

summary(anovaresult)
