#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#part 1
mean_iq <- mean(y) #calculating the mean
sd_iq <- sd(y) #calculating the standard deviation
se_iq <- sd_iq/sqrt(length(y)) #calculating the standard error
alpha <- (1-0.9)/2 #because is is 90% CI
tscore <- qt(alpha, df = length(y)-1, lower.tail = F) #calculating t score for n<30 because it is 25
lower_90 <- mean_iq - (tscore * se_iq) #lower CI interval
upper_90 <- mean_iq + (tscore * se_iq) #upper CI interval
confint90 <- c (lower_90 , upper_90)
confint90 

#part 2
Ho <- 100 #we want to prove that average IQ in school greater than 100 - alternative Hypothesis, so our null hypothesis is that it is 100
alpha2 <- 0.05 #given alpha
test_statistic = (mean_iq-Ho)/se_iq #test statistic is the difference between means with the standard erorr as denominator
p_value <- pt(test_statistic, length(y)-1, lower.tail = F) #should be 1 sided
p_value #0.2784617 which is >0.05 hence we fail to reject Null hypothesis - this means that there is not enough evidence to conclude that the average IQ in this school is higher than 100

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header = TRUE)
head(expenditure)
# Loading ggplot2 package
library(ggplot2)
#PART 1
#Y with X1 - Personal income versus shelter/housing expenditure assistance
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  labs(
    title = "Per Capita Expenditure on Shelters/Housing Assistance versus Personal Income",
    x = "Personal Income in State", y = "Per Capita Expenditure") + theme_minimal()
cor(expenditure$X1, expenditure$Y) #the correlation is postive and upwards but not very strong

#Y with X2 - Per Capita Expenditure on Shelters/Housing Assistance versus Number of Financially Secure residents
ggplot(expenditure, aes(x = X2, y = Y)) +
  geom_point() +
  labs(
    title = "Per Capita Expenditure on Shelters/Housing Assistance versus Number of Financially Secure residents",
    x = "Number of Financially Secure Residents per 100K", y = "Per Capita Expenditure") + theme_minimal()
cor(expenditure$X2, expenditure$Y) #the correlation is downward then upwards, seems like it is weak

#Y with X3 - Per Capita Expenditure on Shelters/Housing Assistance versus Number of people per thousand residing in urban areas in state
ggplot(expenditure, aes(x = X3, y = Y)) +
  geom_point() +
  labs(
    title = "Per Capita Expenditure on Shelters/Housing Assistance versus Number of people per thousand residing in urban areas in state",
    x = "Number of people per thousand residing in urban areas in state", y = "Per Capita Expenditure") + theme_minimal()
cor(expenditure$X3, expenditure$Y) #the correlation is upward then downward, seems like it is also weak


#X1 with X2 - Personal Income versus Number of Financially insecure residents 
ggplot(expenditure, aes(x = X1, y = X2)) +
  geom_point() +
  labs(
    title = "Personal Income versus Number of Financially insecure residents",
    x = "Personal Income in State", y = "Number of Financially insecure residents") + theme_minimal()
cor(expenditure$X1, expenditure$X2) #there is no correlation it seems


#X1 with X3 - Personal Income versus Number of people per thousand residing in urban areas in state
ggplot(expenditure, aes(x = X1, y = X3)) +
  geom_point() +
  labs(
    title = "Personal Income versus Number of people per thousand residing in urban areas in state",
    x = "Personal Income in State", y = "NNumber of people per thousand residing in urban areas in state") + theme_minimal()
cor(expenditure$X1, expenditure$X3) #seems the correlation is upward and positive

#X2 with X3 - Number of Financially insecure residents versus Number of people per thousand residing in urban areas in state
ggplot(expenditure, aes(x = X2, y = X3)) +
  geom_point() +
  labs(
    title = "Number of Financially insecure residents versus Number of people per thousand residing in urban areas in state",
    x = "Number of Financially insecure residents", y = "Number of people per thousand residing in urban areas in state") + theme_minimal()
cor(expenditure$X2, expenditure$X3) #there is no correlation it seems

#PART 2
#Y and Region
ggplot(expenditure, aes(x = factor(Region), y = Y)) +
  geom_bar(stat = "summary", fun = "mean", fill = "grey", color = "black") + #using the mean because you want to see the average per region and this function works on Y statistics
  labs(
    title = "Average Per Capita Expenditure on Shelters/Housing Assistance by Region",
    x = "Region", y = "Average Per Capita Expenditure") + theme_minimal()
#On average, the West region, region 4, has the highest per capita expenditure on housing assistance.

#Part 3
#Y with X1 - Personal income versus sheler/housing expenditure assistance
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  labs(
    title = "Per Capita Expenditure on Shelters/Housing Assistance versus Personal Income",
    x = "Personal Income in State", y = "Per Capita Expenditure") + theme_minimal()
#Y with X1 with Region
ggplot(expenditure, aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +
  geom_point() + 
  labs(
    title = "Per Capita Expenditure on Shelters/Housing Assistance versus Personal Income by Region",
    x = "Personal Income in State", y = "Per Capita Expenditure", color = "Region", shape = "region") + theme_minimal()


