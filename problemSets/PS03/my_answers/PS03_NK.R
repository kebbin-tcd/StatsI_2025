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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

#Question 1
#head(inc.sub)
#Part 1
#runing the regression
model1 <- lm(voteshare ~ difflog, data = inc.sub)
#seeing the summary
summary(model1)
#Part 2 scatterplot with regression line
scatter1 <- ggplot(inc.sub, aes(x = difflog, y = voteshare)) + 
  geom_point() +                        # points
  geom_smooth(method = "lm", col = "red", lwd = 1.2) + 
  labs(x = "Differencelog", y = "Voteshare",  
       title = "Scatterplot of difflog vs voteshare with regression line") +
  theme_minimal()                
scatter1
#Part 3 - saving residuals in a seperate object
residuals_model1 <- residuals(model1)
#head(residuals_model1)
#Part 4 - prediction equation
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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

#Question 1
#head(inc.sub)
#Part 1
#runing the regression
model1 <- lm(voteshare ~ difflog, data = inc.sub)
#seeing the summary
summary(model1)
#Part 2 scatterplot with regression line
scatter1 <- ggplot(inc.sub, aes(x = difflog, y = voteshare)) + 
  geom_point() +                        # points
  geom_smooth(method = "lm", col = "red", lwd = 1.2) + 
  labs(x = "Differencelog", y = "Voteshare",  
       title = "Scatterplot of difflog vs voteshare with regression line") +
  theme_minimal()                
scatter1
#Part 3 - saving residuals in a seperate object
residuals_model1 <- residuals(model1)
#head(residuals_model1)
#Part 4 - prediction equations
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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

#Question 1
#head(inc.sub)
#Part 1
#runing the regression
model1 <- lm(voteshare ~ difflog, data = inc.sub)
#seeing the summary
summary(model1)
#Part 2 scatterplot with regression line
scatter1 <- ggplot(inc.sub, aes(x = difflog, y = voteshare)) + 
  geom_point() +                        # points
  geom_smooth(method = "lm", col = "red", lwd = 1.2) + 
  labs(x = "Differencelog", y = "Voteshare",  
       title = "Scatterplot of difflog vs voteshare with regression line") +
  theme_minimal()                
scatter1
#Part 3 - saving residuals in a seperate object
residuals_model1 <- residuals(model1)
#head(residuals_model1)
#Part 4 - prediction equation
coef_model1 <- coef(model1)
#coef_model1
intercept <- coef_model1[1] 
slope <- coef_model1[2] 
#writing prediction equation below
cat("Prediction equation: voteshare =", round(intercept, 3), "+", round(slope, 3), "* difflog\n")
#Interpretation: on average, for every 1 unit increase in the log difference of spending, the incumbent's vote share is expected to increase by 0.024. The vote share without the difflog is 0.508


#Question 2
#head(inc.sub)
#Part 1
#running the regression
model2 <- lm(presvote ~ difflog, data = inc.sub)
#seeing the summary
summary(model2)
#Part 2 scatterplot with regression line
scatter2 <- ggplot(inc.sub, aes(x = difflog, y = presvote)) + 
  geom_point() +                        # points
  geom_smooth(method = "lm", col = "red", lwd = 1.2) + 
  labs(x = "Differencelog", y = "presvote",  
       title = "Scatterplot of difflog vs presvote with regression line") +
  theme_minimal()                
scatter2
#Part 3 - saving residuals in a seperate object
residuals_model2 <- residuals(model2)
#head(residuals_model1)
#Part 4 - prediction equation
coef_model2 <- coef(model2)
#coef_model2
intercept <- coef_model2[1] 
slope <- coef_model2[2] 
#writing prediction equation below
cat("Prediction equation: presvote =", round(intercept, 3), "+", round(slope, 3), "* difflog\n")
#Interpretation: on average, for every 1 unit increase in the log difference of spending, the pres vote is expected to increase by 0.024. The pres vote without the difflog is 0.508


#Question 3
#head(inc.sub)
#Part 1
#runing the regression
model3 <- lm(voteshare ~ presvote, data = inc.sub)
#seeing the summary
summary(model3)
#Part 2 scatterplot with regression line
scatter3 <- ggplot(inc.sub, aes(x = presvote, y = voteshare)) + 
  geom_point() +                        # points
  geom_smooth(method = "lm", col = "red", lwd = 1.2) + 
  labs(x = "presvote", y = "voteshare",  
       title = "Scatterplot of voteshare vs presvote with regression line") +
  theme_minimal()                
scatter3
#Part 3 - prediction equation
coef_model3 <- coef(model3)
#coef_model3
intercept <- coef_model3[1] 
slope <- coef_model3[2] 
#writing prediction equation below
cat("Prediction equation: voteshare =", round(intercept, 3), "+", round(slope, 3), "* presvote\n")
#Interpretation: on average, for every 1 unit increase in the presvote, the  vote share is expected to increase by 0.388 (slope). The  vote share without the presvote is predicted to be 0.441 (intercept)

#Question 4
#Part 1
#runing the regression
model4 <- lm(residuals_model1 ~ residuals_model2)
#seeing the summary
summary(model4)
#Part 2 scatterplot with regression line
residuals_df <- data.frame(res1 = residuals_model1, res2 = residuals_model2)
scatter4 <- ggplot(residuals_df, aes(x = res2, y = res1)) + 
  geom_point() +                        # points
  geom_smooth(method = "lm", col = "red", lwd = 1.2) + 
  labs(x = "presvote", y = "voteshare",  
       title = "Scatterplot of the residuals with regression line") +
  theme_minimal()                
scatter4
#Part 3 - prediction equation
coef_model4 <- coef(model4)
#coef_model4
intercept <- coef_model4[1] 
slope <- coef_model4[2] 
#writing prediction equation below
cat("Prediction equation: residuals in model 1 =", round(intercept, 3), "+", round(slope, 3), "* residuals in model 2\n")
#Interpretation: on average, for every 1 unit increase in the residuals in model 2, the  residuals in model 1 is expected to increase by 0.257 (slope). The  residuals in model 1 without the ones in model 2 is predicted to be 0 (intercept)


#Question 5
#Part 1 - multiple regression
model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
#seeing the summary
summary(model5)
#Part 2 - prediction equation
coef_model5 <- coef(model5)
#coef_model5
intercept <- coef_model5[1] 
difflog <- coef_model5[2] 
presvote <- coef_model5[3] 
#writing prediction equation below
cat("Prediction equation: voteshare =", round(intercept, 3), "+", round(difflog, 3), "* difflog +", round(presvote, 3), "* presvote\n")
#Interpretation: on average, for every 1 unit increase in the difflog, the  voteshare is expected to increase by 0.0355 (slope) and on average, for every 1 unit increase in the presvote, the  voteshare is expected to increase by 0.256 (slope2). The  voteshare without both variable is predicted to be 0.448 (intercept)
#Part 3
#The slope is the same as in question 4 which is 0.449. This is because the slope in multiple regression of multiple variable is the same as the slope of the residuals of the effect of thee 2 variables on the outcome



















