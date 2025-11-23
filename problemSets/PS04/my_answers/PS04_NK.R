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
lapply(c("car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Question 1
install.packages("car")
library(car)
data(Prestige)
help(Prestige)
#Part A
#head(Prestige)
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0) #creating the variable and setting it as follows: if type is Prof then Professional Variable is 1 else it is 0
#head(Prestige)
#table(Prestige$professional)

#Part B
#running linear model in a  continuous dummy interaction
interact_reg <- lm(prestige ~ income + Professional + income:Professional, data = Prestige)
summary(interact_reg) #seeing the results
#The prestige is estimated to be 21.14 if income is equal to 0 and for non-professionals
#on average, for non-professionals, the prestige will increase 0.003 for every 1 unit of income increase
#on average the prestige will increase 37.78 for every 1 unit of professional (type is Prof) increase when income is 0
#on average, for professionals, the effect of income on prestige will decrease by 0.002 

#Part C
#predicition equation
prestige <- 21.14 + (0.003 * income) + (37.78 * Professional)  - (0.002*(income*Professional))
                                                                
#Part D
#Coeff of Income
#on average, for non-professionals, the prestige will increase 0.003 for every 1 unit of income increase


#Part E
#coeff of Professional
#on average the prestige will increase 37.78 for every 1 unit of professional (type is Prof) increase when income is 0


#Part F
prestige_F = 21.14 + (0.003 * 1000) + (37.78 * 1) - (0.002*(1000))
prestige_F #the prestige will be 59.92
marginal_effect_calcul = 0.003 - 0.002
marginal_effect_calcul #0.001 per unit of income
marginal_effect <- marginal_effect_calcul * 1000
marginal_effect #1 increase in prestige
#the predicted prestige for a professional with $1,000 income is 59.92
#the marginal effect of a 1000 usd increase in income for professionals is an increase of 1 unit to prestige
                                                                
#Part G
prestige_G = 21.14 + (0.003 * 6000) + (37.78 * 1) - (0.002*(6000))
prestige_G #the prestige will be 64.92
marginal_effect_calcul_g = 37.78 - 0.002 * 6000
marginal_effect_calcul_g #25.78 per professional
#the predicted prestige for a professional with 6000 USD income is 64.92
#he marginal effect of changing from non-professional to professional at 6000 usd income is an increase of 25.78 units in prestige
                                               
#Question 2
#Part A
coef <- 0.042
se <- 0.016
t_value <- coef/se
p_value <- 2*(1-pnorm(abs(t_value)))
p_value #0.008
#0.008 is less than 0.05 (pvalue<0.05) then we reject the null hypothesis that the yard signs signficantly affect vote share


#part B
coef_2 <- 0.042
se_2 <- 0.013
t_value_2 <- coef_2/se_2
p_value_2 <- 2*(1-pnorm(abs(t_value_2)))
p_value_2 #0.0012
#since 0.0012 < 0.05, we reject the null hypothesis that being adjacent to precincts with yard signs has no effect on vote share


#part C
#the intercept is 0.302. This is the expected vote share if there are no signs neither in the precincts or adjacent to it

#part D
#r squared is 0.094. this means that the model explains only 9.4% of the variation in the vote share across projects
#the remaining 90.6% of the variation is explained by other factors that are not included in the model
#This means that while yard signs have an effect, they are not a major determinant of election results
