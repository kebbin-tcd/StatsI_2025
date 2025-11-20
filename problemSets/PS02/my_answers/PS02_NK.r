#1 employee can make illegal lefts turns to draw attention of the police officers
#2 employees are upper class and 2 are lower class
#Null hypothesis: Ho = the variables are statistically independent
#Alternative hypothesis: Ha = the variable are statistically dependent

#question 1 part (a)
#calculating test statistic that is distributed according to the x2 distribution yb hand
#defining vectors
#calculating fobserved & fexpected
upper_class <- c("Not stopped" = 14, "Bribe requested" = 6, "Stopped" = 7)
lower_class <- c("Not stopped" = 7, "Bribe requested" = 7, "Stopped" = 1)
upper_class_row_total <- sum(upper_class)
lower_class_row_total <- sum(lower_class)
not_stopped_column <- upper_class["Not stopped"] + lower_class["Not stopped"]
bribe_requested_column <- upper_class["Bribe requested"] + lower_class["Bribe requested"]
stopped_column <- upper_class["Stopped"] + lower_class["Stopped"]
grand_total <- sum(upper_class_row_total+lower_class_row_total)
#claculating the expected values
fe_not_stopped_uc <- (upper_class_row_total*not_stopped_column)/grand_total
fe_not_stopped_lc <- (lower_class_row_total*not_stopped_column)/grand_total
fe_bribe_requested_uc <- (upper_class_row_total*bribe_requested_column)/grand_total
fe_bribe_requested_lc <- (lower_class_row_total*bribe_requested_column)/grand_total
fe_stopped_uc <- (upper_class_row_total*stopped_column)/grand_total
fe_stopped_lc <- (lower_class_row_total*stopped_column)/grand_total
#putting the expected values in a vectors for each class
expected_uc <- c(fe_not_stopped_uc, fe_bribe_requested_uc, fe_stopped_uc)
expected_lc <- c(fe_not_stopped_lc, fe_bribe_requested_lc, fe_stopped_lc)
#putting all observed and expected values in vectors each
observed_values <- c(upper_class, lower_class)
exepected_values <- c(expected_uc, expected_lc)
#claculating test statistic
x2_test_statistic <- sum(((observed_values-exepected_values)^2)/(exepected_values))
x2_test_statistic

#question 1 part (b)
#calculating the exepected_value
#calculating the degrees of freedom, rows-1 * columns-1
p_value <- pchisq(x2_test_statistic,df = 2,lower.tail=FALSE)
#p_value
#since the p-value is 0.1502306 and alpha = 0.1 then p_value is greater than alpha and hence, we fail to reject the Null
#hypothesis that the variables are statistically independent.

#question 1 part (c)
#calculating the standarized residuals for each cell
row_proportion_uc <- upper_class_row_total/grand_total
row_proportion_lc <- lower_class_row_total/grand_total
not_stopped_column_proportion <- not_stopped_column/grand_total
bribe_requested_column_proportion <- bribe_requested_column/grand_total
stopped_column_proportion <- stopped_column/grand_total
#putting the proportions into vectors
row_proportions_vector <- c(rep(row_proportion_uc, 3), rep(row_proportion_lc, 3))
col_proportions_vector <- rep(c(not_stopped_column_proportion, bribe_requested_column_proportion, stopped_column_proportion), 2)
variance <- exepected_values * (1-row_proportions_vector) * (1-col_proportions_vector)
se = sqrt(variance)
standarized_residuals <- (observed_values-exepected_values)/se
z_matrix <- matrix(
  standarized_residuals,
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    c("Upper Class", "Lower Class"),
    c("Not Stopped", "Bribe Requested", "Stopped"))
)
print(round(z_matrix, 4))
#data <- rbind(upper_class, lower_class)
#chisq.test(data)
#test <- chisq.test(data)
#test$stdres


#question 1 part (d)
#We can see from the standarized residuals that upper class drivers are mosre likely to be stopped given the residual is 1.523 than being asked for a bribe (-1.642) or than not being stopped (0.322)
#We can also see that lower class drivers are more likely to be asked for a bribe given the residual is 1.642 than being stopped (-1.523) or than not being stopped (-0.322)


#question 2 part 1
#Ho null hypothesis: having a reservation policy (female) head has no effect on the number of new/repaired drinking water facilities in villages.
#Ha alternate hypothesis: having reservation policy (no female) head has a positive or negative effect on the number of new/repaired drinking water facilities in villages.

#question 2 part 2
#bivariate regression

my_table <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
head(my_table)
#calculating the means for each
mean_water_Y <- mean(my_table$water)
mean_reservation_X <- mean(my_table$reserved)
#calcuolating sums
sum_water_Y <- sum(my_table$water)
sum_reservation_X <- sum(my_table$reserved)
#claculating numerators for each
num_w_Y <- my_table$water - mean_water_Y
num_r_X <- my_table$reserved - mean_reservation_X
num <- sum((num_w_Y)*(num_r_X))
#calculating denominator
denom <- sum((num_r_X)^2)
#calculating beta
beta <- num/denom
beta
alpha = mean_water_Y - (beta*mean_reservation_X)
alpha
model <- lm(water ~ reserved, data = my_table)
summary(model)


#question 2 part 3
#beta is 9.252423 and alpha is 14.73832
# beta is >0 - so villages with female heads (reservation policy) have more drinking water, around +9.252423, than villages with no female heads.
# On average, a 1 female head reservation policy leads to a 9.252423 increase in the drinking water
#alpha is 14.73832 so villages with no female heads have around 14.73832 new/repaired drinking water
#When there is no reservation policy (no female heads), the predicted value for new/repaired drinking water is 14.73832. Note that is may not be a meaningful quantity.
