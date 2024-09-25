##########################  Cleansing Data and IMputation, Week 5, HAP 819#################
library(dplyr)
library(moments)
library(car)
library(mice)


### Take a 10% sample
df <- read.csv("BSTT10.csv", header = TRUE)
BSTT10 <-df %>% sample_frac(0.10)

### Write out a csv file for those who are using Stata.
### write.csv(BSTT10,file="BSTT10.csv")

### Now start data cleansing
### Get a look at the file first
summary(BSTT10)
dim(BSTT10)
## 1] 206301     23
sum(is.na(df))
##[1] 2573203
## Tons and tons of NAs. Maybe that's why this data set was chosen?

### We don't need the first column, it is just IDs. Remove it. 
BSTT10$id <- NULL
dim(BSTT10)
###  [1] 206301     22; we are down to 22 columns

### Note that bs111r and bs151r are all NA's, so we also remove those two columns
BSTT10a <- BSTT10 [,!sapply(BSTT10,function(col)all(is.na(col)))]
dim(BSTT10a)
### [1] 206301     20; we are down to 20 columns
sum(is.na(BSTT10a))  ### [1] 2367412 lots of NAs! 
bstttest <- BSTT10a[1:20,]
bstttest
## Note all the NAs, just in the first 20 rows.

Imputation approach 1: remove all cases that have 1 or more NAs. 
bstttest[complete.cases(bstttest),]
bsttnonas <- BSTT10a[complete.cases(BSTT10a),]
dim(bsttnonas)
## All, or almost all, rows have at least one NA. This approach will not work.




# Plot Progression and Check Distribution
hist(BSTT10a$bs1lr, main = "Progression Distribution", xlab = "Progression Score")
qqnorm(BSTT10a$bs1lr)
qqline(BSTT10a$bs1lr)
# the data is not normal. We can try transformations to make it normal, but not in this lab.

#################################Try MICE imputation ##################################
# Specify the Variables for imputation
vars_to_impute<-c("bs1lr","bs2lr","bs3lr","bs4lr","bs5lr", "bs6lr", "bs7lr",
                  "bs8lr", "bs9lr")
# Set the number of imputations
num_imputations<-5
# Create multiple imputations
imp_data <- mice::mice(BSTT10a[, vars_to_impute], 
                       m = num_imputations, method = "pmm")

summary(imp_data)
# Create an empty list to store regression results
regression_results<-list()
# Compute regression results
for (i in 1:num_imputations) {
  model <- lm(bs1lr ~ bs2lr + bs3lr + bs4lr + bs5lr + bs6lr +
                bs7lr + bs8lr + bs9lr, 
              data = mice::complete(imp_data, action = i)) 
  regression_results[[i]] <- summary(model)
}

### Print percentage of variation explained
avg_rsquared <- mean(sapply(regression_results, function(result) result$r.squared))
cat("Average percent of variation explained:", round(avg_rsquared*100,2),"%")
## 3.47%; this may vary, as each sample is different
## Show model coefficients
summary(model)

#####Try with NAs made to 0##############################
BSTT10b <- BSTT10a[is.na(BSTT10a)]<-0
BSTT10b <- BSTT10
# Fit the linear regression model
model <- lm(bs1lr ~ bs2lr + bs3lr + bs4lr + bs5lr + bs6lr +
              bs7lr + bs8lr + bs9lr, 
            data = BSTT10b)
class(BSTT10b)


summary(model)

# Calculate R-squared, which is the percent of variation explained
rsquared<-summary(model)$r.squared
cat("Percent of variation explained:", round(rsquared * 100, 2), "%\n")
##3.52%, again may vary

#########Try with NAs converted to mean###########################
# Drop the independent variables that no need be used and replace NA with mean
BSTT10c <- BSTT10a %>%
  select(-dm, -TestTrain) %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Fit the linear regression model
model<-lm(bs1lr ~ bs2lr + bs3lr + bs4lr + bs5lr + bs6lr +
            bs7lr + bs8lr + bs9lr, data = BSTT10c)
summary(model)
# Calculate R-squared, which is the percent of variation explained
rsquared<-summary(model)$r.squared
cat("Percent of variation explained:", round(rsquared * 100, 2), "%\n")
# Percent of variation explained: 9.2 %







