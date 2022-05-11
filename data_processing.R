################################################################################
# Load essential libraries
library(tidyverse)
library(caret)

################################################################################
# Import the available data
data_sample <- read.csv("~/Downloads/applications/savii/data/data_sample.csv", 
                        stringsAsFactors=TRUE)
data_dict <- read.csv("~/Downloads/applications/savii/data/data_dictionary.csv", 
                        stringsAsFactors=FALSE)

################################################################################
# Take out index column as this column wont be useful 
data_sample <- data_sample %>%
  select(-X)

## data exclusion and update some values
## Variables Var29, Var30, Var116 all consists factor levels 0, 1, 2, 5, 10, 20.
## I decided to update levels that are 2-digits to 1 digit so that it would be easier for 
## doing the feature selection. Decided to replace 10 with 8, and 20 with 9.
data_sample <- data_sample %>%
  mutate(Var29 = ifelse(Var29 == '10', '8',
                        ifelse(Var29 == '20', '9', Var29)),
         Var30 = ifelse(Var30 == '10', '8',
                        ifelse(Var30 == '20', '9', Var30)),
         Var116 = ifelse(Var116 == '10', '8',
                         ifelse(Var116 == '20', '9', Var116)))  # update values to only 1-digit

################################################################################
# Data type conversion
# Charcter Types: member identifier and categorically nominal fields
# Factor Types: fold assignment and categorically ordinal fields
#               dummy variable fields

## List of columns that will be converted to character type
var_to_char <- c(data_dict[grepl('Id', data_dict$Type), 'Var'])

## List of columns that will be converted to factor type
var_to_fct <- c(data_dict[grepl('Fold', data_dict$Type), 'Var'], 
                data_dict[grepl('Ordinal', data_dict$Type), 'Var'],
                data_dict[grepl('Nominal', data_dict$Type), 'Var'])

## List of columns that dummy variables which will be converted to factor type
dum_to_fct <- data_dict[grepl('Dummy', data_dict$Type), 'Var']

# Transform fields to appropriate data types
data_sample <- data_sample %>%
  mutate(across(.cols=all_of(var_to_char), .fns=as.character)) %>%
  mutate(across(.cols=all_of(var_to_fct), .fns=factor)) %>%
  mutate(across(.cols=all_of(dum_to_fct), .fns=factor))

# additional data transformation 
# this resolves issues that happen during building prediction and prediction models
data_sample <- data_sample %>%
  select(-Var12)    # Var12 contains all 0s, this won't be helpful in building models

################################################################################
# Split train and test data sets
train_data <- data_sample %>% 
  filter(Fold == 'IS')
test_data <- data_sample %>% 
  filter(Fold == 'OS')                           # final test set

################################################################################
# # Create validation set out from training set with 80/20 split
# # The random sampling is done in each class and preserved the class 
# # distribution of the data
# set.seed(145)
# trainIndex <- createDataPartition(train_data$Target, p = .8, 
#                                   list = FALSE, 
#                                   times = 1)
# train_sample <- train_data[trainIndex,]          # training data for building model/s
# validation_sample  <- train_data[-trainIndex,]   # test data for building model/s


################################################################################
# removing highly correlated variables 
train_data_dup <- train_data %>%
  select(-ID, -Fold, -Target)

# correlation threshold
sig <- 0.8     # correlation value of variables considered to be very correlated

#convert data to numeric in order to run correlations
#convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
train_data_dup <- train_data_dup %>% mutate_if(is.character, as.factor)
train_data_dup <- train_data_dup %>% mutate_if(is.factor, as.numeric)

#run a correlation and drop the insignificant ones
corr <- cor(train_data_dup)

#prepare to drop duplicates and correlations of 1     
corr[lower.tri(corr,diag=TRUE)] <- NA 

#drop perfect correlations
corr[corr == 1] <- NA 
#turn into a 3-column table
corr <- as.data.frame(as.table(corr))
#remove the NA values from above 
corr <- na.omit(corr) 

#select significant values  
corr <- subset(corr, abs(Freq) > sig) 
#sort by highest correlation
corr <- corr[order(-abs(corr$Freq)),] 

# identify list of variables that are "redundant" or highly correlated with some other variable
# identify variables common between the 2 variable pairs
redundant_var <- intersect(corr$Var1, corr$Var2)             
# pick between Var1 and Var2 considered as redundant variable and add it to the 
# redundant_vcar list; in this case I choose Var2 to take out
redundant_var <- c(redundant_var, as.character(corr$Var2))  
# extract unique list of redundant variables
redundant_var <- unique(redundant_var)

# create new updated train data without highly correlated variables
train_data_xredun <- train_data[ , !(names(train_data) %in% redundant_var)]

################################################################################
# Remove unused data
rm(var_to_char, var_to_fct, dum_to_fct)
# rm(trainIndex)
rm(data_dict)
rm(train_data_dup)