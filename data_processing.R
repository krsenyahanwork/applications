# Load essential libraries
library(tidyverse)
library(caret)

# Import the available data
data_sample <- read.csv("~/Downloads/applications/savii/data/data_sample.csv", 
                        stringsAsFactors=TRUE)
data_dict <- read.csv("~/Downloads/applications/savii/data/data_dictionary.csv", 
                        stringsAsFactors=FALSE)

# Take out index column as this column wont be useful 
data_sample <- data_sample %>%
  select(-X)

# Data type conversion
# Charcter Types: member identifier and categorically nominal fields
# Factor Types: fold assignment and categorically ordinal fields
#               dummy variable fields

## List of columns that will be converted to character type
var_to_char <- c(data_dict[grepl('Id', data_dict$Type), 'Var'], 
                 data_dict[grepl('Nominal', data_dict$Type), 'Var'])

## List of columns that will be converted to factor type
var_to_fct <- c(data_dict[grepl('Fold', data_dict$Type), 'Var'], 
                data_dict[grepl('Ordinal', data_dict$Type), 'Var'])

## List of columns that dummy variables which will be converted to factor type
dum_to_fct <- data_dict[grepl('Dummy', data_dict$Type), 'Var']

# Transform fields to appropriate data types
data_sample <- data_sample %>%
  mutate(across(.cols=all_of(var_to_char), .fns=as.character)) %>%
  mutate(across(.cols=all_of(var_to_fct), .fns=factor)) %>%
  mutate(across(.cols=all_of(dum_to_fct), .fns=factor))

# Split train and test data sets
train_data <- data_sample %>% 
  filter(Fold == 'IS')
test_data <- data_sample %>% 
  filter(Fold == 'OS')                           # final test set

# # Create validation set out from training set with 80/20 split
# # The random sampling is done in each class and preserved the class 
# # distribution of the data
# set.seed(145)
# trainIndex <- createDataPartition(train_data$Target, p = .8, 
#                                   list = FALSE, 
#                                   times = 1)
# train_sample <- train_data[trainIndex,]          # training data for building model/s
# validation_sample  <- train_data[-trainIndex,]   # test data for building model/s



# Remove unused data
rm(var_to_char, var_to_fct, dum_to_fct)
# rm(trainIndex)
rm(data_dict)