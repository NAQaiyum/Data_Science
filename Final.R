#setwd("D:/11th Semester/Data Science/Final")
us_births <- read.csv("D:/11th Semester/Data Science/Final/us_births_2016_2021.csv")
us_births

#View the structure of the dataset
str(us_births)

#First few row of the dataset
head(us_births)

#Column name of the data set
names(us_births)

#Find the type of this dataset column
sapply(us_births, class)

#Summary statistics of numeric variables
summary(us_births)

#Delete Column (State Abbreviation):
us_births <- us_births[, -which(names(us_births) == "State.Abbreviation")]
print(us_births)



#Conversion............................................................................................................



#Categorical to Numeric (State column)
us_births$State <- factor(us_births$State, levels=c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"), labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51))
us_births


#Categorical to Numeric (Gender column)
us_births$Gender <- factor(us_births$Gender, levels=c("F","M"), labels = c(1,2))
us_births

#Categorical to Numeric (Education Level of Mother column)
us_births$Education.Level.of.Mother <- factor(us_births$Education.Level.of.Mother, levels=c("8th grade or less","9th through 12th grade with no diploma","High school graduate or GED completed","Some college credit, but not a degree","Associate degree (AA, AS)","Bachelor's degree (BA, AB, BS)","Master's degree (MA, MS, MEng, MEd, MSW, MBA)","Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, LLB, JD)","Unknown or Not Stated"), labels = c(1,2,3,4,5,6,7,8,9))
us_births

#Finding the missing value for all attributes: 
number_of_missing_value=colSums(is.na(us_births))
number_of_missing_value


#Delete a column(Educatuion Level of code).........................................................................
us_births <- us_births[, -which(names(us_births) == "Education.Level.Code")]
print(us_births)




#Normalization.......................................................................................
.libPaths()
install.packages("dplyr", repos = "https://cran.rstudio.com/")


library(dplyr)
us_births <- as.data.frame(sapply(us_births, as.numeric))
min_max_norm <- function(x) { 
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

normalized_data <- us_births %>%
  mutate(across(everything(), min_max_norm)) 
print(normalized_data)






#Correlation.......................................................................................

# Load the required library (if not already installed)
# install.packages("dplyr")
#library(dplyr)


# Calculate the correlation between "Education.Level.of.Mother" and "State"
correlation <- cor(normalized_data$Education.Level.of.Mother, normalized_data$State)
print(correlation)

# Calculate the correlation between "Education.Level.of.Mother" and "Year"
correlation <- cor(normalized_data$Education.Level.of.Mother, normalized_data$Year)
print(correlation)

# Calculate the correlation between "Education.Level.of.Mother" and "Gender"
correlation <- cor(normalized_data$Education.Level.of.Mother, normalized_data$Gender)
print(correlation)

# Calculate the correlation between "Education.Level.of.Mother" and "Number.of.Births"
correlation <- cor(normalized_data$Education.Level.of.Mother, normalized_data$Number.of.Births)
print(correlation)

# Calculate the correlation between "Education.Level.of.Mother" and "Average.Age.of.Mother..years."
correlation <- cor(normalized_data$Education.Level.of.Mother, normalized_data$Average.Age.of.Mother..years.)
print(correlation)

# Calculate the correlation between "Education.Level.of.Mother" and "Average.Birth.Weight..g."
correlation <- cor(normalized_data$Education.Level.of.Mother, normalized_data$Average.Birth.Weight..g.)
print(correlation)



#Plot Correlation Matrix............................................
install.packages("corrplot")

library(corrplot)
plot<-cor(normalized_data)
corrplot(plot,method="color")



#..........................................................................................................................................
# Apply KNN classification algorithm to the data set that contains only the important attributes selected using the correlation technique




install.packages("caret")   # for preprocessing
install.packages("class")   # for KNN classification
library(caret)
library(class)

# Assuming 'us_births' is your dataset
selected_attributes <- c("State", "Year", "Gender","Number.of.Births","Average.Age.of.Mother..years.", "Average.Birth.Weight..g.")
data <- us_births[, c(selected_attributes, "Education.Level.of.Mother")]

# Normalize numeric attributes
numeric_cols <- c("State", "Year","Gender","Number.of.Births","Average.Age.of.Mother..years.", "Average.Birth.Weight..g.")
data[numeric_cols] <- scale(data[numeric_cols])

# Convert 'gender' to factor
data$Gender <- as.factor(data$Gender)

#Train-Test Split
set.seed(123)  # For reproducibility
train_index <- createDataPartition(data$Education.Level.of.Mother, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

#KNN Classification
# Prepare predictors and target
predictors <- train_data[, selected_attributes]
target <- train_data$Education.Level.of.Mother

# Train KNN model
knn_model <- knn(train = predictors, test = test_data[, selected_attributes], cl = target, k = 5)

#Evaluate the Model
# Calculate accuracy
accuracy <- sum(knn_model == test_data$Education.Level.of.Mother) / length(knn_model)
cat("Accuracy:", accuracy, "\n")





#.......................................................................................................................................

#Training and testing 

random <- sample(1:nrow(normalized_data), 0.7 * nrow(normalized_data))

# Divide the data set into training and testing sets
Education.Level.of.Mother_train <- normalized_data[random, ]
Education.Level.of.Mother_test <- normalized_data[-random, ]

# Extract the labels (assuming "Education.Level.of.Mother" column is the label)
Education.Level.of.Mother_train_labels <- Education.Level.of.Mother_train$Education.Level.of.Mother
Education.Level.of.Mother_test_labels <- Education.Level.of.Mother_test$Education.Level.of.Mother

Education.Level.of.Mother_train
Education.Level.of.Mother_test


#Accuracy

install.packages("class")
library(class)
set.seed(123)
random <- sample(1:nrow(normalized_data), 0.7 * nrow(normalized_data))
Education.Level.of.Mother_train <- normalized_data[random, ]
Education.Level.of.Mother_test <- normalized_data[-random, ]
Education.Level.of.Mother_train_labels <- Education.Level.of.Mother_train$Education.Level.of.Mother
Education.Level.of.Mother_test_labels <- Education.Level.of.Mother_test$Education.Level.of.Mother

  k <- 3  
predicted_labels <- knn(train = Education.Level.of.Mother_train[, -which(names(Education.Level.of.Mother_train) == "Education.Level.of.Mother")],
                         test = Education.Level.of.Mother_test[, -which(names(Education.Level.of.Mother_test) == "Education.Level.of.Mother")],
                         cl = Education.Level.of.Mother_train_labels,
                         k = k)

accuracy <- sum(predicted_labels == Education.Level.of.Mother_test_labels) / length(Education.Level.of.Mother_test_labels)

cat("Accuracy:", accuracy, "\n")




# Dividing the data into training and test set.............................................................................................


library(class)

# Split the data into training and test sets
set.seed(123)
random <- sample(1:nrow(normalized_data), 0.7 * nrow(normalized_data))
train_data <- normalized_data[random, ]
test_data <- normalized_data[-random, ]

# Extract labels
train_labels <- train_data$Education.Level.of.Mother
test_labels <- test_data$Education.Level.of.Mother

# Define k value
k <- 3

# Train KNN classifier
knn_model <- knn(train = train_data[, -which(names(train_data) == "Education.Level.of.Mother")],
                 test = test_data[, -which(names(test_data) == "Education.Level.of.Mother")],
                 cl = train_labels,
                 k = k)

# Calculate accuracy
accuracy_approach1 <- sum(knn_model == test_labels) / length(test_labels)
cat("Accuracy (Dividing data into training and test sets):", accuracy_approach1, "\n")



# 10 fold cross validation.............................................................................................


install.packages("class")
install.packages("caret")

library(class)
library(caret)

set.seed(123)

num_folds <- 10

fold_indices <- createFolds(normalized_data$Education.Level.of.Mother, k = num_folds)

accuracies <- numeric(num_folds)

for (i in 1:num_folds) {
  
  
  
  test_indices <- fold_indices[[i]]
  
  train_indices <- setdiff(1:nrow(normalized_data), test_indices)
  
  
  
  Education.Level.of.Mother_train <- normalized_data[train_indices, ]
  
  Education.Level.of.Mother_test <- normalized_data[test_indices, ]
  
  
  
  
  
  input_features_train <- Education.Level.of.Mother_train[, c("State", "Year", "Gender","Number.of.Births", "Average.Age.of.Mother..years.", "Average.Birth.Weight..g.")]
  
  input_features_test <- Education.Level.of.Mother_test[, c("State", "Year", "Gender","Number.of.Births", "Average.Age.of.Mother..years.", "Average.Birth.Weight..g.")]
  
  Education.Level.of.Mother_train_labels <- Education.Level.of.Mother_train$Education.Level.of.Mother
  
  Education.Level.of.Mother_test_labels <- Education.Level.of.Mother_test$Education.Level.of.Mother
  
  
  k <- 3  # Set the value of 'k'
  
  predicted_labels <- knn(train = input_features_train,
                          
                          test = input_features_test,
                          
                          cl = Education.Level.of.Mother_train_labels,
                          
                          k = k)
  
  
  
  
  
  accuracies[i] <- sum(predicted_labels == Education.Level.of.Mother_test_labels) / length(Education.Level.of.Mother_test_labels)
  
}



mean_accuracy <- mean(accuracies)

cat("Mean Accuracy (10-Fold Cross-Validation):", mean_accuracy, "\n")







#Confusion matrix................................................................................................

# Load required packages
install.packages("class")
install.packages("caret")
library(class)
library(caret)

# Set seed for reproducibility
set.seed(123)

# Assuming 'normalized_data' is your original dataset
# Replace this with the correct name if necessary

# Number of folds for cross-validation
num_folds <- 10

# Create indices for cross-validation folds
fold_indices <- createFolds(normalized_data$Education.Level.of.Mother, k = num_folds)

# Initialize matrices to store confusion matrices and metrics
confusion_matrices <- list()
recalls <- numeric(num_folds)
precisions <- numeric(num_folds)

# Define a function to calculate recall and precision
calculate_metrics <- function(cm) {
  recall <- cm[1, 1] / sum(cm[1, ])
  precision <- cm[1, 1] / sum(cm[, 1])
  return(list(recall = recall, precision = precision))
}

# Perform 10-fold cross-validation
for (i in 1:num_folds) {
  # Split data into training and testing sets for this fold
  test_indices <- fold_indices[[i]]
  train_indices <- setdiff(1:nrow(normalized_data), test_indices)
  
  data_train <- normalized_data[train_indices, ]
  data_test <- normalized_data[test_indices, ]
  
  # Extract the input features and the decision attribute
  input_features_train <- data_train[, c("State", "Year", "Gender", "Number.of.Births", 
                                         "Average.Age.of.Mother..years.", "Average.Birth.Weight..g.")]
  input_features_test <- data_test[, c("State", "Year", "Gender", "Number.of.Births", 
                                       "Average.Age.of.Mother..years.", "Average.Birth.Weight..g.")]
  decision_train <- data_train$Education.Level.of.Mother
  decision_test <- data_test$Education.Level.of.Mother
  
  # Perform KNN classification
  k <- 3  # Set the value of 'k'
  predicted_decisions <- knn(train = input_features_train,
                             test = input_features_test,
                             cl = decision_train,
                             k = k)
  
  # Calculate confusion matrix for this fold
  confusion_matrices[[i]] <- table(predicted = predicted_decisions, actual = decision_test)
  
  # Calculate recall and precision for this fold
  metrics <- calculate_metrics(confusion_matrices[[i]])
  recalls[i] <- metrics$recall
  precisions[i] <- metrics$precision
}

# Calculate the mean recall and precision across folds
mean_recall <- mean(recalls)
mean_precision <- mean(precisions)

# Print mean recall and precision
cat("Mean Recall:", mean_recall, "\n")
cat("Mean Precision:", mean_precision, "\n")

# Print individual confusion matrices for each fold
for (i in 1:num_folds) {
  cat("Confusion Matrix (Fold", i, "):\n")
  print(confusion_matrices[[i]])
  cat("\n")
}





