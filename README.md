# dry-bean-classification-using-ML
dry bean classification using ML
library(readxl)
library(tidyverse)
library(tidymodels)
library(scales)
theme_set(theme_light())
library(visdat)
library(inspectdf)
library(embed)
beandata=read.csv("C:/Users/USER/Downloads/Dry_Bean_Dataset.csv")
beandata 

Class=beandata$Class
beandata %>% 
  vis_dat()
beandata %>% 
  select(-`Class`) %>% 
  inspect_num() %>% 
  show_plot()
beandata %>% 
  select(-`Class`) %>% 
  select(where(is.numeric)) %>% 
  vis_cor()

barplot(table(beandata$Class), main ="Barplot for the Dry Bean Classes")

table(beandata$Class)


#Extracting missing values
misval<-sapply(beandata,function(x) sum(length(which (is.na(x)))))
misval
require(kableExtra)
table(misval)
kable(as.data.frame(misval)) %>%
  kable_styling(bootstrap_options=c("stripped","hover","condensed","responsive" ))

beandata=beandata[,1:16]
beandata

head(beandata)
require(corrr)
require(ggcorrplot)
require(FactoMineR)

normalbean=scale(beandata)
normalbean
head(normalbean)
beandata=data.frame(normalbean,Class)
beandata
cormatrix= cor(normalbean)
ggcorrplot(cormatrix)


##DECISION TREE CLASSIFICATION

# Load the rpart package
library(rpart)
length(beandata$Area)

# Split the data into training and testing sets
set.seed(1234)
train_index <- sample(1:nrow(beandata), 0.7*nrow(beandata))
train_data <- beandata[train_index, ]
test_data <- beandata[-train_index, ]
train_data
test_data
# Train a decision tree model using the training data
tree_model <- rpart(Class ~ ., data = train_data, method = "class")
tree_model
# Plot the decision tree
rpart.plot(tree_model)
# Predict the species of the test data using the trained model
predictions <- predict(tree_model, newdata = test_data, type = "class")
predictions
# Print the confusion matrix
confusion_matrix <- table(predictions, test_data$Class)
print(confusion_matrix)

# Calculate the accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste0("Accuracy: ", round(accuracy, 4)))


### RANDOM FOREST CLASSIFICATION
library(randomForest)


beandata$Class <- as.factor(beandata$Class)
# Split data into training and testing sets
set.seed(123)
train_index <- sample(nrow(beandata), 0.7 * nrow(beandata))
train_data <- beandata[train_index, ]
test_data <- beandata[-train_index, ]
train_data
test_data
# Train random forest model
rf_model <- randomForest(Class ~ ., data = train_data, ntree = 1)

# Predict on test set
rf_pred <- predict(rf_model, newdata = test_data)

# Evaluate model performance
table(rf_pred, test_data$Class)
accuracy=sum(diag(table(rf_pred, test_data$Class)))/sum(table(rf_pred, test_data$Class))
accuracy


####SVM CLASSIFICATION
# Load the required packages
library(e1071)

# Split data into training and testing sets
set.seed(123)
train_index <- sample(nrow(beandata), 0.7 * nrow(beandata))
train_data <- beandata[train_index, ]
test_data <- beandata[-train_index, ]

# Train the SVM model
svm_model <- svm(Class ~ ., data = train_data, kernel = "polynomial")

# Make predictions on the test set
svm_pred <- predict(svm_model, newdata = test_data)

# Evaluate model performance
table(svm_pred, test_data$Class)
accuracy=sum(diag(table(svm_pred, test_data$Class)))/sum(table(svm_pred, test_data$Class))
accuracy

