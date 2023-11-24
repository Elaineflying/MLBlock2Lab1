# Load necessary library
library(randomForest)

generate_train_test_data <- function(size,condition) {
  # Check the arguments
  if ( !is.numeric(size) ) {
    stop("The argument size should be numeric type.")
  }
  conditions <- c("x1<x2", "x1<0.5", "(x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)")
  if ( !condition %in% conditions) {
    stop("The argument condition should be one of those: `x1<x2`, `x1<0.5`, `(x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)`.")
  }

  # Set seed for reproducibility
  set.seed(1234)

  # Generate dataset
  x1 <- runif(size)
  x2 <- runif(size)
  #datasets <- cbind(x1, x2)
  y <- as.numeric(eval(parse(text=condition)))
  #true_labels <- as.factor(y)
  datasets <- data.frame(x1=x1, x2=x2, true_labels=as.factor(y))
  return(datasets)
}

# Function to train and predict with random forest
train_and_predict <- function(train_dataset,test_dataset,ntree,nodesize) {
  rf_model <- randomForest(true_labels ~ ., data = train_dataset, ntree = ntree, nodesize = nodesize, keep.forest = TRUE)
  predicted_labels <- predict(rf_model, newdata = test_dataset)
  mean_mis_error <- mean(predicted_labels != test_dataset$true_labels)
  var_mis_error <- var(predicted_labels != test_dataset$true_labels)
  return(list(predicted_labels=predicted_labels,mean_mis_error=mean_mis_error,var_mis_error=var_mis_error))
}

# Initialize variables
ntrees <- c(1, 10, 100)
nodesizes <- c(25, 12)

# Train and predict for 1, 10, and 100 trees with condition "x1<x2" and nodesize=25
results_list1 <- list()
train_dataset1 <- generate_train_test_data(size = 100, condition = 'x1<x2' )
test_dataset1 <- generate_train_test_data(size = 1000, condition = 'x1<x2' )
for ( i in 1:length(ntrees) ) {
  results_list1$ntrees[i] <- ntrees[i]
  rf_model_results1 <- train_and_predict(train_dataset = train_dataset1, test_dataset = test_dataset1, ntree = ntrees[i], nodesize = nodesizes[1])
  results_list1$mean_misclassification_error[i] <- rf_model_results1$mean_mis_error
  results_list1$variance_misclassification_error[i] <- rf_model_results1$var_mis_error
  plot(test_dataset1$x1,test_dataset1$x2,col=(as.numeric(rf_model_results1$predicted_labels)+1), main = "random trees", xlab="x1", ylab="x2")
}
results_df1 <- as.data.frame(results_list1)
cat("Train and predict for 1, 10, and 100 trees with condition \"x1<x2\" and nodesize=25")
print(results_df1)



# Train and predict for 1, 10, and 100 trees with condition "x1<0.5" and nodesize=25
results_list2 <- list()
train_dataset2 <- generate_train_test_data(size = 100, condition = 'x1<0.5' )
test_dataset2 <- generate_train_test_data(size = 1000, condition = 'x1<0.5' )
for ( i in 1:length(ntrees) ) {
  results_list2$ntrees[i] <- ntrees[i]
  rf_model_results2 <- train_and_predict(train_dataset = train_dataset2, test_dataset = test_dataset2, ntree = ntrees[i], nodesize = nodesizes[1])
  results_list2$mean_misclassification_error[i] <- rf_model_results2$mean_mis_error
  results_list2$variance_misclassification_error[i] <- rf_model_results2$var_mis_error
  plot(test_dataset2$x1,test_dataset2$x2,col=(as.numeric(rf_model_results2$predicted_labels)+1), main = "random trees", xlab="x1", ylab="x2")
}
results_df2 <- as.data.frame(results_list2)
cat("Train and predict for 1, 10, and 100 trees with condition \"x1<0.5\" and nodesize=25")
print(results_df2)

# Train and predict for 1, 10, and 100 trees with condition "(x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)" and nodesize=12
results_list3 <- list()
train_dataset3 <- generate_train_test_data(size = 100, condition = '(x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)' )
test_dataset3 <- generate_train_test_data(size = 1000, condition = '(x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)' )
for ( i in 1:length(ntrees) ) {
  results_list3$ntrees[i] <- ntrees[i]
  rf_model_results3 <- train_and_predict(train_dataset = train_dataset3, test_dataset = test_dataset3, ntree = ntrees[i], nodesize = nodesizes[2])
  results_list3$mean_misclassification_error[i] <- rf_model_results3$mean_mis_error
  results_list3$variance_misclassification_error[i] <- rf_model_results3$var_mis_error
  plot(test_dataset3$x1,test_dataset3$x2,col=(as.numeric(rf_model_results3$predicted_labels)+1), main = "random trees", xlab="x1", ylab="x2")
}
results_df3 <- as.data.frame(results_list3)
cat("Train and predict for 1, 10, and 100 trees with condition \"(x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)\" and nodesize=12")
print(results_df3)

