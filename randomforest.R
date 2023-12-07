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
num_interation <- 1000
# Train and predict for 1, 10, and 100 trees with condition "x1<x2" and nodesize=25
mean_mt1 <- matrix(ncol = num_interation, nrow = 3)
variance_mt1 <- matrix(ncol = num_interation, nrow = 3)
train_dataset1 <- generate_train_test_data(size = 100, condition = 'x1<x2' )
test_dataset1 <- generate_train_test_data(size = 1000, condition = 'x1<x2' )
for ( i in 1:length(ntrees) ) {
  for ( j in 1:num_interation ) {
    rf_model_results1 <- train_and_predict(train_dataset = train_dataset1, test_dataset = test_dataset1, ntree = ntrees[i], nodesize = nodesizes[1])
    mean_mt1[i,j] <- rf_model_results1$mean_mis_error
    variance_mt1[i,j] <- rf_model_results1$var_mis_error
    #plot(test_dataset1$x1,test_dataset1$x2,col=(as.numeric(rf_model_results1$predicted_labels)+1), main = "random trees", xlab="x1", ylab="x2")
  }
}

results_df1 <- data.frame(ntrees=c("1", "10", "100"),
                             mean_misclassification_error=c(mean(mean_mt1[1,]),
                                                            mean(mean_mt1[2,]),
                                                            mean(mean_mt1[3,])),
                             variance_misclassification_error=c(mean(variance_mt1[1,]),
                                                                mean(variance_mt1[2,]),
                                                                mean(variance_mt1[3,])))

cat("Train and predict for 1, 10, and 100 trees with condition \"x1<x2\" and nodesize=25: \n")
print(results_df1)



# Train and predict for 1, 10, and 100 trees with condition "x1<0.5" and nodesize=25
mean_mt2 <- matrix(ncol = num_interation, nrow = 3)
variance_mt2 <- matrix(ncol = num_interation, nrow = 3)
train_dataset2 <- generate_train_test_data(size = 100, condition = 'x1<0.5' )
test_dataset2 <- generate_train_test_data(size = 1000, condition = 'x1<0.5' )
for ( i in 1:length(ntrees) ) {
  for ( j in 1:num_interation ) {
    rf_model_results2 <- train_and_predict(train_dataset = train_dataset2, test_dataset = test_dataset2, ntree = ntrees[i], nodesize = nodesizes[1])
    mean_mt2[i,j] <- rf_model_results2$mean_mis_error
    variance_mt2[i,j] <- rf_model_results2$var_mis_error
    #plot(test_dataset2$x1,test_dataset2$x2,col=(as.numeric(rf_model_results2$predicted_labels)+1), main = "random trees", xlab="x1", ylab="x2")
  }
}
results_df2 <- data.frame(ntrees=c("1", "10", "100"),
                          mean_misclassification_error=c(mean(mean_mt2[1,]),
                                                         mean(mean_mt2[2,]),
                                                         mean(mean_mt2[3,])),
                          variance_misclassification_error=c(mean(variance_mt2[1,]),
                                                             mean(variance_mt2[2,]),
                                                             mean(variance_mt2[3,])))
cat("Train and predict for 1, 10, and 100 trees with condition \"x1<0.5\" and nodesize=25: \n")
print(results_df2)

# Train and predict for 1, 10, and 100 trees with condition "(x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)" and nodesize=12
mean_mt3 <- matrix(ncol = num_interation, nrow = 3)
variance_mt3 <- matrix(ncol = num_interation, nrow = 3)
train_dataset3 <- generate_train_test_data(size = 100, condition = '(x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)' )
test_dataset3 <- generate_train_test_data(size = 1000, condition = '(x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)' )
for ( i in 1:length(ntrees) ) {
  for ( j in 1:num_interation) {
    rf_model_results3 <- train_and_predict(train_dataset = train_dataset3, test_dataset = test_dataset3, ntree = ntrees[i], nodesize = nodesizes[2])
    mean_mt3[i,j] <- rf_model_results3$mean_mis_error
    variance_mt3[i,j] <- rf_model_results3$var_mis_error
    #plot(test_dataset3$x1,test_dataset3$x2,col=(as.numeric(rf_model_results3$predicted_labels)+1), main = "random trees", xlab="x1", ylab="x2")
  }
}
results_df3 <- data.frame(ntrees=c("1", "10", "100"),
                          mean_misclassification_error=c(mean(mean_mt3[1,]),
                                                         mean(mean_mt3[2,]),
                                                         mean(mean_mt3[3,])),
                          variance_misclassification_error=c(mean(variance_mt3[1,]),
                                                             mean(variance_mt3[2,]),
                                                             mean(variance_mt3[3,])))
cat("Train and predict for 1, 10, and 100 trees with condition \"(x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)\" and nodesize=12: \n")
print(results_df3)

