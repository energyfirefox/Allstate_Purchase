train <- read.csv("rich_train_2step.csv")
str(train)
train <- train[train$extra_next_record == 1, ]

# predict value for NA risk _factor

# obtain train_set

train_risk <- train[!is.na(train$risk_factor), ]
test_risk <- train[is.na(train$risk_factor), ]
train_risk$risk_factor <- as.factor(train_risk$risk_factor)


library(FNN)


train_risk_knn <- train_risk[, c("homeowner", "location", "group_size", "car_age", "age_oldest", "age_youngest", "married_couple", "A", "B", "C", "D", "E", "F", "G")]
test_risk_knn <- test_risk[, c("homeowner", "location", "group_size", "car_age", "age_oldest", "age_youngest", "married_couple", "A", "B", "C", "D", "E", "F", "G")]
cl_risk <- train_risk[ , "risk_factor"]
results <- (1:4)[knn(train_risk_knn, test_risk_knn, cl_risk, k = 100, prob=TRUE)]

test_risk$risk_factor <- results

train_rf <- rbind(train_risk, test_risk)
train_rf <- train_rf[complete.cases(train_rf), ]
str(train_rf)


library(randomForest)

train_rf_t <- train_rf[  , c( "day", "group_size", "homeowner", "car_age", "risk_factor", "married_couple", "location", "age_youngest", "age_oldest", "C_previous", "A", "B", "C", "D", "E", "F", "G", "cost", "next_A", "next_B", "next_C", "next_D", "next_E", "next_F", "next_G")]
str(train_rf_t)


A_rfmod <- randomForest(y = as.factor(train_rf$next_A1), x = train_rf_t, ntree = 100, importance = TRUE)
B_rfmod <- randomForest(y = as.factor(train_rf$next_B1), x = train_rf_t, ntree = 100, importance = TRUE)
C_rfmod <- randomForest(y = as.factor(train_rf$next_C1), x = train_rf_t, ntree = 100, importance = TRUE)
D_rfmod <- randomForest(y = as.factor(train_rf$next_D1), x = train_rf_t, ntree = 100, importance = TRUE)
E_rfmod <- randomForest(y = as.factor(train_rf$next_E1), x = train_rf_t, ntree = 100, importance = TRUE)
F_rfmod <- randomForest(y = as.factor(train_rf$next_F1), x = train_rf_t, ntree = 100, importance = TRUE)
G_rfmod <- randomForest(y = as.factor(train_rf$next_G1), x = train_rf_t, ntree = 100, importance = TRUE)

# enrich test set with previous step

test <- read.csv("data/test_v2.csv")
next_step_df <- test[-1 , c("A", "B", "C", "D", "E", "F", "G", "record_type", "customer_ID")]
colnames(next_step_df) <- c("next_A", "next_B", "next_C", "next_D", "next_E", "next_F", "next_G", "next_record", "next_cust_ID")
rich_test_df <- cbind(test[-nrow(test), ], next_step_df)

# get only rows where oldCustomer != newCustomer
rich_test_df_short <- rich_test_df[rich_test_df$customer_ID != rich_test_df$next_cust_ID, ]


test <- test[ , c( "day", "group_size", "homeowner", "car_age", "risk_factor", "married_couple", "location", "age_youngest", "age_oldest", "C_previous", "A", "B", "C", "D", "E", "F", "G", "cost")]
test$C_previous <- test$C
test$location[is.na(test$location)] <- mean(test$location, na.rm = TRUE)
sum(is.na(test$risk_factor))


values <-  c("homeowner", "location", "group_size", "car_age", "age_oldest", "age_youngest", "married_couple")
inv_with_na_risk <- which(is.na(test$risk_factor))
inv_with_risk <- which(!is.na(test$risk_factor))

# filling risk_factor using KNN
risk_vals <- (1:4)[knn(test[inv_with_risk, values], test[inv_with_na_risk, values], test$risk_factor[inv_with_risk], k = 100, prob=TRUE)]
test$risk_factor[inv_with_na_risk] <- risk_vals

test$risk_factor <- as.factor(test$risk_factor)

# make oredictions

pred_A <- predict(A_rfmod, test)
pred_B <- predict(B_rfmod, test)
pred_C <- predict(C_rfmod, test)
pred_D <- predict(D_rfmod, test)
pred_E <- predict(E_rfmod, test)
pred_F <- predict(F_rfmod, test)
pred_G <- predict(G_rfmod, test)



result_full <- data.frame(customerID = test$customer_ID, A = pred_A, B = pred_B, C= pred_C, D = pred_D, E = pred_E, F = pred_F, G = pred_G)
str(result_full)

result_full$A <- as.character(result_full$A)
result_full$B <- as.character(result_full$B)
result_full$C <- as.character(result_full$C)
result_full$D <- as.character(result_full$D)
result_full$E <- as.character(result_full$E)
result_full$F <- as.character(result_full$F)
result_full$G <- as.character(result_full$G)

uniq_customers <- unique(test$customer_ID)

get_prediction_policy <- function(customer_ID){
  pred_vals <- tail(result_full[result_full$customerID == customer_ID, -c(1)], n =1)
  #   str(pred_vals)
  str_pred <- paste(pred_vals, sep ="", collapse = "")  
  print(customer_ID)
  return(str_pred)
}

get_prediction_policy(uniq_customers[1])
results <- sapply(uniq_customers, get_prediction_policy)
submission <- data.frame(customer_ID = uniq_customers, plan = results)
head(submission)
write.table(submission, file = 'new_base_submission.csv', sep = ",", row.names = FALSE)
getwd()
