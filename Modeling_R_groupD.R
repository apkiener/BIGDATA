
folder_path<-"/Users/apkiener/Desktop/project"
solar_data<-readRDS("/Users/apkiener/Desktop/project/solar_dataset.RData")
addit_data<-readRDS("/Users/apkiener/Desktop/project/additional_variables.RData")
station<- read.table(file.path(folder_path, "station_info.csv"), sep = ",", header = TRUE)

library(data.table)
install.packages("prediction")
library(prediction)
install.packages("caret");
library(caret);


#MODEL

#Linear Regression 
# First 20 PCAs 

train_data <-solar_data[1:5113,1:456]

test_data <-solar_data[5114:nrow(solar_data),100:ncol(solar_data)]

ACME<-lm(ACME~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14
           +PC15+PC16+PC17+PC18, data=train_data)


ACME_test_predict <- predict(ACME, newdata = test_data)
ACME_train_predict <-predict(ACME, newdata = train_data)


test_error <- ACME_test_predict - test_data$ACME
train_error<-ACME_train_predict - train_data$ACME

test_mean_abs_error<- round(mean(abs(test_error)), 2)
train_mean_abs_error<- round(mean(abs(train_error)), 2)

test_mean_sq_error<- round(mean(test_error^2), 2)
train_mean_sq_error<- round(mean(train_error^2), 2)

#..........Top 20 PCs

set.seed(10);

solar$Year <- as.numeric(substr(solar_data$Date,1,4));
solar$Month <- as.numeric(substr(solar_data$Date,5,6));
solar$Day <- as.numeric(substr(solar_data$Date,7,8));

#### Separate NAs from non- NA
prediction <- solar_data[is.na(solar_data$ACME)];
prediction_1 <- solar_data[!is.na(solar_data$ACME)];


#### split 75%

train_i <- sample(1:nrow(prediction_1), 0.75*nrow(prediction_1))
train <- prediction_1[train_i]; 
test  <- prediction_1[-train_i];

#Important Vars

vars_important<-function(dat, n_vars, y){
  varimport <- filterVarImp(x = dat, y=y, nonpara=TRUE);
  varimport <- data.table(variable=rownames(varimp),imp=varimport[, 1]);
  varimport <- varimport[order(-imp)];
  selected <- varimport$variable[1:n_vars];
  return(selected);
}

setDT(train);

compare <- data.table(model = "", 
                   mse_train = "", mae_train = "",
                   mse_test = "", mae_test = "");
final <- prediction[,1];

for (i in 2:99){
  print(i);
  
  train_final <- train[,..index];
  colnames(train_final)[colnames(train_final) %in% colnames(solar[,2:99])] <- "Prod_solar";
  train1 <- cbind(train_final,train[,100:459]);
  important_vars <- vars_important(dat = train_final[,2:ncol(train_final)], n_vars = 20, y = unlist(c(train_final[,1]),use.names=FALSE));
  print(important);
  
  f <- as.formula(paste(colnames(train_final[,1]), 
                        paste(important_vars, collapse = " + "), 
                        sep = " ~ "));
  
  model_train_final <- lm(f, data = train_final);
  
  model_train_final;
  
  #predictions
  predictions_train_1 <- predict(model_train_final, newdata = train);
  predictions_test_1 <- predict(model_train_final, newdata = test);
  
  
  train_error_1 <- predictions_train_1 - unlist(c(train[,..i]),use.names=FALSE);
  test_error_1 <- predictions_test_1 - unlist(c(test[,..i]),use.names=FALSE);
  
  
  mean_sq_err_train <- round(mean(train_error_1^2), 2);
  mean_abs_err_train <- round(mean(abs(train_error_1)), 2);
  
  mean_sq_err_test <- round(mean(test_error_1^2), 2);
  mean_abs_err_test <- round(mean(abs(test_error_1)), 2);
  
  #compare tables
  compare <- rbind(compare,data.table(model = colnames(train[,..i]), 
                           mean_sq_err_train = mean_sq_err_train, mean_abs_err_train = mean_abs_err_train,
                           mean_sq_err_test = mean_sq_err_test, mean_abs_err_test  = mean_abs_err_test));

  
  
  #final predictions
  Final_predict <- predict(model_train_final, newdata = prediction);
  final <- cbind(final, Final_predict);
  colnames(final)[colnames(final)=="final_predict"] <- colnames(train_final[,..index]);
}



write.csv(final, "/Users/apkiener/Desktop/project/final_model_predict.csv")


#Random Forest 
install.packages("randomForest")
library(randomForest);
ACME_5<-randomForest(ACME ~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14
                     +PC15+PC16+PC17+PC18, train_data,importance=T,proximity=T)

train_rf <- predict(ACME_5, newdata = train_data);
test_rf <- predict(ACME_5, newdata = test_data);

errors_train_rf <- train_rf - train_data$ACME;
errors_test_rf <- test_rf - test_data$ACME;

mse_train_rf <- round(mean(errors_train_rf^2), 2);
mae_train_rf <- round(mean(abs(errors_train_rf)), 2);
mse_test_rf<- round(mean(errors_test_rf^2), 2);
mae_test_rf <- round(mean(abs(errors_test_rf)), 2);

