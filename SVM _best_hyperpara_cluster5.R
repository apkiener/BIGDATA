### SVM Model best hyperparamaters and clustering

folder_path<-"/Users/apkiener/Desktop/project"
solar_data<-readRDS("/Users/apkiener/Desktop/project/solar_dataset.RData")
addit_data<-readRDS("/Users/apkiener/Desktop/project/additional_variables.RData")
station<- read.table(file.path(folder_path, "station_info.csv"), sep = ",", header = TRUE)

install.packages("e1071")
library(e1071)
library(foreach)

set.seed(100);

solar$Year <- as.numeric(substr(solar$Date,1,4))
solar$Month <- as.numeric(substr(solar$Date,5,6))
solar$Day <- as.numeric(substr(solar$Date,7,8))

# Separate NA
prediction <- solar_data[is.na(solar$ACME)]
prediction_1 <- solar_data[!is.na(solar$ACME)]


# train and test
#train_index


train_index <- sample(1:nrow(prediction_1), 0.75*nrow(prediction_1))
val_index <- sample(setdiff(1:nrow(prediction_1), train_index), 0.25*nrow(prediction_1))
test_index <- setdiff(1:nrow(prediction_1), c(train_index, val_index))

train <- prediction_1[train_index]
val <- prediction_1[val_index]
test  <- prediction_1[test_index]

# grid for 
cluster_v <- 10^seq(from = -3, to = 3, by = 1) 
epsilon_v <- 10^seq(from = -3, to = 0, by = 1) 
gamma_v <- 10^seq(from = -3, to = 3, by = 1) 



cluster1 <- c("FTCB","NINN","CHIC","KETC","APAC","ACME","MEDI", "WAUR","RING","MADI","BURN","TIPT","MANG","ALTU","HOLL",
         "MINC","ELRE","STIL","PERK","MARE","SPEN","GUTH","WATO", "WASH","PAUL","SULP","CENT","BYAR","ADAX","TISH",
         "BOWL","HINT");
cluster2 <- c("PUTN","SEIL","HOBA","RETR","CHER","FAIR","LAHO","FREE","MAYR","BEAV","SLAP","BUFF","WOOD", "ARNE","BESS",
         "BUTL","WEAT","CHEY","ERIC","CAMA")
cluster3 <- c("VINI","MIAM","PRYO","JAYX","WILB","TALI","CLAY","STUA","MCAL","MTHE","HUGO","CLOU","WIST","LANE","WYNO",
         "SKIA","OKMU","EUFA","OKEM","OILT","BRIS","HASK","BIXB","PAWN","NEWK","BURB","REDR","MEDF","FORA",
         "BREC","BLAC", "SHAW","CHAN","TAHL","SALL","COOK","WEST","STIG","NOWA","COPA")
cluster4 <- c("GOOD","HOOK", "BOIS","KENT")
cluster5 <- c("IDAB","DURA");



train_cl1 <- as.data.table(rowMeans(train[,..cluster1]))
colnames(train_cl1)[colnames(train_cl1) == "Val1"] <- "Production_Cluster1"

train_cl2 <- as.data.table(rowMeans(train[,..cluster2]))
colnames(train_cl2)[colnames(train_cl2) == "Val1"] <- "Production_Cluster2"
cluster_trained <- cbind(train_cl1, train_cl2)

train_cl3 <- as.data.table(rowMeans(train[,..cluster3]))
colnames(train_cl3)[colnames(train_cl3) == "Val1"] <- "Production_Cluster3"
cluster_trained <- cbind(cluster_trained,train_cl3)

train_cl4 <- as.data.table(rowMeans(train[,..cluster4]));
colnames(train_cl4)[colnames(train_cl4) == "Val1"] <- "Production_Cluster4"
cluster_trained <- cbind(cluster_trained,train_cl4)

train_cl5 <- as.data.table(rowMeans(train[,..cluster5]))
colnames(train_cl5)[colnames(train_cl5) == "Val1"] <- "Production_Cluster5"
cluster_trained <- cbind(cluster_trained,train_cl5)



value_cl1 <- as.data.table(rowMeans(val[,..cluster1]));
colnames(value_cl1)[colnames(value_cl1) == "Val1"] <- "Production_Cluster1";

value_cl2 <- as.data.table(rowMeans(val[,..cluster2]));
colnames(value_cl2)[colnames(value_cl2) == "Val1"] <- "Production_Cluster2";
cluster_value <- cbind(value_cl1,value_cl2);

value_cl3 <- as.data.table(rowMeans(val[,..cluster3]));
colnames(value_cl3)[colnames(value_cl3) == "Val1"] <- "Production_Cluster3";
cluster_value <- cbind(cluster_value,CL3_val);

value_cl4 <- as.data.table(rowMeans(val[,..cluster4]));
colnames(value_cl4)[colnames(value_cl4) == "Val1"] <- "Production_Cluster4";
cluster_value <- cbind(cluster_value,value_cl4);

value_cl5 <- as.data.table(rowMeans(val[,..cluster5]));
colnames(value_cl5)[colnames(value_cl5) == "Val1"] <- "Production_Cluster5";
cluster_value <- cbind(cluster_value,value_cl5);



best <- data.table();


for (i in 1:5){
  
  print(i);
  grid_results <- data.table();
  
  foreach (c = cluster_v)%:%
    foreach (eps = epsilon_v)%:%
    foreach (gamma = gamma_v)%dopar%{
      
      print(sprintf("Start of c = %s - eps = %s - gamma = %s", c, eps, gamma));
      
      train1 <- cluster_trained[,..i];
      colnames(train1)[colnames(train1) == paste("Production_Cluster",i, sep = "")] <- "Production";
      train1 <- cbind(train1,train[,100:459]);
      
      value1 <- cluster_value[,..i];
      colnames(value1)[colnames(value1) == paste("Production_Cluster",i, sep = "")] <- "Production";
      val1 <- cbind(value1,val[,100:459]);
      
      f <- as.formula(paste(colnames(train1[,1]), ".", sep = " ~ "));
      
 # train SVM 50% of train data
      train_hyperpara_index <- sample(1:nrow(train1), 0.5*nrow(train1));  
      train_hyper <- train1[train_hyperpara_index]; 
      svm_model <- svm(f, data = train_hyper,
                       cost = c, epsilon = eps, gamma = gamma);
      
  #predictions
      predictions_train <- predict(svm_model, newdata = train);
      predictions_val <- predict(svm_model, newdata = val);
      
   # errors
      errors_train <- predictions_train - unlist(c(train1[,1]),use.names=FALSE);
      errors_val <- predictions_val - unlist(c(val1[,1]),use.names=FALSE);
      
      mse_train <- round(mean(errors_train^2), 2);
      mae_train <- round(mean(abs(errors_train)), 2);
      
      mse_val <- round(mean(errors_val^2), 2);
      mae_val <- round(mean(abs(errors_val)), 2);
      
 # comparee
      grid <- rbind(grid, data.table(cluster = paste("cl",i, sep = ""),
                                  c = c, eps = eps, gamma = gamma, 
                                    mse_train = mse_train, mae_train = mae_train,
                                 mse_val = mse_val, mae_val = mae_val));
    }
  
  
  grid <- grid[order(mse_val, mae_val)];
  
  # Best hyperparameters
  besthyp <- rbind(best, grid[1]);
}


write.csv(besthyp, "/Users/apkiener/Desktop/project/besthyper.csv",
          quote = F, row.names = F)


compare_svm <- data.table()
svm_final_hyp <- predict[,1]

for (i in 2:99){
  
  print(i);
  
  if (colnames(train[,..i]) %in% cluster1){
    cluster <- "cl1"} else if (colnames(train[,..i]) %in% cluster2){
      cluster <- "cl2"} else if (colnames(train[,..i]) %in% cluster3){
        cluster <- "cl3"} else if (colnames(train[,..i]) %in% cluster4){
          cluster <- "cl4"} else {cluster <- "cl5"};
  
  train1 <- train[,..i];
  colnames(train1)[colnames(train1) %in% colnames(solar[,2:99])] <- "Production";
  train1 <- cbind(train1,train[,100:459]);
  
  val1 <- val[,..i];
  colnames(val1)[colnames(val1) %in% colnames(solar[,2:99])] <- "Production";
  val1 <- cbind(val1,val[,100:459]);
  
  
  # train SVM model with best found set of hyperparamets
  model_svm <- svm(f, data = rbind(train1,val1), 
                   cost = best$c[best$cluster == cluster], 
                   epsilon = best$eps[best$cluster == cluster], 
                   gamma = best$gamma[best$cluster == cluster]);
  
  
  # Get model predictions
  predictions_train <- predict(model_svm, newdata = train);
  predictions_val <- predict(model_svm, newdata = val);
  predictions_test <- predict(model_svm, newdata = test);
  
  # Get errors
  errors_train <- predictions_train - unlist(c(train[,..i]),use.names=FALSE);
  errors_val <- predictions_val - unlist(c(val[,..i]),use.names=FALSE);
  errors_test <- predictions_test - unlist(c(test[,..i]),use.names=FALSE);
  
  # Compute Metrics
  mse_train <- round(mean(errors_train^2), 2);
  mae_train <- round(mean(abs(errors_train)), 2);
  
  mse_val <- round(mean(errors_val^2), 2);
  mae_val <- round(mean(abs(errors_val)), 2);
  
  mse_test <- round(mean(errors_test^2), 2);
  mae_test <- round(mean(abs(errors_test)), 2);
  
  
  # Build comparison table
  comp_svm <- rbind(comp_svm,
                    data.table(model = colnames(train[,..i]), 
                               mse_train = mse_train, mae_train = mae_train,
                               mse_test = mse_test, mae_test = mae_test));
  comp_svm;
  
  # Create final predictions
  predict1 <- predict[,100:459];
  a <- predict(model_svm, newdata = predict1);
  final_svm <- cbind(final_svm,a);
  colnames(final_svm)[colnames(final_svm)=="a"] <- colnames(train[,..i]);
  
}



write.csv(final_svm, "/Users/apkiener/Desktop/project/final_cluster_svm.csv",
          quote = F, row.names = F);

write.csv(comp_svm, "/Users/apkiener/Desktop/project/comp_cluster_svm.csv",
          quote = F, row.names = F);

