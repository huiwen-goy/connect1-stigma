
# Libraries
library(caret)
library(party)
library(rpart)
library(knitr)
library(pROC)

### No option for tuning number trees, so manual 

# use my own seeds for reproducibility in Repeated CV
# Using parallel processing prevents reproducibility, even with set.seed before train
set.seed(190)
myseeds <- vector(mode = "list", length = 16) # need 1 extra for some reason
for(i in 1:16) { myseeds[[i]]<- sample.int(n=1000, 1) }

# try different B values
set.seed(10)
mbag400 <- train(formula28x, 
            data = pta,
            method = "bag", 
            B = 400, 
            control = rpart.control(minsplit = 20, 
                                    maxcompete = FALSE, 
                                    maxsurrogate = 0,
                                    split = "gini"), 
            bagControl = bagControl(fit = ctreeBag$fit,
                                    predict = ctreeBag$pred,
                                    aggregate = ctreeBag$aggregate,
                                    downSample = TRUE,
                                    allowParallel = FALSE), 
            trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3,  
                                     seeds = myseeds)
            )


# Extract metrics from saved model results
mbag_list <- c("mbag001", "mbag005", "mbag010", "mbag020", "mbag030", "mbag040", "mbag050", 
               "mbag060", "mbag070", "mbag080", "mbag100", "mbag150", "mbag200", "mbag250",
               "mbag300", "mbag400")

# Make dataframe; 
# note accuracy is OOB results, but sens & spec are from model's predictions on whole dataset
mbag_df <- data.frame(output_name = c(rep(0, length(mbag_list))), 
                      num_trees_B = c(rep(0, length(mbag_list))), 
                      cv_accuracy = c(rep(0, length(mbag_list))),
                      cv_accuracySD = c(rep(0, length(mbag_list))),
                      cv_kappa = c(rep(0, length(mbag_list))), 
                      cv_kappaSD = c(rep(0, length(mbag_list))), 
                      sensitivity = c(rep(0, length(mbag_list))), 
                      specificity = c(rep(0, length(mbag_list)))
                      )

for (i in 1:length(mbag_list)) {
  mbag_df$output_name[i] <- mbag_list[i]
  mbag_df$num_trees_B[i] <- get(mbag_list[i])$finalModel$B
  mbag_df$cv_accuracy[i] <- get(mbag_list[i])$results$Accuracy
  mbag_df$cv_accuracySD[i] <- get(mbag_list[i])$results$AccuracySD
  mbag_df$cv_kappa[i] <- get(mbag_list[i])$results$Kappa
  mbag_df$cv_kappaSD[i] <- get(mbag_list[i])$results$KappaSD
  mbag_tab <- table(predict(get(mbag_list[i]), pta[,var_list]), pta$HA.Purchase, dnn=c("p", "a"))
  mbag_df$sensitivity[i] <- mbag_tab[1] / 604 * 100
  mbag_df$specificity[i] <- mbag_tab[4] / 149 * 100
}

# plot error vs number of trees, with SD lines

df_bag <- mbag_df

df_bag <- df_bag[,-1]  # delete extra index column
df_bag$lower <- df_bag$cv_accuracy - df_bag$cv_accuracySD
df_bag$upper <- df_bag$cv_accuracy + df_bag$cv_accuracySD

plot(x = df_bag$num_trees_B, y = df_bag$cv_accuracy, 
     type='l', ylim=c(0.6, 0.9), lwd = 2,
     ylab = "Out-of-bag accuracy", xlab = "Number of trees, B")
par(new=TRUE)
plot(df_bag$num_trees_B, df_bag$upper, type='l', col='grey', ylim=c(0.6, 0.9), lwd = 2, 
     xaxt='n', yaxt='n', ann=FALSE)
par(new=TRUE)
plot(df_bag$num_trees_B, df_bag$lower, type='l', col='grey', ylim=c(0.6, 0.9), lwd = 2, 
     xaxt='n', yaxt='n', ann=FALSE)


# Final model: no CV; setting seed outside with no parallel processing makes this reproducible
set.seed(10)  
mbag250final <- train(formula28x, 
                      data = pta,
                      method = "bag", 
                      B = 250, 
                      metric = "Accuracy", 
                      control = rpart.control(minsplit = 20, 
                                    maxcompete = FALSE, 
                                    maxsurrogate = 0,
                                    split = "gini"), 
                      bagControl = bagControl(fit = ctreeBag$fit,
                                    predict = ctreeBag$pred,
                                    aggregate = ctreeBag$aggregate,
                                    downSample = TRUE,
                                    allowParallel = FALSE), 
                      trControl = trainControl(method = "none")
                      )

summary(mbag250final)
#Out of bag statistics (B = 250):
#
#       Accuracy    Kappa
#  0.0%   0.2283 -0.08541
#  2.5%   0.2827 -0.03924
# 25.0%   0.5359  0.00000
# 50.0%   0.6575  0.02378
# 75.0%   0.7651  0.07510
# 97.5%   0.8216  0.16777
#100.0%   0.8464  0.20120

mbag250final_pred <- predict(mbag250final, pta[, var_list])
table(mbag250final_pred, pta$HA.Purchase, dnn=c("pred", "obs"))
#     obs
#pred   No Yes
#  No  535 101
#  Yes  69  48

mbag250final_pred_bi <- ifelse(mbag250final_pred == "No", 0, 1)
roc(response = pta$Purchased_HA, predictor = mbag250final_pred_bi)
#Data: mbag250final_pred_bi in 604 controls (pta$Purchased_HA 0) < 149 cases (pta$Purchased_HA 1).
#Area under the curve: 0.604

# Draw confusion matrix
cmat <- matrix(c(535, 69, 101, 48), ncol=2, nrow=2)
rownames(cmat) <- c("Predicted No", "Predicted Yes")
colnames(cmat) <- c("Actual No", "Actual Yes")
print(cmat)

# Plot variable importance
plot(varImp(mbag250final))

# Table of metrics
model_comp <- data.frame(acc = c(63.47, 67.46, 77.42, NA, NA),
                         sens = c(59.73, 78.52, 32.21, NA, NA), 
                         spec = c(64.40, 64.74, 88.58, NA, NA),
                         auc = c(62.07, 71.63, 60.40, NA, NA) )
rownames(model_comp) <- c("Logistic x=4", 
                          "Class tree cp=0.013", 
                          "Bagging (250 trees)", 
                          "Random forest", 
                          "Boosting")
colnames(model_comp) <- c("Accuracy %", "Sensitivity %", "Specificity %", "Area Under Curve")
model_comp <- replace(model_comp, is.na(model_comp), "*")
kable(model_comp, format="pipe", row.names = TRUE)

