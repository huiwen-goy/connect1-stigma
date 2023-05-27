
# Libraries
library(caret)
library(ranger)
library(ggplot2)
library(knitr)
library(pROC)

# Add case weights
pta$caseweights <- ifelse(pta$HA.Purchase == "No", 0.2466887, 1)

# List of 28 predictors
var_list <- c("Age", "PTA4_better_ear", "HHIE_total", "Ability", "Sex.f", "Edu", "Married.f", "Health", "QoL", "Help_neighbours", "Help_problems", "Concern", "Lonely", "Sub_Age_avg", "Age_stigma_avg", "HA_stigma_avg", "Accomp.f", "Soc_Suspect_HL.f", "Soc_Know_HL.f", "Soc_Discuss_HL.f", "Soc_Hearing_test.f", "Soc_Obtain_HA.f", "Soc_Sometimes_use.f", "Soc_Regular_use.f", "Soc_Very_positive.f", "Soc_Somewhat_positive.f", "Soc_Somewhat_negative.f", "Soc_Very_negative.f")

# build grid search to vary number of predictors (8) and node size (5)
grid_mtry <- expand.grid(mtry = c(2,3,4,5,7,9,12,16), 
                         splitrule="gini", 
                         min.node.size=c(1, 5, 10, 20, 30))

# use grid search in each number of trees

set.seed(300)  # make models reproducible
myseeds <- vector(mode = "list", length = 26)
for(i in 1:25) { myseeds[[i]] <- sample.int(n=1000, 40) }
set.seed(301)
myseeds[[26]] <- sample.int(n=1000, 1)

rc005 <- train(
  x = pta[, var_list], y = pta$HA.Purchase,
  method = "ranger", num.trees = 5, tuneGrid = grid_mtry,
  max.depth = 0, replace = TRUE, case.weights = pta$caseweights, 
  trControl = trainControl(method = "boot", seeds = myseeds, allowParallel = FALSE) )

rc010 <- train(
  x = pta[, var_list], y = pta$HA.Purchase,
  method = "ranger", num.trees = 10, tuneGrid = grid_mtry,
  max.depth = 0, replace = TRUE, case.weights = pta$caseweights, 
  trControl = trainControl(method = "boot", seeds = myseeds, allowParallel = FALSE) )

rc025 <- train(
  x = pta[, var_list], y = pta$HA.Purchase,
  method = "ranger", num.trees = 25, tuneGrid = grid_mtry,
  max.depth = 0, replace = TRUE, case.weights = pta$caseweights,
  trControl = trainControl(method = "boot", seeds = myseeds, allowParallel = FALSE) )

rc050 <- train(
  x = pta[, var_list], y = pta$HA.Purchase,
  method = "ranger", num.trees = 50, tuneGrid = grid_mtry,
  max.depth = 0, replace = TRUE, case.weights = pta$caseweights,
  trControl = trainControl(method = "boot", seeds = myseeds, allowParallel = FALSE) )

rc100 <- train(
  x = pta[, var_list], y = pta$HA.Purchase,
  method = "ranger", num.trees = 100, tuneGrid = grid_mtry,
  max.depth = 0, replace = TRUE, case.weights = pta$caseweights, 
  trControl = trainControl(method = "boot", seeds = myseeds, allowParallel = FALSE) )

rc200 <- train(
  x = pta[, var_list], y = pta$HA.Purchase,
  method = "ranger", num.trees = 200, tuneGrid = grid_mtry,
  max.depth = 0, replace = TRUE, case.weights = pta$caseweights, 
  trControl = trainControl(method = "boot", seeds = myseeds, allowParallel = FALSE) )

rc300 <- train(
  x = pta[, var_list], y = pta$HA.Purchase,
  method = "ranger", num.trees = 300, tuneGrid = grid_mtry,
  max.depth = 0, replace = TRUE, case.weights = pta$caseweights, 
  trControl = trainControl(method = "boot", seeds = myseeds, allowParallel = FALSE) )

rc400 <- train(
  x = pta[, var_list], y = pta$HA.Purchase,
  method = "ranger", num.trees = 400, tuneGrid = grid_mtry,
  max.depth = 0, replace = TRUE, case.weights = pta$caseweights, 
  trControl = trainControl(method = "boot", seeds = myseeds, allowParallel = FALSE) )

rc500 <- train(
  x = pta[, var_list], y = pta$HA.Purchase,
  method = "ranger", num.trees = 500, tuneGrid = grid_mtry,
  max.depth = 0, replace = TRUE, case.weights = pta$caseweights, 
  trControl = trainControl(method = "boot", seeds = myseeds, allowParallel = FALSE) )

# append results into dataframe
acc_df <- data.frame(num_trees = c(rep(5, 40), rep(10, 40), rep(20, 40), rep(50, 40), 
                                   rep(100, 40), rep(200, 40), rep(300, 40), 
                                   rep(400, 40), rep(500, 40) ),
                     mtry = NA, splitrule = NA, min.node.size = NA,
                     Accuracy = NA, Kappa = NA, AccuracySD = NA, KappaSD = NA)

acc_df[1:40, 2:8] <- rc005$results
acc_df[41:80, 2:8] <- rc010$results
acc_df[81:120, 2:8] <- rc025$results
acc_df[121:160, 2:8] <- rc050$results
acc_df[161:200, 2:8] <- rc100$results
acc_df[201:240, 2:8] <- rc200$results
acc_df[241:280, 2:8] <- rc300$results
acc_df[281:320, 2:8] <- rc400$results
acc_df[321:360, 2:8] <- rc500$results

# plot accuracy as a function of mtry, ntrees, and min.node.size; shaded area is SD

acc_df$num_trees_f <- as.factor(acc_df$num_trees)
acc_df$Acc_lower <- acc_df$Accuracy - acc_df$AccuracySD
acc_df$Acc_upper <- acc_df$Accuracy + acc_df$AccuracySD

ntrees_colours <-c("grey70", "grey50", "grey30", "cadetblue3", "cyan2", "royalblue1",
                   "darkorange2", "indianred", "red")

node_values <- c("1" = "Min 1", "5" = "Min 5", "10" = "Min 10", 
                 "15" = "Min 15", "20" = "Min 20", "30" = "Min 30")

ggplot(data = acc_df, 
       aes(x = mtry, y = Accuracy, group = num_trees_f, ymin = Acc_lower, ymax = Acc_upper) ) + 
  geom_line(aes(colour = num_trees_f), size = 1.2) +
  geom_ribbon(aes(fill = num_trees_f), alpha = 0.1) + 
  scale_colour_manual(values = ntrees_colours) + 
  scale_fill_manual(values = ntrees_colours, guide = 'none') + 
  facet_grid( ~ min.node.size, 
              labeller = as_labeller(node_values) ) + 
  scale_x_continuous(name="Number of predictors", limits=c(1, 17), breaks=seq(2, 16, 2)) + 
  scale_y_continuous(name="Accuracy", limits=c(0.70, 0.84), breaks=seq(0.70, 0.84, 0.02)) + 
  theme_bw() + 
  theme(axis.title.x = element_text(size = 20, colour = "black"), 
        axis.text.x = element_text(size = 14, colour = "black"), 
        axis.title.y = element_text(size = 20, colour = "black"), 
        axis.text.y = element_text(size = 14, colour = "black") ) + 
  theme(strip.text.x = element_text(size = 12), 
        strip.background = element_rect(color = "black", fill="white", size=1.1) ) + 
  theme(panel.background = element_rect(color = "black", size = 1.1)) + 
  labs(color = "ntrees", size = 16) + 
  theme(legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))

# RF with min.node=20, mtry=c(2,3,4), numtrees=c(50, 100, 200)

newgrid <- expand.grid(mtry = c(2, 3, 4), num.trees = c(50, 100, 200, 300) )
oob_df <- newgrid
oob_df$oob_error <- NA

for (i in 1:12) { 
temprf <- ranger(x = pta[, var_list], y = pta$HA.Purchase, 
                 num.trees = newgrid$num.trees[i], 
                 mtry = newgrid$mtry[i], 
                 importance = "impurity", 
                 write.forest = TRUE, min.node.size = 20, max.depth = 0, 
                 replace = TRUE, case.weights = pta$caseweights, splitrule = "gini",
                 oob.error = TRUE, seed = 50 )

oob_df$oob_error[i] <- temprf$prediction.error
} 

oob_df$oob_accuracy <- 1 - oob_df$oob_error
print(oob_df[, -3])

# Trying class weights
class_rf <- ranger(x = pta[, var_list], y = pta$HA.Purchase, 
                 num.trees = 200, mtry = 4, importance = "impurity", 
                 write.forest = TRUE, min.node.size = 20, max.depth = 0, 
                 replace = TRUE, class.weights = c(0.246688, 1), splitrule = "gini",
                 oob.error = TRUE, seed = 50 )

# Final model
ran_final <- ranger(x = pta[, var_list], y = pta$HA.Purchase,
                   num.trees = 200, mtry = 4, importance = "impurity", splitrule = "gini", 
                   write.forest = TRUE, min.node.size = 20, max.depth = 0,
                   replace = TRUE, case.weights = pta$caseweights, 
                   oob.error = TRUE, seed = 200)

# "Prediction with new data and a saved forest from Ranger"; default type = "response"; seed will break ties randomly
# using the training data, same as previous bagged model
ran_pred <- predict(ran_final, data = pta[, var_list], seed = 8)

confusionMatrix(data = ran_pred$predictions, reference=pta$HA.Purchase, 
                positive="Yes", dnn=c("Pred", "Real"))

ran_pred_bi <- ifelse(ran_pred$predictions == "No", 0, 1)
roc(response = pta$Purchased_HA, predictor = ran_pred_bi)

# Variable importance
vimp <- data.frame(ran_final$variable.importance)
vimp$variable <- rownames(vimp)
rownames(vimp) <- c(1:28)
colnames(vimp) <- c("Importance", "Variable")
vimp$Importance_scaled <- vimp$Importance / 25.345139 * 100
vi <- vimp[order(vimp$Importance_scaled, decreasing = TRUE), ]

ggplot(data = vi, aes(y = Importance_scaled, x = c(1:28))) + 
  geom_col(width=0.2) + 
  coord_flip(xlim=rev(c(1, 28)), ylim=c(0, 102)) + 
  scale_x_continuous(name='', breaks = seq(1, 28, by = 1), labels=vi$Variable) + 
  scale_y_continuous(name="Importance", breaks = seq(0, 100, by=10)) +
  theme_bw() + 
  theme(plot.margin = unit(c(0.25, 0.5, 0.55, 1), "cm")) + 
  theme(axis.title = element_text(size = 20, colour='black'), 
        axis.text = element_text(size = 14, colour='black')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
  theme(plot.margin = unit(c(1, 0.25, 0.25, 0.25), "cm"))

# Table of metrics
model_comp <- data.frame(acc = c(63.47, 67.46, 77.42, 98.14, NA),
                         sens = c(59.73, 78.52, 32.21, 100.00, NA), 
                         spec = c(64.40, 64.74, 88.58, 97.68, NA),
                         auc = c(62.07, 71.63, 60.40, 98.84, NA) )
rownames(model_comp) <- c("Logistic reg (lasso)", 
                          "Classification tree", 
                          "Bagging (250 trees)", 
                          "RF (200 trees, m=4)", 
                          "Boosting")
colnames(model_comp) <- c("Accuracy %", "Sensitivity %", "Specificity %", "Area Under Curve")
model_comp <- replace(model_comp, is.na(model_comp), "*")
kable(model_comp, format="pipe", row.names = TRUE)

