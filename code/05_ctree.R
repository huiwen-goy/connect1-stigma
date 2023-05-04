
# Libraries
library(caret) #for confusionMatrix
library(pROC) #AUC
library(rpart)
library(rpart.plot)
library(rattle)
library(ggplot2)
library(knitr)

# Add case weights
pta$caseweights <- ifelse(pta$HA.Purchase == "No", 0.2466887, 1)

# Standard formula, with predictors as factors, and outcome as string
formula28x <- formula(HA.Purchase ~ 
  Age +  
  PTA4_better_ear +
  HHIE_total +
  Ability +
  Sex.f +
  Edu +
  Married.f +
  Health +
  QoL +
  Help_neighbours +
  Help_problems +
  Concern +
  Lonely +
  Sub_Age_avg +
  Age_stigma_avg +
  HA_stigma_avg +
  Accomp.f +
  Soc_Suspect_HL.f +
  Soc_Know_HL.f +
  Soc_Discuss_HL.f +
  Soc_Hearing_test.f +
  Soc_Obtain_HA.f +
  Soc_Sometimes_use.f +
  Soc_Regular_use.f +
  Soc_Very_positive.f +
  Soc_Somewhat_positive.f +
  Soc_Somewhat_negative.f +
  Soc_Very_negative.f)

# Plot graph showing CP values & accuracy with cross-validation

# note metric=ROC needs class probabilities
set.seed(1331)
mc <- train(formula28x, data = pta, 
            weights = pta$caseweights,
            method = "rpart",
            tuneGrid = data.frame(cp = c(0, 0.001, 0.005, 0.01, 0.011, 0.012, 0.013, 0.015, 0.02, 0.025, 0.03, 0.04, 0.05, 0.1)),
            metric = "Accuracy", 
            control = rpart.control(split="gini", minsplit = 20),
            trControl = trainControl(method="repeatedcv", number = 5, repeats = 10))
# mc$finalModel = A fit object using the best parameters
# mc$bestTune: cp = 0.02

# Plot model accuracy vs CP values
means <- mc$results$Accuracy
bar.up <- means + mc$results$AccuracySD/(5^0.5)
bar.down <- means - mc$results$AccuracySD/(5^0.5)
plot(x = mc$results$cp,
     xlab = "simple --- CP --- complex",
     y = means, 
     ylab = "Mean accuracy (with SE)", 
     type='b', pch=1, xlim=rev(c(0, 0.1)), ylim=c(0.5, 0.65) )
arrows(x0 = mc$results$cp, x1 = mc$results$cp, y0 = bar.down, y1 = bar.up, code = 3, length=0.05, angle=90, col="black")

# add point at cp = 0.02
par(new=TRUE)
plot(x = 0.02, y = 0.6102368, pch=19, col="red", 
     xlim=rev(c(0, 0.1)), ylim=c(0.5, 0.65), xaxt='n', yaxt='n', ann=FALSE)

# model CP = 0.02
m_cp02 <- rpart(formula28x, data = pta, 
                 weights = pta$caseweights, 
                 method = "class", 
                 parms = list(split = "gini"), 
                 control = rpart.control(cp = 0.02, minsplit = 20, maxcompete=FALSE, maxsurrogate=0) )

# plot tree CP = 0.02
rpart.plot(m_cp02, box.palette=0, type = 4, extra = 101, under=TRUE, fallen.leaves=TRUE, varlen=0, faclen=0)

# Calculate more metrics across a range of CP values & plot graph

cpvalues <- c(0, 0.001, 0.005, 0.01, 0.011, 0.012, 0.013, 0.015, 0.02, 0.025, 0.03, 0.04, 0.05, 0.1)
cpdf <- data.frame(cpvalue = rep(0, length(cpvalues)), 
                   nsplits = c(0), 
                   PredNo_RealNo = c(0), 
                   PredYes_RealNo = c(0),
                   PredNo_RealYes = c(0),
                   PredYes_RealYes = c(0),
                   AUC = c(0) )

for (i in 1:length(cpvalues)) {
  
mtemp <- rpart(formula28x, data = pta, 
               weights = pta$caseweights, 
               method = "class", 
               parms = list(split = "gini"), 
               control = rpart.control(cp = cpvalues[i]))
cptab <- mtemp$cptable

predtree <- predict(mtemp, newdata = pta, type = "class")
predtreenum <- ifelse(predtree == "No", 0, 1)

cmtab <- table(predtree, pta$HA.Purchase, dnn = c("pred", "actual"))
roctemp <- roc(response = pta$Purchased_HA, predictor = predtreenum)

cpdf$cpvalue[i] <- cpvalues[i]
cpdf$nsplits[i] <- cptab[nrow(cptab), 2]
cpdf$PredNo_RealNo[i] <- cmtab[1,1]  
cpdf$PredYes_RealNo[i] <- cmtab[2,1] 
cpdf$PredNo_RealYes[i] <- cmtab[1,2]
cpdf$PredYes_RealYes[i] <- cmtab[2,2]
cpdf$AUC[i] <- as.numeric(roctemp$auc)

}

cpdf$Accuracy <- (cpdf$PredNo_RealNo + cpdf$PredYes_RealYes) / 756
cpdf$Sensitivity <- cpdf$PredYes_RealYes / (cpdf$PredNo_RealYes + cpdf$PredYes_RealYes)
cpdf$Specificity <- cpdf$PredNo_RealNo / (cpdf$PredYes_RealNo + cpdf$PredNo_RealNo)

temptab <- data.frame(cbind(cpdf[, c("cpvalue", "nsplits")],
                            round(cpdf[, c("Accuracy", "Sensitivity", "Specificity", "AUC")], 4) ) )


# Table in graph form

p1 <- ggplot(data = temptab) + 
  geom_line(aes(x = cpvalue, y = nsplits), colour='black') + 
  geom_point(aes(x = cpvalue, y = nsplits), colour='black') + 
  labs(y = "Number of splits", x = "simple --- CP --- complex") + 
  scale_x_reverse(limits = c(0.105, -0.005), breaks = seq(0, 0.1, by = 0.01)) + 
  theme_bw() + 
  theme(plot.margin = unit(c(0.25, 0.5, 0.55, 1), "cm")) + 
  theme(axis.title = element_text(size = 14, colour='black'), 
        axis.text = element_text(size = 11, colour='black')) + 
  geom_text(temptab, mapping=aes(x = cpvalues, y = nsplits, label = nsplits), 
            nudge_y = 1, nudge_x = 0.002, colour="grey50", size=4) + 
  geom_vline(xintercept = 0.013, col="black", lty="dotted")

p2 <- ggplot(data = temptab) + 
  geom_line(aes(x = cpvalue, y = Accuracy*100), colour='black') + 
  geom_point(aes(x = cpvalue, y = Accuracy*100), shape = 1, colour='black') + 
  geom_line(aes(x = cpvalue, y = Sensitivity*100), colour='firebrick3') + 
  geom_point(aes(x = cpvalue, y = Sensitivity*100), shape = 4, colour='firebrick3') + 
  geom_line(aes(x = cpvalue, y = Specificity*100), colour='deepskyblue3') + 
  geom_point(aes(x = cpvalue, y = Specificity*100), shape = 5, colour='deepskyblue3') + 
  labs(y = "Metric (%)", x = "simple --- CP --- complex") + 
  scale_y_continuous(limits = c(45, 95), breaks = seq(45, 95, 5)) + 
  scale_x_reverse(limits = c(0.105, -0.005), breaks = seq(0, 0.1, by = 0.01)) + 
  theme_bw() + 
  theme(plot.margin = unit(c(0.25, 0.5, 0.55, 1), "cm")) + 
  theme(axis.title = element_text(size = 14, colour='black'), 
        axis.text = element_text(size = 11, colour='black')) + 
  annotate('text', x=0.09, y=71, label = "Specificity", size=5, colour = 'deepskyblue3', angle='0.25') + 
  annotate('text', x=0.09, y=64, label = "Accuracy", size = 5, colour = 'black', angle='5') + 
  annotate('text', x=0.09, y=51, label = "Sensitivity", size = 5, colour = 'firebrick3', angle='5') + 
  geom_vline(xintercept = 0.013, col="black", lty="dotted")
  
library(patchwork)
p1 / p2

# Print accompanying table from above
print(temptab[order(temptab$cpvalue, decreasing = TRUE), ] )

### Previous analysis using maxdepth = 4

# Dataset from Dec2019; note Age_Stigma_avg still included Q4

# need 0/1 variable for ROC!
pta_orig$Purchased_HA <- ifelse(pta_orig$HA.Purchase == "No", 0, 1)

# original analysis (depth 4), original data
m_dep4_orig <- rpart(formula28x, data = pta_orig, 
                weights = pta_orig$weight, 
                method = "class", 
                parms = list(split = "gini"), 
                control = rpart.control(split="gini", minsplit=20, maxdepth=4) )

# original analysis, revised Age_Stigma_avg
m_dep4_rev <- rpart(formula28x, data = pta, 
                weights = pta$caseweights, 
                method = "class", 
                parms = list(split = "gini"), 
                control = rpart.control(split="gini", minsplit=20, maxdepth=4) )

# original analysis, specifying maxcompete=FALSE, maxsurrogate=0; revised Age_Stigma_avg
m_dep4 <- rpart(formula28x, data = pta, 
                weights = pta$caseweights, 
                method = "class", 
                parms = list(split = "gini"), 
                control = rpart.control(split="gini", minsplit=20, maxdepth=4, #maxcompete=FALSE, maxsurrogate=0) )
# Conclusion: adding maxcompete=FALSE, maxsurrogate=0 does not change the final tree;

# branches containing Age_Stigma_avg dropped out after item 4 was dropped, but otherwise the same

# Model metrics with Q4
m_dep4_orig_pred <- predict(m_dep4_orig, newdata = pta_orig, type = "class") #No=1
m_dep4_orig_pred.f <- factor( m_dep4_orig_pred, levels=c("No", "Yes") )
pta_orig$HA.Purchase.f <- factor( pta_orig$HA.Purchase, levels=c("No", "Yes") )

table(m_dep4_orig_pred, pta_orig$HA.Purchase, dnn=c('Predicted', 'Actual'))

confusionMatrix(data = m_dep4_orig_pred.f, 
                reference = pta_orig$HA.Purchase.f, 
                positive = c("Yes") )
#Accuracy : 0.7145; Sensitivity : 0.6309; Specificity : 0.7351

pta_orig$Purchased_HA <- ifelse(pta_orig$HA.Purchase == "No", 0, 1)
m_dep4_orig_pred_num <- ifelse(m_dep4_orig_pred == "No", 0, 1)
roc(response = pta_orig$Purchased_HA, predictor = m_dep4_orig_pred_num) #0.683

# Model metrics without Q4
m_dep4_rev_pred <- predict(m_dep4_rev, newdata = pta, type = "class") #No=1
m_dep4_rev_pred.f <- factor( m_dep4_rev_pred, levels=c("No", "Yes") )
pta$HA.Purchase.f <- factor( pta$HA.Purchase, levels=c("No", "Yes") )

table(m_dep4_rev_pred, pta$HA.Purchase, dnn=c('Predicted', 'Actual'))

confusionMatrix(data = m_dep4_rev_pred.f, 
                reference = pta$HA.Purchase.f, 
                positive = c("Yes") )
# Accuracy : 0.6999; Sensitivity : 0.6309; Specificity : 0.7169

pta$Purchased_HA <- ifelse(pta$HA.Purchase == "No", 0, 1)
m_dep4_rev_pred_num <- ifelse(m_dep4_rev_pred == "No", 0, 1)
roc(response = pta$Purchased_HA, predictor = m_dep4_rev_pred_num) #0.6739

# Plot with and w/o Q4 side by side
#Q4inc.jpeg
rpart.plot(m_dep4_orig, box.palette=0, type = 4, extra = 101, under=TRUE, fallen.leaves=TRUE, varlen=0, faclen=0)
#Q4exc.jpeg
rpart.plot(m_dep4_rev, box.palette=0, type = 4, extra = 101, under=TRUE, fallen.leaves=TRUE, varlen=0, faclen=0)

### New analysis: Pruning 

# The two trees
m_cp02 <- rpart(formula28x, data = pta, 
                 weights = pta$caseweights, 
                 method = "class", 
                 parms = list(split = "gini"), 
                 control = rpart.control(cp = 0.02, minsplit = 20, maxcompete=FALSE, maxsurrogate=0) )

m_cp013 <- rpart(formula28x, data = pta, 
                 weights = pta$caseweights, 
                 method = "class", 
                 parms = list(split = "gini"), 
                 control = rpart.control(cp = 0.013, minsplit = 20, maxcompete=FALSE, maxsurrogate=0) )

# Calculations of model metrics, tree plots

# Basis of manual tree plots
rpart.plot(m_cp013, box.palette=0, type = 4, extra = 101, under=TRUE, fallen.leaves=TRUE, 
           varlen=0, faclen=0)
rpart.plot(m_cp02, box.palette=0, type = 4, extra = 101, under=TRUE, fallen.leaves=TRUE, 
           varlen=0, faclen=0)

# Model metrics for CP = 0.02
m_cp02_pred <- predict(m_cp02, newdata = pta, type = "class")
m_cp02_pred.f <- factor( m_cp02_pred, levels=c("No", "Yes") )
pta$HA.Purchase.f <- factor( pta$HA.Purchase, levels=c("No", "Yes") )
table(m_cp02_pred, pta$HA.Purchase.f, dnn=c('Predicted', 'Actual'))
confusionMatrix(data = m_cp02_pred.f, reference = pta$HA.Purchase.f, positive = c("Yes") )
# Accuracy : 0.672; Sensitivity : 0.5906; Specificity : 0.6921
m_cp02_pred_bi <- ifelse(m_cp02_pred == "No", 0, 1) # roc() requires 1/0
roc(response = pta$Purchased_HA, predictor = m_cp02_pred_bi) #0.6413

# Model metrics for CP = 0.013
m_cp013_pred <- predict(m_cp013, newdata = pta, type = "class")
m_cp013_pred.f <- factor( m_cp013_pred, levels=c("No", "Yes") )
pta$HA.Purchase.f <- factor( pta$HA.Purchase, levels=c("No", "Yes") )
table(m_cp013_pred, pta$HA.Purchase.f, dnn=c('Predicted', 'Actual'))
confusionMatrix(data = m_cp013_pred.f, reference = pta$HA.Purchase.f, positive = c("Yes") )
# Accuracy : 0.6746; Sensitivity : 0.7852; Specificity : 0.6474
m_cp013_pred_bi <- ifelse(m_cp013_pred == "No", 0, 1) # roc() requires 1/0
roc(response = pta$Purchased_HA, predictor = m_cp013_pred_bi) #0.7163

# Table of metrics, using info calculated
obj <- data.frame(acc = c(0.672, 0.6746)*100, 
                  sens = c(0.5906, 0.7852)*100, 
                  spec = c(0.6921, 0.6474)*100,
                  auc = c(0.6413, 0.7163)*100 )
rownames(obj) <- c("CP = 0.02", "CP = 0.013")
colnames(obj) <- c("Accuracy %", "Sensitivity %", "Specificity %", "AUC")

kable(obj, format="pipe", row.names = TRUE)

# Plot variable importance
# sum(vi$Variable_importance) = 'x'; using 'x' as denominator, % printed in summary(model)

# CP = 0.02
vi_cp02 <- data.frame(m_cp02$variable.importance)
colnames(vi_cp02) <- c("Variable_importance")
vi_cp02$number <- seq(1, 3, by = 1)

sum(vi_cp02$Variable_importance) #12.47614

g1 <- ggplot(data = vi_cp02, aes(y = Variable_importance/12.47614*100, x = number)) + 
  geom_col() + 
  coord_flip(xlim=rev(c(0, 4)), ylim=c(0, 40)) + 
  scale_x_continuous(name='', breaks = seq(1, 3, by = 1), labels=rownames(vi_cp02)) + 
  scale_y_continuous(name="Variable importance (%)", breaks = seq(0, 40, by=5)) +
  ggtitle("CP = 0.02") + 
  theme_bw() + 
  theme(plot.margin = unit(c(0.25, 0.5, 0.55, 1), "cm")) + 
  theme(axis.title = element_text(size = 16, colour='black'), 
        axis.text = element_text(size = 12, colour='black')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
  theme(plot.margin = unit(c(1, 0.25, 0.25, 0.25), "cm"))


# CP = 0.013
vi_cp013 <- data.frame(m_cp013$variable.importance)
colnames(vi_cp013) <- c("Variable_importance")
vi_cp013$number <- c(1:10)

sum(vi_cp013$Variable_importance) #33.57578

g2 <- ggplot(data = vi_cp013, aes(y = Variable_importance/33.57578*100, x = number)) + 
  geom_col() + 
  coord_flip(xlim=rev(c(0, 11)), ylim=c(0, 40)) + 
  scale_x_continuous(name='', breaks = seq(1, 10, by = 1), labels=rownames(vi_cp013)) + 
  scale_y_continuous(name="Variable importance (%)", breaks = seq(0, 40, by = 5)) + 
  ggtitle("CP = 0.013") + 
  theme_bw() + 
  theme(plot.margin = unit(c(0.25, 0.5, 0.55, 1), "cm")) + 
  theme(axis.title = element_text(size = 16, colour='black'), 
        axis.text = element_text(size = 12, colour='black')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
  theme(plot.margin = unit(c(1, 0.25, 0.25, 0.25), "cm"))

library(patchwork)
g1 / g2 

# create 5 folds, stratified by outcome
set.seed(800)
holdout <- createFolds(y = pta$HA.Purchase, k = 5, list = FALSE)

# checked that fold cases were unique; sum(fold5$ID %in% pta[holdout!=5, ]$ID.clinic)
# checked that sampling seems random; which(pta$ID %in% fold1$ID)

# create dataframe to store numbers obtained from each subset

var_list <- c("Age", "PTA4_better_ear", "HHIE_total", "Ability", "Sex.f", "Edu", "Married.f", "Health", "QoL", "Help_neighbours", "Help_problems", "Concern", "Lonely", "Sub_Age_avg", "Age_stigma_avg", "HA_stigma_avg", "Accomp.f", "Soc_Suspect_HL.f", "Soc_Know_HL.f", "Soc_Discuss_HL.f", "Soc_Hearing_test.f", "Soc_Obtain_HA.f", "Soc_Sometimes_use.f", "Soc_Regular_use.f", "Soc_Very_positive.f", "Soc_Somewhat_positive.f", "Soc_Somewhat_negative.f", "Soc_Very_negative.f")

dfvi_cp02 <- data.frame(variable = var_list, temp = c(0) )

# drop out each fold, and get tree and variable importance for remaining data
for (i in 1:5) {
  tempdata <- pta[holdout != i, ]
  
  # construct tree model
  mtree <- rpart(formula28x, data = tempdata, weights = tempdata$caseweights, 
                 method = "class", parms = list(split = "gini"), 
                 control = rpart.control(cp = 0.02, maxcompete = FALSE, maxsurrogate=0))

  # get variable importance as dataframe
  varimp_temp <- data.frame(mtree$variable.importance)
  varimp_temp$variable <- rownames(varimp_temp) 
  colnames(varimp_temp)[1] <- paste0("subset_", i)
  
  # left merge onto master list of variables
  dfvi_cp02 <- merge(x = dfvi_cp02, 
                     y = varimp_temp, 
                     all.x = TRUE, 
                     by.y = c("variable") )
}

# drop temp column of zero's
dfvi_cp02 <- dfvi_cp02[, -2]

# divide each column by sum of column's importances and convert to %
percentages_cp02 <- mapply('/', dfvi_cp02[, 2:6], colSums(dfvi_cp02[, 2:6], na.rm=TRUE)) * 100

# Changes in variable importance (%) across data subsets, CP = 0.02
obj <- data.frame(cbind(dfvi_cp02$variable, round(percentages_cp02, 4))) 
kable(obj, format="pipe", row.names = FALSE)

dfvi_cp013 <- data.frame(variable = var_list, temp = c(0) )

# drop out each fold, and get tree and variable importance for remaining data
for (i in 1:5) {
  tempdata <- pta[holdout != i, ]
  
  # construct tree model
  mtree <- rpart(formula28x, data = tempdata, weights = tempdata$caseweights, 
                 method = "class", parms = list(split = "gini"), 
                 control = rpart.control(cp = 0.013, maxcompete = FALSE, maxsurrogate=0))

  # get variable importance as dataframe
  varimp_temp <- data.frame(mtree$variable.importance)
  varimp_temp$variable <- rownames(varimp_temp) 
  colnames(varimp_temp)[1] <- paste0("subset_", i)
  
  # left merge onto master list of variables
  dfvi_cp013 <- merge(x = dfvi_cp013, 
                     y = varimp_temp, 
                     all.x = TRUE, 
                     by.y = c("variable") )
}

# drop temp column of zero's
dfvi_cp013 <- dfvi_cp013[, -2]

# divide each column by sum of column's importances and convert to %
percentages_cp013 <- mapply('/', dfvi_cp013[, 2:6], colSums(dfvi_cp013[, 2:6], na.rm=TRUE)) * 100

# Changes in variable importance (%) across data subsets, CP = 0.013
obj <- data.frame(cbind(dfvi_cp013$variable, round(percentages_cp013, 4))) 
kable(obj, format="pipe", row.names = FALSE)








