
# Prep
#####
# Libraries
library(caret)
library(glmnet)
library(car)
library(knitr)
library(pROC)

# Standard formula, with 1 & 0 instead of factors
formula28 <- formula(Purchased_HA ~ 
  Age +  
  PTA4_better_ear +
  HHIE_total +
  Ability +
  Sex +
  Edu +
  Married +
  Health +
  QoL +
  Help_neighbours +
  Help_problems +
  Concern +
  Lonely +
  Sub_Age_avg +
  Age_stigma_avg +
  HA_stigma_avg +
  Accomp +
  Soc_Suspect_HL +
  Soc_Know_HL +
  Soc_Discuss_HL +
  Soc_Hearing_test +
  Soc_Obtain_HA +
  Soc_Sometimes_use +
  Soc_Regular_use +
  Soc_Very_positive +
  Soc_Somewhat_positive +
  Soc_Somewhat_negative +
  Soc_Very_negative)
#####

# Full model
#####
# model with all predictors
mfull <- glm(formula28, data = pta, family = binomial("logit"))

summary(mfull)

mfull_pred <- predict.glm(mfull, newdata = pta, type = "response")

# calculate results for different decision thresholds
# note: min predicted value = 0.02414, max = 0.66981
dec_thresholds_full <- c(seq(0.03, 0.19, by=0.01), 0.1978752, seq(0.21, 0.6, by=0.01))

mfull_roc_df <- data.frame(threshold = rep(NA, length(dec_thresholds_full)), 
                           accuracy = rep(NA, length(dec_thresholds_full)), 
                           sensitivity = rep(NA, length(dec_thresholds_full)), 
                           specificity = rep(NA, length(dec_thresholds_full)) )

for (i in 1:length(dec_thresholds_full)) {
  mfull_pred_bi <- ifelse(mfull_pred >= dec_thresholds_full[i], 1, 0)
  confusion_output <- confusionMatrix(data = as.factor(mfull_pred_bi), 
                                    reference = as.factor(pta$Purchased_HA), 
                                    positive = c("1"))
  mfull_roc_df$threshold[i] <- dec_thresholds_full[i]
  mfull_roc_df$accuracy[i] <- confusion_output$overall[1]
  mfull_roc_df$sensitivity[i] <- confusion_output$byClass[1]
  mfull_roc_df$specificity[i] <- confusion_output$byClass[2]
}

# select different thresholds for labelling
dec_labels = data.frame(thres = c(0.05, 0.1, 0.15, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
                        sens = c(0.98657718, 0.93959732, 0.77852349, 0.48322148, 0.40268456, 
                                 0.26174497, 0.18791946, 0.10738255, 0.05369128),
                        spec = c(0.041390728, 0.230132450, 0.475165563, 0.801324503, 0.852649007, 
                                 0.913907285, 0.965231788, 0.978476821, 0.983443709))
# plot ROC
plot(1-mfull_roc_df$specificity, mfull_roc_df$sensitivity, type='l', 
     ylab="Sensitivity", xlab="False positives", ylim=c(0,1), xlim=c(0,1), 
     main="ROC curve with different decision thresholds")

y = c(0,1)
x = c(0,1)
abline(lm(y ~ x), col="grey80")

text(x = (1-dec_labels$spec), y = dec_labels$sens, labels = dec_labels$thres, col = "black")
text(x = (1-0.655629139), y = 0.63758389, labels = c("0.1978"), col = "red")
#####

# Backwards step
#####
mstep <- step(object = mfull, direction = c("backward"), trace = 0)
summary(mstep)
round(exp(cbind(OR = coefficients(mstep), confint(mstep))), 3)

# calculate predicted values
mstep_pred <- predict.glm(mstep, newdata = pta, type = 'response')

# calculate results for different thresholds; min = 0.04135, max = 0.73837
dec_thresholds_step <- c(seq(0.05, 0.19, by=0.01), 0.1978752, seq(0.21, 0.7, by=0.01))

mstep_roc_df <- data.frame(threshold = rep(NA, length(dec_thresholds_step)), 
                           accuracy = rep(NA, length(dec_thresholds_step)), 
                           sensitivity = rep(NA, length(dec_thresholds_step)), 
                           specificity = rep(NA, length(dec_thresholds_step)) )

for (i in 1:length(dec_thresholds_step)) {
 mstep_pred_bi <- ifelse(mstep_pred >= dec_thresholds_step[i], 1, 0)
 confusion_output <- confusionMatrix(data = as.factor(mstep_pred_bi), 
                                    reference = as.factor(pta$Purchased_HA), 
                                    positive = c("1"))
 mstep_roc_df$threshold[i] <- dec_thresholds_step[i]
 mstep_roc_df$accuracy[i] <- confusion_output$overall[1]
 mstep_roc_df$sensitivity[i] <- confusion_output$byClass[1]
 mstep_roc_df$specificity[i] <- confusion_output$byClass[2]
}

# add Full and Step ROC curves to same plot
plot(1-mstep_roc_df$specificity, mstep_roc_df$sensitivity, type='l', 
     xaxt='n', xaxt='n', ann=FALSE, ylim=c(0,1), xlim=c(0,1), 
     main="ROC curve with different decision thresholds", col="red")
par(new=TRUE)
plot(1-mfull_roc_df$specificity, mfull_roc_df$sensitivity, type='l', 
     ylab="Sensitivity", xlab="False positives", ylim=c(0,1), xlim=c(0,1), 
     main="ROC curves", col="black")
y = c(0,1)
x = c(0,1)
abline(lm(y ~ x), col="grey80")
legend(0.6, 0.4, legend = c("Full model: 28 pred", "Back step: 6 pred"), 
       lty = c("solid", "solid"), col = c("black", "red"), bty="n")

#####

# Lasso
#####

# list of 28 predictors of interest
var_28 <- c("Age", "PTA4_better_ear", "HHIE_total", "Ability", 
             "Sex", "Edu", "Married", "Health", "QoL", "Help_neighbours", "Help_problems",
             "Concern", "Lonely", "Sub_Age_avg", "Age_stigma_avg", "HA_stigma_avg",
             "Accomp", "Soc_Suspect_HL", "Soc_Know_HL", "Soc_Discuss_HL", "Soc_Hearing_test",
             "Soc_Obtain_HA", "Soc_Sometimes_use", "Soc_Regular_use", "Soc_Very_positive",
             "Soc_Somewhat_positive", "Soc_Somewhat_negative", "Soc_Very_negative")

# Run a full lasso regression model
set.seed(1331)
mcv_unweighted <- cv.glmnet(x = as.matrix(pta[,var_28]), 
                            y = pta[,"Purchased_HA"], 
                            weights = NULL,
                            nfolds = 5, 
                            family = "binomial", 
                            type.measure = "auc",
                            alpha = 1, nlambda = 100)

# Looking at effects of different lambda values
# cvm = mean cross-validated error >> Note that AUC is the chosen measure, above!
# cvsd = estimate of standard error of cvm
# cvup = cvm+cvsd; cvlo = cvm-cvsd
# df = number of non-zero coefficients
# dev.ratio = fraction of (null) deviance explained; the "R-square"

# lambda = 0.0274954 gives max AUC 0.6444621; 4 non-zero x's
tempdf <- data.frame(lambda = mcv_unweighted$lambda, 
                     auc = mcv_unweighted$cvm,
                     df = mcv_unweighted$glmnet.fit$df,
                     devratio = mcv_unweighted$glmnet.fit$dev.ratio)
# mcv_unweighted$glmnet.fit: when lambda = 0.027500, explained var = 3.71% 

# plot lambda values with model performance, with SD
plot(x = mcv_unweighted$lambda, y = mcv_unweighted$cvm, 
     xlim = c(0, 0.06), ylim = c(0.5, 0.7), 
     type = 'l', col = 'black', xaxt = 'n', yaxt = 'n', ann=FALSE)
par(new=TRUE)
plot(x = mcv_unweighted$lambda, y = mcv_unweighted$cvup, 
     xlim = c(0, 0.06), ylim = c(0.5, 0.7), 
     type = 'l', col = 'grey70', xaxt = 'n', yaxt = 'n', ann=FALSE)
par(new=TRUE)
plot(x = mcv_unweighted$lambda, y = mcv_unweighted$cvlo, 
     xlim = c(0, 0.06), ylim = c(0.5, 0.7), 
     type = 'l', col = 'grey70', xlab="Lambda", ylab="Mean AUC (with SD)")
abline(v = 0.0274954, col = 'red', lty='dotted')

# number of non-zero coefficients at each lambda
plot(x = mcv_unweighted$lambda, y = mcv_unweighted$nzero, 
     xlim = c(0, 0.06), ylim = c(0, 30), 
     pch=1, col = 'black', xlab="Lambda", ylab="Number of non-zero coefficients")
abline(v = 0.0274954, col='red', lty='dotted')

# Run a full lasso regression model
# Note: non-CV glmnet model doesn't have different measurement options (eg. AUC), but exactly the same results
# Reminder: 𝛼=1 is lasso regression (default) and 𝛼=0 is ridge regression
set.seed(1331)
mlasso <- glmnet(x = pta[,var_28], 
                 y = pta[, "Purchased_HA"],
                 family = c("binomial"),
                 weights = NULL,
                 alpha = 1,
                 nlambda = 100,
                 standardize = TRUE)

# See the non-zero coefficients at the lambda value that gives the best performance (AUC) 
# Age, HHIE_total, HA_stigma_avg, Soc_Suspect_HL
coef(mlasso, s = 0.0274954)

# Make a regression model with just those 4 non-zero coefficients (predictors left after "variable selection")
mlasso2 <- glm(Purchased_HA ~ Age + HHIE_total + HA_stigma_avg + Soc_Suspect_HL, 
               family = "binomial", data = pta)
# See regression model results
summary(mlasso2)
# Transform model coefficients into odds ratios
exp(cbind(OR = coef(mlasso2), confint(mlasso2)))

# Calculate model predicted Y values based on X predictors
mlasso2_pred <- predict(mlasso2, newdata = pta[var_28], type = "response")

# calculate results for different thresholds from 0.03594 to 0.66708
dec_thresholds_lasso <- c(seq(0.04, 0.19, by=0.01), 0.1978752, seq(0.21, 0.6, by=0.01))

mlasso2_roc_df <- data.frame(threshold = rep(NA, length(dec_thresholds_lasso)), 
                           accuracy = rep(NA, length(dec_thresholds_lasso)), 
                           sensitivity = rep(NA, length(dec_thresholds_lasso)), 
                           specificity = rep(NA, length(dec_thresholds_lasso)) )

for (i in 1:length(dec_thresholds_lasso)) {
mlasso2_pred_bi <- ifelse(mlasso2_pred >= dec_thresholds_lasso[i], 1, 0)
confusion_output <- confusionMatrix(data = as.factor(mlasso2_pred_bi), 
                                    reference = as.factor(pta$Purchased_HA), 
                                    positive = c("1"))
mlasso2_roc_df$threshold[i] <- dec_thresholds_lasso[i]
mlasso2_roc_df$accuracy[i] <- confusion_output$overall[1]
mlasso2_roc_df$sensitivity[i] <- confusion_output$byClass[1]
mlasso2_roc_df$specificity[i] <- confusion_output$byClass[2]
}

# plot ROC for Full, Step, Lasso-based models

plot(1-mlasso2_roc_df$specificity, mlasso2_roc_df$sensitivity, type='l', 
     xaxt='n', xaxt='n', ann=FALSE, ylim=c(0,1), xlim=c(0,1), 
     main="", col="blue")

par(new=TRUE)
plot(1-mstep_roc_df$specificity, mstep_roc_df$sensitivity, type='l', 
     xaxt='n', xaxt='n', ann=FALSE, ylim=c(0,1), xlim=c(0,1), 
     main="", col="red")

par(new=TRUE)
plot(1-mfull_roc_df$specificity, mfull_roc_df$sensitivity, type='l', 
     ylab="Sensitivity", xlab="False positives", ylim=c(0,1), xlim=c(0,1), 
     main="ROC curves", col="black")

y = c(0,1)
x = c(0,1)
abline(lm(y ~ x), col="grey80")

points(y=0.63758389, x=(1-0.655629139), pch=1, col = "black")
points(y=0.61073826, x=(1-0.67052980), pch=1, col = "red")
points(y=0.597315436, x=(1-0.644039735), pch=1, col = "blue")

text(0.2, 0.7, "Threshold p = 0.1978")

legend(0.56, 0.45, 
       legend = c("Full model: 28 pred", "Back step: 6 pred", "Lasso: 4 pred"), 
       lty = c("solid", "solid", "solid"), col = c("black", "red", "blue"), 
       bty="n")


# accuracy, sensitivity, specificity values fetched from threshold tables
# add AUC; note must recalculate "_bi" to get away from previous loops using different thresholds

mfull_pred_bi <- ifelse(mfull_pred >=0.1978752, 1, 0)
table(mfull_pred_bi, pta$Purchased_HA, dnn = c("predicted", "actual") )
roc(response = pta$Purchased_HA, predictor = mfull_pred_bi) #0.6533

mstep_pred_bi <- ifelse(mstep_pred >=0.1978752, 1, 0)
table(mstep_pred_bi, pta$Purchased_HA, dnn = c("predicted", "actual") )
roc(response = pta$Purchased_HA, predictor = mstep_pred_bi) #0.6406

mlasso2_pred_bi <- ifelse(mlasso2_pred >=0.1978752, 1, 0)
table(mlasso2_pred_bi, pta$Purchased_HA, dnn = c("predicted", "actual") )
roc(response = pta$Purchased_HA, predictor = mlasso2_pred_bi) #0.6207

# summary table with model metrics
obj <- data.frame(preds = c(28, 6, 4), 
                  acc = c(65.47, 65.86, 63.47), 
                  sens = c(65.10, 61.07, 59.73), 
                  spec = c(65.56, 67.05, 64.40),
                  auc = c(65.33, 64.06, 62.07))
rownames(obj) <- c("Full model", "Backwards step", "Lasso regression")
colnames(obj) <- c("Predictors", "Accuracy %", "Sensitivity %", "Specificity %", "AUC")

kable(obj, format="pipe", row.names = TRUE)

#####

# Power calculations
#####

### Calculate VIF to use as adjustment for single-covariate sample size (standardization doesn't matter)
# given VIF = 1/(1-R-squared), then R-squared = 1 - 1/VIF

# get VIF's for 4-predictor model
df_mlasso2 <- data.frame(vif(mod = mlasso2))
colnames(df_mlasso2) <- "VIF"
df_mlasso2$R2 <- 1 - 1/df_mlasso2$VIF
print(df_mlasso2)
#                    VIF          R2
#Age            1.067506 0.063236741
#HHIE_total     1.071112 0.066390459
#HA_stigma_avg  1.009238 0.009153610
#Soc_Suspect_HL 1.008426 0.008355936

### Eq. 1 from Hsieh et al (1998) "A Simple Method of Sample Size Calculation for Linear and Logistic Regression"

### Continuous predictor, Age

# standardize Age variable
pta$Age_z <- scale(pta$Age, center=TRUE, scale=TRUE)
m_age <- glm(Purchased_HA ~ Age_z, data = pta, family="binomial")
summary(m_age)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.42770    0.09369 -15.239  < 2e-16 ***
#Age_z        0.30673    0.09309   3.295 0.000985 ***

# for alpha=0.05 two-tailed, power=0.8
# p at mean of standardized Age = exp(-1.42770) / (1 - exp(-1.42770 )) = 0.3155471
# log odds for Age_z = 0.30673
# adjustment VIF for Age = 1.067506
# total n = (1.96 + 0.8417857)^2 / (0.3155471 * (1-0.3155471) * 0.30673^2) * 1.067506 = 412.4012

### Continuous predictor, HHIE_total

# standardize HHIE_total variable
pta$HHIE_total_z <- scale(pta$HHIE_total, center=TRUE, scale=TRUE)
m_hhie <- glm(Purchased_HA ~ HHIE_total_z, data = pta, family="binomial")
summary(m_hhie)
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.43743    0.09439 -15.229  < 2e-16 ***
#HHIE_total_z  0.35572    0.09030   3.939 8.17e-05 ***

# for alpha=0.05 two-tailed, power=0.8
# p at mean of standardized HHIE = exp(-1.43743) / (1 - exp(-1.43743 )) = 0.3115398
# log odds for HHIE_z = 0.35572
# VIF for HHIE = 1.071112
# total n = (1.96 + 0.8417857)^2 / (0.3115398 * (1-0.3115398) * 0.35572^2) * 1.071112 = 309.8102

### Continuous predictor, HA_stigma_avg

# standardize HA_stigma_avg
pta$HA_stigma_avg_z <- scale(pta$HA_stigma_avg, center=TRUE, scale=TRUE)
m_hstig <- glm(Purchased_HA ~ HA_stigma_avg_z, data = pta, family="binomial")
summary(m_hstig)
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -1.41041    0.09233 -15.276   <2e-16 ***
#HA_stigma_avg_z -0.18944    0.09227  -2.053     0.04 *  

# p at mean of standardized HA Stigma = exp(-1.41041)/(1 - exp(-1.41041)) = 0.3228269
# log odds for HA Stigma_z = -0.18944
# VIF for HA Stigma = 1.009238
# total n = (1.96 + 0.8417857)^2 / (0.3228269 * (1-0.3228269) * (-0.18944)^2) * 1.009238 = 1009.836

### Eq 2 from Hsieh et al (1998)
### Binary predictor, Soc_Suspect_HL

t1 <- table(pta$Purchased_HA, pta$Soc_Suspect_HL, dnn = c("Purchased", "Soc_Suspect_HL"))
addmargins(t1)
#        Soc_Suspect_HL
#Purchased   0   1 Sum
#      0    95 509 604
#      1    11 138 149
#      Sum 106 647 753

# P, overall event rate = 149/753 = 0.1978752
# P1, event rate at Soc_Suspect_HL==0 = 11/106 = 0.1037736
# P2, event rate at Soc_Suspect_HL==1 = 138/647 = 0.2132921
# B, proportion of sample with Soc_Suspect_HL==1 = 647/753 = 0.8592297

# for alpha = 0.95 2-tailed (z = 1.96), power = 0.8 (z = 0.8417857), n for single covariate = 762.4415 
( 1.96*( 0.1978752 * (1-0.1978752) / 0.8592297 )^0.5 + 0.8418*( 0.1037736*(1-0.1037736) + 0.2132921*(1-0.2132921)*(1-0.8592297)/0.8592297 )^0.5 )^2 / ((0.1037736 - 0.2132921)^2 * (1-0.8592297))
# adding inflation factor, n with three covariates = 762.4415 * 1.008426 = 768.8658

### Using G*Power
# use single-covariate models from above with standardized coefficients for continuous variables

# Age: total sample size = 428, power = 0.80056
# OR = exp(0.30673) = 1.358974
# pH0 = exp(-1.42770) / (1 - exp(-1.42770)) = 0.3155471
# G*Power: tails=2, OR=1.358974, PrH0=0.3155471, alpha=0.05, power=0.8, R2=0.063236741, x Normal, x mean 0 sd 1

# HHIE_total: total sample size = 325, power = 0.80026
# OR = exp(0.35572) = 1.427208
# pH0 = exp(-1.43743) / (1 - exp(-1.43743)) = 0.3115398
# G*Power: tails=2, OR=1.427208, PrH0=0.3115398, alpha=0.05, power=0.8, R2=0.066390459, x Normal, x mean 0 sd 1

# HA_stigma_avg: total sample size = 1025, power = 0.80035
# OR = exp(-0.18944) = 0.8274224
# pH0 = exp(-1.4104) / (1 - exp(-1.4104)) = 0.3228312
# G*Power: tails=2, OR=0.8412722, PrH0=, alpha=0.05, power=0.8, R2=0.009153610, x Normal, x mean 0 sd 1

# Soc_Suspect_HL; 
# OR = ((138/647)/(509/647)) / ((11/106)/(95/106)) = 2.341489; agrees with model exp(0.8508)=2.341519
# pi = 647/753 = 0.8592297
# What to use for p (Y=1 | X=1) H0 ?? 
# Is this the overall proportion of Y=1, which is 0.1978752 ?? if so, sample size 507
# if using model intercept, exp(-2.1560)/(1-exp(-2.1560)) = 0.1309497, sample size 680
# G*Power: tails=2, PrH0=??, alpha=0.05, power=0.8, R2=0.008355596, x binomial, x pi=0.8592297

# Draw table
obj <- data.frame(oddsratio = c(1.046, 1.047, 0.849, 2.101), 
                  sampsize = c(413, 310, 1010, 769))
rownames(obj) <- c("Age", "HHIE", "HA_Stigma", "Soc_Suspect_HL")
colnames(obj) <- c("Odds ratio", "Required sample size")

kable(obj, format="pipe", row.names = TRUE)

#####


