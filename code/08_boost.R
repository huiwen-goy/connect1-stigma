
# Libraries
library(caret)
library(mboost)
library(partykit)
library(plyr)
library(ggplot2)
library(dplyr)
library(knitr)

# 28 variables of interest predicting factor outcome
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

# Add case weights
pta$caseweights <- ifelse(pta$HA.Purchase == "No", 0.2466887, 1)

# grid search
# Note: set allowParallel=FALSE to be reproducible with seed
# caret/blackboost doesn't take nu in grid, so manually run different nu's

set.seed(200)
myseeds <- vector(mode = "list", length = 16)
for(i in 1:16) { myseeds[[i]]<- sample.int(n=1000, 3) }

msearch_nu001 <- train(formula28x, 
                 data = pta,
                 method = "blackboost", 
                 metric = "Accuracy",
                 weights = pta$caseweights,
                 family = AdaExp(),
                 tuneGrid = expand.grid(mstop = c(5, 10, 25, 50, 100, 200),
                                        maxdepth = c(1, 2, 3) ), 
                 control = boost_control(nu = 0.01), 
                 tree_controls = partykit::ctree_control( teststat = "quadratic", 
                                                          testtype = "Bonferroni",
                                                          mincriterion = 0.95,
                                                          minsplit = 20, 
                                                          maxsurrogate = 0,
                                                          caseweights = TRUE, 
                                                          mtry = Inf ), 
                 trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3,
                                          returnResamp = "all",
                                          allowParallel = FALSE, 
                                          seeds = myseeds )
                 )

write.csv(msearch_nu001$resample,    "~/blackboost_msearch_nu001_resample.csv")
write.csv(msearch_nu001$resampledCM, "~/blackboost_msearch_nu001_resampledCM.csv")
write.csv(msearch_nu001$results,     "~/blackboost_msearch_nu001_results.csv")

# Stack different nu results

nu001 <- read.csv("blackboost_msearch_nu001_results.csv", header=T)
  colnames(nu001)[1] <- "nu"
  nu001[, 1] <- rep(0.01, 18)

nu005 <- read.csv("blackboost_msearch_nu005_results.csv", header=T)
  colnames(nu005)[1] <- "nu"
  nu005[, 1] <- rep(0.05, 18)
  
nu01 <- read.csv("blackboost_msearch_nu01_results.csv", header=T)
  colnames(nu01)[1] <- "nu"
  nu01[, 1] <- rep(0.1, 18)
  
nu025 <- read.csv("blackboost_msearch_nu025_results.csv", header=T)
  colnames(nu025)[1] <- "nu"
  nu025[, 1] <- rep(0.25, 18)

nu05 <- read.csv("blackboost_msearch_nu05_results.csv", header=T)
  colnames(nu05)[1] <- "nu"
  nu05[, 1] <- rep(0.5, 18)

nu1 <- read.csv("blackboost_msearch_nu1_results.csv", header=T)
  colnames(nu1)[1] <- "nu"
  nu1[, 1] <- rep(1, 18)
  
dfnu <- data.frame(rbind(nu001, nu005, nu01, nu025, nu05, nu1))  

dfnu$nu <- as.factor(dfnu$nu)
dfnu$maxdepth <- as.factor(dfnu$maxdepth)
dfnu$lower <- dfnu$Accuracy - dfnu$AccuracySD
dfnu$upper <- dfnu$Accuracy + dfnu$AccuracySD

# Plot accuracy vs. num trees, with maxdepth as separate lines, nu in separate panels  

nu_values <- c("0.01" = "nu = 0.01", "0.05" = "nu = 0.05", "0.1" = "nu = 0.1", 
               "0.25" = "nu = 0.25", "0.5" = "nu = 0.5", "1" = "nu = 1")  
depth_colours <- c("grey30", "cadetblue3", "indianred")

ggplot(data = dfnu, 
       aes(x = mstop, y = Accuracy, group = maxdepth, ymin = lower, ymax = upper) ) + 
  geom_line(aes(colour = maxdepth), linewidth = 1.2) +
  geom_ribbon(aes(fill = maxdepth), alpha = 0.1) + 
  scale_colour_manual(values = depth_colours) + 
  scale_fill_manual(values = depth_colours, guide = 'none') + 
  scale_x_continuous(name="Number of trees (iterations)", limits=c(0, 200), breaks=seq(0, 200, 20)) + 
  scale_y_continuous(name="Accuracy", limits=c(0.45, 0.75), breaks=seq(0.45, 0.75, 0.05)) + 
  facet_wrap( ~ dfnu$nu, ncol=3, 
              labeller = as_labeller(nu_values)) + 
  theme_bw() + 
  theme(axis.title.x = element_text(size = 20, colour = "black"), 
        axis.text.x = element_text(size = 14, colour = "black"), 
        axis.title.y = element_text(size = 20, colour = "black"), 
        axis.text.y = element_text(size = 14, colour = "black") ) + 
  theme(strip.text.x = element_text(size = 12), 
        strip.background = element_rect(color = "black", fill="white", linewidth=1.1) ) +
  theme(panel.background = element_rect(color = "black", linewidth = 1.1)) + 
  theme(legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))

# final blackboost model; need to use caret::train to get variable importance
mfinal <- train(formula28x, 
                 data = pta,
                 method = "blackboost", 
                 metric = "Accuracy",
                 weights = pta$caseweights,
                 family = AdaExp(),
                 control = boost_control(mstop = 10, nu = 0.1), 
                 tree_controls = partykit::ctree_control( teststat = "quadratic", 
                                                          testtype = "Bonferroni",
                                                          mincriterion = 0.95,
                                                          minsplit = 20, 
                                                          maxdepth = 3, 
                                                          maxsurrogate = 0,
                                                          caseweights = TRUE, 
                                                          mtry = Inf ), 
                 trControl = trainControl(method = "none",
                                          allowParallel = FALSE, 
                                          seeds = 201 )
                 )

# predictions using final model
mfinal_preds <- predict(mfinal, newdata=pta)

# confusion matrix of predicted vs. actual outcomes
table(mfinal_preds, pta$HA.Purchase, dnn=c("predicted", "actual"))

# get accuracy, sensitivity, specificity
confusionMatrix(data = mfinal_preds, reference = pta$HA.Purchase, positive = c("Yes"))

# get AUC; need numeric or ordered outcome
library(pROC)
mfinal_preds_bi <- ifelse(mfinal_preds == "No", 0, 1)
roc(response = pta$Purchased_HA, predictor = mfinal_preds_bi)

# Get variable importance from final model
vimp <- data.frame(varImp(mfinal)[[1]][1])
vimp$variable <- rownames(vimp)
colnames(vimp) <- c("Importance", "Variable")
rownames(vimp) <- c(1:28)
vi <- vimp[order(vimp$Importance, decreasing = TRUE), ]

# Plot variable importance
ggplot(data = vi, aes(y = Importance, x = c(1:28))) + 
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




