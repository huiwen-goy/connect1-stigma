

### Prep dataset for analysis
#####
# Read raw data
orig <- read.csv("Connect_Data_20191025.csv", header=TRUE)

# Must have known age and sex, 50+ years old, never used a HA
data <- orig[which(is.na(orig$Q44) == FALSE & is.na(orig$Q43) == FALSE & orig$Q44 > 49.5 & orig$Q42 == "No"), ]

# Rename variables, recode variables
data$Age <- data$Q44

data$Sex <- c(NA)
data$Sex[data$Q43 == "Male"] <- 1
data$Sex[data$Q43 == "Female"] <- 0

data$Edu <- data$Q46_Corrected

data$Health <- data$Q53

data$QoL <- data$Q54

data$Accomp <- c(NA)
data$Accomp[data$Q45 == "Yes"] <- 1
data$Accomp[data$Q45 == "No"] <- 0

data$Married <- c(NA) 
data$Married[data$marital_recoded == "Married"] <- 1
data$Married[data$marital_recoded == "Sep_Divor"] <- 0
data$Married[data$marital_recoded == "Single"] <- 0
data$Married[data$marital_recoded == "Widowed"] <- 0
data$Married[data$marital_recoded == "Other"] <- 0

data$Help_neighbours <- data$Q50_Corrected

data$Help_problems <- c(NA)
data$Help_problems[is.na(data$Q51)==FALSE & data$Q51 == "0"] <- 0
data$Help_problems[is.na(data$Q51)==FALSE & data$Q51 == "1_2"] <- 1
data$Help_problems[is.na(data$Q51)==FALSE & data$Q51 == "3_5"] <- 2
data$Help_problems[is.na(data$Q51)==FALSE & data$Q51 == "5+"] <- 3

data$Concern <- data$Q52

data$Lonely <- data$Q55

# Q21
data$Soc_Suspect_HL <- c(NA)
data$Soc_Suspect_HL[data$Q21_Corrected != "0" & is.na(data$Q21_Corrected)==FALSE] <- 1
data$Soc_Suspect_HL[data$Q21_Corrected == "0" & is.na(data$Q21_Corrected)==FALSE] <- 0 

# Q22
data$Soc_Know_HL <- c(NA)
data$Soc_Know_HL[data$Q22_Corrected != "0" & is.na(data$Q22_Corrected)==FALSE] <- 1
data$Soc_Know_HL[data$Q22_Corrected == "0" & is.na(data$Q22_Corrected)==FALSE] <- 0 

# Q23
data$Soc_Discuss_HL <- c(NA)
data$Soc_Discuss_HL[data$Q23_Corrected != "0" & is.na(data$Q23_Corrected)==FALSE] <- 1
data$Soc_Discuss_HL[data$Q23_Corrected == "0" & is.na(data$Q23_Corrected)==FALSE] <- 0 

# Q24
data$Soc_Hearing_test <- c(NA)
data$Soc_Hearing_test[data$Q24_Corrected != "0" & is.na(data$Q24_Corrected)==FALSE] <- 1
data$Soc_Hearing_test[data$Q24_Corrected == "0" & is.na(data$Q24_Corrected)==FALSE] <- 0 

# Q25
data$Soc_Obtain_HA <- c(NA)
data$Soc_Obtain_HA[data$Q25_Corrected != "0" & is.na(data$Q25_Corrected)==FALSE] <- 1
data$Soc_Obtain_HA[data$Q25_Corrected == "0" & is.na(data$Q25_Corrected)==FALSE] <- 0 

# Q26
data$Soc_Sometimes_use <- c(NA)
data$Soc_Sometimes_use[data$Q26_Corrected != "0" & is.na(data$Q26_Corrected)==FALSE] <- 1
data$Soc_Sometimes_use[data$Q26_Corrected == "0" & is.na(data$Q26_Corrected)==FALSE] <- 0 

# Q27
data$Soc_Regular_use <- c(NA)
data$Soc_Regular_use[data$Q27_Corrected != "0" & is.na(data$Q27_Corrected)==FALSE] <- 1
data$Soc_Regular_use[data$Q27_Corrected == "0" & is.na(data$Q27_Corrected)==FALSE] <- 0 

# Q28
data$Soc_Very_positive <- c(NA)
data$Soc_Very_positive[data$Q28_Corrected != "0" & is.na(data$Q28_Corrected)==FALSE] <- 1
data$Soc_Very_positive[data$Q28_Corrected == "0" & is.na(data$Q28_Corrected)==FALSE] <- 0

# Q29
data$Soc_Somewhat_positive <- c(NA)
data$Soc_Somewhat_positive[data$Q29_Corrected != "0" & is.na(data$Q29_Corrected)==FALSE] <- 1
data$Soc_Somewhat_positive[data$Q29_Corrected == "0" & is.na(data$Q29_Corrected)==FALSE] <- 0 

# Q30
data$Soc_Somewhat_negative <- c(NA)
data$Soc_Somewhat_negative[data$Q30_Corrected != "0" & is.na(data$Q30_Corrected)==FALSE] <- 1
data$Soc_Somewhat_negative[data$Q30_Corrected == "0" & is.na(data$Q30_Corrected)==FALSE] <- 0 

# Q31
data$Soc_Very_negative <- c(NA)
data$Soc_Very_negative[data$Q31_Corrected != "0" & is.na(data$Q31_Corrected)==FALSE] <- 1
data$Soc_Very_negative[data$Q31_Corrected == "0" & is.na(data$Q31_Corrected)==FALSE] <- 0 

#Q56 overall hearing ability
data$Ability <- data$Q56_Corrected

# outcome measure
data$Purchased_HA <- c(NA)
data$Purchased_HA[data$HA.Purchase == "Yes" & is.na(data$HA.Purchase)==FALSE] <- 1
data$Purchased_HA[data$HA.Purchase == "No" & is.na(data$HA.Purchase)==FALSE] <- 0

# make into factors
data$Sex.f <- factor(data$Q43, levels=c("Female", "Male"))
data$Accomp.f <- factor(data$Q45, levels=c("No", "Yes"))
data$Married.f <- as.factor(data$Married)
  levels(data$Married.f) <- c("No", "Yes")
data$Soc_Suspect_HL.f <- as.factor(data$Soc_Suspect_HL)
	levels(data$Soc_Suspect_HL.f) <- c("No", "Yes")
data$Soc_Know_HL.f <- as.factor(data$Soc_Know_HL)
	levels(data$Soc_Know_HL.f) <- c("No", "Yes")
data$Soc_Discuss_HL.f <- as.factor(data$Soc_Discuss_HL)
	levels(data$Soc_Discuss_HL.f) <- c("No", "Yes")
data$Soc_Hearing_test.f <- as.factor(data$Soc_Hearing_test)
	levels(data$Soc_Hearing_test.f) <- c("No", "Yes")
data$Soc_Obtain_HA.f <- as.factor(data$Soc_Obtain_HA)
	levels(data$Soc_Obtain_HA.f) <- c("No", "Yes")
data$Soc_Sometimes_use.f <- as.factor(data$Soc_Sometimes_use)
	levels(data$Soc_Sometimes_use.f) <- c("No", "Yes")
data$Soc_Regular_use.f <- as.factor(data$Soc_Regular_use)
	levels(data$Soc_Regular_use.f) <- c("No", "Yes")
data$Soc_Very_positive.f <- as.factor(data$Soc_Very_positive)
	levels(data$Soc_Very_positive.f) <- c("No", "Yes")
data$Soc_Somewhat_positive.f <- as.factor(data$Soc_Somewhat_positive)
	levels(data$Soc_Somewhat_positive.f) <- c("No", "Yes")
data$Soc_Somewhat_negative.f <- as.factor(data$Soc_Somewhat_negative)
	levels(data$Soc_Somewhat_negative.f) <- c("No", "Yes")
data$Soc_Very_negative.f <- as.factor(data$Soc_Very_negative)
	levels(data$Soc_Very_negative.f) <- c("No", "Yes")
data$HA.Purchase <- factor(data$HA.Purchase, levels=c("No", "Yes"))

# select cases with PTA-BE > 25 and complete data on variables of interest
pta <- subset(data, 
  PTA4_better_ear > 25 &           
  is.na(data$Age)==F & 
  is.na(data$PTA4_better_ear)==F & 
  is.na(data$HHIE_total)==F & 
  is.na(data$Ability)==F &  
  is.na(data$Sex)==F & 
  is.na(data$Edu)==F &  
  is.na(data$Married)==F &  
  is.na(data$Health)==F & 
  is.na(data$QoL)==F & 
  is.na(data$Help_neighbours)==F &  
  is.na(data$Help_problems)==F &
  is.na(data$Concern)==F & 
  is.na(data$Lonely)==F &   
  is.na(data$Sub_Age_avg)==F &  
  is.na(data$Age_stigma_avg)==F & 
  is.na(data$HA_stigma_avg)==F & 
  is.na(data$Accomp)==F & 
  is.na(data$Soc_Suspect_HL)==F &
  is.na(data$Soc_Know_HL)==F &
  is.na(data$Soc_Discuss_HL)==F &
  is.na(data$Soc_Hearing_test)==F &
  is.na(data$Soc_Obtain_HA)==F &   
  is.na(data$Soc_Sometimes_use)==F &
  is.na(data$Soc_Regular_use)==F &
  is.na(data$Soc_Very_positive)==F &
  is.na(data$Soc_Somewhat_positive)==F &
  is.na(data$Soc_Somewhat_negative)==F &
  is.na(data$Soc_Very_negative)==F)

# Tidy up
rm(list = c("orig", "data"))
 
#####


### Get descriptives (Table 1)
#####

# Make a dataframe to store descriptives of continuous variables
dfcont <- data.frame(variable = character(), 
                     group1 = character(), mean.No = numeric(), sd.No = numeric(),
                     group2 = character(), mean.Yes = numeric(), sd.Yes = numeric())

# List of continuous variables  
pcont <- c('Age', 'PTA4_better_ear', 'HHIE_total', 'Ability', 'Edu', 'Health', 'QoL',
           'Help_neighbours','Help_problems','Concern','Lonely',
           'Sub_Age_avg','Age_stigma_avg','HA_stigma_avg')

# Get mean, sd, name of each variable, name of each group
for (i in 1:14) {
  tempmeans <- tapply(pta[, pcont[i]], list(Group = pta$HA.Purchase), mean)
  tempsd <- tapply(pta[, pcont[i]], list(Group = pta$HA.Purchase), sd)
  dfcont[i, 1] <- pcont[i]
  dfcont[i, 2] <- names(tempmeans)[1]
  dfcont[i, 3] <- tempmeans[[1]]
  dfcont[i, 4] <- tempsd[[1]]
  dfcont[i, 5] <- names(tempmeans)[2]
  dfcont[i, 6] <- tempmeans[[2]]
  dfcont[i, 7] <- tempsd[[2]]
}

# Do the same for categorical variables, except with counts
dfcat <- data.frame(variable = character(), 
                    HAgroup1 = character(), HAgroup1cat1 = character(), HAgroup1count1 = integer(), 
                    HAgroup1cat2 = character(), HAgroup1count2 = integer(), 
                    HAgroup2 = character(), HAgroup2cat1 = character(), HAgroup2count1 = integer(), 
                    HAgroup2cat2 = character(), HAgroup2count2 = integer())

pcat <- c('Sex.f', 'Married.f', 'Accomp.f', 
          'Soc_Suspect_HL.f','Soc_Know_HL.f','Soc_Discuss_HL.f','Soc_Hearing_test.f',
          'Soc_Obtain_HA.f','Soc_Sometimes_use.f','Soc_Regular_use.f',
          'Soc_Very_positive.f','Soc_Somewhat_positive.f','Soc_Somewhat_negative.f','Soc_Very_negative.f')

for (i in 1:14) {
  temptable <- table(pta[, pcat[i]], pta$HA.Purchase)
  dfcat[i, 1] <- pcat[i] #variable
  dfcat[i, 2] <- dimnames(temptable)[[2]][1] #HAgroup1
  dfcat[i, 3] <- dimnames(temptable)[[1]][1] #HAgroup1cat1
  dfcat[i, 4] <- temptable[[1]] #HAgroup1count1
  dfcat[i, 5] <- dimnames(temptable)[[1]][2] #HAgroup1cat2
  dfcat[i, 6] <- temptable[[2]] #HAgroup1count2
  dfcat[i, 7] <- dimnames(temptable)[[2]][2] #HAgroup2
  dfcat[i, 8] <- dimnames(temptable)[[1]][1] #HAgroup2cat1
  dfcat[i, 9] <- temptable[[3]] #HAgroup2count1
  dfcat[i, 10] <- dimnames(temptable)[[1]][2] #HAgroup2cat2
  dfcat[i, 11] <- temptable[[4]] #HAgroup2count2


#####


### Correlation matrices (Tables 2, 3, 4)
#####
library(psych)
  
sub_age <- subset(pta, select = c(Q1_Corrected, Q2_Corrected, Q3_Corrected))
age_stigma <- subset(pta, select = c(Q4_Corrected, Q5_Corrected, Q6_Corrected, Q7_Corrected, Q8_Corrected))
ha_stigma <- subset(pta, select = c(Q9_Corrected, Q10_Corrected, Q11_Corrected, Q12_Corrected))

cm_sub_age <- corr.test(sub_age)
cm_age_stigma <- corr.test(age_stigma)
cm_ha_stigma <- corr.test(ha_stigma)

round(cor(sub_age), 2)
cm_sub_age$p

round(cor(age_stigma), 2)
cm_age_stigma$p

round(cor(ha_stigma), 2)
cm_ha_stigma$p
#####      
  
  
### Plot audiogram (Figure 1)
#####
# Group participants by HA purchase
pta.yes <- subset(pta, HA.Purchase == "Yes")
pta.no <- subset(pta, HA.Purchase == "No")

library(ggplot2)

# Specify x-axis datapoints
freq <- c(250, 500, 1000, 2000, 4000, 8000)

# Purchase Yes threshold means & SDs
audio.yes <- pta.yes[, c(1, 83:94)]

audio.yes$Better_250 <- pmin(audio.yes$LE250c, audio.yes$RE250c)
audio.yes$Better_500 <- pmin(audio.yes$LE500c, audio.yes$RE500c)
audio.yes$Better_1000 <- pmin(audio.yes$LE1000c, audio.yes$RE1000c)
audio.yes$Better_2000 <- pmin(audio.yes$LE2000c, audio.yes$RE2000c)
audio.yes$Better_4000 <- pmin(audio.yes$LE4000c, audio.yes$RE4000c)
audio.yes$Better_8000 <- pmin(audio.yes$LE8000c, audio.yes$RE8000c)

audio.yes$Worse_250 <- pmax(audio.yes$LE250c, audio.yes$RE250c)
audio.yes$Worse_500 <- pmax(audio.yes$LE500c, audio.yes$RE500c)
audio.yes$Worse_1000 <- pmax(audio.yes$LE1000c, audio.yes$RE1000c)
audio.yes$Worse_2000 <- pmax(audio.yes$LE2000c, audio.yes$RE2000c)
audio.yes$Worse_4000 <- pmax(audio.yes$LE4000c, audio.yes$RE4000c)
audio.yes$Worse_8000 <- pmax(audio.yes$LE8000c, audio.yes$RE8000c)

yes.better.m <- as.vector(colMeans(audio.yes[, c(14:19)], na.rm=TRUE))
yes.better.sd <- as.vector(sapply(X = audio.yes[, c(14:19)], FUN = sd, na.rm=TRUE))
yes.worse.m <- as.vector(colMeans(audio.yes[, c(20:25)], na.rm=TRUE))
yes.worse.sd <- as.vector(sapply(X = audio.yes[, c(20:25)], FUN = sd, na.rm=TRUE))

audio.yes2 <- data.frame(freq = rep(freq, 2), 
                         ear = c(rep('Better ear', 6), rep('Worse ear', 6)),
                         threshold = c(yes.better.m, yes.worse.m), 
                         sd = c(yes.better.sd, yes.worse.sd))

# Purchase No threshold means & SDs
audio.no <- pta.no[, c(1, 83:94)]

audio.no$Better_250 <- pmin(audio.no$LE250c, audio.no$RE250c)
audio.no$Better_500 <- pmin(audio.no$LE500c, audio.no$RE500c)
audio.no$Better_1000 <- pmin(audio.no$LE1000c, audio.no$RE1000c)
audio.no$Better_2000 <- pmin(audio.no$LE2000c, audio.no$RE2000c)
audio.no$Better_4000 <- pmin(audio.no$LE4000c, audio.no$RE4000c)
audio.no$Better_8000 <- pmin(audio.no$LE8000c, audio.no$RE8000c)

audio.no$Worse_250 <- pmax(audio.no$LE250c, audio.no$RE250c)
audio.no$Worse_500 <- pmax(audio.no$LE500c, audio.no$RE500c)
audio.no$Worse_1000 <- pmax(audio.no$LE1000c, audio.no$RE1000c)
audio.no$Worse_2000 <- pmax(audio.no$LE2000c, audio.no$RE2000c)
audio.no$Worse_4000 <- pmax(audio.no$LE4000c, audio.no$RE4000c)
audio.no$Worse_8000 <- pmax(audio.no$LE8000c, audio.no$RE8000c)

no.better.m <- as.vector(colMeans(audio.no[, c(14:19)], na.rm=TRUE))
no.better.sd <- as.vector(sapply(X = audio.no[, c(14:19)], FUN = sd, na.rm=TRUE))
no.worse.m <- as.vector(colMeans(audio.no[, c(20:25)], na.rm=TRUE))
no.worse.sd <- as.vector(sapply(X = audio.no[, c(20:25)], FUN = sd, na.rm=TRUE))

audio.no2 <- data.frame(freq = rep(freq, 2), 
                         ear = c(rep('Better ear', 6), rep('Worse ear', 6)),
                         threshold = c(no.better.m, no.worse.m), 
                         sd = c(no.better.sd, no.worse.sd))

# Stack means & SDs from Purchase Yes and Purchase No
audio2 <- rbind(audio.no2, audio.yes2)
audio2$HA.Purchase <- c(rep("No", 12), rep("Yes", 12))

# Export as PDF, 6 x 12 inches landscape
ggplot(data = audio2, aes(x = freq, y = threshold, group = ear, shape = ear)) + 
  facet_grid(~ HA.Purchase) + 
  geom_point(size = 3) +
    scale_shape_manual(values = c(19, 17)) +
  geom_errorbar(data = audio2, 
    aes(x = freq, ymin = (threshold - sd), ymax = (threshold + sd)), 
    width = 0.05, position = position_dodge(0.01)) +
  geom_line(aes(linetype = ear), color = "black") + 
    scale_linetype_manual(values = c("solid", "longdash")) + 
    guides(linetype = FALSE) + 
  labs(x = "Frequency (Hz)", y = "Threshold (dB HL)") +
  scale_y_reverse(limits = c(80, 0), breaks = seq(0, 80, by = 10)) + 
  scale_x_log10(breaks = unique(audio2$freq), labels = unique(c('250', '500', '1000', '2000', '4000', '8000'))) +
  theme_bw() + 
  theme(axis.title = element_text(size = 25), 
    axis.text = element_text(colour = "black", size = 20), 
    text = element_text(size = 20)) + 
  theme(panel.background = element_rect(color = "black", size = 1)) + 
  theme(strip.background = element_rect(color = "black", size = 1, 
                                        fill="white", linetype="solid")) + 
  theme(legend.position = c(0.12, 0.15), 
        legend.background = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(colour = "black", size = 20))

#####    
    

### Logistic regression
#####

# Dataset = ptacomp.n; PTA-BE > 25, complete data for 28 predictors, using numeric version of binary variables

# Libraries
library(caret)
library(DescTools)

# Standard formula, predicting HA purchase from 28 variables
formula.28x <- formula(Purchased_HA ~ 
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

# Full model, and model with backwards elimination procedure
m.full <- glm(formula.28x, family = binomial("logit"), data = ptacomp.n)
m.step <- step(object = m.full, direction = c("backward"), trace = 0)

summary(m.step)

# Calculate odds ratios from coefficients
exp(cbind(OR = coef(m.step), confint(m.step)))

# Proportion of how many were 'Yes', to use as threshold for predicted binary outcome
table(ptacomp.n$Purchased_HA)

# Check model performance
predm <- predict.glm(m.step, newdata = ptacomp.n, type = 'response')
predm.bi <- ifelse(predm >= 0.1978752, 1, 0)
confusionMatrix(data = as.factor(predm.bi), 
                reference = as.factor(ptacomp.n$Purchased_HA), 
                positive = c("1"))

# McFadden's pseudo-R-squared
m.null <- glm(Purchased_HA ~ 1, family = binomial("logit"), data = ptacomp.n)
m.step <- glm(Purchased_HA ~ Age + HHIE_total + Health + HA_stigma_avg + 
              Soc_Suspect_HL + Soc_Discuss_HL, family = binomial("logit"), data = ptacomp.n)
R2 <- 1 - logLik(m.step) / logLik(m.null) #'log Lik.' 0.06582303 (df=7)

# AUC
Cstat(m.null) # checking; 0.5
Cstat(m.step) # 0.6806136

#####


### Net Reclassification Index
#####

# Dataset ptacomp.f = PTA-BE > 25, complete data for 28 predictors, using factor version of binary variables

# Libraries
library(caret)
library(PredictABEL)

# List of 26 predictors to cycle through, excluding Age and HHIE_total in the base model
predictors <- c('PTA4_better_ear','Ability','Sex.f','Edu','Married.f','Health','QoL',
                'Help_neighbours','Help_problems','Concern','Lonely',
                'Sub_Age_avg','Age_stigma_avg','HA_stigma_avg','Accomp.f',
                'Soc_Suspect_HL.f','Soc_Know_HL.f','Soc_Discuss_HL.f','Soc_Hearing_test.f',
                'Soc_Obtain_HA.f','Soc_Sometimes_use.f','Soc_Regular_use.f',
                'Soc_Very_positive.f','Soc_Somewhat_positive.f',
                'Soc_Somewhat_negative.f','Soc_Very_negative.f')

# Create function to run NRI procedure for a specified dataset, with specified cutoff
procedureNRI <- function(foldX, foldProb) {
  m.base <- glm(Purchased_HA ~ Age + HHIE_total, family = binomial("logit"), data = foldX)
  fit.base <- predict.glm(m.base, newdata = foldX, type = 'response')
  print(summary(m.base))
  
  for (i in 1:26) {
  new.formula <- as.formula(paste0("Purchased_HA ~ Age + HHIE_total + ", predictors[i]))  
  m.new <- glm(new.formula, family = binomial("logit"), data = foldX)
  fit.new <- predict.glm(m.new, newdata = foldX, type = 'response')
  
  print(predictors[i])
  reclassification(data = foldX, cOutcome = 31, 
                   predrisk1 = fit.base, predrisk2 = fit.new, 
                   cutoff = c(0, foldProb, 1))
  }
}

# Evaluate each added predictor to base model
procedureNRI(ptacomp.f, 0.1978752)

# Performance of just the base model
mbase <- glm(Purchased_HA ~ Age + HHIE_total, data = ptacomp.f, family = "binomial")
summary(mbase)
mbasefit <- predict.glm(mbase, newdata = ptacomp.f, type = 'response')
mbasefit.bi <- ifelse(mbasefit >= 0.1978752, 1, 0)
confusionMatrix(data = as.factor(mbasefit.bi), 
                reference = as.factor(ptacomp.f$Purchased_HA), 
                positive = c("1"))

# Adding significant predictors from NRI procedure to base model
mplus <- glm(Purchased_HA ~ Age + HHIE_total + Sub_Age_avg + HA_stigma_avg, 
             data = ptacomp.f, family = "binomial")
summary(mplus)
mplusfit <- predict.glm(mplus, newdata = ptacomp.f, type = 'response')
mplusfit.bi <- ifelse(mplusfit >= 0.1978752, 1, 0)
confusionMatrix(data = as.factor(mplusfit.bi), 
                reference = as.factor(ptacomp.f$Purchased_HA), 
                positive = c("1"))

#####


### Classification tree
#####

# Dataset ptacomp.f = PTA-BE > 25, complete data for 28 predictors, using factor version of binary variables

# Libraries
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)

# Standard formula
formula.28x <- formula(HA.Purchase ~ 
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

# Full tree
mcomp <- rpart(formula.28x, data = ptacomp.f, weights = ptacomp.f$weight, 
               method = "class", parms = list(split = "gini"), control = rpart.control(cp = 0))

# View CP table, with added info on which CP value leads to error within 1 SD of lowest error
cptab <- data.frame(mcomp$cptable)
cptab$sum <- cptab$xerror + cptab$xstd
cptab$candidate <- ifelse(cptab$xerror <= (min(cptab$xerror) + cptab$xstd[cptab$xerror == min(cptab$xerror)]), TRUE, FALSE)
print(cptab)

predtree <- predict(mcomp, newdata = ptacomp.f, type = "class")
confusionMatrix(data = as.factor(predtree), 
                reference = as.factor(ptacomp.f$HA.Purchase), 
                positive = c("Yes"))

plotcp(mcomp, upper = c("splits"))

# Plot full tree
fancyRpartPlot(mcomp, 
               main = "", sub = "Complete data (n=753), CP = 0", 
               palette = "Reds", type = 4, tweak=1)

# Tree with depth = 2
mcomp.d2 <- rpart(formula.28x, data = ptacomp.f, weights = ptacomp.f$weight, 
               method = "class", parms = list(split = "gini"),
               control = rpart.control(maxdepth = 2))

predtree <- predict(mcomp.d2, newdata = ptacomp.f, type = "class")
confusionMatrix(data = as.factor(predtree), 
                reference = as.factor(ptacomp.f$HA.Purchase), 
                positive = c("Yes"))

fancyRpartPlot(mcomp.d2, 
               main = "", sub = "Complete data (n=753), depth = 2", 
               palette = "Reds", type = 4, tweak=1)

# Tree with depth = 3
mcomp.d3 <- rpart(formula.28x, data = ptacomp.f, weights = ptacomp.f$weight, 
               method = "class", parms = list(split = "gini"),
               control = rpart.control(maxdepth = 3))

predtree <- predict(mcomp.d3, newdata = ptacomp.f, type = "class")
confusionMatrix(data = as.factor(predtree), 
                reference = as.factor(ptacomp.f$HA.Purchase), 
                positive = c("Yes"))

fancyRpartPlot(mcomp.d3, 
               main = "", sub = "Complete data (n=753), depth = 3", 
               palette = "Reds", type = 4, tweak=1)

# Tree with depth = 4
mcomp.d4 <- rpart(formula.28x, data = ptacomp.f, weights = ptacomp.f$weight, 
               method = "class", parms = list(split = "gini"),
               control = rpart.control(maxdepth = 4))

predtree <- predict(mcomp.d4, newdata = ptacomp.f, type = "class")
confusionMatrix(data = as.factor(predtree), 
                reference = as.factor(ptacomp.f$HA.Purchase), 
                positive = c("Yes"))

fancyRpartPlot(mcomp.d4, 
               main = "", sub = "Complete data (n=753), depth = 4", 
               palette = "Reds", type = 4, tweak=1)

#####

