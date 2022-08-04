
### 1. Prep dataset for analysis
#####

# Read raw data
orig <- read.csv("Connect_Data_20191025.csv", header=TRUE)

# Must have known age and sex, 50+ years old, never used a HA
data <- orig[which(is.na(orig$Q44) == FALSE & is.na(orig$Q43) == FALSE & orig$Q44 > 49.5 & orig$Q42 == "No"), ]

# Give some variables more user-friendly names, and recode other variables
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

# Make 0/1 variables into factors, for labels in classification tree
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

# New: relabel individual items in scales
data$SubAge_1 <- data$Q1_Corrected
data$SubAge_2 <- data$Q2_Corrected 
data$SubAge_3 <- data$Q3_Corrected

data$AgeStigma_1 <- data$Q4_Corrected 
data$AgeStigma_2 <- data$Q5_Corrected 
data$AgeStigma_3 <- data$Q6_Corrected 
data$AgeStigma_4 <- data$Q7_Corrected
data$AgeStigma_5 <- data$Q8_Corrected

data$HaStigma_1 <- data$Q9_Corrected 
data$HaStigma_2 <- data$Q10_Corrected 
data$HaStigma_3 <- data$Q11_Corrected 
data$HaStigma_4 <- data$Q12_Corrected

#####

### 2. Participant descriptives
#####

# Those with PTA-BE > 25 and complete data, n=753
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
  is.na(data$SubAge_1)==F &  
    is.na(data$SubAge_2)==F & 
    is.na(data$SubAge_3)==F & 
  is.na(data$AgeStigma_1)==F &
    is.na(data$AgeStigma_2)==F &
    is.na(data$AgeStigma_3)==F &
    is.na(data$AgeStigma_4)==F &
    is.na(data$AgeStigma_5)==F &
  is.na(data$HaStigma_1)==F & 
    is.na(data$HaStigma_2)==F &
    is.na(data$HaStigma_3)==F &
    is.na(data$HaStigma_4)==F &
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

# list of variables of interest
varlist <- c("ID", 
             "Age", "PTA4_better_ear", "HHIE_total", "Ability", "Sex", "Edu", "Married",
             "Health", "QoL", "Help_neighbours", "Help_problems", "Concern", "Lonely",
             "SubAge_1", "SubAge_2", "SubAge_3", "AgeStigma_1", "AgeStigma_2", "AgeStigma_3", 
             "AgeStigma_4", "AgeStigma_5", "HaStigma_1", "HaStigma_2", "HaStigma_3", "HaStigma_4", 
             "Accomp", "Soc_Suspect_HL", "Soc_Know_HL", "Soc_Discuss_HL", "Soc_Hearing_test",
             "Soc_Obtain_HA", "Soc_Sometimes_use", "Soc_Regular_use", "Soc_Very_positive",
             "Soc_Somewhat_positive", "Soc_Somewhat_negative", "Soc_Very_negative",
             "group")

# All who qualified because PTA-BE > 25, regardless of missing data, n=1396
all_pta <- subset(data, PTA4_better_ear > 25)

# Make a new "group" variable
all_pta$group <- ifelse(all_pta$ID %in% pta$ID, "Included", "Excluded")

# Delete ID variable; keep group variable
all_pta <- subset(all_pta, select = c(varlist[-1]))  

# Get percent missing data in n=643 excluded, vs n=753 included
library(dplyr)
library(tidyr)

all_pta %>% 
  group_by(group) %>% 
  summarise_all( ~ sum(is.na(.)) / 643 * 100) %>% 
  pivot_longer(cols = -1) %>%
  pivot_wider(names_from=group, values_from=value) %>% 
  print(n = 37)

# Compare characteristics of subsamples with complete vs missing data
pta_miss <- subset(all_pta,  group=="Excluded")
pta_comp <- subset(all_pta,  group=="Included")

# Continuous variables only
cont_list <- c("Age", "PTA4_better_ear", "HHIE_total", "Ability",
               "Edu", "Health", "QoL", "Help_neighbours", "Help_problems", "Concern", "Lonely", 
               "SubAge_1", "SubAge_2", "SubAge_3", "AgeStigma_1", "AgeStigma_2", "AgeStigma_3", 
               "AgeStigma_4", "AgeStigma_5", "HaStigma_1", "HaStigma_2", "HaStigma_3", "HaStigma_4")  

# Make a dataframe to receive stats values, including effect size
output_cont <- data.frame(Variable=as.character(), 
                     Mean_Excl=as.numeric(),
                     SD_Excl=as.numeric(),
                     Mean_Incl=as.numeric(), 
                     SD_Incl=as.numeric(), 
                     t=as.numeric(), 
                     p=as.numeric(), 
                     cohens_d=as.numeric(), stringsAsFactors=FALSE)

# Fill dataframe
for (i in 1:23) {
    ttest.data <- t.test(pta_miss[, cont_list[i]], pta_comp[, cont_list[i]], 
                         alternative=c("two.sided"), paired=FALSE)
    mean_miss <- mean(pta_miss[, cont_list[i]], na.rm=TRUE)
    var_miss <- var(pta_miss[, cont_list[i]], na.rm=TRUE)
    n_miss <- sum(is.na(pta_miss[, cont_list[i]])==FALSE)
    mean_comp <- mean(pta_comp[, cont_list[i]])
    var_comp <- var(pta_comp[, cont_list[i]])  
    pooled_var <- ( (n_miss-1)*var_miss + (753-1)*var_comp ) / (n_miss + 753 - 2)
    
    output_cont[i,1] <- cont_list[i]
    output_cont[i,2] <- round( ttest.data$estimate[1], 2)
    output_cont[i,3] <- round( sd(pta_miss[, cont_list[i]], na.rm=TRUE), 2)
    output_cont[i,4] <- round( ttest.data$estimate[2], 2)
    output_cont[i,5] <- round( sd(pta_comp[, cont_list[i]] ), 2)
    output_cont[i,6] <- round( ttest.data$statistic, 2)
    output_cont[i,7] <- round( ttest.data$p.value, 4)
    output_cont[i,8] <- round( (mean_miss - mean_comp)/(pooled_var^0.5), 3)
}

print(data.frame(output_cont))

# Binary variables only
cat_list <- c("Sex", "Married", "Accomp", "Soc_Suspect_HL", "Soc_Know_HL", "Soc_Discuss_HL", 
              "Soc_Hearing_test", "Soc_Obtain_HA", "Soc_Sometimes_use", "Soc_Regular_use",
              "Soc_Very_positive", "Soc_Somewhat_positive", "Soc_Somewhat_negative", "Soc_Very_negative")   

pta_cat <- subset( all_pta, select = c(cat_list, "group") )

# Make a dataframe to receive stats values, including effect size
output_cat <- data.frame(Variable=as.character(), 
                     Proportion_Excl=as.numeric(),
                     Proportion_Incl=as.numeric(), 
                     chisq=as.numeric(), 
                     p_value=as.numeric(), 
                     cohens_h = as.numeric(), stringsAsFactors=FALSE)

# Fill dataframe
for (i in 1:14) {
  count_1_miss <- sum(pta_miss[, cat_list[i]], na.rm=TRUE)
  base_miss <- sum(is.na(pta_miss[, cat_list[i]])==FALSE)
  count_1_comp <- sum(pta_comp[, cat_list[i]])
    ptest.data <- prop.test(x = c(count_1_miss, count_1_comp), n = c(base_miss, 753))
    output_cat[i,1] <- cat_list[i]
    output_cat[i,2] <- round( ptest.data$estimate[1], 3)
    output_cat[i,3] <- round( ptest.data$estimate[2], 3)
    output_cat[i,4] <- round( ptest.data$statistic, 3)
    output_cat[i,5] <- round( ptest.data$p.value, 4)
    output_cat[i,6] <- round( 2*asin((ptest.data$estimate[1])^0.5) - 2*asin((ptest.data$estimate[2])^0.5), 3)
}

print(data.frame(output_cat))

#####

### 3. Correlation matrices & Cronbach's alpha
#####

# As requested by reviewer, a larger sample for calculating correlations in subjective age and stigma scales
# Participants who are 50+, never used HA, PTA BE > 25, all scale items present, but may have missing data in other variables of interest
# n=1309

orig$SubAge_1 <- orig$Q1_Corrected
orig$SubAge_2 <- orig$Q2_Corrected 
orig$SubAge_3 <- orig$Q3_Corrected

orig$AgeStigma_1 <- orig$Q4_Corrected 
orig$AgeStigma_2 <- orig$Q5_Corrected 
orig$AgeStigma_3 <- orig$Q6_Corrected 
orig$AgeStigma_4 <- orig$Q7_Corrected
orig$AgeStigma_5 <- orig$Q8_Corrected

orig$HaStigma_1 <- orig$Q9_Corrected 
orig$HaStigma_2 <- orig$Q10_Corrected 
orig$HaStigma_3 <- orig$Q11_Corrected 
orig$HaStigma_4 <- orig$Q12_Corrected

tempdata <- subset( orig, 
                   is.na(orig$Q44)==F & orig$Q44 > 49.5 & orig$Q42=="No" & 
                     is.na(orig$PTA4_better_ear)==F & orig$PTA4_better_ear > 25 & 
                   is.na(orig$SubAge_1)==F & 
                     is.na(orig$SubAge_2)==F & 
                     is.na(orig$SubAge_3)==F & 
                     is.na(orig$AgeStigma_1)==F &
                     is.na(orig$AgeStigma_2)==F &
                     is.na(orig$AgeStigma_3)==F &
                     is.na(orig$AgeStigma_4)==F &
                     is.na(orig$AgeStigma_5)==F &
                     is.na(orig$HaStigma_1)==F & 
                     is.na(orig$HaStigma_2)==F &
                     is.na(orig$HaStigma_3)==F &
                     is.na(orig$HaStigma_4)==F )

sub_age_temp <- subset(tempdata, select = c("SubAge_1", "SubAge_2", "SubAge_3"))
age_stigma_temp <- subset(tempdata, select = c("AgeStigma_1", "AgeStigma_2", "AgeStigma_3", "AgeStigma_4", "AgeStigma_5"))
ha_stigma_temp <- subset(tempdata, select = c("HaStigma_1", "HaStigma_2", "HaStigma_3", "HaStigma_4"))

# The smaller n=753 sample, a subset of the above who have complete data on other variables of interest
sub_age <- subset(pta, select = c("SubAge_1", "SubAge_2", "SubAge_3"))
age_stigma <- subset(pta, select = c("AgeStigma_1", "AgeStigma_2", "AgeStigma_3", "AgeStigma_4", "AgeStigma_5"))
ha_stigma <- subset(pta, select = c("HaStigma_1", "HaStigma_2", "HaStigma_3", "HaStigma_4"))

library(psych)

# Subjective age correlation matrices, n=1309 and n=753
lowerMat(cor(sub_age_temp, use="complete.obs", method="pearson"), digits = 2)
lowerMat(cor(sub_age, use="complete.obs", method="pearson"), digits = 2)

# Age stigma correlation matrices, n=1309 and n=753
lowerMat(cor(age_stigma_temp, use="complete.obs", method="pearson"), digits = 2)
lowerMat(cor(age_stigma, use="complete.obs", method="pearson"), digits = 2)

# Hearing aid stigma correlation matrices, n=1309 and n=753
lowerMat(cor(ha_stigma_temp, use="complete.obs", method="pearson"), digits = 2)
lowerMat(cor(ha_stigma, use="complete.obs", method="pearson"), digits = 2)

# Cronbach's alpha for n=753
cronbach_sub <- alpha(sub_age, check.keys = FALSE, n.iter = 500)
print(cronbach_sub)

cronbach_age_st <- alpha(age_stigma, check.keys = FALSE, n.iter = 500)
print(cronbach_age_st)

cronbach_ha_st <- alpha(ha_stigma, check.keys = FALSE, n.iter = 500)
print(cronbach_ha_st)

# Plot example scatterplot Age Stigma 4 & 5, vs 1 & 5
par(mfrow=c(1,2))
set.seed(100)
plot(jitter(age_stigma[,4]), jitter(age_stigma[,5]), xlab="Item 4", ylab="Item 5")
plot(jitter(age_stigma[,1]), jitter(age_stigma[,5]), xlab="Item 1", ylab="Item 5")

# Plot histograms of responses for Age Stigma items
library(ggplot2)
library(gridExtra)

as1 <- ggplot(age_stigma, aes(AgeStigma_1)) + 
  geom_histogram(binwidth = 1.0, fill="white", colour="black") + 
  scale_y_continuous(name="Count of responses", limits=c(0, 400)) + 
  theme(axis.title = element_text(size=16, color="black"), axis.text = element_text(size=12, color="black"))

as2 <- ggplot(age_stigma, aes(AgeStigma_2)) + 
  geom_histogram(binwidth = 1.0, fill="white", colour="black") + 
  scale_y_continuous(name="Count of responses", limits=c(0, 400)) + 
  theme(axis.title = element_text(size=16, color="black"), axis.text = element_text(size=12, color="black"))

as3 <- ggplot(age_stigma, aes(AgeStigma_3)) + 
  geom_histogram(binwidth = 1.0, fill="white", colour="black") + 
  scale_y_continuous(name="Count of responses", limits=c(0, 400)) + 
  theme(axis.title = element_text(size=16, color="black"), axis.text = element_text(size=12, color="black"))

as4 <- ggplot(age_stigma, aes(AgeStigma_4)) + 
  geom_histogram(binwidth = 1.0, fill="white", colour="black") + 
  scale_y_continuous(name="Count of responses", limits=c(0, 400)) + 
  theme(axis.title = element_text(size=16, color="black"), axis.text = element_text(size=12, color="black"))

as5 <- ggplot(age_stigma, aes(AgeStigma_5)) + 
  geom_histogram(binwidth = 1.0, fill="white", colour="black") + 
  scale_y_continuous(name="Count of responses", limits=c(0, 400)) + 
  theme(axis.title = element_text(size=16, color="black"), axis.text = element_text(size=12, color="black"))

grid.arrange(as1, as2, as3, as4, as5, ncol=3, nrow=2)


#####

### 4. Duration of follow-up
#####

# Format dates in dataset
pta$date_firstvisit <- as.Date(pta$Date.1st.Visit.Clinic, format = "%m %d %Y")
pta$date_recentvisit <- as.Date(pta$Date.Most.Recent.Visit.Clinic, format = "%m %d %Y")
pta$date_recenteval <- as.Date(pta$Date.Most.Recent.Hearing.Eval, format = "%m %d %Y")
pta$date_purchase <- as.Date(pta$Date.HA.Purchase, format = "%m %d %Y")

# Make a temporary dataset just for date info
temp <- data.frame(first_visit = c(rep(0, 753)), 
                   most_recent_hearing = c(rep(0, 753)),
                   most_recent_visit = c(rep(0, 753)), 
                   HA_purchase = c(rep(0, 753)))

# To plot order of four dates for individual participants, subtract date of each visit from study start date;
# Earliest date in whole dataset is "2018-01-17", so take arbitrary start date of "2018-01-15"
temp$first_visit <- as.numeric(pta$date_firstvisit - as.Date("2018-01-15"))
temp$most_recent_hearing <- as.numeric(pta$date_recenteval - as.Date("2018-01-15"))
temp$most_recent_visit <- as.numeric(pta$date_recentvisit - as.Date("2018-01-15"))
temp$HA_purchase <- as.numeric(pta$date_purchase - as.Date("2018-01-15"))

par(mar=c(6, 4, 2, 1))
plot(x = c(1:4), y = temp[1, ], xaxt='n', yaxt='n', ann=FALSE, xlim=c(0.5, 4.5), ylim=c(0,700), 
     type='l', lwd=0.2) 
  par(new=TRUE)
for (i in 2:752) {
 plot(x = c(1:4), y = temp[i, ], xaxt='n', yaxt='n', ann=FALSE, xlim=c(0.5, 4.5), ylim=c(0,700), 
      type='l', lwd=0.2) 
  par(new=TRUE)
}
par(new=TRUE)
plot(x = c(1:4), y = temp[753, ], xaxt='n', xlim=c(0.5, 4.5), ylim=c(0,700), type='l', lwd=0.2, 
     xlab = "", ylab = "Time (Days)", main="Visits of N=753")
axis(1, at = c(1:4), las=3, 
     labels = c("First visit", "Most recent hearing", "Most recent visit", "HA purchase"))

# Get the earliest of three dates, as "start" of study for that participant
newtib <- pta %>% 
  rowwise %>% 
  mutate(earliest_visit = min(date_firstvisit, date_recentvisit, date_recenteval, na.rm=TRUE))
pta$earliest_visit <- newtib$earliest_visit

# Number of days each person was in the study = study end date - first visit date
# Final spreadsheet from study coordinator was dated July 30, 2019, so set this as study end date
pta$days_in_study <- as.Date("2019-07-30", format = '%Y-%m-%d') - pta$earliest_visit

# Number of days to purchase HA, from start of study for each participant
pta$days_to_purchase <- pta$date_purchase - pta$earliest_visit

# 5 cases who purchased, but missing first visit date, so days in study & to purchase were messed up;
# set to NA
pta[which(pta$days_to_purchase < 0), ]$days_in_study <- NA
pta[which(pta$days_to_purchase < 0), ]$days_to_purchase <- NA

# Histogram of how long participants were tracked
hist(as.numeric(pta$days_in_study), 
     xlab="Days in study",
     ylab="Count",
     main="", breaks = 20, ylim=c(0,80), xlim=c(0,600))

# Cumulative plot of n=149 who purchased HA; how many days people took to purchase
yes <- subset(pta, HA.Purchase == "Yes", select = c('days_to_purchase'))
yes2 <- yes[order(yes$days_to_purchase, decreasing=FALSE), ]
yes2 <- data.frame(yes2[-c(145:149)])  # drop NAs
colnames(yes2) <- c("days_to_purchase")
yes2$cumulative_percent <- seq(1, 144, by=1)/144*100

plot(yes2$days_to_purchase, yes2$cumulative_percent, type='l',
     xlim=c(0,350), ylim=c(0,100), 
     xlab="Days to purchase", ylab="Cumulative % of those who purchased")

# Draw lines on cumulative plot; interpolation for 90 days: (81.94-81.25)/6*3+81.25
abline(h = 81.595, col='red', lty='dotted')
abline(v = 90, col='red', lty='dotted')

# Sorting participants into 90-day bins depending on how long each was in the study
pta$days_bin <- NA
pta$days_bin[pta$days_in_study <= 90] <- 1
pta$days_bin[pta$days_in_study > 90 & pta$days_in_study <= 180] <- 2
pta$days_bin[pta$days_in_study > 180 & pta$days_in_study <= 270] <- 3
pta$days_bin[pta$days_in_study > 270 & pta$days_in_study <= 360] <- 4
pta$days_bin[pta$days_in_study > 360 & pta$days_in_study <= 450] <- 5
pta$days_bin[pta$days_in_study > 450 & pta$days_in_study <= 540] <- 6
pta$days_bin[pta$days_in_study > 540] <- 7

# Table of how many in each 90-day bin
table(pta$HA.Purchase, pta$days_bin, dnn = c("Purchased", "90-day bins"))
prop.table(table(pta$HA.Purchase, pta$days_bin, dnn = c("Purchased", "90-day bins")),
           margin=2)

# Test of independence; drop first and last bins because too few; recall there were 5 missing, so n=753-12=741
# X-squared = 1.6791, df = 4, p-value = 0.7945
pta_bins <- subset(pta, days_bin > 1 & days_bin < 7)
t <- table(pta_bins$HA.Purchase, pta_bins$days_bin)
chisq.test(t, correct=FALSE)

#####

### 5. Logistic regression & sample size
#####
library(caret)
library(pROC)

# Standard formula, using numeric variables instead of factors
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

# Variables of interest
varlist <- c("Age", "PTA4_better_ear", "HHIE_total", "Ability", "Sex", "Edu", "Married",
             "Health", "QoL", "Help_neighbours", "Help_problems", "Concern", "Lonely",
             "Sub_Age_avg", "Age_stigma_avg", "HA_stigma_avg",  
             "Accomp", "Soc_Suspect_HL", "Soc_Know_HL", "Soc_Discuss_HL", "Soc_Hearing_test",
             "Soc_Obtain_HA", "Soc_Sometimes_use", "Soc_Regular_use", "Soc_Very_positive",
             "Soc_Somewhat_positive", "Soc_Somewhat_negative", "Soc_Very_negative")

# Model with all variables
mfull <- glm(formula28, data = pta, family = binomial("logit"))
summary(mfull)

# Get predicted outcomes; note that min = 0.02414, max = 0.66981
mfull_pred <- predict.glm(mfull, newdata = data, type = "response")

# Create a range of decision threshold values to test
dec_thresholds_full <- c(seq(0.03, 0.19, by=0.01), 0.1978752, seq(0.21, 0.6, by=0.01))

# Create empty dataframe to fill
mfull_roc_df <- data.frame(threshold = rep(NA, length(dec_thresholds_full)), 
                           accuracy = rep(NA, length(dec_thresholds_full)), 
                           sensitivity = rep(NA, length(dec_thresholds_full)), 
                           specificity = rep(NA, length(dec_thresholds_full)) )

# Calculate model results for different decision thresholds
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

# Select different thresholds for labelling in plot
dec_labels = data.frame(thres = c(0.05, 0.1, 0.15, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
                        sens = c(0.98657718, 0.93959732, 0.77852349, 0.48322148, 0.40268456, 
                                 0.26174497, 0.18791946, 0.10738255, 0.05369128),
                        spec = c(0.041390728, 0.230132450, 0.475165563, 0.801324503, 0.852649007, 
                                 0.913907285, 0.965231788, 0.978476821, 0.983443709))
# Plot ROC curve
plot(1-mfull_roc_df$specificity, mfull_roc_df$sensitivity, type='l', 
     ylab="Sensitivity", xlab="False positives", ylim=c(0,1), xlim=c(0,1), 
     main="ROC curve with different decision thresholds")
# Add diagonal line to plot
y = c(0,1)
x = c(0,1)
abline(lm(y ~ x), col="grey80")
# Add some threshold labels to plot
text(x = (1-dec_labels$spec), y = dec_labels$sens, labels = dec_labels$thres, col = "black")
text(x = (1-0.655629139), y = 0.63758389, labels = c("0.1978"), col = "red")

# Checking for multicollinearity 

library(car)

df_full <- data.frame(vif(mod = mfull))
colnames(df_full) <- "Variance_inflation_factor"
print(df_full)

# Checking for high-influence datapoints

# leverage; only 4 values exceed 3x the mean value
hat <- as.vector(hatvalues(mfull))

# Cook's distance
cook <- as.vector(cooks.distance(mfull))

# See relationship of leverage and influence
df <- data.frame(hat, cook)
df$large_cook <- ifelse(df$cook > 0.012, "red", "grey80")
plot(df$hat, df$cook, ylim=c(0, 0.02), xlim=c(0, 0.15), 
     col = df$large_cook, ylab="Influence", xlab="Leverage")

# Estimates of model with and without high-influence observations
ptacomp_temp <- pta[-which(df$cook > 0.012), ]
mfull_temp <- glm(formula28, data = ptacomp_temp, family = binomial("logit"))

# Compare model coefficient values with and without high-influence observations
b_mfull <- as.vector(mfull$coefficients)
b_mfull_temp <- as.vector(mfull_temp$coefficients)

plot(b_mfull_temp, b_mfull, xlim=c(-6,2), ylim=c(-6,2), 
     xlab="Without high-influence observations", ylab="With all observations", 
     main="Comparing model coefficients")

# Backwards elimination procedure using AIC

mstep <- step(object = mfull, direction = c("backward"), trace = 0)
summary(mstep)

# odds ratios
round(exp(cbind(OR = coef(mstep), confint(mstep))), 3)

# Get predicted values for backwards step model
mstep_pred <- predict.glm(mstep, newdata = data, type = 'response')

# Calculate results for different decision thresholds; min = 0.04135, max = 0.73837
dec_thresholds_step <- c(seq(0.05, 0.19, by=0.01), 0.1978752, seq(0.21, 0.7, by=0.01))

mstep_roc_df <- data.frame(threshold = rep(NA, length(dec_thresholds_step)), 
                           accuracy = rep(NA, length(dec_thresholds_step)), 
                           sensitivity = rep(NA, length(dec_thresholds_step)), 
                           specificity = rep(NA, length(dec_thresholds_step)) )

for (i in 1:length(dec_thresholds_step)) {
mstep_pred_bi <- ifelse(mstep_pred >= dec_thresholds_step[i], 1, 0)
confusion_output <- confusionMatrix(data = as.factor(mstep_pred_bi), 
                                    reference = as.factor(ptacomp.n$Purchased_HA), 
                                    positive = c("1"))
mstep_roc_df$threshold[i] <- dec_thresholds_step[i]
mstep_roc_df$accuracy[i] <- confusion_output$overall[1]
mstep_roc_df$sensitivity[i] <- confusion_output$byClass[1]
mstep_roc_df$specificity[i] <- confusion_output$byClass[2]
}

# Add Full vs Backwards Step ROC curves to same plot
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

# Alternative to stepwise selection: Lasso regression

library(glmnet)

set.seed(1331)
mcv_unweighted <- cv.glmnet(x = as.matrix(pta[varlist]), 
                            y = pta$Purchased_HA, 
                            weights = NULL,
                            nfolds = 5, 
                            family = "binomial", 
                            type.measure = "auc",
                            alpha = 1, nlambda = 100)

# Plot AUC at different lambda values, with SD
# Dotted red line lambda = 0.0274954 gives max AUC 0.6444621
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

# A model with lambda=0.0274954 explains 3.71% variance
mcv_unweighted$glmnet.fit 

# Number of non-zero coefficients at each lambda
plot(x = mcv_unweighted$lambda, y = mcv_unweighted$nzero, 
     xlim = c(0, 0.06), ylim = c(0, 30), 
     pch=1, col = 'black', xlab="Lambda", ylab="Number of non-zero coefficients")
abline(v = 0.0274954, col='red', lty='dotted')

# The four non-zero coefficients
coef(mlasso, s = 0.0274954)

# Refitting an ordinary logistic regression model with these 4 variables
mlasso2 <- glm(Purchased_HA ~ Age + HHIE_total + HA_stigma_avg + Soc_Suspect_HL, 
               family = "binomial", data = pta)
summary(mlasso2)
exp(cbind(OR = coef(mlasso2), confint(mlasso2)))

mlasso2_pred <- predict(mlasso2, newdata = pta[varlist], type = "response")

# Calculate model metrics for different decision thresholds; min = 0.03594, max = 0.66708
dec_thresholds_lasso <- c(seq(0.04, 0.19, by=0.01), 0.1978752, seq(0.21, 0.6, by=0.01))

mlasso2_roc_df <- data.frame(threshold = rep(NA, length(dec_thresholds_lasso)), 
                           accuracy = rep(NA, length(dec_thresholds_lasso)), 
                           sensitivity = rep(NA, length(dec_thresholds_lasso)), 
                           specificity = rep(NA, length(dec_thresholds_lasso)) )

for (i in 1:length(dec_thresholds_lasso)) {
mlasso2_pred_bi <- ifelse(mlasso2_pred >= dec_thresholds_lasso[i], 1, 0)
confusion_output <- confusionMatrix(data = as.factor(mlasso2_pred_bi), 
                                    reference = as.factor(ptacomp.n$Purchased_HA), 
                                    positive = c("1"))
mlasso2_roc_df$threshold[i] <- dec_thresholds_lasso[i]
mlasso2_roc_df$accuracy[i] <- confusion_output$overall[1]
mlasso2_roc_df$sensitivity[i] <- confusion_output$byClass[1]
mlasso2_roc_df$specificity[i] <- confusion_output$byClass[2]
}

# Plot ROC curves for Full, Step, Lasso-based models
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

# Power analysis

# Calculate VIF to use as adjustment for single-covariate sample size
# If VIF = 1/(1-R-squared), R-squared = 1 - 1/VIF

library(car)
df_mlasso2 <- data.frame(vif(mod = mlasso2))
colnames(df_mlasso2) <- "VIF"
df_mlasso2$R2 <- 1 - 1/df_mlasso2$VIF
print(df_mlasso2)

# Eq. 1 from Hsieh et al (1998) "A Simple Method of Sample Size Calculation for Linear and Logistic Regression"

# Continuous predictor, Age

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

# Continuous predictor, HHIE_total

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

# Continuous predictor, HA_stigma_avg

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

# Using G*Power
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

#####

### 6. Classification tree
#####
library(rpart)
library(rpart.plot)
library(rattle)
library(ggplot2)

# Standard formula, with outcome as string
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

# Add weight variable to dataset
pta$weight <- NA
pta$weight[pta$Purchased_HA == 0] <- 0.2466887
pta$weight[pta$Purchased_HA == 1] <- 1

# Trying different CP values
set.seed(1331)
mc <- train(formula.28x, data = ptacomp.f, weights = ptacomp.f$weight,
      method = "rpart",
      tuneGrid = data.frame(cp = c(0, 0.001, 0.005, 0.01, 0.011, 0.012, 0.013, 0.015, 0.02, 0.025, 0.03, 0.04, 0.05, 0.1)),
      metric = "Accuracy", 
      control = rpart.control(split="gini", minsplit = 20),
      trControl = trainControl(method="repeatedcv", number = 5, repeats = 10))

# Plot model accuracy vs CP values
means <- mc$results$Accuracy
bar.up <- means + mc$results$AccuracySD/(5^0.5)
bar.down <- means - mc$results$AccuracySD/(5^0.5)
plot(x = mc$results$cp,
     xlab = "less complex - CP - more complex",
     y = means, 
     ylab = "Mean accuracy (with SE)", 
     type='b', pch=16, xlim=rev(c(0, 0.1)), ylim=c(0.5, 0.65) )
arrows(x0 = mc$results$cp, x1 = mc$results$cp, y0 = bar.down, y1 = bar.up, code = 3, length=0.05, angle=90, col="black")

# Plot the three-split tree
m_cp02 <- rpart(formula.28x, data = ptacomp.f, weights = ptacomp.f$weight, 
               method = "class", parms = list(split = "gini"), 
               control = rpart.control(cp = 0.02))

rpart.plot(m_cp02, box.palette=0, type = 4, extra = 101, under=TRUE, fallen.leaves=TRUE, 
           varlen=0, faclen=0)

# Evaluating sensitivity and specificity (AUC)

# Numbers for table; models for plotting ROC curves
# tree with cp = 0.1; 1 split
m_cp1n <- rpart(formula.28n, data = pta, weights = pta$weight, 
               method = "class", parms = list(split = "gini"), 
               control = rpart.control(cp = 0.1))
pred_cp1n <- predict(m_cp1n, newdata = pta, type="class") 
pred_cp1nn <- as.numeric(as.character(pred_cp1n)) 
#          Reference
#Prediction   0   1
#         0 420  79
#         1 184  70
# Accuracy : 0.6507          
# Sensitivity : 0.46980         
# Specificity : 0.69536 

# tree with cp = 0.05; 3 splits
#m_cp05n <- rpart(formula.28n, data = pta, weights = pta$weight, 
#               method = "class", parms = list(split = "gini"), 
#               control = rpart.control(cp = 0.05))
#pred_cp05n <- predict(m_cp05n, newdata = pta, type="class") 
#pred_cp05nn <- as.numeric(as.character(pred_cp05n)) 
#         Reference
#Prediction   0   1
#         0 418  61
#         1 186  88
#Accuracy : 0.672
#Sensitivity : 0.5906
#Specificity : 0.6921

# tree with cp = 0.03; 3 splits
#m_cp03n <- rpart(formula.28n, data = pta, weights = pta$weight, 
#               method = "class", parms = list(split = "gini"), 
#               control = rpart.control(cp = 0.03))
#pred_cp03n <- predict(m_cp03n, newdata = pta, type="class") 
#pred_cp03nn <- as.numeric(as.character(pred_cp03n)) 
#         Reference
#Prediction   0   1
#         0 418  61
#         1 186  88
#Accuracy : 0.672 
#Sensitivity : 0.5906
#Specificity : 0.6921

# tree with cp = 0.025; 3 splits
#m_cp025n <- rpart(formula.28n, data = pta, weights = pta$weight, 
#               method = "class", parms = list(split = "gini"), 
#               control = rpart.control(cp = 0.025))
#pred_cp025n <- predict(m_cp025n, newdata = pta, type="class") 
#pred_cp025nn <- as.numeric(as.character(pred_cp025n)) 
#           Reference
#Prediction  No Yes
#       No  418  61
#       Yes 186  88
#Accuracy : 0.672
#Sensitivity : 0.5906          
#Specificity : 0.6921

# tree with cp = 0.02; 3 splits
m_cp02n <- rpart(formula.28n, data = pta, weights = pta$weight, 
               method = "class", parms = list(split = "gini"), 
               control = rpart.control(cp = 0.02))
pred_cp02n <- predict(m_cp02n, newdata = pta, type="class") 
pred_cp02nn <- as.numeric(as.character(pred_cp02n)) 
#         Reference
#Prediction   0   1
#         0 418  61
#         1 186  88
#               Accuracy : 0.672           
#            Sensitivity : 0.5906          
#            Specificity : 0.6921 
            
# tree with cp = 0.015; 5 splits
m_cp015n <- rpart(formula.28n, data = pta, weights = pta$weight, 
               method = "class", parms = list(split = "gini"), 
               control = rpart.control(cp = 0.015))
summary(m_cp015n)
pred_cp015n <- predict(m_cp015n, newdata = pta, type="class") 
pred_cp015nn <- as.numeric(as.character(pred_cp015n)) 
#          Reference
#Prediction   0   1
#         0 441  61
#         1 163  88
#               Accuracy : 0.7025         
#            Sensitivity : 0.5906         
#            Specificity : 0.7301 
            
# tree with cp = 0.013; 12 splits
m_cp013n <- rpart(formula.28n, data = pta, weights = pta$weight, 
               method = "class", parms = list(split = "gini"), 
               control = rpart.control(cp = 0.013))
summary(m_cp013n)
pred_cp013n <- predict(m_cp013n, newdata = pta, type="class") 
pred_cp013nn <- as.numeric(as.character(pred_cp013n)) 
#          Reference
#Prediction   0   1
#         0 370  27
#         1 234 122
#              Accuracy : 0.6534          
#            Sensitivity : 0.8188          
#            Specificity : 0.6126 

# tree with cp = 0.012; 12 splits
#m_cp012n <- rpart(formula.28n, data = pta, weights = pta$weight, 
#               method = "class", parms = list(split = "gini"), 
#               control = rpart.control(cp = 0.012))
#summary(m_cp012n)
#pred_cp012n <- predict(m_cp012n, newdata = pta, type="class") 
#pred_cp012nn <- as.numeric(as.character(pred_cp012n)) 
#          Reference
#Prediction   0   1
#         0 370  27
#         1 234 122
#               Accuracy : 0.6534          
#            Sensitivity : 0.8188          
#            Specificity : 0.6126  

# tree with cp = 0.011; 15 splits
#m_cp011n <- rpart(formula.28n, data = pta, weights = pta$weight, 
#               method = "class", parms = list(split = "gini"), 
#               control = rpart.control(cp = 0.011))
#summary(m_cp011n)
#pred_cp011n <- predict(m_cp011n, newdata = pta, type="class") 
#pred_cp011nn <- as.numeric(as.character(pred_cp011n)) 
#          Reference
#Prediction   0   1
#         0 355  18
#         1 249 131
#               Accuracy : 0.6454           
#            Sensitivity : 0.8792          
#            Specificity : 0.5877 

# tree with cp = 0.01; 22 splits
m_cp01n <- rpart(formula.28n, data = pta, weights = pta$weight, 
               method = "class", parms = list(split = "gini"), 
               control = rpart.control(cp = 0.01))
pred_cp01n <- predict(m_cp01n, newdata = pta, type="class")
pred_cp01nn <- as.numeric(as.character(pred_cp01n))
#           Reference
#Prediction   0   1
#         0 413  19
#         1 191 130
#Accuracy : 0.7211  
#Sensitivity : 0.8725          
#Specificity : 0.6838 

# tree with cp = 0.005; 31 splits
#m_cp005n <- rpart(formula.28n, data = pta, weights = pta$weight, 
#               method = "class", parms = list(split = "gini"), 
#               control = rpart.control(cp = 0.005))
#pred_cp005n <- predict(m_cp005n, newdata = pta, type="class")
#pred_cp005nn <- as.numeric(as.character(pred_cp005n))
#          Reference
#Prediction   0   1
#         0 423  11
#         1 181 138
#Accuracy : 0.745 
#Sensitivity : 0.9262          
#Specificity : 0.7003  

# tree with cp = 0; 
m_cp0n <- rpart(formula.28n, data = pta, weights = pta$weight, 
               method = "class", parms = list(split = "gini"), 
               control = rpart.control(cp = 0))
pred_cp0n <- predict(m_cp0n, newdata = pta, type="class") 
pred_cp0nn <- as.numeric(as.character(pred_cp0n)) 
#          Reference
#Prediction   0   1
#         0 452  12
#         1 152 137
#               Accuracy : 0.7822         
#            Sensitivity : 0.9195         
#            Specificity : 0.7483 

# Plot a few ROC curves from different CP values

roc_cp1 <- roc(response = pta$HA.Purchase.n, predictor = pred_cp1nn)
roc_cp02 <- roc(response = pta$HA.Purchase.n, predictor = pred_cp02nn)
roc_cp015 <- roc(response = pta$HA.Purchase.n, predictor = pred_cp015nn)
roc_cp013 <- roc(response = pta$HA.Purchase.n, predictor = pred_cp013nn)
roc_cp01 <- roc(response = pta$HA.Purchase.n, predictor = pred_cp01nn)
roc_cp0 <- roc(response = pta$HA.Purchase.n, predictor = pred_cp0nn)

plot(roc_cp1, type = "n")
lines(roc_cp1, type="l", col="grey80") # 1 split
lines(roc_cp02, type="l", col="black") # 3 splits
lines(roc_cp015, type="l", col="blue") # 5 splits
lines(roc_cp013, type="l", col="red") # 12 splits
lines(roc_cp01, type="l", col="grey60") # 22 splits
lines(roc_cp0, type="l", col="grey80") # 41 splits

text(0.69536, 0.45, pos=4, "1 split", col="grey80", cex=1.2)
text(0.6921, 0.57, pos=4, "3", col="black", cex=1.2)
text(0.77, 0.6, pos=4, "5", col="blue", cex=1.2)
text(0.6126, 0.8, pos=4, "12", col="red", cex=1.2)
text(0.6838, 0.85, pos=4, "22", col="grey60", cex=1.2)
text(0.7483, 0.9, pos=4, "41", col="grey80", cex=1.2)

# Variable importance; note no surrogate variables
set.seed(1331)
m_cp013 <- rpart(formula.28x, data = pta, weights = pta$weight, 
               method = "class", parms = list(split = "gini"), 
               control = rpart.control(cp = 0.013, maxcompete = FALSE, maxsurrogate=0))

# plot variable importance for current 12-split tree
vi <- data.frame(m_cp013$variable.importance)
colnames(vi) <- c("Variable_importance")
vi$number <- seq(1, 9, by = 1)

g1 <- ggplot(data = vi, aes(y = Variable_importance/34.60907*100, x = number)) + 
  geom_col() + 
  coord_flip(xlim=rev(c(1, 9)), ylim=c(0, 30)) + 
  scale_x_continuous(name='', breaks = seq(1, 9, by = 1), labels=rownames(vi)) + 
  scale_y_continuous(name="Variable importance (%)", breaks = seq(0, 30, by=5)) +
  ggtitle("New 12-split tree") + 
  theme_bw() + 
  theme(plot.margin = unit(c(0.25, 0.5, 0.55, 1), "cm")) + 
  theme(axis.title = element_text(size = 20, colour='black'), 
        axis.text = element_text(size = 16, colour='black')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
  theme(plot.margin = unit(c(1, 0.25, 0.25, 0.25), "cm"))

# previous tree of depth=4 in manuscript
m_depth4 <- rpart(formula.28x, data = pta, weights = pta$weight, 
               method = "class", parms = list(split = "gini"), 
               control = rpart.control(split="gini", minsplit = 20, maxdepth = 4, maxcompete = FALSE, maxsurrogate=0))

# plot variable importance, for previous tree of depth=4
vi4 <- data.frame(m_depth4$variable.importance)
colnames(vi4) <- c("Variable_importance")
vi4$number <- c(1:7)

g2 <- ggplot(data = vi4, aes(y = Variable_importance/26.34157*100, x = number)) + 
  geom_col() + 
  coord_flip(xlim=rev(c(1, 7)), ylim=c(0, 30)) + 
  scale_x_continuous(name='', breaks = seq(1, 7, by = 1), labels=rownames(vi4)) + 
  scale_y_continuous(name="Variable importance (%)", breaks = seq(0, 30, by = 5)) + 
  ggtitle("Previous depth4 tree") + 
  theme_bw() + 
  theme(plot.margin = unit(c(0.25, 0.5, 0.55, 1), "cm")) + 
  theme(axis.title = element_text(size = 20, colour='black'), 
        axis.text = element_text(size = 16, colour='black')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
  theme(plot.margin = unit(c(1, 0.25, 0.25, 0.25), "cm"))

# make panels the same width
library(gtable)
library(grid)
grob1 <- ggplotGrob(g1)
grob2 <- ggplotGrob(g2)
g <- rbind(grob1, grob2, size = "first")
g$widths <- unit.pmax(grob1$widths, grob2$widths)
grid.newpage()
grid.draw(g)


#####






