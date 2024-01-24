
# Missing data, internal reliability, follow-up

# Percentage of missing data in Excluded vs Included
#####
# note "Age_stigma_avg" already has one item dropped 
varlist <- c("ID", 
             "Age", "PTA4_better_ear", "HHIE_total", "Ability", "Sex", "Edu", "Married",
             "Health", "QoL", "Help_neighbours", "Help_problems", "Concern", "Lonely",
             "SubAge_1", "SubAge_2", "SubAge_3", "Sub_Age_avg", 
             "AgeStigma_1", "AgeStigma_2", "AgeStigma_3", "AgeStigma_4", "AgeStigma_5", "Age_stigma_avg",
             "HaStigma_1", "HaStigma_2", "HaStigma_3", "HaStigma_4", "HA_stigma_avg", 
             "Accomp", "Soc_Suspect_HL", "Soc_Know_HL", "Soc_Discuss_HL", "Soc_Hearing_test",
             "Soc_Obtain_HA", "Soc_Sometimes_use", "Soc_Regular_use", "Soc_Very_positive",
             "Soc_Somewhat_positive", "Soc_Somewhat_negative", "Soc_Very_negative",
             "HA.Purchase", "group")

# those who qualified, 1396
all_pta <- subset(data, PTA4_better_ear > 25)
all_pta$group <- ifelse(all_pta$ID %in% pta$ID, "Included", "Excluded")

library(dplyr)
library(tidyr)

# percent missing data in n=643, vs n=753
all_pta %>% 
  group_by(group) %>% 
  summarise_all( ~ sum(is.na(.)) / 643 * 100) %>% 
  pivot_longer(cols = -1) %>%
  pivot_wider(names_from=group, values_from=value) %>% 
  print(n = 37)

# number of missing observations per participant, in excluded group n=643
all_pta$num_missing_obs <- apply(is.na(all_pta), MARGIN = 1, FUN = sum)

summary(subset(all_pta, group == "Excluded")$num_missing_obs)

hist(subset(all_pta, group == "Excluded")$num_missing_obs, 
     main="Excluded participants (n=643)", xlab="Number missing observations", ylab="Frequency", 
     xlim=c(0, 20), ylim=c(0, 500), breaks=20)

#####

# Descriptives
#####
# Split participants into Excluded, Included_NoHA, Included_YesHA
all_pta$three_groups <- c(NA)
all_pta$three_groups[all_pta$group == "Excluded"] <- "Excluded"
all_pta$three_groups[all_pta$group == "Included" & all_pta$HA.Purchase == "No"] <- "Included_NoHA"
all_pta$three_groups[all_pta$group == "Included" & all_pta$HA.Purchase == "Yes"] <- "Included_YesHA"

# Table 1: continuous variables
Table1_cont_var <- c("Age", "PTA4_better_ear", "HHIE_total", "Ability",
               "Edu", "Health", "QoL", "Help_neighbours", "Help_problems", "Concern", "Lonely",
               "Sub_Age_avg", "Age_stigma_avg", "HA_stigma_avg")  

library(psych)

temp_cont <- describeBy(all_pta[, Table1_cont_var], group=all_pta$three_groups)  

temp_cont_df <- data.frame(Variable = as.character(rep(NA, 14)), 
                        Mean_Ex = as.numeric(rep(NA, 14)), 
                        SD_Ex = as.numeric(rep(NA, 14)), 
                        Mean_InNo = as.numeric(rep(NA, 14)), 
                        SD_InNo = as.numeric(rep(NA, 14)), 
                        Mean_InYes = as.numeric(rep(NA, 14)), 
                        SD_InYes = as.numeric(rep(NA, 14)), stringsAsFactors=FALSE)  

temp_cont_df$Variable <- Table1_cont_var
temp_cont_df$Mean_Ex <- temp_cont$Excluded$mean
temp_cont_df$SD_Ex <- temp_cont$Excluded$sd
temp_cont_df$Mean_InNo <- temp_cont$Included_NoHA$mean
temp_cont_df$SD_InNo <- temp_cont$Included_NoHA$sd
temp_cont_df$Mean_InYes <- temp_cont$Included_YesHA$mean
temp_cont_df$SD_InYes <- temp_cont$Included_YesHA$sd
  
table1_cont <- cbind(temp_cont_df[,1], round(temp_cont_df[,2:7], 1))
colnames(table1_cont)[1] <- "Variable"

print(table1_cont)

# Table 1: binary variables
Table1_cat_var <- c("Sex", "Married", "Accomp", "Soc_Suspect_HL", "Soc_Know_HL", "Soc_Discuss_HL", 
              "Soc_Hearing_test", "Soc_Obtain_HA", "Soc_Sometimes_use", "Soc_Regular_use",
              "Soc_Very_positive", "Soc_Somewhat_positive", "Soc_Somewhat_negative", "Soc_Very_negative",
              "three_groups")

# get count of 1's in binary variables; troublesome because of NA's in Excluded
cat_sum_wide <- all_pta[, Table1_cat_var] %>% 
  group_by(three_groups) %>% 
  summarise_all(.f = list(sum=sum), na.rm = TRUE)

cat_sum_long <- data.frame(t(cat_sum_wide))
colnames(cat_sum_long) <- c("Excl", "InclNoHA", "InclYesHA")
cat_sum_long <- cat_sum_long[-1, ]
cat_sum_long[, 1] <- as.numeric(cat_sum_long[, 1])
cat_sum_long[, 2] <- as.numeric(cat_sum_long[, 2])
cat_sum_long[, 3] <- as.numeric(cat_sum_long[, 3])

# get counts of non-missing data to use as denominators
cat_notmiss_wide <- data.frame(
  all_pta[, Table1_cat_var] %>% 
  group_by(three_groups) %>% 
  summarise_all( ~ sum(!is.na(.))) )

cat_notmiss_long <- data.frame(t(cat_notmiss_wide))
colnames(cat_notmiss_long) <- c("Excluded", "Included_NoHA", "Included_YesHA")
cat_notmiss_long <- cat_notmiss_long[-1, ]
cat_notmiss_long[,1] <- as.numeric(cat_notmiss_long[,1])
cat_notmiss_long[,2] <- as.numeric(cat_notmiss_long[,2])
cat_notmiss_long[,3] <- as.numeric(cat_notmiss_long[,3])

# divide counts by non-missing cases
Table1_cat <- data.frame(cat_sum_long / cat_notmiss_long)
Table1_cat$Variable <- Table1_cat_var[1:14]
Table1_cat <- Table1_cat[c(4,1,2,3)]
rownames(Table1_cat) <- c(1:14)

print(cbind(Table1_cat[1], round(Table1_cat[,2:4], 2)))
#####

# Included vs Excluded; Continuous variables
#####
# compare complete and missing subsamples
pta_miss <- subset(all_pta,  group=="Excluded")
pta_comp <- subset(all_pta,  group=="Included")

cont_list <- c("Age", "PTA4_better_ear", "HHIE_total", "Ability",
               "Edu", "Health", "QoL", "Help_neighbours", "Help_problems", "Concern", "Lonely", 
               "SubAge_1", "SubAge_2", "SubAge_3", "AgeStigma_1", "AgeStigma_2", "AgeStigma_3", 
               "AgeStigma_4", "AgeStigma_5", "HaStigma_1", "HaStigma_2", "HaStigma_3", "HaStigma_4")  

output_cont <- data.frame(Variable=as.character(), 
                     Mean_Excl=as.numeric(),
                     SD_Excl=as.numeric(),
                     Mean_Incl=as.numeric(), 
                     SD_Incl=as.numeric(), 
                     t=as.numeric(), 
                     p=as.numeric(), 
                     cohens_d=as.numeric(), stringsAsFactors=FALSE)
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
#####


# Included vs Excluded; Binary variables 
#####
cat_list <- c("Sex", "Married", "Accomp", "Soc_Suspect_HL", "Soc_Know_HL", "Soc_Discuss_HL", 
              "Soc_Hearing_test", "Soc_Obtain_HA", "Soc_Sometimes_use", "Soc_Regular_use",
              "Soc_Very_positive", "Soc_Somewhat_positive", "Soc_Somewhat_negative", "Soc_Very_negative")   

pta_cat <- subset( all_pta, select = c(cat_list, "group") )

# remember that there are lots of missing values, except for Sex

output_cat <- data.frame(Variable=as.character(), 
                     Proportion_Excl=as.numeric(),
                     Proportion_Incl=as.numeric(), 
                     chisq=as.numeric(), 
                     p_value=as.numeric(), 
                     cohens_h = as.numeric(), stringsAsFactors=FALSE)
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


# Yes vs No in Included; Continuous variables
#####
# compare yes and no groups within complete data
pta_yes <- subset(pta,  HA.Purchase=="Yes")
pta_no <- subset(pta,  HA.Purchase=="No")

cont_list <- c("Age", "PTA4_better_ear", "HHIE_total", "Ability",
               "Edu", "Health", "QoL", "Help_neighbours", "Help_problems", "Concern", "Lonely", 
               "SubAge_1", "SubAge_2", "SubAge_3", "Sub_Age_avg",
               "AgeStigma_1", "AgeStigma_2", "AgeStigma_3", "AgeStigma_4", "AgeStigma_5", "Age_stigma_avg",
               "HaStigma_1", "HaStigma_2", "HaStigma_3", "HaStigma_4", "HA_stigma_avg")  

output_cont <- data.frame(Variable=as.character(), 
                     Mean_No=as.numeric(),
                     SD_No=as.numeric(),
                     Mean_Yes=as.numeric(), 
                     SD_Yes=as.numeric(), 
                     t=as.numeric(), 
                     p=as.numeric(), stringsAsFactors=FALSE)
for (i in 1:26) {
    ttest.data <- t.test(pta_no[, cont_list[i]], pta_yes[, cont_list[i]], 
                         alternative=c("two.sided"), paired=FALSE)
    mean_no <- mean(pta_no[, cont_list[i]], na.rm=TRUE)
    var_no <- var(pta_no[, cont_list[i]], na.rm=TRUE)
    n_no <- sum(is.na(pta_no[, cont_list[i]])==FALSE)
    mean_yes <- mean(pta_yes[, cont_list[i]])
    var_yes <- var(pta_yes[, cont_list[i]])  
    pooled_var <- ( (149-1)*var_no + (604-1)*var_yes ) / (753 - 2)
    
    output_cont[i,1] <- cont_list[i]
    output_cont[i,2] <- round( ttest.data$estimate[1], 2)
    output_cont[i,3] <- round( sd(pta_no[, cont_list[i]], na.rm=TRUE), 2)
    output_cont[i,4] <- round( ttest.data$estimate[2], 2)
    output_cont[i,5] <- round( sd(pta_yes[, cont_list[i]] ), 2)
    output_cont[i,6] <- round( ttest.data$statistic, 2)
    output_cont[i,7] <- round( ttest.data$p.value, 4)
}

print(data.frame(output_cont))
#####


# Yes vs No in Included; Binary variables
#####
cat_list <- c("Sex", "Married", "Accomp", "Soc_Suspect_HL", "Soc_Know_HL", "Soc_Discuss_HL", 
              "Soc_Hearing_test", "Soc_Obtain_HA", "Soc_Sometimes_use", "Soc_Regular_use",
              "Soc_Very_positive", "Soc_Somewhat_positive", "Soc_Somewhat_negative", "Soc_Very_negative")   

pta_cat <- subset(pta, select = c(cat_list) )

output_cat <- data.frame(Variable=as.character(), 
                     Proportion_No=as.numeric(),
                     Proportion_Yes=as.numeric(), 
                     chisq=as.numeric(), 
                     p_value=as.numeric(), stringsAsFactors=FALSE)
for (i in 1:14) {
  count_no <- sum(pta_no[, cat_list[i]])
  count_yes <- sum(pta_yes[, cat_list[i]])
    ptest.data <- prop.test(x = c(count_no, count_yes), n = c(604, 149))
    output_cat[i,1] <- cat_list[i]
    output_cat[i,2] <- round( ptest.data$estimate[1], 3)
    output_cat[i,3] <- round( ptest.data$estimate[2], 3)
    output_cat[i,4] <- round( ptest.data$statistic, 3)
    output_cat[i,5] <- round( ptest.data$p.value, 4)
}

print(data.frame(output_cat))
#####


# Internal reliability of three scales 
#####
library(psych)

# making a larger sample

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

# must have age data, be 50+, never used HA, PTA BE > 25, all scale items present; n=1309
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

# the n=753 sample
sub_age <- subset(pta, select = c("SubAge_1", "SubAge_2", "SubAge_3"))
age_stigma <- subset(pta, select = c("AgeStigma_1", "AgeStigma_2", "AgeStigma_3", "AgeStigma_4", "AgeStigma_5"))
ha_stigma <- subset(pta, select = c("HaStigma_1", "HaStigma_2", "HaStigma_3", "HaStigma_4"))

#####


# Cronbach's alpha for n=753
#####
cronbach_sub <- alpha(sub_age, check.keys = FALSE, n.iter = 500)
cronbach_age_st <- alpha(age_stigma, check.keys = FALSE, n.iter = 500)
cronbach_ha_st <- alpha(ha_stigma, check.keys = FALSE, n.iter = 500)

lowerMat(cor(sub_age, use="complete.obs", method="pearson"), digits = 2)
print(cronbach_sub)

lowerMat(cor(age_stigma, use="complete.obs", method="pearson"), digits = 2)
print(cronbach_age_st)

lowerMat(cor(ha_stigma, use="complete.obs", method="pearson"), digits = 2)
print(cronbach_ha_st)

#####


# example plots of correlation between Age Stigma items
#####
par(mfrow=c(1,2))
set.seed(100)
plot(jitter(age_stigma[,4]), jitter(age_stigma[,5]), xlab="Item 4", ylab="Item 5")
plot(jitter(age_stigma[,1]), jitter(age_stigma[,5]), xlab="Item 1", ylab="Item 5")

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


# Comparing correlations within larger sample to n=753 sample  
#####
lowerMat(cor(sub_age_temp, use="complete.obs", method="pearson"), digits = 2)
lowerMat(cor(sub_age, use="complete.obs", method="pearson"), digits = 2)

lowerMat(cor(age_stigma_temp, use="complete.obs", method="pearson"), digits = 2)
lowerMat(cor(age_stigma, use="complete.obs", method="pearson"), digits = 2)

lowerMat(cor(ha_stigma_temp, use="complete.obs", method="pearson"), digits = 2)
lowerMat(cor(ha_stigma, use="complete.obs", method="pearson"), digits = 2)

#####


# Duration of follow-up
#####

# format dates
pta$date_firstvisit <- as.Date(pta$Date.1st.Visit.Clinic, format = "%m %d %Y")
pta$date_recentvisit <- as.Date(pta$Date.Most.Recent.Visit.Clinic, format = "%m %d %Y")
pta$date_recenteval <- as.Date(pta$Date.Most.Recent.Hearing.Eval, format = "%m %d %Y")
pta$date_purchase <- as.Date(pta$Date.HA.Purchase, format = "%m %d %Y")

# those with complete data, 753; how much date info is available?
#sum(is.na(pta$Date.1st.Visit.Clinic)) #243
#sum(is.na(pta$Date.Most.Recent.Visit.Clinic)) #4
#sum(is.na(pta$Date.Most.Recent.Hearing.Eval)) #2
#sum(is.na(pta$Date.HA.Purchase)) #604

# first visit vs. most recent visit
# 341 identical dates; 246 NA; 164 most recent -after- first visit
# 2 had most recent visit long -before- first visit
#visit <- pta$date_recentvisit - pta$date_firstvisit
#data.frame(table(visit, useNA='always'))

# first visit vs most recent hearing evaluation
# 391 are identical dates; 246 NA; 106 -after- first visit
# 10 had most recent eval -before- first visit
#eval <- pta$date_recenteval - pta$date_firstvisit
#data.frame(table(eval, useNA='always'))

# most recent visit vs. hearing evaluation; 
# 441 identical dates, 6 NA; 100 eval -after- recent visit
# 201 had most recent eval -before- most recent visit
#eval2 <- pta$date_recenteval - pta$date_recentvisit
#data.frame(table(eval2, useNA='always'))

# most recent hearing evaluation vs. date of purchase
# 5 identical; 133 eval -before- purchase; 604 NA
# 11 eval eval -after- purchase
#purch <- pta$date_purchase - pta$date_recenteval
#data.frame(table(purch, useNA='al'))

# these 2 cases are missing hearing eval, but did not purchase HA anyway
#pta[which(is.na(pta$date_recenteval)),] 
 
# follow-up time, using July 30, 2019 as end point; 2 NA's
#pta$days_followed <- as.Date("2019-07-30", format = "%Y-%m-%d") - pta$date_recenteval 

# earliest date in whole dataset is "2018-01-17", so take arbitrary start of "2018-01-15" if needed
# max date is either "2019-11-01" or "2019-07-26", which is more likely because of the final spreadsheet
# two cases with likely error in "first visit" dates: 2019-08-01 and 2019-11-01
#pta[which(temp[, c(1)] > as.Date("2019-07-30")), ]  

# plot order of four dates
temp <- data.frame(first_visit = c(rep(0, 753)), 
                   most_recent_hearing = c(rep(0, 753)),
                   most_recent_visit = c(rep(0, 753)), 
                   HA_purchase = c(rep(0, 753)))

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

# no participant is completely missing all three dates
#missdates
#  0   1   2 
#507 240   6 
#missdates <- apply(is.na(pta[, c(161:163)]), MARGIN = 1, FUN = sum)

# get earliest of three dates, as "start" of study for that participant
newtib <- pta %>% 
  rowwise %>% 
  mutate(earliest_visit = min(date_firstvisit, date_recentvisit, date_recenteval, na.rm=TRUE))
pta$earliest_visit <- newtib$earliest_visit

pta$days_in_study <- as.Date("2019-07-30", format = '%Y-%m-%d') - pta$earliest_visit
#summary(as.numeric(pta$days_in_study))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    5.0   267.0   369.0   350.3   446.0   559.0 

pta$days_to_purchase <- pta$date_purchase - pta$earliest_visit
#summary(as.numeric(pta$days_to_purchase))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-407.00    9.00   21.00   35.62   58.00  349.00     604 

# 5 cases who purchased, but missing first visit date, so days in study & to purchase were messed up 
pta[which(pta$days_to_purchase < 0), ]$days_in_study <- NA
pta[which(pta$days_to_purchase < 0), ]$days_to_purchase <- NA
# check NA applied
#pta[which(rownames(pta) %in% c(427, 445, 816, 1518, 2395)), ]

# how long people were tracked
hist(as.numeric(pta$days_in_study), 
     xlab="Days in study",
     ylab="Count",
     main="", breaks = 20, ylim=c(0,80), xlim=c(0,600))

# cumulative plot of 149
yes <- subset(pta, HA.Purchase == "Yes", select = c('days_to_purchase'))
yes2 <- yes[order(yes$days_to_purchase, decreasing=FALSE), ]
yes2 <- data.frame(yes2[-c(145:149)])  # drop NAs
colnames(yes2) <- c("days_to_purchase")
yes2$cumulative_percent <- seq(1, 144, by=1)/144*100

plot(yes2$days_to_purchase, yes2$cumulative_percent, type='l',
     xlim=c(0,350), ylim=c(0,100), 
     xlab="Days to purchase", ylab="Cumulative % of those who purchased")

#interpolation for 90 days: (81.94-81.25)/6*3+81.25
abline(h = 81.595, col='red', lty='dotted')
abline(v = 90, col='red', lty='dotted')

no <- subset(pta, HA.Purchase == "No", select = c('days_in_study'))

#table(pta$days_bin)
#  1   2   3   4   5   6   7 
#  6  61 121 159 223 177   1
pta$days_bin <- NA
pta$days_bin[pta$days_in_study <= 90] <- 1
pta$days_bin[pta$days_in_study > 90 & pta$days_in_study <= 180] <- 2
pta$days_bin[pta$days_in_study > 180 & pta$days_in_study <= 270] <- 3
pta$days_bin[pta$days_in_study > 270 & pta$days_in_study <= 360] <- 4
pta$days_bin[pta$days_in_study > 360 & pta$days_in_study <= 450] <- 5
pta$days_bin[pta$days_in_study > 450 & pta$days_in_study <= 540] <- 6
pta$days_bin[pta$days_in_study > 540] <- 7

# table
table(pta$HA.Purchase, pta$days_bin, dnn = c("Purchased", "90-day bins"))
prop.table(table(pta$HA.Purchase, pta$days_bin, dnn = c("Purchased", "90-day bins")),
           margin=2)

# test of independence; drop first and last bins because too few; 5 missing, so n=753-12=741
# X-squared = 1.6791, df = 4, p-value = 0.7945
pta_bins <- subset(pta, days_bin > 1 & days_bin < 7)
t <- table(pta_bins$HA.Purchase, pta_bins$days_bin)
chisq.test(t, correct=FALSE)

#####