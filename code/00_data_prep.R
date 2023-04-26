# Read raw data
orig <- read.csv("Connect_Data_20191025.csv", header=TRUE)

# Must have known age and sex, 50+ years old, never used a HA
data <- orig[which(is.na(orig$Q44) == FALSE & is.na(orig$Q43) == FALSE & orig$Q44 > 49.5 & orig$Q42 == "No"), ]

# Recalculate Age_stigma_avg without Q4, replacing the existing variable
data$Age_stigma_avg <- rowMeans(data[, c("Q5_Corrected_reversed", "Q6_Corrected_reversed", "Q7_Corrected_reversed", "Q8_Corrected_reversed")])

# Rename variables, recode to numeric
data$Age <- data$Q44

data$Sex <- c(NA)
data$Sex[data$Q43 == "Male"] <- 1
data$Sex[data$Q43 == "Female"] <- 0

data$Edu <- data$Q46_Corrected
data$Health <- data$Q53
data$QoL <- data$Q54

data$Married <- c(NA) 
data$Married[data$marital_recoded == "Married"] <- 1
data$Married[data$marital_recoded == "Sep_Divor"] <- 0
data$Married[data$marital_recoded == "Single"] <- 0
data$Married[data$marital_recoded == "Widowed"] <- 0
data$Married[data$marital_recoded == "Other"] <- 0

# Hearing ability scale; PTABE already calculated
data$Ability <- data$Q56_Corrected

data$Accomp <- c(NA)
data$Accomp[data$Q45 == "Yes"] <- 1
data$Accomp[data$Q45 == "No"] <- 0

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

# Make outcome measure into 0/1
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

# Rename variables: individual items in stigma scales
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

# PTA-BE > 25 and complete data, including all original Age Stigma items
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

# Clean up interim datasets
rm(data)
rm(orig)
