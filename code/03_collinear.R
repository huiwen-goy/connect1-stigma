
# Checking for multicollinearity 
#####
library(car) #for VIF

# Standard formula, with 1 & 0 as outcome
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

mfull <- glm(formula28, data = pta, family = binomial("logit"))

df_full <- data.frame(vif(mod = mfull))
colnames(df_full) <- "Variance_inflation_factor"
print(df_full)
#####


# Checking for high-influence datapoints

#####
# leverage; only 4 values exceed 3x mean; 88 644 649 748
#sum( hat > mean(hat)*3 )
hat <- as.vector(hatvalues(mfull))

# Cook's distance
cook <- as.vector(cooks.distance(mfull))

# see relationship of leverage and influence
#hist(df$cook, xlim=c(0, 0.02))
#stem(df$cook)
#tail(df$cook[order(df$cook, decreasing = FALSE)], 10)
# try dropping points with D > 0.012

df <- data.frame(hat, cook)
df$large_cook <- ifelse(df$cook > 0.012, "red", "grey80")
plot(df$hat, df$cook, ylim=c(0, 0.02), xlim=c(0, 0.15), 
     col = df$large_cook, ylab="Influence", xlab="Leverage")

# Estimates of model without vs with high-influence observations
pta_temp <- pta[-which(df$cook > 0.012), ]
mfull_temp <- glm(formula28, data = pta_temp, family = binomial("logit"))

b_mfull <- as.vector(mfull$coefficients)
b_mfull_temp <- as.vector(mfull_temp$coefficients)

plot(b_mfull_temp, b_mfull, xlim=c(-6,2), ylim=c(-6,2), 
     xlab="Without high-influence observations", ylab="With all observations", 
     main="Comparing model coefficients")
#####
