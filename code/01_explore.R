
# Read libraries
library(ggplot2)
library(gridExtra)

# Age, Sex.f, Edu, Health, QoL, Married.f (6 plots)
#####
d1 <- ggplot(data = pta, aes(y = Age, x = HA.Purchase)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill="grey95") +
  geom_jitter(aes(y = Age, x = HA.Purchase), height=0.1, width=0.1, alpha=0.75) + 
  theme_bw() + 
  scale_y_continuous(name = "Years", breaks=seq(50,100,10)) + 
  xlab("Purchased hearing aids") + 
  ggtitle("Age") + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5) )

d2 <- ggplot(data = pta, aes(x = HA.Purchase, group = Sex.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Sex.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Sex") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 18, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") )

node_values <- c("1"="<HS", "2"="HS", "3"="Some coll", "4"="Bach", "5"="Postgrad")
d3 <- ggplot(data = pta, aes(x = HA.Purchase, group = Edu)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Edu) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Education") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 16, colour = "black"), 
        axis.text = element_text(size = 12, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") )  

node_values <- c("1"="Poor", "2"="Fair", "3"="Average", "4"="Good", "5"="Excellent")
d4 <- ggplot(data = pta, aes(x = HA.Purchase, group = Health)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Health, labeller = as_labeller(node_values)) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Health") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 16, colour = "black"), 
        axis.text = element_text(size = 12, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") )  

node_values <- c("1"="Poor", "2"="Fair", "3"="Average", "4"="Good", "5"="Excellent")
d5 <- ggplot(data = pta, aes(x = HA.Purchase, group = QoL)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ QoL, labeller = as_labeller(node_values)) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Quality of life") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 16, colour = "black"), 
        axis.text = element_text(size = 12, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") )  

node_values <- c("No" = "Not married", "Yes" = "Married")
d6 <- ggplot(data = pta, aes(x = HA.Purchase, group = Married.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Married.f, labeller = as_labeller(node_values)) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Marital status") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

grid.arrange(d1, d2, d3, d4, d5, d6, ncol=2, nrow=3)
#####

# PTA4_better_ear, HHIE, Ability (3 plots)
#####
ggplot(data = pta, aes(y = PTA4_better_ear, x = HA.Purchase)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill="grey95") +
  geom_jitter(aes(y=PTA4_better_ear, x=HA.Purchase), height=0.05, width=0.1, alpha=0.75) + 
  theme_bw() + 
  scale_y_continuous(name = "PTA (better ear) dB HL", breaks=seq(20,80,10)) + 
  xlab("Purchased hearing aids") + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"))

ggplot(data = pta, aes(y = HHIE_total, x = HA.Purchase)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill="grey95") +
  geom_jitter(aes(y=HHIE_total, x=HA.Purchase), height=0.05, width=0.1, alpha=0.75) +
  theme_bw() + 
  scale_y_continuous(name = "HHIE total", breaks=seq(0, 50, 10)) + 
  xlab("Purchased hearing aids") + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"))

ggplot(data = pta, aes(y = Ability, x = HA.Purchase)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill="grey95") +
  geom_jitter(aes(y=Ability, x=HA.Purchase), height=0.05, width=0.1, alpha=0.75) +
  theme_bw() + 
  scale_y_continuous(name = "Ability", breaks=seq(0, 10, 1)) + 
  xlab("Purchased hearing aids") + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"))
#####

# Sub_Age_avg, Age_stigma_avg, HA_stigma_avg (3 plots)
#####
ggplot(data = pta, aes(y = Sub_Age_avg, x = HA.Purchase)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill="grey95") +
  geom_jitter(aes(y=Sub_Age_avg, x=HA.Purchase), height=0.05, width=0.1, alpha=0.75) +
  theme_bw() + 
  scale_y_continuous(name = "Subjective age", breaks=seq(0, 8, 1)) + 
  xlab("Purchased hearing aids") + 
  theme(axis.title = element_text(size = 24, colour = "black"), 
        axis.text = element_text(size = 18, colour = "black")) + 
  annotate("text", x=1.5, y=7, label = "Older", size = 7) + 
  annotate("text", x=1.5, y=1, label = "Younger", size = 7) 

ggplot(data = pta, aes(y = Age_stigma_avg, x = HA.Purchase)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill="grey95") +
  geom_jitter(aes(y=Age_stigma_avg, x=HA.Purchase), height=0.05, width=0.1, alpha=0.75) +
  theme_bw() + 
  scale_y_continuous(name = "Age stigma", breaks=seq(0, 6, 1)) + 
  xlab("Purchased hearing aids") + 
  theme(axis.title = element_text(size = 24, colour = "black"), 
        axis.text = element_text(size = 18, colour = "black")) + 
  annotate("text", x=1.5, y=5, label = "High stigma", size = 7) + 
  annotate("text", x=1.5, y=1, label = "Low stigma", size = 7) 

ggplot(data = pta, aes(y = HA_stigma_avg, x = HA.Purchase)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill="grey95") +
  geom_jitter(aes(y=HA_stigma_avg, x=HA.Purchase), height=0.05, width=0.1, alpha=0.75) +
  theme_bw() + 
  scale_y_continuous(name = "HA stigma", breaks=seq(0, 6, 1)) + 
  xlab("Purchased hearing aids") + 
  theme(axis.title = element_text(size = 24, colour = "black"), 
        axis.text = element_text(size = 18, colour = "black")) + 
  annotate("text", x=1.5, y=5, label = "High stigma", size = 7) + 
  annotate("text", x=1.5, y=1, label = "Low stigma", size = 7) 

#####

# Accomp.f, Help_neighbours, Help_problems, Concern, Lonely (5 plots)
#####
ggplot(data = pta, aes(x = HA.Purchase, group = Accomp.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Accomp.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Accompanied") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 24, colour = "black"), 
        axis.text = element_text(size = 18, colour = "black"), 
        plot.title = element_text(size = 24, hjust = 0.5), 
        strip.text.x = element_text(size = 20, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") )

node_values <- c("1"="Very easy", "2"="Easy", "3"="Possible", 
                 "4"="Difficult", "5"="Very diff")
ggplot(data = pta, aes(x = HA.Purchase, group = Help_neighbours)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Help_neighbours, labeller = as_labeller(node_values)) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Get help from neighbours") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 22, colour = "black"), 
        axis.text = element_text(size = 18, colour = "black"), 
        plot.title = element_text(size = 24, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") )  


node_values <- c("0"="0", "1"="1-2", "2"="3-5", "3"="5+")
ggplot(data = pta, aes(x = HA.Purchase, group = Help_problems)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Help_problems, labeller = as_labeller(node_values)) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("People to help w/ problems") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 22, colour = "black"), 
        axis.text = element_text(size = 18, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") )  


node_values <- c("1"="None", "2"="Little", "3"="Uncert", "4"="Some", "5"="A lot")
ggplot(data = pta, aes(x = HA.Purchase, group = Concern)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Concern, labeller = as_labeller(node_values)) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Concern from others") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 22, colour = "black"), 
        axis.text = element_text(size = 18, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") )  


node_values <- c("1"="Rarely", "2"="Some", "3"="Occas", "4"="All")
ggplot(data = pta, aes(x = HA.Purchase, group = Lonely)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Lonely, labeller = as_labeller(node_values)) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("How often lonely") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 22, colour = "black"), 
        axis.text = element_text(size = 18, colour = "black"), 
        plot.title = element_text(size = 20, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") )  
#####

# 11 social contagion measures
#####
s1 <- ggplot(data = pta, aes(x = HA.Purchase, group = Soc_Suspect_HL.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Soc_Suspect_HL.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("...with suspected HL") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

s2 <- ggplot(data = pta, aes(x = HA.Purchase, group = Soc_Know_HL.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Soc_Know_HL.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("...with HL") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

grid.arrange(s1, s2, ncol=2, nrow=1)

s3 <- ggplot(data = pta, aes(x = HA.Purchase, group = Soc_Discuss_HL.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Soc_Discuss_HL.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("...discussed HL") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

s4 <- ggplot(data = pta, aes(x = HA.Purchase, group = Soc_Hearing_test.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Soc_Hearing_test.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("...had hearing test") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

grid.arrange(s3, s4, ncol=2, nrow=1)

s5 <- ggplot(data = pta, aes(x = HA.Purchase, group = Soc_Obtain_HA.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Soc_Obtain_HA.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("...obtained HA") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

s6 <- ggplot(data = pta, aes(x = HA.Purchase, group = Soc_Sometimes_use.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Soc_Sometimes_use.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("...sometimes uses HA") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

grid.arrange(s5, s6, ncol=2, nrow=1)

s7 <- ggplot(data = pta, aes(x = HA.Purchase, group = Soc_Regular_use.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Soc_Regular_use.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("...regularly uses HA") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

s8 <- ggplot(data = pta, aes(x = HA.Purchase, group = Soc_Very_positive.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Soc_Very_positive.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Very positive experience") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

grid.arrange(s7, s8, ncol=2, nrow=1)

s9 <- ggplot(data = pta, aes(x = HA.Purchase, group = Soc_Somewhat_positive.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Soc_Somewhat_positive.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Somewhat positive exp") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

s10 <- ggplot(data = pta, aes(x = HA.Purchase, group = Soc_Somewhat_negative.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Soc_Somewhat_negative.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Somewhat negative exp") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

grid.arrange(s9, s10, ncol=2, nrow=1)

s11 <- ggplot(data = pta, aes(x = HA.Purchase, group = Soc_Very_negative.f)) +
  geom_bar(aes(y = ..prop..), fill="grey80", colour="black") + 
  facet_grid(~ Soc_Very_negative.f) + 
  geom_text(aes(label = ..count.., y= ..prop..), stat="count", vjust = -.5) + 
  scale_y_continuous(name="Percentage", labels = scales::percent, limits=c(0, 1)) +
  xlab("Purchased hearing aids") + 
  ggtitle("Very negative experience") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18, colour = "black"), 
        axis.text = element_text(size = 16, colour = "black"), 
        plot.title = element_text(size = 22, hjust = 0.5), 
        strip.text.x = element_text(size = 12, colour = "black"), 
        strip.background = element_rect(color = "black", fill="white") ) 

grid.arrange(s11, ncol=2, nrow=1)
#####
