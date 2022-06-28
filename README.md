#### About this project
 
The goal of this project was to examine how well hearing, health and social factors predicted a person's decision to purchase hearing aids after a first visit to a hearing clinic. 
 
#### About the data
 
The original dataset contained 4300+ survey responses. After cleaning the data and retaining only those with known age and sex, who were 50+ years old, and had never used a HA, there were 3312 responses left. Of these 3312 responses, 1396 were from participants who had a PTA of 25 or greater in their better ear (other choices of criteria (placeholder) were HHIE score and self-rated ability). All 1396 participants had complete data for the outcome measure of hearing aid purchase, but only **753 participants** had complete data across 28 predictors of interest. The extent of missing data for the remaining 643 participants ranged from 3% to 53% per person. 
 
#### About the code
 
The R file contains code for:  
a) re-coding variables; e.g. converting categorical variables with multiple categories to binary variables  
b) re-naming variables for to make them more user-friendly; e.g. Q55 -> Lonely   
c) selecting observations according to certain criteria   
d) getting counts and descriptive statistics   
e) calculating correlations among survey items  
f) calculating and plotting group audiograms   
g) analyses listed below 
 
#### Reports: Methods, results, and conclusions

[Descriptives and study duration effects](https://huiwen-goy.github.io/connect1-stigma/Connect1_Stigma_descriptives_2022.html)  

[Classification tree](https://huiwen-goy.github.io/connect1-stigma/Connect1_Stigma_Tree_2022.html)  
  
Logistic regression with backwards elimination 
