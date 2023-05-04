**About this project**  
The goal of this project was to determine how well hearing, health and social factors predicted a person's decision to purchase hearing aids after a first visit to a hearing clinic.  

**Data inclusion criteria**  
1. Participants with known age and sex, who were 50+ years old, and had never used a hearing aid  
2. Pure-tone average of 25 dB HL or greater in the better ear  
3. Complete data across 28 predictors of interest, including all original items on the Age Stigma scale  

[**Exploratory plots**](https://huiwen-goy.github.io/connect1-stigma/01_explore.html)  
Demographic, hearing, stigma, social support, and social network measures.  
Note that percentage breakdowns for categories are labelled with counts, to highlight that few people tended to choose the most negative options.  

[**About the participants**](https://huiwen-goy.github.io/connect1-stigma/02_participants.html)  
* Comparison of included and excluded participants, showing that they were generally similar.  
* Internal reliability analyses for stigma scales, justifying dropping one Age Stigma item.   
* Analysis of study duration effects, showing that 80+% purchase HAs in the first three months.  

[**Multicollinearity and high-influence datapoints**](https://huiwen-goy.github.io/connect1-stigma/03_collinear.html)  
None of the 28 predictors were multicollinear, and there were no observations with a large influence on a regression model.  

[**Logistic regression**](https://huiwen-goy.github.io/connect1-stigma/04_lr.html)  
* Choosing a threshold  
* Previous analysis: Backwards elimination procedure using AIC  
* New analysis: Penalized regression  
* Power analysis (response to comments)  
* Takeaways

[**Classification tree**](https://huiwen-goy.github.io/connect1-stigma/05_ctree.html) 
* About classification trees  
* Using case weights  
* Pruning procedure  
* Previous analysis: max depth  
* New analysis: pruning  
* Takeaways  

[**Introduction to tree techniques**](https://huiwen-goy.github.io/connect1-stigma/intro_tree_techniques_Jan2023.01.18.2021.pdf)  
  
[**Bagged tree**](https://huiwen-goy.github.io/connect1-stigma/Connect1_Stigma_bagging_2022.html)  
  
[**Random forest**](https://huiwen-goy.github.io/connect1-stigma/Connect1_Stigma_RF_2022.html)  

[**Boosted tree**](https://huiwen-goy.github.io/connect1-stigma/Connect1_Stigma_RF_2022.html)  
