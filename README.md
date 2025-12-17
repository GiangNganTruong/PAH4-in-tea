# PAH4-in-tea

## Introduction
This repository contains the R scripts used for statistical analysis and health risk assessment in the study:
**PAH4 in Ready-to-Drink Tea in Taiwan: Determination using HPLC-FLD and Health Risk Assessment**

My initial project aims to 
1. Develop and validate an HPLC-FLD method for PAH4 detection in tea.
2. Analyze commercial tea samples from the Taiwanese market.
3. Assess PAH4 contamination levels and compare with traditional tea infusion studies.
4. Conduct a probabilistic health risk assessment using Monte Carlo simulation.

This repository includes R code to:
-   Best-fit mean estimation for interval-censored PAH4 concentration data
-   Probabilistic Monte Carlo simulation for dietary exposure and risk assessment
-   Generate visualizations

## Repo structure
1. docs: knitted HTML reports
2. main analysis: survival regression model, Monte Carlo models for BaP and PAH4 consumption risk assessment.
3. rmarkdown: original .Rmd files
4. visualizations: visual outputs (png)

## HTML reports
You can view the knitted HTML reports here:
- BaP Risk Assessment: https://giangngantruong.github.io/PAH4-in-tea/bap-risk-assessment.html
- PAH4 Risk Assessment: https://giangngantruong.github.io/PAH4-in-tea/pah4-risk-assessment.html
- Survival regression model: https://giangngantruong.github.io/PAH4-in-tea/pah4-survival-regression-model.html

## Notes:
This repository contains only the statistical analysis code. The original manuscript, raw data, and full methodological details are not included here due to data-sharing restrictions.
