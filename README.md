# Modeling Early Learning Gains from Sesame Street

## Project Overview:

This project investigates whether *Sesame Street* viewing is associated with improved preschool learning outcomes and how these effects vary by socioeconomic background, viewing frequency, and viewing setting. Using data from an Educational Testing Service study from the 1970s, we model changes in six early learning areas.

## Research Question:

How do *Sesame Street* viewing habits, socioeconomic context, and child characteristics relate to preschool learning gains?

## Data Description

* **Source**: Educational Testing Service, early 1970s *Sesame Street* evaluation
* **Observations**: 240 preschool children
* **Target Variables**:

  * Improvement in scores for body, letters, forms, numbers, relational terms, and classification
* **Key Predictors**:

  * Age and sex
  * Baseline Peabody vocabulary score
  * Viewing frequency
  * Viewing setting (home vs. school)
  * Viewing encouragement
  * Site/socioeconomic context
* **Interaction Terms**:

  * Site × viewing encouragement
  * Sex × viewing frequency
  * Setting × baseline vocabulary

## Methodology

* Converted pre- and post-test scores into percentage improvement
* Built six beta regression models with a logit link
* Included main effects and pairwise interactions
* Used lasso regularization and confidence intervals for variable selection
* Compared models using BIC and training RMSE

## Key Findings

* **Learning Gains**: Children showed positive average improvement across all six learning areas, with the largest gains in forms, letters, and numbers
* **Viewing Frequency**: More frequent viewing was generally associated with greater improvement, especially above five viewings per week
* **Context Matters**: Effects varied by socioeconomic site, viewing setting, baseline vocabulary, and encouragement
* **Interaction Effects**: Site × encouragement, sex × viewing frequency, and setting × vocabulary were among the strongest interactions
* **No Universal Effect**: Higher viewing frequency did not consistently produce greater gains in every setting or for every group

## Limitations

* Data comes from the early 1970s and uses outdated testing measures
* Small sample size and substantial variability in children's outcomes
* Site combines socioeconomic and geographic factors
* Important influences such as home environment and preschool quality were not measured
* Models focus on inference rather than predicting modern children

## Future Work

* Replicate the analysis using modern preschool data
* Include family income, home environment, and preschool quality
* Examine why socioeconomic context and viewing setting change the effects of educational media
* Study whether similar patterns occur with modern educational television and digital media
