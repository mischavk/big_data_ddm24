# A Big Data Analysis of the Associations Between Cognitive Parameters and Socioeconomic Outcomes

## Overview

This repository contains the code and resources for reproducing the results presented in the preprint:  
**A Big Data Analysis of the Associations Between Cognitive Parameters and Socioeconomic Outcomes**  
by Mischa von Krause and Stefan T. Radev. This project is a collaboration between Heidelberg University and Rensselaer Polytechnic Institute.

**TL;DR** We examined how cognitive parameters estimated using evidence accumulation models relate to socioeconomic outcomes. Drift rate variability, reflecting trial-by-trial stability of mental speed, emerged as the strongest predictor.

## Summary

Evidence accumulation models estimate cognitive parameters, such as mental speed and decision caution, from empirical response time and accuracy data. These parameters can quantify individual differences and offer insights into cognitive abilities, which are known predictors of socioeconomic outcomes like educational attainment.  

In this study, we explore the associations between evidence accumulation model parameters and three socioeconomic outcomes—educational attainment, income, and job prestige—using a large dataset derived from online implicit association tests (IATs). **Drift rate variability**, a measure of trial-by-trial mental speed stability, emerged as the strongest predictor of socioeconomic outcomes. Thus, higher-order parameters, often overlooked in studies of individual differences, demonstrate potential for novel research directions.

For more details, see the full preprint.

## Amortized Bayesian Inference

This project leverages **Amortized Bayesian Inference (ABI)** techniques to efficiently estimate cognitive model parameters across a large dataset. ABI combines deep learning with Bayesian principles to enable scalable, high-throughput parameter estimation. [Add additional details about ABI here if needed.]

## How to Use This Repository

### Code Execution

1. **File Order:**  
   Run the scripts and notebooks in numerical order, starting with:
   - `01_outcome_prepred.R`  
   - `02_Data_Preparation.ipynb`  

   Follow this sequence for all subsequent files.

2. **Parameter Estimation:**  
   After completing the preparatory scripts, proceed with parameter estimation as outlined in the provided files.

3. **Statistical Analyses:**  
   Complete the analyses using the scripts in the `final_analysis_code` folder.  

   **Processed Data:** Processed datasets for reproducing statistical analyses are available on OSF: [https://osf.io/xm3dr/](https://osf.io/xm3dr/).  
   **Raw Data:** Original data can be accessed via the Project Implicit OSF page:  
   - [https://osf.io/52qxl/](https://osf.io/52qxl/)  
   - Specific files include:  
     - `Race_IAT.public.2015.zip` (for socioeconomic outcomes in 2015)  
     - `iat_Jan2007-Dec2007.zip` (for by-trial IAT data used in evidence accumulation modeling).