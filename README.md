# Master Thesis - Frequentists and Bayesian methods to incorporate recruitment rate stochasticity at the design stage of a clinical trial

## Overview

### **Project Description**

This project focuses on improving participant recruitment projections at the design stage of clinical trials. Recruitment projections are crucial for ensuring that a study reaches its target sample size within a limited timeframe, often constrained by funding. Traditional methods, like Carter’s Poisson-based approach using Monte Carlo simulations, account only for aleatory (random) uncertainty. This Master's thesis aims to enhance Carter’s method by integrating epistemic (knowledge-based) uncertainty, making recruitment forecasts more realistic and robust. The project involves both theoretical model development and practical application to real clinical trial data.


### **Project Scope**

- **Review of Clinical Trial Design:** Overview of the standard structure of a clinical trial, with emphasis on patient flow and potential attrition (leakage) at various stages.
- **Uncertainty Classification:** Introduction and distinction between **aleatory** and **epistemic** uncertainty, and their relevance in recruitment modeling.
- **Methodological Framework:**

  - Development and comparison of **deterministic**, **Monte Carlo**, and **exact methods** for recruitment prediction.
  - Focus on extending Carter’s Poisson-based DSRA model to include epistemic uncertainty.
- **Sensitivity Analysis:** Examination of how different assumptions affect recruitment predictions, with statistical summaries and uncertainty quantification.
- **Application to Real Data:** Validation of proposed methods using data from an actual clinical trial (Carter, 2004), with comparisons to existing approaches.
- **Open-Source Tools:** Implementation of the methods in R, with code made available in the appendix for reproducibility and further use by the research community.

### **Reproducibility guidance**
In **Report** folder:
1. Open report.Rproj
2. Run knit.R script
3. Compile compile.Rnw

## Author Responsible

Pilar Pastor Martínez

## Contributors

PD Dr. Malgorzata Roos
