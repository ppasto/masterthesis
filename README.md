# Master Thesis - Frequentists and Bayesian methods to incorporate recruitment rate stochasticity at the design stage of a clinical trial

## Overview

### **Project Description**

In clinical trials, inaccurate or overly optimistic projections of recruitment rates can lead to the failure of enrolling a sufficient number of patients within the required time frame. This can ultimately result in study discontinuation, delays, or inconclusive statistical findings. In practice, many researchers rely on deterministic models that do not adequately capture the uncertainty inherent in the patient recruitment process. While more sophisticated stochastic methods are available, they often use simulations, which can yield approximate estimates of trial duration. These limitations highlight the need for exact methods for predicting recruitment timelines. To address this gap, we developed exact statistical methods that account for both aleatory uncertainty (stemming from random variability) and epistemic uncertainty (due to lack of knowledge about the true recruitment rate). To support the implementation of the methods, visualizations of patient leakage and a unified mathematical notation were developed. For modeling the number of patients recruited over time  ($Ttarget$), we use a Poisson-Gamma model, while for waiting times until a certain number of patients is recruited ($Ctarget$), we apply a Gamma-Gamma approach. These exact models allow for more accurate predictions of the time needed to reach a desired sample size. This provides a more reliable basis for planning clinical trials, allocating resources, and setting expectations on realistic recruitment. By integrating uncertainty more thoroughly, our approach enhances the transparency and credibility of trial planning and supports better-informed decision-making in clinical research.

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

Guidances to reproduce the report and presentation can be found in the .md of the respective folders. 

## Author Responsible

Pilar Pastor Martínez

## Contributors

PD Dr. Malgorzata Roos
