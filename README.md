# Colon-Carcinoma-Screening
Health Technology Assessment (HTA) model in R evaluating the cost-effectiveness of colorectal cancer screening (FIT vs. Colonoscopy) using a Hybrid Decision Tree &amp; Markov Cohort approach.
# Cost-Effectiveness of Colorectal Cancer Screening: A Hybrid Simulation Model

# Overview
This repository contains a Health Technology Assessment (HTA) simulation model developed in R. The project evaluates the cost-effectiveness of biennial Fecal Immunochemical Test (FIT) screening compared to 10-yearly colonoscopy and a "no screening" baseline. The target population is asymptomatic adults aged 50-80.

# Model Architecture
The evaluation is conducted using a **Hybrid Simulation Model**.
* **Decision Tree:** Handles short-term screening events, including test accuracy, adherence, and immediate diagnoses.
* **Markov Cohort Model:** Simulates long-term disease progression (Natural History) over a 30-year time horizon using 1-year cycles.The health states include Normal, Adenoma, Pre-clinical Cancer, Clinical Cancer, CRC Death, and Other Cause Death.

# Key Features
* **Risk Subgroup Analysis:** Includes dynamic risk adjustments for average-risk individuals, smokers ($RR \approx 1.14$), and individuals with diabetes ($RR \approx 1.30$) based on literature-derived transition modifiers.
* **Incidence Cohort Approach:** The entire population is initialized in the "Healthy" state at age 50 to track incident cases and disease progression.
* **Annualized Expected Costs:** Screening costs are strictly weighted by the annualized adherence rate (`Unit Cost × [Adherence Rate / Interval]`) within the state definitions to accurately reflect the financial burden of participating individuals without overestimating cohort costs.

# Technologies Used
* **R Programming Language** 
* **`heemod`**: Core package for integrating Decision Trees with Markov Models.
* **`dplyr`**: For data manipulation and age-dependent parameter definitions.
* **`ggplot2`**: For generating Cost-Effectiveness Planes and visualizing absolute outcomes.
