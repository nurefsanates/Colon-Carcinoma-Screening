if(!require(heemod)) install.packages("heemod")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
library(heemod); library(dplyr); library(ggplot2)

starting_age<-50
time_horizon<-30
discount_rate<-0.03 #3% Discount rate for costs and effects

#COSTS
#Average cost of treating Cancer (Stage I-IV weighted-Griebler et al. 2019)
cost_cancer_avg<-19064.00

#Screening costs
cost_fit_test<-41.00
cost_colonoscopy<-292.00

#Test Accuracy 
sens_fit_adenoma<-0.076
sens_fit_cancer<-0.872
sens_col_adenoma<-0.690
sens_col_cancer<-0.947

#Participation
#Based on data from Griebler's estimates
p_adherence_fit<-0.389
p_adherence_col<-0.20

#Since screening is every 2 years, we divide the uptake by 2 for the annual model(same for colonoscopy)
p_fit_effective<-p_adherence_fit / 2
p_col_effective<-p_adherence_col / 10

#Risk Factors
rr_smoking<-1.14 #Relative Risk for Smokers (Botteri et al.)
rr_diabetes<-1.30 #Relative Risk for Diabetes (Larsson et al.)

#"Cure Model" Logic
#If screening detects cancer early, the patient moves to "Normal" state (Cured),
#avoiding cancer death risk and treatment costs.

#Strategy 1: No Screening
strat_none<-define_strategy(
  transition = define_transition(
    C,p_norm_ade,0,0,0.01,
    0,C,0.027,0,0.01,
    0,0,C,0.105,0.01, #Preclinical -> Clinical (Symptomatic)
    0,0,0,C,0.15+0.01, #Clinical -> Death (High Mortality)
    0,0,0,0,1
  ),
  define_state(cost=0,utility=1.0),
  define_state(cost=0,utility=1.0),
  define_state(cost=0,utility=1.0),
  define_state(cost=cost_cancer_avg,utility=1.0), #Cancer treatment cost applied here
  define_state(cost=0, utility=0.0)
)

#Strategy 2: FIT Screening
strat_fit<-define_strategy(
  transition = define_transition(
    C,p_norm_ade,0,0,0.01,
    
    #Polypectomy (Adenoma to Normal)
    p_fit_effective* sens_fit_adenoma, C, 0.027,0,0.01,
    
    #Early Detection (Preclinical to Normal)
    #If detected, patient is cured and avoids Clinical state/Costs
    p_fit_effective* sens_fit_cancer, 0,C,0.105,0.01,
    
    0,0,0,C,0.15+0.01,
    0,0,0,0,1
  ),
  define_state(cost=cost_fit_test * p_fit_effective, utility=1.0), #Annualized cost
  define_state(cost=0, utility=1.0), 
  define_state(cost=0, utility=1.0), 
  define_state(cost=cost_cancer_avg, utility=1.0), 
  define_state(cost=0, utility=0.0)
)

#Strategy 3: Colonoscopy
strat_col<-define_strategy(
  transition = define_transition(
    C,p_norm_ade,0,0,0.01,
    
    #Polypectomy (Higher Sensitivity)
    p_col_effective* sens_col_adenoma ,C,0.027,0,0.01,
    
    #Early Detection (Higher sensitivity->Cured)
    p_col_effective* sens_col_cancer, 0,C,0.105,0.01,
    
    0,0,0,C,0.15+0.01,
    0,0,0,0,1
  ),
  define_state(cost=cost_colonoscopy * p_col_effective, utility=1.0), #Annualized cost
  define_state(cost=0, utility=1.0), 
  define_state(cost=0, utility=1.0), 
  define_state(cost=cost_cancer_avg, utility=1.0), 
  define_state(cost=0, utility=0.0)
)

#Scenario Analysis
run_scenario_analysis<-function(risk_multiplier, scenario_name){
  #Update parameters based on age and the specific risk multiplier
  param_custom<-define_parameters(
    current_age=starting_age + markov_cycle,
    
    #Normal to Adenoma based on age (Data derived from Griebler et al. / Statistics Austria)
    p_norm_ade_base=dplyr::case_when(
      current_age<55 ~0.030,
      current_age<60 ~0.034,
      current_age<65 ~0.041,
      current_age<70 ~0.047,
      TRUE ~0.057
    ),
    p_norm_ade=p_norm_ade_base * risk_multiplier
  )
  #Run Markov Model
  res<-run_model(
    No_Screening= strat_none,
    FIT_Screening= strat_fit,
    Colonoscopy= strat_col,
    parameters= param_custom,
    cycles = time_horizon,
    init= c(1000, 0, 0, 0, 0),
    cost = cost,
    effect = utility,
    method = "life-table"
  )
  #ICER
  print(paste("SCENARIO:", scenario_name, "| Risk Multiplier:", round(risk_multiplier, 2)))
  print(summary(res))
  
  df_plot <- summary(res)$res_values
  
  plot_absolute <- ggplot(df_plot, aes(x = utility, y = cost, color = .strategy_names)) +
    geom_point(size = 6, alpha = 0.8) +
    
    geom_text(aes(label = .strategy_names), vjust = -1.5, fontface = "bold") +
    
    labs(
      title = paste("Cost-Effectiveness Landscape:", scenario_name),
      subtitle = "Absolute Values (Total Cost vs Total Life Years per 1000 people)",
      x = "Total Life Years (LY)",
      y = "Total Cost (EUR)",
      color = "Strategy"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(legend.position = "none") + 
    
    scale_y_continuous(labels = scales::comma) +
    expand_limits(y = max(df_plot$cost) * 1.1, x = max(df_plot$utility) * 1.001)
  
  print(plot_absolute)
  
  return(res)
}

#4 Profiles
#Average Risk (No Smoking, No Diabetes)
#Risk Multiplier = 1.0
res_avg<-run_scenario_analysis(1.0,"Average Risk")

#Smoker Only (Risk= 1.14)
res_smoker<-run_scenario_analysis(rr_smoking,"Smoker Only")

#Diabetes Only (Risk=1.30)
res_diabetes<-run_scenario_analysis(rr_diabetes,"Diabetes Only")

#High Risk (Smoker + Diabetes)
res_high<-run_scenario_analysis(rr_diabetes*rr_smoking,"High Risk (Smoker + Diabetes)")

#Final Comparison Table
extract_data_complete<-function(model_res,name){
  tab<-summary(model_res)$res_values
  tab$.strategy_names <- trimws(as.character(tab$.strategy_names))
  cohort_size <- 1000
  #No Screening
  base_row<-tab[tab$.strategy_names %in% c("No_Screening","No Screening"), ]
  base_cost<-base_row$cost
  base_eff<-base_row$utility
  
  df_base<-data.frame(
    Risk_Profile= name,
    Strategy= "1. No Screening(Ref)",
    Total_Cost_EUR= round(base_cost/cohort_size,0),
    Life_Years= round(base_eff/cohort_size,3),
    Cost_Saving= 0,
    LY_Gained= 0
  )
  
  #FIT Screening
  fit_row<-tab[tab$.strategy_names %in% c("FIT_Screening","FIT Screening"), ]
  df_fit<-data.frame(
    Risk_Profile= name,
    Strategy= "2. FIT Screening",
    Total_Cost_EUR= round(fit_row$cost/cohort_size,0),
    Life_Years= round(fit_row$utility/cohort_size,3),
    Cost_Saving= round((base_cost - fit_row$cost)/cohort_size, 0),
    LY_Gained= round((fit_row$utility - base_eff)/cohort_size, 3)
  )
  #Colonoscopy Screening
  col_row<-tab[tab$.strategy_names %in% c("Colonoscopy"), ]
  df_col <- data.frame(
    Risk_Profile= name,
    Strategy= "3. Colonoscopy",
    Total_Cost_EUR= round(col_row$cost/cohort_size, 0),
    Life_Years= round(col_row$utility/cohort_size, 3),
    Cost_Saving= round((base_cost - col_row$cost)/cohort_size, 0),
    LY_Gained= round((col_row$utility - base_eff)/cohort_size, 3)
  )
  return(rbind(df_base, df_fit, df_col))
}

master_table_complete<-rbind(
  extract_data_complete(res_avg,"A. Average Risk"),
  extract_data_complete(res_smoker,"B. Smoker Only"),
  extract_data_complete(res_diabetes,"C. Diabetes Only"),
  extract_data_complete(res_high,"D. High Risk (Both)")
)
print(master_table_complete)



