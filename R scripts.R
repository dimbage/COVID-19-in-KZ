options(warn=-1)
suppressMessages()

library(ggplot2)
library(tidyverse)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggsci)
library(dplyr)
library(magrittr)

# read database from github repo

db <- read_delim("https://raw.githubusercontent.com/dimbage/COVID-19-in-KZ/main/covid-19_db.txt",
                          delim = "\t", 
                          escape_double = FALSE, 
                          col_types = cols(
                            Disease_severity = col_factor(levels = c("1","2", "3", "4")),
                            Sex = col_factor(levels = c("0","1")), 
                            Kazakh_ethnicity = col_factor(levels = c("0","1")), 
                            Deaths = col_factor(levels = c("0","1")), 
                            Any_comorbidities = col_factor(levels = c("0","1")), 
                            Hypertension = col_factor(levels = c("0","1")), 
                            Coronary_heart_disease = col_factor(levels = c("0","1")), 
                            COPD = col_factor(levels = c("0","1")),
                            Chronic_kidney_disease = col_factor(levels = c("0","1")),
                            Cancer = col_factor(levels = c("0","1")),
                            Diabetes = col_factor(levels = c("0","1")),
                            Liver_disease = col_factor(levels = c("0","1")),
                            Othercomorb = col_factor(levels = c("0","1")),
                            Cough = col_factor(levels = c("0","1")),
                            Sputum_production = col_factor(levels = c("0","1")),
                            Shortness_of_breath = col_factor(levels = c("0","1")),
                            Dyspnoea = col_factor(levels = c("0","1")),
                            Stuffy = col_factor(levels = c("0","1")),
                            Sore_throat = col_factor(levels = c("0","1")),
                            Oropharynx_hyperemia = col_factor(levels = c("0","1")),
                            Tonsill_hypertrophy = col_factor(levels = c("0","1")),
                            Chest_pain = col_factor(levels = c("0","1")),
                            Chest_tightness = col_factor(levels = c("0","1")),
                            Wheezing = col_factor(levels = c("0","1")),
                            Diarrhoea = col_factor(levels = c("0","1")),
                            Nausea_vomiting = col_factor(levels = c("0","1")),
                            Headache = col_factor(levels = c("0","1")),
                            Conjuctivitis = col_factor(levels = c("0","1")),
                            Myalgia_fatigue = col_factor(levels = c("0","1")),
                            Joint_pain = col_factor(levels = c("0","1")),
                            Pneumonia = col_factor(levels = c("0","1")),
                            Bronchitis = col_factor(levels = c("0","1")),
                            Antiviral_medications = col_factor(levels = c("0","1")),
                            Ribavirin = col_factor(levels = c("0","1")),
                            Lopinavir_and_ritonavir = col_factor(levels = c("0","1")),
                            Oseltamivir = col_factor(levels = c("0","1")),
                            Antibiotics = col_factor(levels = c("0","1")),
                            Hydroxychloroquine = col_factor(levels = c("0","1")),
                            Anticoagulant = col_factor(levels = c("0","1")),
                            Corticosteroids = col_factor(levels = c("0","1")),
                            Oxygen_therapy = col_factor(levels = c("0","1")),
                            Mechanical_ventilation = col_factor(levels = c("0","1"))), 
                          trim_ws = TRUE)


# TABLE 1 and 2
library(finalfit)

tmp = db
# 3 Groups based on  Disease_severity (3 and 4 are merged into one)
tmp %<>% mutate(Disease_severity_group = case_when(Disease_severity == 1 ~ "Asymptomatic_Mild", 
                                                   Disease_severity == 2 ~ "Moderate", 
                                                   Disease_severity %in% c(3,4) ~ "Severe_critical")
                )
tmp$Disease_severity_group <- factor(tmp$Disease_severity_group, levels = c("Asymptomatic_Mild","Moderate","Severe_critical"))

# Age group based on age
tmp %<>% mutate(Age_group = case_when(Age < 15 ~ "0-14",
                                      Age >= 15 & Age < 50 ~ "15-49", 
                                      Age >= 50 & Age < 65 ~ "50-64",
                                      Age >= 65 ~ "65 and more")
)

tmp$Age_group <- factor(tmp$Age_group, levels = c("0-14", "15-49", "50-64","65 and more"))


# Fever based on body temp
tmp %<>% mutate(Fever = case_when(Body_temperature >= 37.3 ~ 1, 
                                  Body_temperature < 37.3 ~ 0)
                )
tmp$Fever <- factor(tmp$Fever, levels = c(0, 1))

# Systolic_pressure_binary based on Systolic_pressure
tmp %<>% mutate(Systolic_pressure_low_binary = case_when(Systolic_pressure < 90 ~ 1,
                                                         Systolic_pressure >= 90 ~ 0)
)
tmp$Systolic_pressure_low_binary <- factor(tmp$Systolic_pressure_low_binary, levels = c(0, 1))

# Respiratory_rate_binary based on Respiratory_rate
tmp %<>% mutate(Respiratory_rate_high_binary = case_when(Respiratory_rate > 24 ~ 1,
                                                         Respiratory_rate <= 24 ~ 0)
)
tmp$Respiratory_rate_high_binary <- factor(tmp$Respiratory_rate_high_binary, levels = c(0, 1))


# White_blood_cells_low_binary based on White_blood_cells
tmp %<>% mutate(White_blood_cells_low_binary = case_when(White_blood_cells < 4 ~ 1,
                                                         White_blood_cells >= 4 ~ 0)
)
tmp$White_blood_cells_low_binary <- factor(tmp$White_blood_cells_low_binary, levels = c(0, 1))

# White_blood_cells_high_binary based on White_blood_cells
tmp %<>% mutate(White_blood_cells_high_binary = case_when(White_blood_cells > 10 ~ 1,
                                                          White_blood_cells <= 10 ~ 0)
)
tmp$White_blood_cells_high_binary <- factor(tmp$White_blood_cells_high_binary, levels = c(0, 1))


# NLR_high_binary based on NLR
tmp %<>% mutate(NLR_high_binary = case_when(NLR > 4 ~ 1,
                                            NLR <= 4 ~ 0)
)
tmp$NLR_high_binary <- factor(tmp$NLR_high_binary, levels = c(0, 1))

##########################################################################################################################################################
# Table 1: Demographic and clinical characteristics of laboratory-confirmed COVID-19 patients categorized on admission by disease severity. 
##########################################################################################################################################################
dependent1 = "Disease_severity_group"

explanatory1 <- c("Age",
                 "Age_group",
                 "Sex", 
                 "Kazakh_ethnicity",
                 "BMI",
                 "Days_from_symptom_onset_to_admission",
                 "Days_in_hospital",
                 "Deaths",
                 "Any_comorbidities", 
                 "Fever",
                 "Cough",
                 "Systolic_pressure",
                 "Systolic_pressure_low_binary",
                 "Respiratory_rate", 
                 "Respiratory_rate_high_binary", 
                 "SpO2",
                 "White_blood_cells",
                 "White_blood_cells_low_binary",
                 "White_blood_cells_high_binary",
                 "NLR", 
                 "NLR_high_binary",
                 "Haemoglobin",
                 "Prothrombin_time",
                 "Aspartate_aminotransferase",
                 "Total_bilirubin",
                 "Creatinine",
                 "C_reactive_protein",
                 "Pneumonia",
                 "Bronchitis",
                 "Antiviral_medications",
                 "Antibiotics",
                 "Anticoagulant",
                 "Corticosteroids",
                 "Oxygen_therapy",
                 "Mechanical_ventilation"
                 )


tmp %>% summary_factorlist(dependent1, explanatory1, p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F) -> table1
# export table1 as excel file
rio::export(table1, "~/Table1.Demographic and clinical characteristics of laboratory-confirmed COVID-19 patients categorized on admission by disease severity.xlsx")

##########################################################################################################################################################
# Table 2: Demographic and clinical characteristics of laboratory-confirmed COVID-19 patients, who had survived (survivors) or died (non-survivors) by April 30 2020.
##########################################################################################################################################################
dependent2 = "Deaths"

explanatory2 <- c("Age",
                 "Age_group",
                 "Sex", 
                 "Kazakh_ethnicity",
                 "BMI",
                 "Days_from_symptom_onset_to_admission",
                 "Days_in_hospital",
                 "Any_comorbidities", 
                 "Fever",
                 "Cough",
                 "Systolic_pressure",
                 "Systolic_pressure_low_binary",
                 "Respiratory_rate", 
                 "Respiratory_rate_high_binary", 
                 "SpO2",
                 "White_blood_cells",
                 "White_blood_cells_low_binary",
                 "White_blood_cells_high_binary",
                 "NLR", 
                 "NLR_high_binary",
                 "Haemoglobin",
                 "Prothrombin_time",
                 "Aspartate_aminotransferase",
                 "Direct_bilirubin",
                 "Creatinine",
                 "C_reactive_protein",
                 "Pneumonia",
                 "Bronchitis",
                 "Antiviral_medications",
                 "Antibiotics",
                 "Anticoagulant",
                 "Corticosteroids",
                 "Oxygen_therapy",
                 "Mechanical_ventilation"
                 )

tmp %>% summary_factorlist(dependent2, explanatory2, p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F) -> table2
# export table2 as excel file
rio::export(table2, "~/Table2.Demographic and clinical characteristics of laboratory-confirmed COVID-19 patients, who had survived (survivors) or died (non-survivors) by April 30 2020.xlsx")

##########################################################################################################################################################
# Preparation for Tables 3 and 4 -- Bivariate logistic regression of factors associated with the odds of severe and mortality COVID-19 disease  in Kazakhstan.
##########################################################################################################################################################

# 2 Groups based on  Disease_severity (1 and 2 into non-severe; 3 and 4 -> into severe)

tmp %<>% mutate(Disease_severity_group_binary = case_when(Disease_severity %in% c(1,2) ~ "non-severe", 
                                                          Disease_severity %in% c(3,4) ~ "severe"))

tmp$Disease_severity_group_binary <- factor(tmp$Disease_severity_group_binary, levels = c("non-severe","severe"))

############
# We have now specified in the methods section that the Box-Tidwell Test was used to assess the linearity of continuous predictors 
# and the logit in our models. Most of the continuous variables exhibited a linear relationship with the logit, except four 
# (AST, total bilirubin, direct bilirubin and creatinine). 
# These four variables were entered into the models as categorical using clinical cut-off values for normal laboratory ranges.
############

tmp %<>% mutate(Aspartate_aminotransferase_binary = case_when(Aspartate_aminotransferase > 40 ~ 1, TRUE ~ 0)); 
tmp$Aspartate_aminotransferase_binary = factor(tmp$Aspartate_aminotransferase_binary)

tmp %<>% mutate(Total_bilirubin_binary = case_when(Total_bilirubin > 17 ~ 1, TRUE ~ 0)); 
tmp$Total_bilirubin_binary = factor(tmp$Total_bilirubin_binary)

tmp %<>% mutate(Direct_bilirubin_binary = case_when(Direct_bilirubin > 5.1 ~ 1, TRUE ~ 0)) 
tmp$Direct_bilirubin_binary = factor(tmp$Direct_bilirubin_binary)

tmp %<>% mutate(Creatinine_binary = case_when(Creatinine > 118 ~ 1, TRUE ~ 0)); 
tmp$Creatinine_binary = factor(tmp$Creatinine_binary)

##########################################################################################################################################################
# Table 3: Bivariate logistic regression of factors associated with the odds of severe COVID-19 disease in Kazakhstan.
##########################################################################################################################################################

dependent3 <- "Disease_severity_group_binary"

explanatory3 = c(
                  "Age",
                  "Sex",
                  "Kazakh_ethnicity",
                  "Any_comorbidities",
                  "White_blood_cells",
                  "NLR",
                  "Haemoglobin",
                  "Prothrombin_time",
                  "Fibrinogen",
                  "Albumin",
                  "Aspartate_aminotransferase_binary",
                  "Total_bilirubin_binary",
                  "Glucose",
                  "Blood_urea_nitrogen",
                  "Creatinine_binary",
                  "C_reactive_protein",
                  "Potassium",
                  "Calcium"
                  )
  
  
explanatory_multi3 = c(
                      "Age",
                      "Kazakh_ethnicity",
                      "Any_comorbidities",
                      "White_blood_cells"
                      )

tmp %>% finalfit::finalfit(dependent3, explanatory3, explanatory_multi3) -> table3

# export table3 as excel file
rio::export(table3, "~/Table3.Bivariate logistic regression of factors associated with the odds of severe COVID-19 disease in Kazakhstan.xlsx")

##########################################################################################################################################################
# Table 4: Bivariate logistic regression of factors associated with COVID-19 mortality in Kazakhstan.
##########################################################################################################################################################

dependent4 = "Deaths"

explanatory4 = c(
                  "Age",
                  "Sex",
                  "Kazakh_ethnicity",
                  "Any_comorbidities",
                  "White_blood_cells",
                  "Platelets",
                  "Fibrinogen",
                  "Albumin",
                  "Aspartate_aminotransferase_binary",
                  "Direct_bilirubin_binary",
                  "Glucose",
                  "Blood_urea_nitrogen",
                  "Creatinine_binary",
                  "Sodium"
                )
  


explanatory_multi4 = c(
                        "Age",
                        "Sex"
                      )

tmp %>% finalfit::finalfit(dependent4, explanatory4, explanatory_multi4) -> table4

# export table3 as excel file
rio::export(table4, "~/Table4.Bivariate logistic regression of factors associated with COVID-19 mortality in Kazakhstan.xlsx")


##########################################################################################################################################################
# Supplementary material :: Correlations among clinical (A) and laboratory (B) findings in COVID-19 patients from Kazakhstan.
##########################################################################################################################################################

symptoms = c(
  "Body_temperature", 
  "Cough", 
  "Sputum_production", 
  "Shortness_of_breath", 
  "Dyspnoea", 
  "Stuffy", 
  "Sore_throat", 
  "Oropharynx_hyperemia", 
  "Tonsill_hypertrophy",
  "Chest_pain", 
  "Chest_tightness", 
  "Wheezing", 
  "Diarrhoea", 
  "Nausea_vomiting", 
  "Headache", 
  "Conjuctivitis", 
  "Myalgia_fatigue", 
  "Joint_pain", 
  "Pulse",
  "Systolic_pressure", 
  "Diastolic_pressure", 
  "Respiratory_rate", 
  "SpO2")


cor_mat <- db %>% dplyr::select(dplyr::all_of(symptoms)) %>%  
  mutate_if(is.factor, as.numeric) %>% 
  data.frame()%>% 
  rstatix::cor_mat(method = "spearman", alternative = "two.sided", conf.level = 0.95)

cor_lower_tri <- cor_mat %>% rstatix::cor_reorder() %>% rstatix::pull_lower_triangle()

my.palette <- ggpubr::get_palette(c("#00468BFF", "white", "#AD002AFF"), 20)

#cor_lower_tri %>% rstatix::cor_plot(type = "lower",palette = my.palette, font.label = list(size = 6, color = "black", style = "bold"))

jpeg("~/pairwise Spearman correlation on symptoms.jpeg", units="in", width=10, height=10, res=300)
cor_lower_tri %>% rstatix::cor_plot(type = "lower",palette = my.palette, font.label = list(size = 6, color = "black", style = "bold")) 
dev.off()



lab_data = c(
             "White_blood_cells", 
             "Neutrophil", 
             "Lymphocyte", 
             "NLR", 
             "Haemoglobin", 
             "Monocytes", 
             "Eosinophils", 
             "Platelets", 
             "Prothrombin_time", 
             "Fibrinogen", 
             "Albumin",
             "Alanine_aminotransferase", 
             "Aspartate_aminotransferase", 
             "Total_bilirubin",
             "Direct_bilirubin", 
             "Glucose", 
             "Blood_urea_nitrogen",
             "Creatinine", 
             "C_reactive_protein", 
             "Sodium", 
             "Potassium", 
             "Calcium"
             )

cor_mat <- db %>% dplyr::select(dplyr::all_of(lab_data)) %>% data.frame() %>% rstatix::cor_mat(method = "spearman", alternative = "two.sided", conf.level = 0.95)
cor_lower_tri <- cor_mat %>% rstatix::cor_reorder() %>% rstatix::pull_lower_triangle()
my.palette <- ggpubr::get_palette(c("#00468BFF", "white", "#AD002AFF"), 20)

#cor_lower_tri %>% rstatix::cor_plot(type = "lower",palette = my.palette, font.label = list(size = 6, color = "black", style = "bold")) 

jpeg("~/pairwise Spearman correlation on lab data.jpeg", units="in", width=10, height=10, res=300)
cor_lower_tri %>% rstatix::cor_plot(type = "lower",palette = my.palette, font.label = list(size = 6, color = "black", style = "bold")) 
dev.off()


##########################################################################################################################################################
# Supplementary material :: Principal component analysis of signs and symptoms in COVID-19 patients from Kazakhstan.
##########################################################################################################################################################
#data preparation

symptoms = c(
            "Body_temperature", 
            "Cough", 
            "Sputum_production", 
            "Shortness_of_breath", 
            "Dyspnoea", 
            "Stuffy", 
            "Sore_throat", 
            "Oropharynx_hyperemia", 
            "Tonsill_hypertrophy",
            "Chest_pain", 
            "Chest_tightness", 
            "Wheezing", 
            "Diarrhoea", 
            "Nausea_vomiting", 
            "Headache", 
            "Conjuctivitis", 
            "Myalgia_fatigue", 
            "Joint_pain", 
            "Pulse",
            "Respiratory_rate"
            )
  


tmp <- db %>% dplyr::select(c(Deaths, Disease_severity, dplyr::all_of(symptoms))) %>% 
              dplyr::mutate_if(is.factor, as.character) %>% 
              dplyr::mutate_if(is.character, as.numeric) 

tmp %<>% mutate(Disease_severity_group_binary = case_when(Disease_severity %in% c(1,2) ~ "non-severe", 
                                                          Disease_severity %in% c(3,4) ~ "severe"))

tmp %<>% dplyr::select(-Disease_severity)

tmp$Deaths = factor(tmp$Deaths, levels = c(0,1))
tmp$Disease_severity_group_binary = factor(tmp$Disease_severity_group_binary, levels = c("non-severe", "severe"))

tmp %<>% dplyr::relocate(Disease_severity_group_binary, .before = Deaths)

# remove outliers from "Body_temperature","Pulse", "Systolic_pressure", "Diastolic_pressure", "Respiratory_rate", "SpO2"  as they are true numeric data
for (i in c(3,21:22)){
  tmp %>% dplyr::select(dplyr::all_of(i)) %>% rstatix::identify_outliers() %>% filter(is.outlier == T) %>% dplyr::select(1) %>% tibble::deframe() -> nn
  if(length(nn)!=0){tmp[tmp[,i] %in% nn, i] = NA}
  remove(nn)
}

# columns with less than 40% missing data
tmp_nm <- tmp %>% is.na() %>% colSums() < nrow(tmp)*0.4 
tmp_nm <- which(tmp_nm == T) %>% names() 

# filter columns based on missing data % (< 40%)
tmp <- tmp %>% dplyr::select(dplyr::all_of(tmp_nm))

# Rename first two columns
colnames(tmp)[1:2] = c("Odds of severe", "Mortality")
# Rename levels in first two columns

levels(tmp$`Odds of severe`)[levels(tmp$`Odds of severe`)=="non-severe"] <- "Mild/Moderate"
levels(tmp$`Odds of severe`)[levels(tmp$`Odds of severe`)=="severe"] <- "Severe/Critical"
levels(tmp$Mortality)[levels(tmp$Mortality)=="0"] <- "Survivors"
levels(tmp$Mortality)[levels(tmp$Mortality)=="1"] <- "Non-survivors"

library("FactoMineR")
# imputation using imputePCA function of missMDA R package
tmp_imp =  missMDA::imputePCA(tmp, quali.sup = 1:2, scale = T, seed = 2021)

#PCA analysis using FactoMineR R package
res.pca <- FactoMineR::PCA(X = tmp_imp$completeObs, scale.unit = T, quali.sup = 1:2, graph = FALSE)


library("factoextra")
# Scree plot of PCA results
p_scree <- factoextra::fviz_eig(res.pca, addlabels = F, ylim = c(0, 23), ncp = 50,  barfill = "#00468BFF", ggtheme = theme_classic())+ 
          theme(axis.title.x=element_blank())

# Correlation plot of PCA results
var <- factoextra::get_pca_var(res.pca)
p_corr <- ggcorrplot::ggcorrplot(var$cos2, method = "circle", ggtheme = theme_classic(), colors = c("#00468BFF","white", "#AD002AFF"), tl.cex = 10) 

# PCA Biplot of combinations of the first three dimensions
combn(1:3,2) -> cmbn
p_severity = list()
p_mortality = list()

for (i in 1:3){
  
  if (i == 3){
    p_severity[[i]] = factoextra::fviz_pca_biplot(res.pca, axes = cmbn[,i], geom.ind = "point", habillage= 1, pointsize = 1.6, invisible = "var", palette = "lanonc", addEllipses = TRUE, title = NULL) + theme(panel.background = element_rect(fill = NULL),panel.grid.major = element_line(colour = "white"),panel.grid.minor =  element_line(colour = "white"),legend.position= "right") + ggsci::scale_color_lancet()
    
    p_mortality[[i]] = factoextra::fviz_pca_biplot(res.pca, axes = cmbn[,i], geom.ind = "point", habillage= 2, pointsize = 1.6, invisible = "var", palette = "lanonc", addEllipses = TRUE, title = NULL) + theme(panel.background = element_rect(fill = NULL),panel.grid.major = element_line(colour = "white"),panel.grid.minor =  element_line(colour = "white"),legend.position="right") + ggsci::scale_color_lancet()  
    
  } else {
    p_severity[[i]] = factoextra::fviz_pca_biplot(res.pca, axes = cmbn[,i], geom.ind = "point", habillage= 1, pointsize = 1.6, invisible = "var", palette = "lanonc", addEllipses = TRUE, title = NULL) + theme(panel.background = element_rect(fill = NULL),panel.grid.major = element_line(colour = "white"),panel.grid.minor =  element_line(colour = "white"),legend.position= "none") + ggsci::scale_color_lancet()
    
    p_mortality[[i]] = factoextra::fviz_pca_biplot(res.pca, axes = cmbn[,i], geom.ind = "point", habillage= 2, pointsize = 1.6, invisible = "var", palette = "lanonc", addEllipses = TRUE, title = NULL) + theme(panel.background = element_rect(fill = NULL),panel.grid.major = element_line(colour = "white"),panel.grid.minor =  element_line(colour = "white"),legend.position="none") + ggsci::scale_color_lancet()
  }
}

library(patchwork)

patchwork <- (p_scree|p_corr)/
  (p_severity[[1]]|p_severity[[2]]|p_severity[[3]])/
  (p_mortality[[1]]|p_mortality[[2]]|p_mortality[[3]])


patchwork + plot_annotation(tag_levels = 'A')

jpeg("~/PCA analysis on clinics.jpeg", units="in",  height=13, width=15,res=600)
patchwork + plot_annotation(tag_levels = 'A')
dev.off()

##########################################################################################################################################################
# Supplementary material :: Principal component analysis of signs and symptoms in COVID-19 patients from Kazakhstan.
##########################################################################################################################################################
#data preparation

lab_data = c(
            "White_blood_cells", 
            "Neutrophil", 
            "Lymphocyte", 
            "NLR", 
            "Haemoglobin", 
            "Monocytes", 
            "Eosinophils", 
            "Platelets", 
            "Prothrombin_time", 
            "Fibrinogen", 
            "Albumin",
            "Alanine_aminotransferase", 
            "Aspartate_aminotransferase", 
            "Total_bilirubin",
            "Direct_bilirubin", 
            "Glucose", 
            "Blood_urea_nitrogen",
            "Creatinine", 
            "C_reactive_protein", 
            "Sodium", 
            "Potassium", 
            "Calcium"
          )


tmp <- db %>% dplyr::select(c(Deaths, Disease_severity, dplyr::all_of(lab_data)))

tmp %<>% mutate(Disease_severity_group_binary = case_when(Disease_severity %in% c(1,2) ~ "non-severe", 
                                                          Disease_severity %in% c(3,4) ~ "severe"))

tmp %<>% dplyr::select(-Disease_severity)

tmp$Deaths = factor(tmp$Deaths, levels = c(0,1))
tmp$Disease_severity_group_binary = factor(tmp$Disease_severity_group_binary, levels = c("non-severe", "severe"))

tmp %<>% dplyr::relocate(Disease_severity_group_binary, .before = Deaths)


# detect and remove outliers 
for (i in c(3:ncol(tmp))){
  tmp %>% dplyr::select(i) %>% rstatix::identify_outliers() %>% dplyr::filter(is.outlier == T) %>% dplyr::select(1) %>% tibble::deframe() -> nn
  if(length(nn)!=0){tmp[tmp[,i] %in% nn, i] = NA}
  remove(nn)
}

# Scale
for (i in c(3:ncol(tmp))){
  tmp[,i] = tmp %>% dplyr::select(i) %>% tibble::deframe() %>% scales::rescale()
}

# columns with less than 40% missing data
tmp_nm <- tmp %>% is.na() %>% colSums() < nrow(tmp)*0.4 
tmp_nm <- which(tmp_nm == T) %>% names() 

# filter columns based on missing data % (< 40%)
tmp <- tmp %>% dplyr::select(dplyr::all_of(tmp_nm))

# Rename first two columns
colnames(tmp)[1:2] = c("Odds of severe", "Mortality")
# Rename levels in first two columns

levels(tmp$`Odds of severe`)[levels(tmp$`Odds of severe`)=="non-severe"] <- "Mild/Moderate"
levels(tmp$`Odds of severe`)[levels(tmp$`Odds of severe`)=="severe"] <- "Severe/Critical"
levels(tmp$Mortality)[levels(tmp$Mortality)=="0"] <- "Survivors"
levels(tmp$Mortality)[levels(tmp$Mortality)=="1"] <- "Non-survivors"

library("FactoMineR")
# imputation using imputePCA function of missMDA R package
tmp_imp =  missMDA::imputePCA(tmp, quali.sup = 1:2, scale = F, seed = 2021)

#PCA analysis using FactoMineR R package
res.pca <- FactoMineR::PCA(X = tmp_imp$completeObs, scale.unit = F, quali.sup = 1:2, graph = FALSE)


library("factoextra")
# Scree plot of PCA results
p_scree <- factoextra::fviz_eig(res.pca, addlabels = F, ylim = c(0, 35), ncp = 50,  barfill = "#00468BFF", ggtheme = theme_classic())+ 
  theme(axis.title.x=element_blank())

# Correlation plot of PCA results
var <- factoextra::get_pca_var(res.pca)
p_corr <- ggcorrplot::ggcorrplot(var$cos2, method = "circle", ggtheme = theme_classic(), colors = c("#00468BFF","white", "#AD002AFF"), tl.cex = 10) 

# PCA Biplot of combinations of the first three dimensions
combn(1:3,2) -> cmbn
p_severity = list()
p_mortality = list()

for (i in 1:3){
  
  if (i == 3){
    p_severity[[i]] = factoextra::fviz_pca_biplot(res.pca, axes = cmbn[,i], geom.ind = "point", habillage= 1, pointsize = 1.6, invisible = "var", palette = "lanonc", addEllipses = TRUE, title = NULL) + theme(panel.background = element_rect(fill = NULL),panel.grid.major = element_line(colour = "white"),panel.grid.minor =  element_line(colour = "white"),legend.position= "right") + ggsci::scale_color_lancet()
    
    p_mortality[[i]] = factoextra::fviz_pca_biplot(res.pca, axes = cmbn[,i], geom.ind = "point", habillage= 2, pointsize = 1.6, invisible = "var", palette = "lanonc", addEllipses = TRUE, title = NULL) + theme(panel.background = element_rect(fill = NULL),panel.grid.major = element_line(colour = "white"),panel.grid.minor =  element_line(colour = "white"),legend.position="right") + ggsci::scale_color_lancet()  
    
  } else {
    p_severity[[i]] = factoextra::fviz_pca_biplot(res.pca, axes = cmbn[,i], geom.ind = "point", habillage= 1, pointsize = 1.6, invisible = "var", palette = "lanonc", addEllipses = TRUE, title = NULL) + theme(panel.background = element_rect(fill = NULL),panel.grid.major = element_line(colour = "white"),panel.grid.minor =  element_line(colour = "white"),legend.position= "none") + ggsci::scale_color_lancet()
    
    p_mortality[[i]] = factoextra::fviz_pca_biplot(res.pca, axes = cmbn[,i], geom.ind = "point", habillage= 2, pointsize = 1.6, invisible = "var", palette = "lanonc", addEllipses = TRUE, title = NULL) + theme(panel.background = element_rect(fill = NULL),panel.grid.major = element_line(colour = "white"),panel.grid.minor =  element_line(colour = "white"),legend.position="none") + ggsci::scale_color_lancet()
  }
}

library(patchwork)

patchwork <- (p_scree|p_corr)/
  (p_severity[[1]]|p_severity[[2]]|p_severity[[3]])/
  (p_mortality[[1]]|p_mortality[[2]]|p_mortality[[3]])


patchwork + plot_annotation(tag_levels = 'A')

jpeg("~/PCA analysis on lab data.jpeg", units="in",  height=10, width=11.5 ,res=600)
patchwork + plot_annotation(tag_levels = 'A')
dev.off()


##########################################################################################################################################################
# Supplementary material :: Estimated log-transformed odds ratios for COVID-19-related severe disease and in-hospital death by age
##########################################################################################################################################################

# data preparation
tmp <- db 

tmp %<>% mutate(Disease_severity_group_binary = case_when(Disease_severity %in% c(1,2) ~ "non-severe", 
                                                          Disease_severity %in% c(3,4) ~ "severe"))

tmp$Disease_severity_group_binary <- factor(tmp$Disease_severity_group_binary, levels = c("non-severe","severe"))

# the removal of extreme values
tmp %<>% dplyr::filter(Age != 0 & Age != 100)

# model fitting
# severity model is based on Age, Kazakh_ethnicity, Any_comorbidities and White_blood_cells
# mortality model is based on Age and Sex

fit_severity <- glm(Disease_severity_group_binary ~ splines::bs(Age, knots=c(15,50,65),Boundary.knots=c(1,99),degre=1) + Kazakh_ethnicity + Any_comorbidities + White_blood_cells, data = tmp, family = binomial())
fit_mortality <- glm(Deaths ~ splines::bs(Age, knots=c(15,50,65),Boundary.knots=c(1,99),degre=1) + Sex, data = tmp, family = binomial())

# Normalize odds ratio to 15-49 age group
OR_severity = exp(coef(fit_severity))[2:5] %>% as.vector()/exp(coef(fit_severity))[3] %>% as.vector()
OR_mortality = exp(coef(fit_mortality))[2:5] %>% as.vector()/exp(coef(fit_mortality))[3] %>% as.vector()

# C-statistics of fitted models
DescTools::Cstat(fit_severity)
DescTools::Cstat(fit_mortality)
# Plot
library(scales)
library(ggsci)

ORdf = tibble(Age = c(1, 32.5, 57.5, 99), ORadj = OR_severity)

p <- ORdf %>% ggplot() + geom_line(aes(x=Age, y=ORadj), size = 1, color = "#00468BFF") 

p <- p + geom_vline(xintercept = c(15, 50, 65), linetype="dotted", color = "gray", size=0.5)+ geom_hline(yintercept = c(1), linetype="dotted", color = "gray", size=0.5)

p_severity_OR <- p + scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  theme_classic() + xlab("Age in years") + ylab("COVID-19 severity OR \ncompared to 15-49 age group")


ORdf = tibble(Age = c(1, 32.5, 57.5, 99), ORadj = OR_mortality)

p <- ORdf %>% ggplot() + geom_line(aes(x=Age, y=ORadj), size = 1, color = "#AD002AFF") 

p <- p + geom_vline(xintercept = c(15, 50, 65), linetype="dotted", color = "gray", size=0.5)+ geom_hline(yintercept = c(1), linetype="dotted", color = "gray", size=0.5)

p_mortality_OR <- p + scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  theme_classic() + xlab("Age in years") + ylab("COVID-19 mortality OR \ncompared to 15-49 age group")

library(patchwork)
patchwork <- p_severity_OR / p_mortality_OR
patchwork + plot_annotation(tag_levels = 'A')

jpeg("~/OR for severity and mortality according age.jpeg", units="in",  height=8.5, width=8 ,res=600)
patchwork + plot_annotation(tag_levels = 'A')
dev.off()


##########################################################################################################################################################
# Supplementary material :: Risk factors associated with COVID-19 severity for patients in Kazakhstan in a generalized linear model adjusted for clinical site.
##########################################################################################################################################################

# data preparation
tmp <- db 

tmp %<>% mutate(Disease_severity_group_binary = case_when(Disease_severity %in% c(1,2) ~ "non-severe", 
                                                          Disease_severity %in% c(3,4) ~ "severe"))

tmp$Disease_severity_group_binary <- factor(tmp$Disease_severity_group_binary, levels = c("non-severe","severe"))
# model fitting
fit_severity <- glm(Disease_severity_group_binary ~ Age + Kazakh_ethnicity + Any_comorbidities + White_blood_cells, data = tmp, family = binomial())
fit_mortality <- glm(Deaths ~ Age + Sex, data = tmp, family = binomial())

# model characteristics
broom::tidy(fit_severity)
broom::tidy(fit_mortality)

##########################################################################################################################################################
# Supplementary material :: Body mass index (BMI) imputation
##########################################################################################################################################################
# data preparation
tmp <- db
# Body mass index (BMI) imputation
tmp_imp <- missRanger::missRanger(data = tmp, 
                                  formula = BMI ~ 
                                    Age+
                                    Sex + 
                                    Kazakh_ethnicity + 
                                    Hypertension + 
                                    Coronary_heart_disease + 
                                    COPD + 
                                    Chronic_kidney_disease +
                                    Cancer + 
                                    Diabetes + 
                                    Liver_disease + 
                                    Othercomorb + 
                                    Cough +
                                    Sputum_production +
                                    Shortness_of_breath +
                                    Dyspnoea +
                                    Stuffy +
                                    Sore_throat +
                                    Oropharynx_hyperemia +
                                    Tonsill_hypertrophy +
                                    Chest_pain +
                                    Chest_tightness +
                                    Wheezing + 
                                    Diarrhoea + 
                                    Nausea_vomiting +
                                    Headache +
                                    Conjuctivitis + 
                                    Myalgia_fatigue +
                                    Joint_pain,
                                seed = 2021)

tmp_imp$BMI %<>% round(digits = 0)

# Cohort BMI statistics prior to imputation:  
tmp$BMI %>% summary()
# Cohort BMI statistics post-imputation:
tmp_imp$BMI %>% summary()

# Disease_severity_group
tmp_imp%<>% mutate(Disease_severity_group = case_when(Disease_severity == 1 ~ "Asymptomatic_Mild", 
                                                   Disease_severity == 2 ~ "Moderate", 
                                                   Disease_severity %in% c(3,4) ~ "Severe_critical")
)
tmp_imp$Disease_severity_group <- factor(tmp_imp$Disease_severity_group, levels = c("Asymptomatic_Mild","Moderate","Severe_critical"))


##########################################################################################################################################################
# Supplementary material :: Imputed BMI of laboratory-confirmed COVID-19 patients categorized on admission by disease severity
##########################################################################################################################################################
table5 <- rbind(
  tmp_imp %>% finalfit::summary_factorlist(dependent = "Disease_severity_group", explanatory = "BMI", p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F), 
  tmp_imp %>% filter(BMI < 18.5) %>% finalfit::summary_factorlist(dependent = "Disease_severity_group", explanatory = "BMI", p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F),
  tmp_imp %>% filter(BMI >= 18.5 & BMI < 25) %>% finalfit::summary_factorlist(dependent = "Disease_severity_group", explanatory = "BMI", p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F),
  tmp_imp %>% filter(BMI >= 25 & BMI < 30) %>% finalfit::summary_factorlist(dependent = "Disease_severity_group", explanatory = "BMI", p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F),
  tmp_imp %>% filter(BMI >= 30) %>% finalfit::summary_factorlist(dependent = "Disease_severity_group", explanatory = "BMI", p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F)
)

table5[2:5,1]  = c("less than 18.5", "18.5-24.9", "25-29.9 (overweight)", "more than 30 (obese)")
# export table1 as excel file
rio::export(table5, "~/Imputed BMI of laboratory-confirmed COVID-19 patients categorized on admission by disease severity.xlsx")



table6 <- rbind(
  tmp_imp %>% finalfit::summary_factorlist(dependent = "Deaths", explanatory = "BMI", p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F), 
  tmp_imp %>% filter(BMI < 18.5) %>% finalfit::summary_factorlist(dependent = "Deaths", explanatory = "BMI", p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F),
  tmp_imp %>% filter(BMI >= 18.5 & BMI < 25) %>% finalfit::summary_factorlist(dependent = "Deaths", explanatory = "BMI", p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F),
  tmp_imp %>% filter(BMI >= 25 & BMI < 30) %>% finalfit::summary_factorlist(dependent = "Deaths", explanatory = "BMI", p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F),
  tmp_imp %>% filter(BMI >= 30) %>% finalfit::summary_factorlist(dependent = "Deaths", explanatory = "BMI", p = T, cont = "median", total_col = TRUE, column = TRUE, na_include = F)
)

table6[2:5,1]  = c("less than 18.5", "18.5-24.9", "25-29.9 (overweight)", "more than 30 (obese)")
# export table1 as excel file
rio::export(table6, "~/Imputed BMI of laboratory-confirmed COVID-19 patients, who had survived (survivors) or died (non-survivors) .xlsx")


##########################################################################################################################################################
# Supplementary material :: Risk factors associated with COVID-19 severity for patients in Kazakhstan in a generalized linear model adjusted for clinical site.
##########################################################################################################################################################

tmp_imp %<>% mutate(Disease_severity_group_binary = case_when(Disease_severity %in% c(1,2) ~ "non-severe", 
                                                          Disease_severity %in% c(3,4) ~ "severe"))

tmp_imp$Disease_severity_group_binary <- factor(tmp_imp$Disease_severity_group_binary, levels = c("non-severe", "severe"))


tmp_imp %<>% mutate(BMI_group = case_when(BMI < 25 ~ "Ref",
                                          BMI >= 25 & BMI < 30 ~ "25-30 (overweight)",
                                          BMI >= 30  ~ "30 and more (obese)")
)

tmp_imp$BMI_group <- factor(tmp_imp$BMI_group, levels = c("Ref", "25-30 (overweight)", "30 and more (obese)"))                                                            

explanatory <- c("Age","Sex",  "Any_comorbidities", "White_blood_cells", "BMI_group")
explanatory_multi <- c("Age", "Any_comorbidities", "White_blood_cells", "BMI_group")
dependent <- "Disease_severity_group_binary"

table7 <- tmp_imp %>% finalfit::finalfit(dependent, explanatory, explanatory_multi) 
# export table
rio::export(table7, "~/Bivariate logistic regression of factors associated with the odds of severe COVID-19 disease in Kazakhstan using imputed BMI values.xlsx")


explanatory <- c("Age","BMI_group")
explanatory_multi <- c("Age", "BMI_group")
dependent <- "Deaths"

table8 <- tmp_imp %>% finalfit::finalfit(dependent, explanatory, explanatory_multi)
# export table
rio::export(table8, "~/Bivariate logistic regression of factors associated with COVID-19 mortality in Kazakhstan using imputed BMI values.xlsx")

##########################################################################################################################################################
# Supplementary material :: Sensitivity analyses excluding patients classified as having a mild degree of COVID-19 severity despite the presence of radiologically confirmed pneumonia (n=69)..
##########################################################################################################################################################

# Below are the results of the logistic regression analysis done on n=1003 patients.
tmp <- db
rxcl <- tmp %>% filter(Disease_severity == 1 & Pneumonia == 1) %>% select(ID) %>% deframe()
tmp %<>% filter(!ID %in% rxcl)


tmp %<>% mutate(Disease_severity_group_binary = case_when(Disease_severity %in% c(1,2) ~ "non-severe", 
                                                              Disease_severity %in% c(3,4) ~ "severe"))

tmp$Disease_severity_group_binary <- factor(tmp$Disease_severity_group_binary, levels = c("non-severe", "severe"))


tmp %<>% mutate(Aspartate_aminotransferase_binary = case_when(Aspartate_aminotransferase > 40 ~ 1, TRUE ~ 0)); 
tmp$Aspartate_aminotransferase_binary = factor(tmp$Aspartate_aminotransferase_binary)

tmp %<>% mutate(Total_bilirubin_binary = case_when(Total_bilirubin > 17 ~ 1, TRUE ~ 0)); 
tmp$Total_bilirubin_binary = factor(tmp$Total_bilirubin_binary)

tmp %<>% mutate(Direct_bilirubin_binary = case_when(Direct_bilirubin > 5.1 ~ 1, TRUE ~ 0)) 
tmp$Direct_bilirubin_binary = factor(tmp$Direct_bilirubin_binary)

tmp %<>% mutate(Creatinine_binary = case_when(Creatinine > 118 ~ 1, TRUE ~ 0)); 
tmp$Creatinine_binary = factor(tmp$Creatinine_binary)


dependent9 <- "Disease_severity_group_binary"

explanatory9 = c(
  "Age",
  "Sex",
  "Kazakh_ethnicity",
  "Any_comorbidities",
  "White_blood_cells",
  "NLR",
  "Haemoglobin",
  "Prothrombin_time",
  "Fibrinogen",
  "Albumin",
  "Aspartate_aminotransferase_binary",
  "Total_bilirubin_binary",
  "Glucose",
  "Blood_urea_nitrogen",
  "Creatinine_binary",
  "C_reactive_protein",
  "Potassium",
  "Calcium"
)


explanatory_multi9 = c(
  "Age",
  "Kazakh_ethnicity",
  "Any_comorbidities",
  "White_blood_cells"
)

# table 9 Bivariate logistic regression of factors associated with the odds of severe COVID-19 disease.
table9 <- tmp %>% finalfit::finalfit(dependent9, explanatory9, explanatory_multi9)

# export table3 as excel file
rio::export(table9, "~/Bivariate logistic regression of factors associated with the odds of severe COVID-19 disease on updated data (excluded the discrepancy between mild degree and pneumonia on Xray n = 69).xlsx")

# table 10 Bivariate logistic regression of factors associated with COVID-19 mortality.

dependent10 = "Deaths"

explanatory10 = c(
  "Age",
  "Sex",
  "Kazakh_ethnicity",
  "Any_comorbidities",
  "White_blood_cells",
  "Platelets",
  "Fibrinogen",
  "Albumin",
  "Aspartate_aminotransferase_binary",
  "Direct_bilirubin_binary",
  "Glucose",
  "Blood_urea_nitrogen",
  "Creatinine_binary",
  "Sodium"
)



explanatory_multi10 = c(
  "Age",
  "Sex"
)

table10 <- tmp %>% finalfit::finalfit(dependent10, explanatory10, explanatory_multi10) 

# export table3 as excel file
rio::export(table10, "~/Bivariate logistic regression of factors associated with COVID-19 mortality on updated data (excluded the discrepancy between mild degree and pneumonia on Xray n = 69).xlsx")
