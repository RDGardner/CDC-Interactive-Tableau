# Ricca Callis
# EN 605.662 Data Visualization
# Project 3 - Interactive Visualization Using Tableau
#Data from CDC 500 Cities Project: https://chronicdata.cdc.gov/browse?category=500+Cities

# Project Instructions:

# I. Purpose:
# A number of software tools have been designed to help users visually explore data and to quickly
# create visualizations or dashboards. Popular off-the-shelf tools include Tableau, Qlikview,
# Spotfire, Microsoft Power BI, MicroStrategy, Birst, and Logi among many others. The purpose of
# this assignment is to use existing software tools to formulate and answer a series of analytical
# questions about a specific dataset of your choice. After writing a number of analytical questions or
# hypotheses, you must create a Tableau dashboard designed to present some of the answers of
# your hypotheses.


# II. Task

# 1. Pick a dataset of a domain that you are interested in. The data should have at least 10
# independent variables and 1,000 rows. If you used such a dataset for Project #2, you are
# welcomed to reuse the same dataset for this project. However, you are not obligated to
# use the same dataset as in Project #2.
# 2. Describe your data in writing by thoroughly explaining the dataset and its properties. In a
# table, please list each of the data elements, briefly describe the meaning of them, show
# the corresponding descriptive statistics (e.g. min, max, average, etc..) and their category
# (e.g. nominal, ordinal, quantitative, etc…).
# 3. List five analytical questions that users examining the data might have.
# 4. By using Tableau (students can download an educational license that will be valid for 1
# year) develop a minimum of three visualizations (i.e. Tableau worksheets) for different
# attributes of the data that can be used to answer some of your analytical questions.
# 5. Student must use a minimum of two different visualization techniques (i.e. bar chart,
# stacked bar, pie chart, line chart, table, etc…). This is, not all visualizations should be the
# same (e.g. bar chart).
# 6. Combine the different visualizations (i.e. the 3+ Tableau worksheets described above)
# into a single Tableau dashboard.
# 7. Connect the different visualizations using at least two global filters. This is, when a user
# updates the filters all the 3+ visualizations within the Tableau dashboard will update.
# 8. Create an extract of the dashboard with embedded data (i.e. export your dashboard as a
# .twbx file).
# 9. Students must (a) submit dashboard as part of their project, (b) publish dashboard into
# Tableau Public, and (c) post a screenshot and description of their dashboard into the
# Project #3 discussion forum in Blackboard.

# Load standard libraries
library(readr)
library(ggplot2)
library(readxl)
library (MPV)
library (olsrr)
library("ggpubr")
library("Hmisc")
library(caret)
library (lattice)
library(leaps)
library(MASS)
library(alr3)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(ggthemes)
library(viridis)
library("plotly")
library(corrplot)
library(maps)
library(alr3)
library(rms)
library(ppcor)
library(ggthemes)
library(data.table)
library(ISLR)
library(tibble)
library(aod)
library(tidyverse)
library(psych)
library(pastecs)
library(summarytools)
library(magrittr)
library(scales)
library(sf)
library(lubridate)
library(ggraph)
library(igraph)
library(reshape)
library(tidygraph)
library(ggthemes)
library(ggExtra)
library(cowplot)
library(maps)
library(highcharter)
library(imputeTS)
library(corrplot)
library(network)
library(sna)
library(visNetwork)
library(threejs)
library(networkD3)
library(ndtv)
library(animation)
library(igraph)
library(Rcpp)
library(ggcorrplot)
library(pivottabler)

#Variables:

# ACCESS2_AdjPrev : prevalence estimate of current lack of health insurance
# ARTHRITIS_AdjPrev : estimate for age-adjusted prevalence of arthritis
# BINGE_AdjPrev : estimate for age-adjusted prevalence of binge drinking
# BPHIGH_AdjPrev : estimate for age-adjusted prevalence of high blood pressure
# BPMED_AdjPrev : estimate for age-adjusted prevalence of taking medicine for high blood pressure control [prevention]
# CANCER_AdjPrev : estimate for age-adjusted prevalence of cancer (excluding skin cancer) among adults aged >=18 years
# CASTHMA_AdjPrev : estimate for age-adjusted prevalence of current asthma
# CHD_AdjPrev : estimate for age-adjusted prevalence of coronary heart disease
# CHECKUP_AdjPrev : estimate for age-adjusted prevalence of visits to doctor for routine checkup within the past year [prevention]
# CHOLSCREEN_AdjPrev : estimate for age-adjusted prevalence of cholesterol screening [prevention]
# COLON_SCREEN_AdjPrev : estimate for age-adjusted prevalence of fecal occult blood test, sigmoidoscopy, or colonoscopy
# COPD_AdjPrev : estimate for age-adjusted prevalence of chronic obstructive pulmonary disease
# COREM_AdjPrev : estimate for age-adjusted prevalence of older adult men aged >=65 years who are up to date on a core set of clinical preventive services: Flu shot past year, PPV shot ever, Colorectal cancer screening [prevention]
# COREW_AdjPrev : same as above, for women [prevention]
# CSMOKING_AdjPrev : estimate for age-adjusted prevalence of current smoking
# DENTAL_AdjPrev : estimate for age-adjusted prevalence of visits to dentist [prevention]
# DIABETES_AdjPrev : estimate for age-adjusted prevalence of diagnosed diabetes
# HIGHCHOL_AdjPrev : estimate for age-adjusted prevalence of high cholesterol
# KIDNEY_AdjPrev : estimate for age-adjusted prevalence of chronic kidney disease
# LPA_AdjPrev : estimate for age-adjusted prevalence of no leisure-time physical activity
# MAMMOUSE_AdjPrev : estimate for age-adjusted prevalence of mammography
# MHLTH_AdjPrev : stimate for age-adjusted prevalence of mental health not good for >=14 days
# OBESITY_AdjPrev : estimate for age-adjusted prevalence of obesity
# PAPTEST_AdjPrev : estimate for age-adjusted prevalence of pap test
# PHLTH_AdjPrev : estimate for age-adjusted prevalence of physical health not good for >=14 days
# SLEEP_AdjPrev : estimate for age-adjusted prevalence of sleeping less than 7 hours
# STROKE_AdjPrev : estimate for age-adjusted prevalence of stroke
# TEETHLOST_AdjPrev : estimate for age-adjusted prevalence of all teeth lost'

# Seg  working directory
#setwd("/Users/riccacallis/Desktop/JHU/Data Science/Data Visualization/Project 3/input")

df<- read_xlsx("/Users/riccacallis/Desktop/JHU/Data Science/Data Visualization/Project 3/input/500_Cities_Cleaned2.xlsx")
features_1<-c("StateAbbr","PlaceName","PlaceFIPS","TractFIPS", "Place_TractID", "Population2010","Geolocation")
# Look at the first six rows
head(df)
# Look at all data & attach it
View(df)
attach(df)

#check column names
names(df)

# UNNECESSARY NOW
# #select only necessary columns
# df2<-data.frame(cbind(df %>% select(.dots = features_1), df %>% select(grep("_CrudePrev", colnames(df)))))
# colnames(df2)[1:7]<-features_1
# head(df2)
# View(df2)
# 
# #get longitude and latitude
# df2$longitude<-sapply(df2$Geolocation, function(x) as.double(gsub("\\)","",strsplit(x,",")[[1]][2])))
# df2$latitude<-sapply(df2$Geolocation, function(x) as.double(gsub("\\(","",strsplit(x,",")[[1]][1])))
# head(df2)
# View(df2)

# UNNECESSARY NOW
# #rename columns to eliminate _CrudePrev from title & add detail
# names(df2)[names(df2) == "BINGE_CrudePrev"] <- "Binge Drinking"
# names(df2)[names(df2) == "CSMOKING_CrudePrev"] <- "Current Smoker"
# names(df2)[names(df2) == "LPA_CrudePrev"] <- "No Physical Activity"
# names(df2)[names(df2) == "OBESITY_CrudePrev"] <- "Obesity"
# names(df2)[names(df2) == "SLEEP_CrudePrev"] <- "Lack Of Sleep"
# names(df2)[names(df2) == "ARTHRITIS_CrudePrev"] <- "Arthritis"
# names(df2)[names(df2) == "CASTHMA_CrudePrev"] <- "Current Asthma"
# names(df2)[names(df2) == "BPHIGH_CrudePrev"] <- "High BP"
# names(df2)[names(df2) == "CANCER_CrudePrev"] <- "Cancer"
# names(df2)[names(df2) == "HIGHCHOL_CrudePrev"] <- "High Cholesterol"
# names(df2)[names(df2) == "KIDNEY_CrudePrev"] <- "Kidney Disease"
# names(df2)[names(df2) == "COPD_CrudePrev"] <- "COPD"
# names(df2)[names(df2) == "CHD_CrudePrev"] <- "Coronary Heart Disease"
# names(df2)[names(df2) == "DIABETES_CrudePrev"] <- "Diabetes"
# names(df2)[names(df2) == "MHLTH_CrudePrev"] <- "Poor Mental Health"
# names(df2)[names(df2) == "PHLTH_CrudePrev"] <- "Poor Physical Health"
# names(df2)[names(df2) == "STROKE_CrudePrev"] <- "Stroke"
# names(df2)[names(df2) == "TEETHLOST_CrudePrev"] <- "Teeth Lost"
# names(df2)[names(df2) == "ACCESS2_CrudePrev"] <- "No Health Insurance"
# names(df2)[names(df2) == "CHOLSCREEN_CrudePrev"] <- "Cholesterol Screen"
# names(df2)[names(df2) == "CHECKUP_CrudePrev"] <- "PCM Checkup"
# names(df2)[names(df2) == "MAMMOUSE_CrudePrev"] <- "Mammogram"
# names(df2)[names(df2) == "DENTAL_CrudePrev"] <- "Dental"
# names(df2)[names(df2) == "COLON_SCREEN_CrudePrev"] <- "Colon Screen"
# names(df2)[names(df2) == "BPMED_CrudePrev"] <- "BP Med"
# names(df2)[names(df2) == "COREM_CrudePrev"] <- "Core Prevention Services Men"
# names(df2)[names(df2) == "COREW_CrudePrev"] <- "Core Prevention Services Women"
# names(df2)[names(df2) == "PAPTEST_CrudePrev"] <- "Pap Test"

#Create category variables
#Unhealthy Behaviors: 
# Binge Drinking, No Physical Activity, Lack Of Sleep, 
# Current Smoker,  Obesity
# 
# #Health Outcomes:
# Arthritis, Current Asthma, High BP, Cancer, High Cholesterol,
# Kidney Disease, COPD, Coronary Heart Disease, Diabetes,
# Poor Mental Health, Poor Physical Health, Teeth Lost, Stroke
# 
# #Prevention:
# No Health Insurance, Cholesterol Screen, PCM Checkup, 
# Mammogram, Dental, Colon Screen, BP Med, 
# Core Prevention Services Men, Core Prevention Services Women,
# Pap Test

# UNNECESSARY NOW
# #Create new variables
# UnhealthyBehaviors<-select(df2,'Binge Drinking', 'No Physical Activity', 
#                            'Lack Of Sleep', 'Current Smoker',  'Obesity')
# HealthOutcomes<-select(df2,'Arthritis', 'Current Asthma', 'High BP', 
#                        'Cancer', 'High Cholesterol', 'Kidney Disease', 
#                        'COPD', 'Coronary Heart Disease', 'Diabetes',
#                        'Poor Mental Health', 'Poor Physical Health',
#                        'Stroke', 'Teeth Lost')
# Prevention<-select(df2,'No Health Insurance', 'Cholesterol Screen',
#                    'PCM Checkup', 'Mammogram', 'Dental',
#                    'Colon Screen', 'BP Med', 'Core Prevention Services Men',
#                    'Core Prevention Services Women', 'Pap Test')
# #Confirm
# head(df2)
# View(df2)
# head(UnhealthyBehaviors)
# head(HealthOutcomes)
# head(Prevention)

#Descriptive Statistics
#Summary Statistics
summary(df)
describe(df)
stat.desc(df)

# UNNECESSARY NOW
# vars<-select(df2, 8:35)
# describe(vars)
# describe(UnhealthyBehaviors)
# describe(HealthOutcomes)
# describe(Prevention)

# Use pipes to look at each measure individually
# National metric stats
# Access2
ACCESS2 <-df %>%
  filter (MeasureId=="ACCESS2") %>%
  select (Data_Value)
summary(ACCESS2)
# Min.   : 4.80  
# 1st Qu.:12.00  
# Median :15.10  
# Mean   :16.31  
# 3rd Qu.:19.40  
# Max.   :43.90
stat.desc(ACCESS2)
# nbr.val       501.0000000
# nbr.null        0.0000000
# nbr.na          0.0000000
# min             4.8000000
# max            43.9000000
# range          39.1000000
# sum          8169.6000000
# median         15.1000000
# mean           16.3065868
# SE.mean         0.2799489
# CI.mean.0.95    0.5500211
# var            39.2640565
# std.dev         6.2661038
# coef.var        0.3842683

# Arthristis
ARTHRITIS <-df%>%
  filter(MeasureId=="ARTHRITIS")%>%
  select(Data_Value)
summary(ARTHRITIS)
# Min.   :13.3  
# 1st Qu.:19.8  
# Median :22.0  
# Mean   :22.1  
# 3rd Qu.:24.2  
# Max.   :33.9  
stat.desc(ARTHRITIS)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          1.330000e+01
# max          3.390000e+01
# range        2.060000e+01
# sum          1.107330e+04
# median       2.200000e+01
# mean         2.210240e+01
# SE.mean      1.489331e-01
# CI.mean.0.95 2.926118e-01
# var          1.111271e+01
# std.dev      3.333574e+00
# coef.var     1.508241e-01


# Binge Drinking
BINGE <-df%>%
  filter(MeasureId=="BINGE")%>%
  select(Data_Value)
summary(BINGE)
# Min.   : 6.20  
# 1st Qu.:16.20  
# Median :17.70  
# Mean   :17.69  
# 3rd Qu.:19.20  
# Max.   :25.40
stat.desc(BINGE)
# nbr.val       501.0000000
# nbr.null        0.0000000
# nbr.na          0.0000000
# min             6.2000000
# max            25.4000000
# range          19.2000000
# sum          8860.8000000
# median         17.7000000
# mean           17.6862275
# SE.mean         0.1168040
# CI.mean.0.95    0.2294871
# var             6.8352299
# std.dev         2.6144273
# coef.var        0.1478228

# High Blood Pressure
BPHIGH <-df%>%
  filter(MeasureId=="BPHIGH")%>%
  select(Data_Value)
summary(BPHIGH)
# Min.   :20.70  
# 1st Qu.:26.80  
# Median :30.00  
# Mean   :30.58  
# 3rd Qu.:33.90  
# Max.   :47.30
stat.desc(BPHIGH)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          2.070000e+01
# max          4.730000e+01
# range        2.660000e+01
# sum          1.532130e+04
# median       3.000000e+01
# mean         3.058144e+01
# SE.mean      2.138572e-01
# CI.mean.0.95 4.201696e-01
# var          2.291319e+01
# std.dev      4.786773e+00
# coef.var     1.565254e-01

# Blood Pressure Medication
BPMED <-df%>%
  filter(MeasureId=="BPMED")%>%
  select(Data_Value)
summary(BPMED)
# Min.   :46.6  
# 1st Qu.:51.9  
# Median :57.0  
# Mean   :56.5  
# 3rd Qu.:60.1  
# Max.   :69.7
stat.desc(BPMED)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          4.660000e+01
# max          6.970000e+01
# range        2.310000e+01
# sum          2.830820e+04
# median       5.700000e+01
# mean         5.650339e+01
# SE.mean      2.205588e-01
# CI.mean.0.95 4.333362e-01
# var          2.437173e+01
# std.dev      4.936773e+00
# coef.var     8.737127e-02

# Cancer
CANCER <-df%>%
  filter(MeasureId=="CANCER")%>%
  select(Data_Value)
summary(CANCER)
# Min.   :4.700  
# 1st Qu.:5.900  
# Median :6.100  
# Mean   :6.058  
# 3rd Qu.:6.400  
# Max.   :6.800 
stat.desc(CANCER)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          4.700000e+00
# max          6.800000e+00
# range        2.100000e+00
# sum          3.035300e+03
# median       6.100000e+00
# mean         6.058483e+00
# SE.mean      1.853264e-02
# CI.mean.0.95 3.641145e-02
# var          1.720729e-01
# std.dev      4.148167e-01
# coef.var     6.846874e-02

# Current Asthma
CASTHMA <-df%>%
  filter(MeasureId=="CASTHMA")%>%
  select(Data_Value)
summary(CASTHMA)
# Min.   : 6.700
# 1st Qu.: 8.600
# Median : 9.400
# Mean   : 9.464
# 3rd Qu.:10.200
# Max.   :14.200
stat.desc(CASTHMA)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          6.700000e+00
# max          1.420000e+01
# range        7.500000e+00
# sum          4.741300e+03
# median       9.400000e+00
# mean         9.463673e+00
# SE.mean      5.298736e-02
# CI.mean.0.95 1.041053e-01
# var          1.406638e+00
# std.dev      1.186018e+00
# coef.var     1.253232e-01

# Coronary Heart Disease
CHD <-df%>%
  filter(MeasureId=="CHD")%>%
  select(Data_Value)
summary(CHD)
# Min.   :3.500  
# 1st Qu.:4.900  
# Median :5.700  
# Mean   :5.676  
# 3rd Qu.:6.400  
# Max.   :8.800 
stat.desc(CHD)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          3.500000e+00
# max          8.800000e+00
# range        5.300000e+00
# sum          2.843600e+03
# median       5.700000e+00
# mean         5.675848e+00
# SE.mean      4.535413e-02
# CI.mean.0.95 8.910816e-02
# var          1.030556e+00
# std.dev      1.015163e+00
# coef.var     1.788566e-01

# Annual PCM Check Ups
CHECKUP <-df%>%
  filter(MeasureId=="CHECKUP")%>%
  select(Data_Value)
summary(CHECKUP)
# Min.   :54.20  
# 1st Qu.:65.90  
# Median :67.90  
# Mean   :68.59  
# 3rd Qu.:72.00  
# Max.   :81.30  
stat.desc(CHECKUP)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          5.420000e+01
# max          8.130000e+01
# range        2.710000e+01
# sum          3.436390e+04
# median       6.790000e+01
# mean         6.859062e+01
# SE.mean      2.088066e-01
# CI.mean.0.95 4.102464e-01
# var          2.184369e+01
# std.dev      4.673724e+00
# coef.var     6.813940e-02

# Cholesterol Screening
CHOLSCREEN <-df%>%
  filter(MeasureId=="CHOLSCREEN")%>%
  select(Data_Value)
summary(CHOLSCREEN)
# Min.   :70.20  
# 1st Qu.:78.10  
# Median :79.80  
# Mean   :79.44  
# 3rd Qu.:81.20  
# Max.   :85.20 
stat.desc(CHOLSCREEN)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          7.020000e+01
# max          8.520000e+01
# range        1.500000e+01
# sum          3.979840e+04
# median       7.980000e+01
# mean         7.943792e+01
# SE.mean      1.187005e-01
# CI.mean.0.95 2.332133e-01
# var          7.058999e+00
# std.dev      2.656878e+00
# coef.var     3.344596e-02

# Colon Screen
COLONSCREEN <-df%>%
  filter(MeasureId=="COLON_SCREEN")%>%
  select(Data_Value)
summary(COLONSCREEN)
# Min.   :43.20  
# 1st Qu.:60.60  
# Median :64.50  
# Mean   :64.14  
# 3rd Qu.:68.00  
# Max.   :77.70 
stat.desc(COLONSCREEN)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          4.320000e+01
# max          7.770000e+01
# range        3.450000e+01
# sum          3.213170e+04
# median       6.450000e+01
# mean         6.413513e+01
# SE.mean      2.583637e-01
# CI.mean.0.95 5.076122e-01
# var          3.344264e+01
# std.dev      5.782961e+00
# coef.var     9.016839e-02

# COPD
COPD <-df%>%
  filter(MeasureId=="COPD")%>%
  select(Data_Value)
summary(COPD)
# Min.   : 3.100  
# 1st Qu.: 5.000  
# Median : 6.100  
# Mean   : 6.256  
# 3rd Qu.: 7.400  
# Max.   :11.300 
stat.desc(COPD)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          3.100000e+00
# max          1.130000e+01
# range        8.200000e+00
# sum          3.134300e+03
# median       6.100000e+00
# mean         6.256088e+00
# SE.mean      7.244776e-02
# CI.mean.0.95 1.423396e-01
# var          2.629588e+00
# std.dev      1.621600e+00
# coef.var     2.592036e-01

# Core Prevening Men
COREM <-df%>%
  filter(MeasureId=="COREM")%>%
  select(Data_Value)
summary(COREM)
# Min.   :19.40  
# 1st Qu.:30.30  
# Median :34.00  
# Mean   :34.04  
# 3rd Qu.:37.90  
# Max.   :53.30 
stat.desc(COREM)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          1.940000e+01
# max          5.330000e+01
# range        3.390000e+01
# sum          1.705580e+04
# median       3.400000e+01
# mean         3.404351e+01
# SE.mean      2.627289e-01
# CI.mean.0.95 5.161887e-01
# var          3.458226e+01
# std.dev      5.880669e+00
# coef.var     1.727398e-01

# Core Prevention Women
COREW <-df%>%
  filter(MeasureId=="COREW")%>%
  select(Data_Value)
summary(COREW)
# Min.   :17.50  
# 1st Qu.:28.60  
# Median :32.00  
# Mean   :31.93  
# 3rd Qu.:34.90  
# Max.   :46.60 
stat.desc(COREW)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          1.750000e+01
# max          4.660000e+01
# range        2.910000e+01
# sum          1.599450e+04
# median       3.200000e+01
# mean         3.192515e+01
# SE.mean      2.200811e-01
# CI.mean.0.95 4.323977e-01
# var          2.426629e+01
# std.dev      4.926082e+00
# coef.var     1.543010e-01

# Current Smoker
CSMOKING <-df%>%
  filter(MeasureId=="CSMOKING")%>%
  select(Data_Value)
summary(CSMOKING)
# Min.   : 7.90  
# 1st Qu.:13.90  
# Median :17.10  
# Mean   :17.26  
# 3rd Qu.:20.40  
# Max.   :29.60  
stat.desc(CSMOKING)
# nbr.val       501.0000000
# nbr.null        0.0000000
# nbr.na          0.0000000
# min             7.9000000
# max            29.6000000
# range          21.7000000
# sum          8648.8000000
# median         17.1000000
# mean           17.2630739
# SE.mean         0.1936283
# CI.mean.0.95    0.3804254
# var            18.7834537
# std.dev         4.3339882
# coef.var        0.2510554

# Dental
DENTAL <-df%>%
  filter(MeasureId=="DENTAL")%>%
  select(Data_Value)
summary(DENTAL)
# Min.   :41.80  
# 1st Qu.:57.70  
# Median :63.40  
# Mean   :63.28  
# 3rd Qu.:68.70  
# Max.   :81.50 
stat.desc(DENTAL)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          4.180000e+01
# max          8.150000e+01
# range        3.970000e+01
# sum          3.170340e+04
# median       6.340000e+01
# mean         6.328024e+01
# SE.mean      3.378076e-01
# CI.mean.0.95 6.636973e-01
# var          5.717111e+01
# std.dev      7.561158e+00
# coef.var     1.194869e-01

# Diabetes
DIABETES <-df%>%
  filter(MeasureId=="DIABETES")%>%
  select(Data_Value)
summary(DIABETES)
# Min.   : 5.60  
# 1st Qu.: 8.60  
# Median :10.30  
# Mean   :10.46  
# 3rd Qu.:12.00  
# Max.   :20.30 
stat.desc(DIABETES)
# nbr.val       501.0000000
# nbr.null        0.0000000
# nbr.na          0.0000000
# min             5.6000000
# max            20.3000000
# range          14.7000000
# sum          5238.7000000
# median         10.3000000
# mean           10.4564870
# SE.mean         0.1130466
# CI.mean.0.95    0.2221048
# var             6.4025428
# std.dev         2.5303246
# coef.var        0.2419861

# High Cholesterol
HIGHCHOL <-df%>%
  filter(MeasureId=="HIGHCHOL")%>%
  select(Data_Value)
summary(HIGHCHOL)
# Min.   :24.10  
# 1st Qu.:28.00  
# Median :29.50  
# Mean   :29.31  
# 3rd Qu.:30.60  
# Max.   :34.10 
stat.desc(HIGHCHOL)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          2.410000e+01
# max          3.410000e+01
# range        1.000000e+01
# sum          1.468420e+04
# median       2.950000e+01
# mean         2.930978e+01
# SE.mean      8.612672e-02
# CI.mean.0.95 1.692149e-01
# var          3.716324e+00
# std.dev      1.927777e+00
# coef.var     6.577248e-02

# Kidney Disease
KIDNEY <-df%>%
  filter(MeasureId=="KIDNEY")%>%
  select(Data_Value)
summary(KIDNEY)
# Min.   :2.100  
# 1st Qu.:2.700  
# Median :3.100  
# Mean   :3.069  
# 3rd Qu.:3.400  
# Max.   :4.800 
stat.desc(KIDNEY)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          2.100000e+00
# max          4.800000e+00
# range        2.700000e+00
# sum          1.537400e+03
# median       3.100000e+00
# mean         3.068663e+00
# SE.mean      2.155789e-02
# CI.mean.0.95 4.235521e-02
# var          2.328360e-01
# std.dev      4.825308e-01
# coef.var     1.572447e-01

# Low Physical Activity
LPA <-df%>%
  filter(MeasureId=="LPA")%>%
  select(Data_Value)
summary(LPA)
# Min.   :12.90  
# 1st Qu.:21.60  
# Median :26.20  
# Mean   :26.52  
# 3rd Qu.:30.90  
# Max.   :45.40 
stat.desc(LPA)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          1.290000e+01
# max          4.540000e+01
# range        3.250000e+01
# sum          1.328870e+04
# median       2.620000e+01
# mean         2.652435e+01
# SE.mean      2.857367e-01
# CI.mean.0.95 5.613925e-01
# var          4.090437e+01
# std.dev      6.395652e+00
# coef.var     2.411238e-01

# Mammogram Use
MAMMOUSE <-df%>%
  filter(MeasureId=="MAMMOUSE")%>%
  select(Data_Value)
summary(MAMMOUSE)
# Min.   :60.00  
# 1st Qu.:72.00  
# Median :74.90  
# Mean   :74.44  
# 3rd Qu.:77.20  
# Max.   :83.50  
stat.desc(MAMMOUSE)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          6.000000e+01
# max          8.350000e+01
# range        2.350000e+01
# sum          3.729420e+04
# median       7.490000e+01
# mean         7.443952e+01
# SE.mean      1.633403e-01
# CI.mean.0.95 3.209180e-01
# var          1.336671e+01
# std.dev      3.656052e+00
# coef.var     4.911439e-02

# Poor Mental Health
MHLTH <-df%>%
  filter(MeasureId=="MHLTH")%>%
  select(Data_Value)
summary(MHLTH)
# Min.   : 8.30  
# 1st Qu.:11.90  
# Median :13.60  
# Mean   :13.42  
# 3rd Qu.:14.90  
# Max.   :19.60  
stat.desc(MHLTH)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          8.300000e+00
# max          1.960000e+01
# range        1.130000e+01
# sum          6.721000e+03
# median       1.360000e+01
# mean         1.341517e+01
# SE.mean      9.758327e-02
# CI.mean.0.95 1.917238e-01
# var          4.770769e+00
# std.dev      2.184209e+00
# coef.var     1.628164e-01

# Obesity
OBESITY <-df%>%
  filter(MeasureId=="OBESITY")%>%
  select(Data_Value)
summary(OBESITY)
# Min.   :15.30  
# 1st Qu.:25.90  
# Median :30.50  
# Mean   :30.24  
# 3rd Qu.:34.80  
# Max.   :49.10  
stat.desc(OBESITY)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          1.530000e+01
# max          4.910000e+01
# range        3.380000e+01
# sum          1.515070e+04
# median       3.050000e+01
# mean         3.024092e+01
# SE.mean      2.775505e-01
# CI.mean.0.95 5.453090e-01
# var          3.859418e+01
# std.dev      6.212422e+00
# coef.var     2.054310e-01

# Pap Test
PAPTEST <-df%>%
  filter(MeasureId=="PAPTEST")%>%
  select(Data_Value)
summary(PAPTEST, na.rm=TRUE)
is.null(PAPTEST)
is.na(PAPTEST)
which(is.na(PAPTEST)) # Get index of nas
mean(PAPTEST, na.rm=TRUE)
summary(PAPTEST, exclude = NULL)
summary(PAPTEST, na.exclude)
# Min.   :67.70  
# 1st Qu.:76.20  
# Median :78.60  
# Mean   :78.36  
# 3rd Qu.:80.60  
# Max.   :85.90  
# NA's   :47 
stat.desc(PAPTEST)
# nbr.val      4.540000e+02
# nbr.null     0.000000e+00
# nbr.na       4.700000e+01
# min          6.770000e+01
# max          8.590000e+01
# range        1.820000e+01
# sum          3.557620e+04
# median       7.860000e+01
# mean         7.836167e+01
# SE.mean      1.556021e-01
# CI.mean.0.95 3.057916e-01
# var          1.099226e+01
# std.dev      3.315458e+00
# coef.var     4.230968e-02

# Poor Physical Health
PHLTH <-df%>%
  filter(MeasureId=="PHLTH")%>%
  select(Data_Value)
summary(PHLTH)
# Min.   : 7.00  
# 1st Qu.:10.80  
# Median :12.70  
# Mean   :12.63  
# 3rd Qu.:14.30  
# Max.   :20.50 
stat.desc(PHLTH)
# nbr.val       501.0000000
# nbr.null        0.0000000
# nbr.na          0.0000000
# min             7.0000000
# max            20.5000000
# range          13.5000000
# sum          6326.8000000
# median         12.7000000
# mean           12.6283433
# SE.mean         0.1111069
# CI.mean.0.95    0.2182939
# var             6.1847150
# std.dev         2.4869087
# coef.var        0.1969307

# Lack of Sleep
SLEEP <-df%>%
  filter(MeasureId=="SLEEP")%>%
  select(Data_Value)
summary(SLEEP)
# Min.   :24.50  
# 1st Qu.:32.40  
# Median :35.30  
# Mean   :35.52  
# 3rd Qu.:38.50  
# Max.   :50.10  
stat.desc(SLEEP)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          2.450000e+01
# max          5.010000e+01
# range        2.560000e+01
# sum          1.779570e+04
# median       3.530000e+01
# mean         3.552036e+01
# SE.mean      1.946405e-01
# CI.mean.0.95 3.824140e-01
# var          1.898034e+01
# std.dev      4.356644e+00
# coef.var     1.226520e-01

# Stroke
STROKE <-df%>%
  filter(MeasureId=="STROKE")%>%
  select(Data_Value)
summary(STROKE)
# Min.   :1.800  
# 1st Qu.:2.600  
# Median :3.100  
# Mean   :3.122  
# 3rd Qu.:3.500  
# Max.   :6.100  
stat.desc(STROKE)
# nbr.val      5.010000e+02
# nbr.null     0.000000e+00
# nbr.na       0.000000e+00
# min          1.800000e+00
# max          6.100000e+00
# range        4.300000e+00
# sum          1.563900e+03
# median       3.100000e+00
# mean         3.121557e+00
# SE.mean      3.183836e-02
# CI.mean.0.95 6.255346e-02
# var          5.078544e-01
# std.dev      7.126390e-01
# coef.var     2.282960e-01

# Teeth Lost
TEETHLOST <-df%>%
  filter(MeasureId=="TEETHLOST")%>%
  select(Data_Value)
summary(TEETHLOST)
# Min.   : 5.10  
# 1st Qu.:10.90  
# Median :14.50  
# Mean   :14.59  
# 3rd Qu.:17.80  
# Max.   :31.80  
stat.desc(TEETHLOST)
# nbr.val       501.0000000
# nbr.null        0.0000000
# nbr.na          0.0000000
# min             5.1000000
# max            31.8000000
# range          26.7000000
# sum          7311.5000000
# median         14.5000000
# mean           14.5938124
# SE.mean         0.2216987
# CI.mean.0.95    0.4355758
# var            24.6243016
# std.dev         4.9622879
# coef.var        0.3400268



