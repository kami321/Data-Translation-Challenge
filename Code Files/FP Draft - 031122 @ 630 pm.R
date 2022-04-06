library(ipumsr)
library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(tidylog)
library(estimatr)
library(ggplot2)
library(dplyr)
library(plotly)
library(srvyr) 
library(zoo) 
options(scipen=999)

ddi <- read_ipums_ddi("cps_00003.xml")
data <- read_ipums_micro(ddi)
summary(data)

#Removing missing data
# data <- drop_na(data)
# sum(is.na(data))

#Drop the NIU in SEX variable
# table(data$RACE)

#Rename binary record in SEX to gender
data$SEX <- ifelse(data$SEX == 1, "Male", "Female")

#Group marital status into 2 variable 
data$MARST <- ifelse(data$MARST == 1, 1,0)

#Label Education variable
data$EDUC <- case_when(
  data$EDUC %in% c(2,10,20,30,40) ~ 'Below_HS_Graduate',
  data$EDUC %in% c(50,60,71,73,81) ~ 'HighSchoolGraduate',
  data$EDUC %in% c(91,92) ~ 'AssociateDegree',
  data$EDUC == 111 ~ 'BachelorDegree',
  data$EDUC == 123 ~ 'MasterDegree',
  data$EDUC == 124 ~ 'ProfessionalDegree',
  data$EDUC == 125 ~ 'DoctorateDegree')

# Label Race variable
data$RACE <- case_when(
  data$RACE == 100 ~ "White",
  data$RACE == 200 ~ "Black",
  data$RACE == 300 ~ "American_Indian_Aleut_Eskimo",
  data$RACE == 651 ~ "Asian",
  data$RACE == 652 ~ "Hawaiian_Pacific_Islander",
  data$RACE %in% c(801:820, 830) ~ "Mixed")

# Convert Age into Categorical variable (Generation)
data$AGE <- case_when(
  data$AGE %in% c(18:22) ~ "Generation_Z",
  data$AGE %in% c(23:38) ~ "Millennials",
  data$AGE %in% c(39:55) ~ "Generation_X")

# Label employed variable
data$EMPSTAT = case_when(
  data$EMPSTAT %in% c(10, 12) ~ 1, 
  data$EMPSTAT %in% c(20, 21, 22) ~ 0)

# Combine month and year to 1 column year_month
data <- data %>%
  unite(year_month, c("YEAR", "MONTH"), sep = "-", remove = FALSE)  %>%
  mutate(year_month = as.yearmon(year_month), covid = case_when(
             year_month < as.yearmon("2020-4") ~ 0,
             year_month > as.yearmon("2020-2") ~ 1)) %>%
    filter(year_month != as.yearmon("2020-3"), YEAR > 2019) %>%
    mutate(time = cumsum(c(1,as.numeric(diff(year_month))!=0)))  %>%
    select(YEAR, MONTH, WTFINL, AGE, RACE, SEX, MARST, EMPSTAT, EDUC, covid, time)
# Add weight to survey data 
survey <- as_survey(data, weights = c(WTFINL)) 

## Gender Analysis

# Linear Probability Model: Gender Regression Results 
sex_reg <- lm_robust(EMPSTAT ~ SEX*covid + covid*time, survey)
export_summs(sex_reg, digits = 3, robust = TRUE)

# Calculate employment rate male vs female  
sex_emp_monthly <- survey %>% 
  group_by(YEAR, MONTH, SEX) %>% 
  summarize(employment_rate = survey_mean(EMPSTAT, vartype = "ci")) %>% 
  pivot_wider(names_from = SEX, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

# Join data
sex_data <- left_join(data, sex_emp_monthly, by = c("YEAR", "MONTH"))

# Rename columns, add date variables
vtable(sex_data)

sex_emp_data <- sex_data %>%
  rename(male_employment_rate = 'employment_rate_Male',
         female_employment_rate = 'employment_rate_Female',
         maleHigh = "employment_rate_upp_Male",
         maleLow = "employment_rate_low_Male",
         femaleHigh = "employment_rate_upp_Female",
         femaleLow = "employment_rate_low_Female") %>%
  unite(year_month, c("YEAR", "MONTH"), sep = "-")  %>%
  mutate(year_month = (DATE = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

#Plot
ggplot(sex_emp_data, aes(year_month)) + 
  geom_line(aes(
    y = male_employment_rate, color = "male")) + 
  geom_ribbon(aes(ymax = maleHigh, ymin = maleLow), alpha=0.2) +
  geom_line(aes(
    y = female_employment_rate, color = "female")) + 
  geom_ribbon(aes(ymax = femaleHigh, ymin = femaleLow), alpha=0.2) +
  labs(title = "Employment Rates For Male and Female", 
       x = "Date", 
       y = "Employment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  theme(legend.title= element_blank())
  theme_bw()
  #theme(axis.text.x=element_text(angle=60, hjust=1))
  
## Marriage status Analysis
# Linear Probability Model: Marriage Status Regression Results
mrst_reg <- lm_robust(EMPSTAT ~ MARST*covid + covid*time, survey)
 export_summs(mrst_reg , digits = 3, robust = TRUE)
#Calculate rate of employment between married VS non-married

married_emp_monthly <- survey %>% 
  group_by(YEAR, MONTH, MARST) %>% 
  summarize(employment_rate = survey_mean(EMPSTAT, vartype = "ci")) %>% 
  pivot_wider(names_from = MARST, 
              values_from = c(employment_rate,employment_rate_low,employment_rate_upp))
# Join data
married_data <- left_join(data, married_emp_monthly , by = c("YEAR", "MONTH"))

# Rename columns, add date variables
married_emp_data <- married_data %>%
  rename(not_employment_rate = 'employment_rate_0',
         not_eHigh = "employment_rate_upp_0",
         not_eLow = "employment_rate_low_0",
         married_employment_rate = 'employment_rate_1',
         married_eHigh = "employment_rate_upp_1",
         married_eLow = "employment_rate_low_1") %>%
  unite(year_month, c("YEAR", "MONTH"), sep = "-")  %>%
  mutate(year_month = (DATE = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

#Plot
ggplot(married_emp_data, aes(year_month)) + 
  geom_line(aes(
    y = not_employment_rate, color = "Not Married")) + 
  geom_ribbon(aes(ymax = not_eHigh, ymin = not_eLow), alpha=0.2) +
  geom_line(aes(
    y = married_employment_rate, color = "Married")) + 
  geom_ribbon(aes(ymax = married_eHigh, ymin = married_eLow), alpha=0.2) +
  labs(title = "Employment Rates For Married And Not Married People", 
       x = "Date", 
       y = "Employment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  theme_minimal()
  #theme(axis.text.x=element_text(angle=60, hjust=1))

## Age Generation Analysis

### Linear Probability Model:  Age Generation Regression Results 
age_reg <- lm_robust(EMPSTAT~ AGE*covid + covid*time, survey)
export_summs(age_reg, digits = 3, robust = TRUE)


# Calculate employment rate by age categories
age_emp_monthly <- survey %>% 
  group_by(YEAR, MONTH, AGE) %>% 
  summarize(employment_rate = survey_mean(EMPSTAT, vartype = "ci")) %>% 
  pivot_wider(names_from = AGE, 
              values_from = c(employment_rate,employment_rate_low,employment_rate_upp))
# join data
age_data <- left_join(data, age_emp_monthly, by = c("YEAR", "MONTH"))
# convert to years type
age_emp_data <- age_data %>%
  unite(year_month, c("YEAR", "MONTH"), sep = "-")  %>%
  mutate(year_month = (DATE = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

# Plot
ggplot(age_emp_data, aes(year_month)) + 
  geom_line(aes(
    y = employment_rate_Generation_X, color = "Generation X")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Generation_X, 
                  ymin = employment_rate_low_Generation_X), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_Generation_Z, color = "Generation Z")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Generation_Z, 
                  ymin = employment_rate_low_Generation_Z), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_low_Millennials, color = "Millennials")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Millennials, 
                  ymin = employment_rate_low_Millennials), alpha=0.2) +
  
  labs(title = "Employment Rates for Age Generations", 
       x = "Data", 
       y = "Employment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") 

# Education Level Analysis
# Linear Probability Model: Education Level Regression
EducCreg <- lm_robust(EMPSTAT ~ EDUC*covid + covid*time, survey)
Educ_reg_table <- export_summs(EducCreg, digits = 3, robust = TRUE)

# Calculate employment rate by education level
Educ_emp_monthly <- survey %>% 
  group_by(YEAR, MONTH, EDUC) %>% 
  summarize(employment_rate = survey_mean(EMPSTAT, vartype = "ci")) %>% 
  pivot_wider(names_from = EDUC, 
              values_from = c(employment_rate,employment_rate_low,employment_rate_upp))
# Join data
Educ_data <- left_join(data, Educ_emp_monthly, by = c("YEAR", "MONTH"))
# Add dates
Educ_All_data <- Educ_data %>%
  unite(year_month, c("YEAR", "MONTH"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

# Plot
Emp_educ_cat <- ggplot(Educ_All_data, aes(year_month)) + 
  geom_line(aes(y = employment_rate_AssociateDegree, color = "Associate Degree")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_AssociateDegree, 
                  ymin = employment_rate_low_AssociateDegree), alpha=0.2) +
  
  geom_line(aes(y = employment_rate_BachelorDegree, color = "Bachelor Degree")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_BachelorDegree, 
                  ymin = employment_rate_low_BachelorDegree), alpha=0.2) +
  
  geom_line(aes(y = employment_rate_Below_HS_Graduate, color = "Below High School Graduate")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Below_HS_Graduate, 
                  ymin = employment_rate_low_Below_HS_Graduate), alpha=0.2) +
  
  geom_line(aes(y = employment_rate_DoctorateDegree, color = "Doctorate Degree")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_DoctorateDegree, 
                  ymin = employment_rate_low_DoctorateDegree), alpha=0.2) +
  
  geom_line(aes(y = employment_rate_HighSchoolGraduate, color = "High School Graduate")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_HighSchoolGraduate, 
                  ymin = employment_rate_low_HighSchoolGraduate), alpha=0.2) +
  
  geom_line(aes(y = employment_rate_MasterDegree, color = "Master Degree")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_MasterDegree, 
                  ymin = employment_rate_low_MasterDegree), alpha=0.2) +
  
  geom_line(aes(y = employment_rate_ProfessionalDegree, color = "Professional Degree")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_ProfessionalDegree, 
                  ymin = employment_rate_low_ProfessionalDegree), alpha=0.2) +
  
  labs(title = "Employment Rates for Education Levels", 
       x = "Date", 
       y = "Employment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b")

# Race Analysis
# Linear Probability Model: Race Regression
Race_reg <- lm_robust(EMPSTAT ~ RACE*covid + covid*time, survey)
Race_reg_table <-export_summs(Race_reg, digits = 3, robust = TRUE)

# Calculate monthly employment rate for race categories
Race_emp_monthly <- survey %>% 
  group_by(YEAR, MONTH, RACE) %>% 
  summarize(employment_rate = survey_mean(EMPSTAT, vartype = "ci")) %>% 
  pivot_wider(names_from = RACE, values_from = c(employment_rate,employment_rate_low,employment_rate_upp)) %>%
  ungroup()
# Join data
Race_data <- left_join(data, Race_emp_monthly, by = c("YEAR", "MONTH"))
# Convert to years 
Race_All_data <- Race_data %>% 
  unite(year_month, c("YEAR", "MONTH"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month)) 

# Plot
Emp_Race <- ggplot(Race_All_data, aes(year_month)) + 
  geom_line(aes(y = employment_rate_White, color = "White")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_White, 
                  ymin = employment_rate_low_White), alpha=0.2) +
  
  geom_line(aes(y = employment_rate_Black, color = "Black")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Black, 
                  ymin = employment_rate_low_Black), alpha=0.2) +
  
  geom_line(aes(y = employment_rate_American_Indian_Aleut_Eskimo, color = "American Indian/Aleut/Eskimo")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_American_Indian_Aleut_Eskimo, 
                  ymin = employment_rate_low_American_Indian_Aleut_Eskimo), alpha=0.2) +
  
  geom_line(aes(y = employment_rate_Asian, color = "Asian")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Asian, 
                  ymin = employment_rate_low_Asian), alpha=0.2) +
  
  geom_line(aes(y = employment_rate_Hawaiian_Pacific_Islander, color = "Hawaiian/Pacific Islander")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Hawaiian_Pacific_Islander, 
                  ymin = employment_rate_low_Hawaiian_Pacific_Islander), alpha=0.2) +
  
  geom_line(aes(y = employment_rate_Mixed, color = "Mixed")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Mixed, 
                  ymin = employment_rate_low_Mixed), alpha=0.2) +
  
  labs(title = "Employment Rates for Race", 
       x = "Date", 
       y = "Employment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b")

# Race Pie Chart
# Filter data to January 2020
Race_data_Jan_20 <- Race_All_data %>% 
  filter(year_month >= "2020-01-01", year_month <= "2020-01-31") %>% 
  filter(EMPSTAT == 1) %>% 
  count(RACE)
# Pie Chart for January 2020
Fig1 <- plot_ly(Race_data_Jan_20, labels = ~ RACE, values = ~ n, type = 'pie')
Fig1 <- Fig1 %>% 
  layout(title = "Employed by Race in January 2020",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Filter data to January 2021
Race_data_Jan_21 <- Race_All_data %>% 
  filter(year_month >= "2021-01-01", year_month <= "2021-01-31") %>% 
  filter(EMPSTAT == 1) %>% 
  count(RACE)
# Pie Chart for January 2021
Fig2 <- plot_ly(Race_data_Jan_21, labels = ~ RACE, values = ~ n, type = 'pie')
Fig2 <- Fig2 %>% 
  layout(title = "Employed by Race in January 2021",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
