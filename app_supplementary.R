### Sample codes for creating a shiny dashboard useful in tracking and managing a Clinical Trial
# "Keep your trial on track: A data visualization approach to data collection monitoring 
# in decentralized trials studies."

## Authors: Thiago Augusto Hernandes Rocha, Catherine A. Staton, Linda Minja, 
# Kennedy Ngowi, João Vítor Perez de Souza, Ashley J. Phillips, Siddhesh Zadey, 
# Judith Boshe, Blandina Mmbaga, Joao Ricardo Nickenig Vissoci
### 

### The sample codes provided here were made available so other research groups can build upon
# our strategy. The sample data provided are from fictional patients, and were generated randomly.
# This includes everything from patient ids, dates of admission and discharge and variables such as
# age, sex and others.


# Preparation -------------------------------------------------------------

# Setting up timezone for date formatting needs.
Sys.setlocale("LC_TIME", "C")

# setwd("/media/newhd/joao-souza/projects/GEMINI/PRACT/Dashboard/PRACT_data_dashboard/")

## Load packages

library(shiny) # load shiny structure
library(shinydashboard) # load shiny dashboard - tutorial (http://rstudio.github.io/shinydashboard/structure.html#boxes)
library(shinyWidgets) # load shiny widgets - tutorial (http://shinyapps.dreamrs.fr/shinyWidgets/)
library(reshape2) # manipulating data
library(lubridate) # works with dates
library(visdat) # predefined missing plots 
library(naniar) # predefined missing plots
library(UpSetR) # creates the object that allow the missing combination analysis
library(gtsummary) # creates the cross tabulations to display distribution of data
library(gt) # creates the cross tabulations to display distribution of data
library(highcharter) # library for dynamic plots
library(tidyverse) # works with data wrangling
library(DT) # load dynamic tables  - tutorial (https://rstudio.github.io/DT/server.html)
library(plotly) # load dynamic graphics  - tutorial (https://plotly.com/ggplot2/#basic-charts)
library(xts) # create the time matrix for the stock plot of the SMS API

## List of packages required for downloading data from REDCap and telveriet APIs
# They are not required to run this demonstration.

# library(redcapAPI) # extract data from redcapAPI - This package is *not required*
# for this demonstration
# library(httr) # access the SMS telerivet API - This package is used for downloading SMS data
# via an API. It might no be necssary in all projects
# library(jsonlite)# process the SMS telerivet API response



## Defining functions ------------------------------------------------------

##### Set of functions to generate a matrix of all follow-up periods + days of the month selected by the user
# This combination will be used to generate heatmaps for safety monitoring

### generate_grid 
# Generate grid takes the time period selected by the user (e.g. "2022-02") and a 
# list of follow-up periods.
# returns a long data-frame with all period-day (of selected month) combinations.

generate_grid <- function(time_period, fup_periods) {
  
  # Filtering date for the current month
  safety_current <- safety_melt %>% 
    filter(format(date, format="%Y-%m") == time_period)
  
  date_period <- as.Date.character(paste0(time_period, "-01"), format="%Y-%m-%d")
  
  # How many days are in this month?
  days_month <- seq(1, days_in_month(date_period))
  # padding with zeros
  days_month <- formatC(days_month, width=2, flag="0")
  
  ## Creating combinations of periods of follow up and all days of current month
  
  day_period <- tidyr::expand_grid(fup_periods, days_month)
  # creating ID variable for merging
  day_period <- day_period %>% mutate(ID=paste(fup_periods, days_month, sep="_"))
  
  return(day_period)
}

### get_depression_selected 
# Function to merge the safety data to the combination generated before
# returns a matrix that is ready for plotting the heatmap
get_depression_selected <- function(data_melt, time_period){
  
  
  fup_periods <- c("Baseline","3mo","6mo","9mo","12mo","24mo")
  
  # generating combinations
  day_period <- generate_grid(time_period, fup_periods)
  
  # Grouping by date, period of follow up and filtering for depression only and
  # creating ID variable to do matching late
  depression_current <- data_melt %>% filter(format(date, format="%Y-%m") == time_period) %>%
    mutate(date=as.Date.character(format(date, "%Y-%m-%d"),"%Y-%m-%d")) %>% 
    group_by(date, period) %>% summarize(depression=sum(depression)) %>% mutate(ID=paste(period, substr(date,9,10), sep="_"))
  
  # merging with depression
  merged_depression <- merge(day_period[c("ID","days_month", "fup_periods")],
                             depression_current[c("ID","depression")], by="ID", all.x=TRUE)
  
  # filling NA with 0
  merged_depression$depression[is.na(merged_depression$depression)] <- 0
  
  # Transforming to wide format
  merged_depression <- merged_depression %>%
    tidyr::pivot_wider(names_from = days_month, values_from = depression, id_cols = fup_periods)
  # # Ordering the rows
  merged_depression <- merged_depression %>% slice(match(levels_desired, fup_periods))
  # # transforming to matrix
  merged_depression <- as.matrix(merged_depression[-1])
  # Changing row names
  row.names(merged_depression) <- c("Baseline", "3-Months", "6-Months", "9-Months", "12-Months", "24-Months")
  
  return(merged_depression)
}

### get_suicidality_selected 
get_suicidality_selected <- function(data_melt, time_period){
  
  fup_periods <- c("Baseline","3mo","6mo","9mo","12mo","24mo")
  
  day_period <- generate_grid(time_period, fup_periods)
  
  # Grouping by date, period of follow up and filtering for suicidality only and
  # creating ID variable to do matching late
  suicidality_current <- data_melt %>% filter(format(date, format="%Y-%m") == time_period) %>%
    mutate(date=as.Date.character(format(date, "%Y-%m-%d"),"%Y-%m-%d")) %>% 
    group_by(date, period) %>% summarize(suicidality=sum(suicidality)) %>% mutate(ID=paste(period, substr(date,9,10), sep="_"))
  
  # merging with suicidality
  merged_suicidality <- merge(day_period[c("ID","days_month", "fup_periods")],
                              suicidality_current[c("ID","suicidality")], by="ID", all.x=TRUE)
  
  # filling NA with 0
  merged_suicidality$suicidality[is.na(merged_suicidality$suicidality)] <- 0
  
  # Transforming to wide format
  merged_suicidality <- merged_suicidality %>%
    tidyr::pivot_wider(names_from = days_month, values_from = suicidality, id_cols = fup_periods)
  # # Ordering the rows
  merged_suicidality <- merged_suicidality %>% slice(match(levels_desired, fup_periods))
  # # transforming to matrix
  merged_suicidality <- as.matrix(merged_suicidality[-1])
  # Changing row names
  row.names(merged_suicidality) <- c("Baseline", "3-Months", "6-Months", "9-Months", "12-Months", "24-Months")
  
  return(merged_suicidality)
}

### calc_miss 
# Function to create the missing values % ranking. 
# Estimate the % of missing values within a variable
calc_miss <- function(x){
  round((sum(is.na(x))/dim(data_useful)[1])*100,0)
}


## Reading data 
load("simulated_patient_data.Rdata")

# Setting up the first day of the week
start_date <- floor_date(today(), "week", 0)

# Subset data to create the tables
data_useful <- with(pract, 
                    data.frame(
                      date, age, female, edu_years, death, fu_3m, 
                      fu_6m, fu_9m, fu_12m, fu_24m, dc_date))
# Including dummy variable to be the standard selection when generating tables
data_useful$NoSelection <- as.factor(c("No Selection"))


# Subset the data useful of the week
data_useful_week <- subset(data_useful, data_useful$date >= start_date)

### Monitoring the number of missingness in the data set

## Getting the variables with high missing for patients admitted recently
missing_rank_week <- sort(sapply(data_useful_week, calc_miss))
high_missing_week <- missing_rank_week[(length(missing_rank_week)-9):length(missing_rank_week)]

# Create the missing values rank data frame to plot
plot1_data_week <- data.frame(high_missing_week, 
                              names=names(high_missing_week))

## Getting the variables with high missing
missing_rank <- sort(sapply(data_useful, calc_miss))
high_missing <- missing_rank[(length(missing_rank)-9):length(missing_rank)]

# Create the missing values rank data frame to plot
plot2_data <- data.frame(high_missing, 
                         names=names(high_missing))



# Selecting patients recently screened
screened_week <- subset(pract, subset= date>= start_date)

# Screening and randomization calculations --------------------------------------------------------------

## Calculating enrollment numbers
enrollment_randomization <- pract %>% 
  arrange(date) %>% 
  # creating variables for week and year of screening
  mutate(week_screen=week(date), 
         week_screen = ifelse(nchar(week_screen) == 1, paste0("0", week_screen), week_screen),
         year_screen=year(date),
         week_screen=paste(year_screen, week_screen, sep="-")) %>% 
  # grouping and counting patients
  group_by(week_screen) %>% 
  summarize(screened=n()) %>% 
  ungroup() %>% 
  # joining screening data with enrollment data
  inner_join(pract %>% 
               arrange(date) %>% 
               filter(!is.na(practid)) %>% 
               mutate(week_screen=week(date), 
                      week_screen = ifelse(nchar(week_screen) == 1, paste0("0", week_screen), week_screen), 
                      year_screen=year(date),
                      week_screen=paste(year_screen, week_screen, sep="-")) %>% 
               group_by(week_screen) %>% 
               summarize(randomized=n()) %>% 
               ungroup(), by="week_screen") 

# Creating the plotly graph of enrollment and randomization
enroll_plotly <- plot_ly(enrollment_randomization,
                         x = ~week_screen, 
                         y = ~screened, 
                         type = 'bar',
                         name = 'Screened') %>%
  add_trace(y = ~randomized, name = 'Randomized') %>%
  layout(xaxis = list(title = "",
                      tickangle = -45,
                      tickfont = list(size = 10)),
         yaxis = list(title = "Number of patients",
                      tickfont = list(size = 10)),
         legend = list(orientation = "h", x = 0, y = 18.5),
         barmode='group')

# Repeating the process above, but focusing on the weekday of screening and randomization
enrollment_randomization_weekday <- pract %>% 
  arrange(date) %>% 
  mutate(day_screen=weekdays(date)) %>% 
  group_by(day_screen) %>% 
  summarize(screened=n()) %>% 
  ungroup() %>% slice(match(c("Sunday", "Monday", "Tuesday", "Wednesday",
                              "Thursday", "Friday", "Saturday"),
                            day_screen)) %>% 
  inner_join(pract %>% 
               arrange(date) %>% 
               filter(!is.na(practid)) %>% 
               mutate(day_screen=weekdays(date)) %>% 
               group_by(day_screen) %>% 
               summarize(randomized=n()) %>% 
               ungroup() %>% slice(match(c("Sunday", "Monday", "Tuesday", "Wednesday",
                                           "Thursday", "Friday", "Saturday"),
                                         day_screen)), 
                                   by="day_screen")


enroll_plotly_weekday <- plot_ly(enrollment_randomization_weekday,
                                 x = ~day_screen, 
                                 y = ~screened, 
                                 type = 'bar',
                                 name = 'Screened') %>%
  add_trace(y = ~randomized, name = 'Randomized') %>%
  layout(xaxis = list(title = "",
                      tickangle = -45,
                      tickfont = list(size = 10)),
         yaxis = list(title = "Number of patients",
                      tickfont = list(size = 10)),
         legend = list(orientation = "h", x = 0, y = 18.5),
         barmode='group')

# Follow-up status --------------------------------------------------------

# Testing if patients have reached the time for follow-up windows and monitoring
# the status of follow-up.
pract %>% filter(!is.na(practid)) %>% 
  select(date, practid, fu_3m, fu_6m, fu_9m, fu_12m, fu_24m, 
         death, dc_date) %>%
  arrange(date) %>%
  mutate(days_elapsed = (today()-dc_date),
         months_elapsed = as.numeric(floor(days_elapsed/30)),
         months_3 = ifelse(months_elapsed>=3,1,0),
         months_6 = ifelse(months_elapsed>=6,1,0),
         months_9 = ifelse(months_elapsed>=9,1,0),
         months_12 = ifelse(months_elapsed>=12,1,0),
         months_24 = ifelse(months_elapsed>=24,1,0),
         follow_ok_3 = ifelse(months_3 ==1 & fu_3m == "Yes and complete" & death == 0,1,0), # follow-up perfect
         follow_ok_6 = ifelse(months_6 ==1 & fu_6m == "Yes and complete"& death == 0,1,0),
         follow_ok_9 = ifelse(months_9 ==1 & fu_9m == "Yes and complete"& death == 0,1,0),
         follow_ok_12 = ifelse(months_12 ==1 & fu_12m == "Yes and complete"& death == 0,1,0),
         follow_ok_24 = ifelse(months_24 ==1 & fu_24m == "Yes and complete"& death == 0,1,0),
         status_3m = case_when(months_3 ==1 & fu_3m == "Yes and complete" ~ "Follow-up OK",
                               months_3 ==1 & (fu_3m == "No" | fu_3m == "Partially, could not complete" | months_elapsed >=5) ~"Not completed",
                               months_3 ==1 & is.na(fu_3m) & months_elapsed <=4~"Not started - PRIORITIZE",
                               months_3 == 0 ~"Did not reach the window",
                               is.na(dc_date)~ "Missing discharge date"),
         status_6m = case_when(months_6 ==1 & fu_6m == "Yes and complete" ~ "Follow-up OK",
                               months_6 ==1 & (fu_6m == "No" | fu_6m == "Partially, could not complete" | months_elapsed >=8) ~"Not completed",
                               months_6 ==1 & is.na(fu_6m) & months_elapsed <=7~"Not started - PRIORITIZE",
                               months_6 == 0 ~"Did not reach the window",
                               is.na(dc_date)~ "Missing discharge date"),
         status_9m = case_when(months_9 ==1 & fu_9m == "Yes and complete" ~ "Follow-up OK",
                               months_9 ==1 & (fu_9m == "No" | fu_9m == "Partially, could not complete" | months_elapsed >=11) ~"Not completed",
                               months_9 ==1 & is.na(fu_9m)& months_elapsed <=10 ~"Not started - PRIORITIZE",
                               months_9 == 0 ~"Did not reach the window",
                               is.na(dc_date)~ "Missing discharge date"),
         status_12m = case_when(months_12 ==1 & fu_12m == "Yes and complete" ~ "Follow-up OK",
                                months_12 ==1 & (fu_12m == "No" | fu_12m == "Partially, could not complete" | months_elapsed >=14) ~"Not completed",
                                months_12 ==1 & is.na(fu_12m) & months_elapsed <=13 ~"Not started - PRIORITIZE",
                                months_12 == 0 ~"Did not reach the window",
                                is.na(dc_date)~ "Missing discharge date"),
         status_24m = case_when(months_24 ==1 & fu_24m == "Yes and complete" ~ "Follow-up OK",
                                months_24 ==1 & (fu_24m == "No" | fu_24m == "Partially, could not complete" | months_elapsed >=26) ~"Not completed",
                                months_24 ==1 & is.na(fu_24m)& months_elapsed <=25 ~"Not started - PRIORITIZE",
                                months_24 == 0 ~"Did not reach the window",
                                is.na(dc_date)~ "Missing discharge date"),
         lost_3m = case_when((status_3m == "Not completed" | status_3m == "Not started - PRIORITIZE") & months_elapsed>=5 ~ "Patient lost window to follow", TRUE ~ "Monitor"),
         lost_6m = case_when((status_6m == "Not completed" | status_6m == "Not started - PRIORITIZE") & months_elapsed>=8 ~ "Patient lost window to follow", TRUE ~ "Monitor"),
         lost_9m = case_when((status_9m == "Not completed" | status_9m == "Not started - PRIORITIZE") & months_elapsed>=11 ~ "Patient lost window to follow", TRUE ~ "Monitor"),
         lost_12m = case_when((status_12m == "Not completed" | status_12m == "Not started - PRIORITIZE") & months_elapsed>=14 ~ "Patient lost window to follow", TRUE ~ "Monitor"),
         lost_24m = case_when((status_24m == "Not completed" | status_24m == "Not started - PRIORITIZE") & months_elapsed>=26 ~ "Patient lost window to follow", TRUE ~ "Monitor")
  ) -> data_followup 


p_month <- data_followup %>% 
  dplyr::group_by(months_elapsed) %>%
  dplyr::summarise(value = n(),
                   follow_3m_complete = sum(status_3m == "Follow-up OK", na.rm = TRUE),
                   follow_6m_complete = sum(status_6m == "Follow-up OK", na.rm = TRUE),
                   follow_9m_complete = sum(status_9m == "Follow-up OK", na.rm = TRUE),
                   follow_12m_complete = sum(status_12m == "Follow-up OK", na.rm = TRUE),
                   follow_24m_complete = sum(status_24m == "Follow-up OK", na.rm = TRUE),
                   follow_3m_prioritize = sum(status_3m == "Not started - PRIORITIZE", na.rm = TRUE),
                   follow_6m_prioritize = sum(status_6m == "Not started - PRIORITIZE", na.rm = TRUE),
                   follow_9m_prioritize = sum(status_9m == "Not started - PRIORITIZE", na.rm = TRUE),
                   follow_12m_prioritize = sum(status_12m == "Not started - PRIORITIZE", na.rm = TRUE),
                   follow_24m_prioritize = sum(status_24m == "Not started - PRIORITIZE", na.rm = TRUE),
                   follow_3m_lost = sum(lost_3m == "Patient lost window to follow", na.rm = TRUE),
                   follow_6m_lost = sum(lost_6m == "Patient lost window to follow", na.rm = TRUE),
                   follow_9m_lost = sum(lost_9m == "Patient lost window to follow", na.rm = TRUE),
                   follow_12m_lost = sum(lost_12m == "Patient lost window to follow", na.rm = TRUE),
                   follow_24m_lost = sum(lost_24m == "Patient lost window to follow", na.rm = TRUE)) ->data_followup_month

followed_3months <- sum(data_followup_month$follow_3m_complete) #follow-up ok
followed_3months_complet <- sum(data_followup$months_3, na.rm = TRUE) #should be followed
followed_3months_prioritize <-  sum(data_followup_month$follow_3m_prioritize) #should be followed and are not and close to loose
followed_3months_lost <-  sum(data_followup_month$follow_3m_lost) #lost follow-up window

followed_6months <- sum(data_followup_month$follow_6m_complete)
followed_6months_complet <- sum(data_followup$months_6, na.rm = TRUE)
followed_6months_prioritize <-  sum(data_followup_month$follow_6m_prioritize)
followed_6months_lost <-  sum(data_followup_month$follow_6m_lost) #lost follow-up window

followed_9months <- sum(data_followup_month$follow_9m_complete)
followed_9months_complet <- sum(data_followup$months_9, na.rm = TRUE)
followed_9months_prioritize <-  sum(data_followup_month$follow_9m_prioritize)
followed_9months_lost <-  sum(data_followup_month$follow_9m_lost) #lost follow-up window

followed_12months <- sum(data_followup_month$follow_12m_complete)
followed_12months_complet <- sum(data_followup$months_12, na.rm = TRUE)
followed_12months_prioritize <-  sum(data_followup_month$follow_12m_prioritize)
followed_12months_lost <-  sum(data_followup_month$follow_12m_lost) #lost follow-up window

followed_24months <- sum(data_followup_month$follow_24m_complete)
followed_24months_complet <- sum(data_followup$months_24, na.rm = TRUE)
followed_24months_prioritize <-  sum(data_followup_month$follow_24m_prioritize)
followed_24months_lost <-  sum(data_followup_month$follow_24m_lost) #lost follow-up window

missing_discharge_date <- sum(is.na(data_followup$dc_date[!is.na(data_followup$practid)]))


data_followup %>%
  select(date,practid,death,dc_date, status_3m, 
         status_6m, status_9m, status_12m, status_24m) -> followp_plot

# Gerenating plots of the status of follow-up for different periods

graph_3m <-ggplot(data=followp_plot, aes(x=status_3m)) +
  geom_bar(stat="count", fill="steelblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(face="bold", color="black",size=8, angle=0)) +
  scale_y_continuous(name="Number of patients")+
  scale_x_discrete(name="")+
  coord_flip()


graph_6m <-ggplot(data=followp_plot, aes(x=status_6m)) +
  geom_bar(stat="count", fill="steelblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(face="bold", color="black",size=8, angle=0)) +
  scale_y_continuous(name="Number of patients")+
  scale_x_discrete(name="")+
  coord_flip()


graph_9m <-ggplot(data=followp_plot, aes(x=status_9m)) +
  geom_bar(stat="count", fill="steelblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(face="bold", color="black",size=8, angle=0)) +
  scale_y_continuous(name="Number of patients")+
  scale_x_discrete(name="")+
  coord_flip()


graph_12m <-ggplot(data=followp_plot, aes(x=status_12m)) +
  geom_bar(stat="count", fill="steelblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(face="bold", color="black",size=8, angle=0)) +
  scale_y_continuous(name="Number of patients")+
  scale_x_discrete(name="")+
  coord_flip()


graph_24m <-ggplot(data=followp_plot, aes(x=status_24m)) +
  geom_bar(stat="count", fill="steelblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(face="bold", color="black",size=8, angle=0)) +
  scale_y_continuous(name="Number of patients")+
  scale_x_discrete(name="")+
  coord_flip()

# Safety Monitoring --------------------------------------------------------

# This section comprises the routine check for feelings of depression and self-harm as
# evaluated in the Patient Health Questionnaire questions.

### PHQ9 --------------------------------------------------------------------

# PHQ columns for each period
phq_base <- c("phq1","phq2","phq3","phq4","phq5","phq6","phq7","phq8","phq9")
phq_3mo <- c("phq1_3mo","phq2_3mo","phq3_3mo","phq4_3mo","phq5_3mo","phq6_3mo","phq7_3mo","phq8_3mo","phq9_3mo")
phq_6mo <- c("phq1_6mo","phq2_6mo","phq3_6mo","phq4_6mo","phq5_6mo","phq6_6mo","phq7_6mo","phq8_6mo","phq9_6mo")
phq_9mo <- c("phq1_9mo","phq2_9mo","phq3_9mo","phq4_9mo","phq5_9mo","phq6_9mo","phq7_9mo","phq8_9mo","phq9_9mo")
phq_12mo<- c("phq1_12mo","phq2_12mo","phq3_12mo","phq4_12mo","phq5_12mo","phq6_12mo","phq7_12mo","phq8_12mo","phq9_12mo")
phq_24mo <- c("phq1_24mo","phq2_24mo","phq3_24mo","phq4_24mo","phq5_24mo","phq6_24mo","phq7_24mo","phq8_24mo","phq9_24mo")
# Column selection to recode separetely
cols_phq1 <- colnames(pract)[startsWith(colnames(pract), "phq1")]
cols_phq_other <- colnames(pract)[startsWith(colnames(pract), "phq") & !startsWith(colnames(pract), "phq1")]


# This chunk of code works to recode all the PHQ9 variables at once, considering every period studied.
# some changes of data types are required to make it work. Basically dates need to avoid being converted,
# that is why i use mutate_at with those exceptions 
pract %>% select(practid, date,date_3mo,date_6mo,
                 date_9mo,date_12mo,date_24mo, starts_with("phq")) %>% 
  # ensure these are numeric to be able to sum PHQ9
  mutate_at(vars(-practid, -date, -date_3mo, -date_6mo, -date_9mo,
                 -date_12mo, -date_24mo), as.numeric) %>% 
  mutate(practid=as.numeric(practid),
         # Summing the PHQ9 total score
         sum_phq9_base=rowSums(select(., all_of(phq_base))),
         sum_phq9_3mo=rowSums(select(., all_of(phq_3mo))),
         sum_phq9_6mo=rowSums(select(., all_of(phq_6mo))),
         sum_phq9_9mo=rowSums(select(., all_of(phq_9mo))),
         sum_phq9_12mo=rowSums(select(., all_of(phq_12mo))),
         sum_phq9_24mo=rowSums(select(., all_of(phq_24mo))),
         # depression - calculated by scores >= 9
         depression_base = if_else(sum_phq9_base >= 9, 1,0),
         depression_3mo = if_else(sum_phq9_3mo >= 9, 1,0),
         depression_6mo = if_else(sum_phq9_6mo >= 9, 1,0),
         depression_9mo = if_else(sum_phq9_9mo >= 9, 1,0),
         depression_12mo = if_else(sum_phq9_12mo >= 9, 1,0),
         depression_24mo = if_else(sum_phq9_24mo >= 9, 1,0),
         # suicidality - assessed by question 9 of phq9
         suicidality_base = if_else(phq9 >= 1, 1,0),
         suicidality_3mo = if_else(phq9_3mo >= 1, 1,0),
         suicidality_6mo = if_else(phq9_6mo >= 1, 1,0),
         suicidality_9mo = if_else(phq9_9mo >= 1, 1,0),
         suicidality_12mo = if_else(phq9_12mo >= 1, 1,0),
         suicidality_24mo = if_else(phq9_24mo >= 1, 1,0)) -> data_phq.2

# Removing patients with missing practid and only selecting the columns for testing for all periods
data_phq.2 <- data_phq.2 %>% filter(!is.na(practid)) %>% 
  select(practid, starts_with("date"), starts_with("depression"), starts_with("suicidality"))
periods <- c("Baseline","3mo","6mo","9mo","12mo","24mo")

base <- data_phq.2 %>% 
  mutate(safety_monitoring_pawss=0,depression=depression_base, suicidality=suicidality_base, period="Baseline") %>% 
  select(period, date, depression, suicidality, safety_monitoring_pawss)

fu_3mo <- data_phq.2 %>% 
  mutate(safety_monitoring_pawss=0, date=date_3mo, depression=depression_3mo, suicidality=suicidality_3mo, period="3mo") %>% 
  select(period, date, depression, suicidality, safety_monitoring_pawss)

fu_6mo <- data_phq.2 %>% 
  mutate(safety_monitoring_pawss=0, date=date_6mo, depression=depression_6mo, suicidality=suicidality_6mo, period="6mo") %>% 
  select(period, date, depression, suicidality, safety_monitoring_pawss)

fu_9mo <- data_phq.2 %>% 
  mutate(safety_monitoring_pawss=0, date=date_9mo, depression=depression_9mo, suicidality=suicidality_9mo, period="9mo") %>% 
  select(period, date, depression, suicidality, safety_monitoring_pawss)

fu_12mo <- data_phq.2 %>% 
  mutate(safety_monitoring_pawss=0, date=date_12mo, depression=depression_12mo, suicidality=suicidality_12mo, period="12mo") %>% 
  select(period, date, depression, suicidality, safety_monitoring_pawss)

fu_24mo <- data_phq.2 %>% 
  mutate(safety_monitoring_pawss=0, date=date_24mo, depression=depression_24mo, suicidality=suicidality_24mo, period="24mo") %>% 
  select(period, date, depression, suicidality, safety_monitoring_pawss)

safety_melt <- rbind(base, fu_3mo, fu_6mo, fu_9mo, fu_12mo, fu_24mo)

periods_safety <- rev(unique(format(safety_melt$date, format="%Y-%m")))
periods_safety <- periods_safety[!is.na(periods_safety)]
current_period <- format(Sys.Date(), format="%Y-%m")


# Filtering date for the current month
safety_current <- safety_melt %>% 
  filter(format(date, format="%Y-%m") == format(Sys.Date(), format="%Y-%m"))

# How many days are in this month?
days_month <- seq(1, days_in_month(Sys.Date())[[1]])
# padding with zeros
days_month <- formatC(days_month, width=2, flag="0")

## Creating combinations of periods of follow up and all days of current month

day_period <- tidyr::expand_grid(periods, days_month)
# creating ID variable for merging
day_period <- day_period %>% mutate(ID=paste(periods, days_month, sep="_"))

# List of order of periods
levels_desired <- c("Baseline", "3mo", "6mo", "9mo", "12mo", "24mo")


safety_table <- safety_melt %>% 
  filter(!is.na(date)) %>% 
  rename("Depression" = depression,
         "Suicidality"=suicidality,
         "PAWSS" = safety_monitoring_pawss) %>% 
  mutate(period=factor(period, levels=levels_desired, 
                       labels = c("Baseline", "3 Months", "6 Months", "9 Months", "12 Months", "24 Months"))) %>%
  select(-date) %>% tbl_summary(by=period, statistic=list(all_categorical() ~ "{n} / {N} ({p}%)"),
                                missing="no") %>% bold_labels()
# SMS calculations -------------------------------------------------------------------------


### Heatmap #-----------------------------------------------------------------

# Reading sample of sms data as a matrix
heatmap_matrix_sms_outgoing <- as.matrix(read.table("sms_weekday_heatmap.txt",
                                                    header=TRUE,row.names=1))
# renaming columns
colnames(heatmap_matrix_sms_outgoing) <- c(2,3,4,5,6,7,8,9,10,11,12,13,19)

# plotting a heatmap
heat_sms_outgoing <- hchart(heatmap_matrix_sms_outgoing, type = "heatmap") %>%
  hc_colorAxis(min = 0, max = 50,
               minColor = "#ffffbf",
               maxColor = "#2c7bb6") %>%
  hc_subtitle(text = "The color represents the lowest number of messages sent per day" )


### Line graphs -------------------------------------------------------------

sms_graphic <- read.csv("sms_summaries.csv")
sms_graphic$uniq_dates <- as.Date.character(sms_graphic$uniq_dates, format="%Y-%m-%d")

# Sum of each category of SMS
total_sms_sent <- sum(sms_graphic$sms_outgoing)
total_sms_delivered <- sum(sms_graphic$sms_delivered)
total_sms_not_delivered <- sum(sms_graphic$sms_not_delivered)
total_sms_calls <- sum(sms_graphic$calls_received)

# Creating a time series object to plot a line graph
sms_xts <- xts(x= sms_graphic[,2:5],order.by = sms_graphic[,1])
ind_1_mess_sent <- highchart(type = "stock") %>%
  hc_add_series(sms_xts$sms_outgoing,pointWidth=8, type = "column",color = "blue", name = "Outgoing sms") %>%
  hc_add_series(sms_xts$sms_delivered,pointWidth=5, type = "column",color = "green", name = "Delivered sms")  %>%
  hc_add_series(sms_xts$sms_not_delivered,pointWidth=3, type = "column",color = "red", name = "Not delivered sms") %>%
  hc_legend(enabled = TRUE)

# UI -  Beggining of the APP -------------------------------------------------------------------------

#This header contains an image (*.png) file. 
dbHeader <- dashboardHeader(titleWidth = 440)
dbHeader$children[[2]]$children <- tags$img(src="./your_image.png", height=56, width=429)

#begin of the user interface of the app (it is always composed of three parts: header, sidebar, and body) 
ui <- dashboardPage(
  dbHeader,
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      # Setting labels and Ids to use the tabs and create the codes later
      id = "tabs",
      menuItem("Main", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Descriptive tables", tabName = "tables", icon = icon("th")),
      menuItem("Follow-up", tabName = "followup", icon = icon("scroll")),
      menuItem("SMS Monitoring", tabName = "smsmoniroting", icon = icon("mobile-alt")),
      menuItem("Safety Monitoring", tabName = "safety", icon = icon("hand-holding-medical"))
    )),
  # Body of the dashboard
  dashboardBody(
    tabItems(
      ### First tab content UI #### ------------------------------------------
      tabItem(tabName = "dashboard",
              fluidRow(
                # Box of the number of patients screened
                valueBox(length(pract$practid), "Total patients screened", icon = icon("user-injured"), width = 3),# you need to check the data variable for this metric
                # Box of the total number of patients randomized 
                valueBox(length(pract$practid[!is.na(pract$practid)]), "Total patients randomized", icon = icon("hospital-user"), width = 3),# you need to check the data variable for this metric
                # Box of the the progress to complete the first phase of our study
                valueBoxOutput("progressBox", width = 3)
              ),
              fluidRow(
                box(title = "Patients screened this week", width = 4, 
                    solidHeader = TRUE, status = "warning", length(screened_week$practid)), 
                box(title = "Patients randomized this week", width = 4, 
                    solidHeader = TRUE, status = "danger", length(screened_week$practid))
              ),
              fluidRow(
                box(title = "Enrollment per Day/Week", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, plotlyOutput("fig1_enrollment_day", height = 250)
                ),
                box(title = "Enrollment per Week", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,plotlyOutput("fig2_enrollment_week", height = 250))
              ),
              fluidRow(
                box(title = "Missing values ranking/week", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, plotlyOutput("plot1", height = 400)
                ),
                box(title = "Missing values overall", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,plotlyOutput("plot2", height = 400))
              ),
              fluidRow(
                box(title = "High level of missings", width = 12, solidHeader = TRUE, 
                    status = "danger", paste(plot1_data_week$names,collapse="\n -"))
              )
      ), 
      ### Second tab content UI #### -----------------------------------------
      tabItem(tabName = "tables",
              fluidRow(
                box(title = "Descriptive statistics", width = 12, status = "primary", 
                    solidHeader = TRUE, 
                    pickerInput(inputId = "by_var",
                                selected = "NoSelection",
                                label = "Select a to display the descriptive statistics", 
                                choices = colnames(data_useful), #drive this attribute to a list or vector with question names to be displayed. 
                                multiple = FALSE
                    ))
              ),
              fluidRow(
                box(title = "Descriptive distribution", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, plotlyOutput("plot5", height = 400)),
                box(title = "Missing values distribution", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, plotlyOutput("plot6", height = 400))
              ),
              fluidRow(
                box(title = "Desctiptive table", width = 12, status = "primary", solidHeader = TRUE,
                    pickerInput(inputId = "tbl_rows",
                                selected = "NoSelection", #direct this object to the PRACT data frame
                                label = "Select variables to display on the rows", 
                                choices = colnames(data_useful), #drive this attribute to a list or vector with question names to be displayed. 
                                multiple = TRUE
                    ))
              ),
              fluidRow(
                box(title = "Table. Descriptive characteristics of the PRACT participants", 
                    width = 12, status = "primary", solidHeader = TRUE,
                    gt::gt_output("descrip_table"))
              )
      ),
      ### Third tab content UI #### ------------------------------------------
      tabItem(tabName = "followup",
              fluidRow(
                valueBox(length(pract$practid[!is.na(pract$practid)]), "Total patients randomized", icon = icon("hospital-user"), width = 6),# you need to check the data variable for this metric
                valueBox(missing_discharge_date, "Patients with missing discharge date", icon = icon("calendar-times"), width = 6, color = "red"),# you need to check the data variable for this metric
              ),
              fluidRow(
                valueBox(tags$p(paste(followed_3months,"/",followed_3months_complet," (",round(followed_3months/followed_3months_complet*100,0),"%)",sep=""),style = "font-size: 60%;"), tags$p("Follow up completed / Patients in 3 months window",style = "font-size: 85%;"), icon = icon("sms"), width = 2, color = "blue"),
                valueBox(tags$p(paste(followed_6months,"/",followed_6months_complet," (",round(followed_6months/followed_6months_complet*100,0),"%)", sep=""),style = "font-size: 60%;"), tags$p("Follow up completed / Patients in 6 months window",style = "font-size: 85%;"), icon = icon("sms"), width = 2, color = "blue"),
                valueBox(tags$p(paste(followed_9months,"/",followed_9months_complet," (",round(followed_9months/followed_9months_complet*100,0),"%)", sep=""),style = "font-size: 60%;"), tags$p("Follow up completed / Patients in 9 months window",style = "font-size: 85%;"), icon = icon("sms"), width = 2, color = "blue"),
                valueBox(tags$p(paste(followed_12months,"/",followed_12months_complet," (",round(followed_12months/followed_12months_complet*100,0),"%)", sep=""),style = "font-size: 60%;"), tags$p("Follow up completed / Patients in 12 months window",style = "font-size: 85%;"), icon = icon("sms"), width = 2, color = "blue"),
                valueBox(tags$p(paste(followed_24months,"/",followed_24months_complet," (",round(followed_24months/followed_24months_complet*100,0),"%)", sep=""),style = "font-size: 60%;"), tags$p("Follow up completed / Patients in 24 months window",style = "font-size: 85%;"), icon = icon("sms"), width = 2, color = "blue"),
              ),
              fluidRow(
                valueBox(followed_3months_prioritize, tags$p("Patients in 3 months window and not followed - Prioritize",style = "font-size: 85%;"), icon = icon("exclamation"), width = 2, color = "yellow"),
                valueBox(followed_6months_prioritize, tags$p("Patients in 6 months  window and not followed - Prioritize",style = "font-size: 85%;"), icon = icon("exclamation"), width = 2, color = "yellow"),
                valueBox(followed_9months_prioritize, tags$p("Patients in 9 months  window and not followed - Prioritize",style = "font-size: 85%;"), icon = icon("exclamation"), width = 2, color = "yellow"),
                valueBox(followed_12months_prioritize, tags$p("Patients in 12 months  window and not followed - Prioritize",style = "font-size: 85%;"), icon = icon("exclamation"), width = 2, color = "yellow"),
                valueBox(followed_24months_prioritize, tags$p("Patients in 24 months  window and not followed - Prioritize",style = "font-size: 85%;"), icon = icon("exclamation"), width = 2, color = "yellow"),
              ),
              fluidRow(
                box(title = "Complete status regarding 3 months window", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, plotlyOutput("followup_3m_graph", height = 250)
                ),
                box(title = "Complete status regarding 6 months window", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, plotlyOutput("followup_6m_graph", height = 250))
              ),
              fluidRow(
                box(title = "Complete status regarding 9 months window", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, plotlyOutput("followup_9m_graph", height = 250)
                ),
                box(title = "Complete status regarding 12 months window", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, plotlyOutput("followup_12m_graph", height = 250)
                )
              ),
              fluidRow(
                box(title = "Complete status regarding 24 months window", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, plotlyOutput("followup_24m_graph", height = 250))
              ),
      ), 
      
      ### Fourth tab content IU #### -----------------------------------
      tabItem(tabName = "smsmoniroting",
              fluidRow(
                valueBox(length(pract$practid[!is.na(pract$practid)]), "Total patients randomized", icon = icon("hospital-user"), width = 12),# you need to check the data variable for this metric
              ),
              fluidRow(
                valueBox((total_sms_delivered+total_sms_not_delivered), "Messages sent", icon = icon("sms"), width = 3, color = "blue"),# you need to check the data variable for this metric
                valueBox(paste(total_sms_delivered,"/",round(total_sms_delivered/(total_sms_delivered+total_sms_not_delivered)*100,digits=0),"%"), "Messages delivered", icon = icon("sms"), width = 3, color = "green"), # you need to check the data variable for this metric
                valueBox(paste(total_sms_not_delivered,"/",round(total_sms_not_delivered/(total_sms_delivered+total_sms_not_delivered)*100,digits=0),"%"), "Messages undelivered", icon = icon("sms"), width = 3, color = "red"),# you need to check the data variable for this metric
                valueBox(total_sms_calls, "Calls received", icon = icon("sms"), width = 3, color = "blue")# you need to check the data variable for this metric
              ),
              fluidRow(
                box(title = "Messages daily monitoring", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, highchartOutput("stock_sms", height = 350)
                  ),
                box(title = "Outgoing messages heatmap", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, highchartOutput("heat_sms_graphic", height = 350))
              )
      ),
      ###  Sixth tab content UI #### ----------------------------------------------
      tabItem(tabName = "safety",
              fluidRow(column(width = 4, 
                              valueBox(length(pract$practid[!is.na(pract$practid)]), "Total patients randomized", 
                                       icon = icon("hospital-user"), width = NULL),
                              infoBox(title="Depression","Patients with PHQ9 ≥ 9", width = NULL),
                              infoBox(title="Suicidality","Patients with PHQ9 #9 ≥ 1", width = NULL)),
                       column(width = 8,
                              box(title = "Total number of safety events registered by time window",
                                  status = "primary", solidHeader = TRUE, width=NULL,
                                  collapsible = TRUE, # highchartOutput("heat_safety")
                                  gt::gt_output("table_safety"))
                       )
              ),
              fluidRow(
                box(title = "Daily number of Depression and Suicidality events flagged", width = 12, 
                    status = "primary", solidHeader = TRUE, 
                    pickerInput(inputId = "period_chosen",
                                selected = current_period,
                                label = "Select a period to filter data and display the heatmaps below", 
                                choices = periods_safety, #drive this attribute to a list or vector with question names to be displayed. 
                                multiple = FALSE))
                ),
              fluidRow(                
                box(title = "Daily number of patients flagged for Depression in the period selected", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, highchartOutput("heat_depression", height = 300)),
                box(title = "Daily number of patients flagged for Suicidality in the period selected",
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, highchartOutput("heat_suicidality", height = 300))
              ), #fluidrow safety
      )
    ) #tabitens
  ) #dashboard body
) #dashboard


server <- function(input, output, session) {
  
  # renders all elements regarding the dashboard page
  prop_enrolled<-round(length(pract$practid[!is.na(pract$practid)])/615*100,0)
  
  output$progressBox <- renderValueBox({
    valueBox(paste0(prop_enrolled, "%"), "% of total needed in Phase I", icon = icon("poll-h"),color = "purple")
  })
  output$daysElapsed <- renderValueBox({
    valueBox("35", "Days since start", icon = icon("calendar", lib = "glyphicon"),color = "light-blue")
  })
  
  output$fig1_enrollment_day<-renderPlotly(enroll_plotly_weekday)
  
  output$fig2_enrollment_week <- 
    renderPlotly(enroll_plotly
  )
  # Plot 1 object
  output$plot1 <- renderPlotly(
    p <- ggplot(data=plot1_data_week, aes(x=reorder(names, high_missing_week), y=high_missing_week)) +
      geom_bar(stat="identity", fill="steelblue") +
      ylab("% of missing values") +
      xlab("Top 10 questions")+
      coord_flip()+
      theme_bw()
  )


  # Plot 1 object
  output$plot2 <- renderPlotly(
    p <- ggplot(data=plot2_data, aes(x=reorder(names, high_missing), y=high_missing)) +
      geom_bar(stat="identity", fill="steelblue") +
      ylab("% of missing values") +
      xlab("Top 10 questions")+
      # geom_text(aes(label=high_missing), position = position_dodge(width = 1), vjust=0.25, hjust= -0.25, size=3)+
      coord_flip()+
      theme_bw()
  )


  ## TAB 2 -------------------------------------------------------------------
  
  x <- reactive(input$by_var)
  
  output$plot5 <- renderPlotly(
    
    if (is.factor(data_useful[,x()])==TRUE) {
      
      p<-ggplot(data=data_useful, aes(x=data_useful[,x()])) +
        geom_bar(stat="count", width=0.7, fill="steelblue") +
        ylab("# per category") +
        xlab("")+
        theme_bw()
    }
    else {
      
      p<-ggplot(data_useful, aes(y=data_useful[,x()])) + 
        geom_boxplot() +
        ylab("Indicator distribution") +
        # xlab("Top 10 questions")+
        theme_bw()
    }   
    
  )
  
  output$plot6 <- renderPlotly({
    
    input_na<-mutate(data_useful, 
                     var_na= case_when(
                       is.na(data_useful[,x()]) ~ 'Missing',
                       TRUE ~ 'Valid'
                     ))
    
    p<-ggplot(data=input_na, aes(x=as.factor(var_na))) +
      geom_bar(stat="count", width=0.7, fill="steelblue") +
      ylab("Missing Vs. Valid entries") +
      xlab("# per category")+
      theme_bw()
    
  })
  
  
  tbl_row_list<-reactive(input$tbl_rows)
  
  output$descrip_table <- gt::render_gt({
    
    tbl_data <- data_useful %>% select(tbl_row_list())
    
    # tbl_data$by_var<-data_useful[,x()]
    
    if(is.factor(data_useful[,x()])==TRUE) {
      
      tbl_data$by_var<-data_useful[,x()]
      
    } else {
      
      median<-median(data_useful[,x()],na.rm=TRUE)
      
      tbl_data<-tbl_data %>%
        mutate(by_var= case_when(
          data_useful[,x()] >= median ~ "Above median",
          data_useful[,x()] < median ~ "Below median"   
        ))
      
    }   
    
    tab_2_display <- tbl_summary(tbl_data, 
                                 by = by_var, #variable 1
                                 # col = female, #variable 2
                                 percent = "row",  # if you want to display % values options are c("none", "column", "row", "cell")
                                 missing = "always", # if you want to display missing values, options are c("ifany", "always", "no")
                                 missing_text = "Missings"#, # The text to display regarding the missing values colunm or row
    ) %>% as_gt() # The text to display regarding the total value
  })
  ## TAB 3 -------------------------------------------------------------------
  
  output$followup_3m_graph <-renderPlotly(ggplotly(graph_3m))
  output$followup_6m_graph <-renderPlotly(ggplotly(graph_6m))
  output$followup_9m_graph <-renderPlotly(ggplotly(graph_9m))
  output$followup_12m_graph <-renderPlotly(ggplotly(graph_12m))
  output$followup_24m_graph <-renderPlotly(ggplotly(graph_24m))
  
  
  ## TAB 4 -------------------------------------------------------------------
  
  
  output$heat_sms_graphic <- renderHighchart({heat_sms_outgoing})
  output$stock_sms <- renderHighchart({ind_1_mess_sent})
  
  ## TAB 6 -------------------------------------------------------------------
  
  ### Heatmap safety  --------------------------------------------------------------------
  
  output$table_safety <- gt::render_gt({
    
    safety_table %>% as_gt()
  })
  
    ### Heatmap Depression ---------------------------------------------------------
  
  
  output$heat_depression <- renderHighchart({
    
    depression_matrix <- get_depression_selected(safety_melt, input$period_chosen)
    
    heatmap_depression <- hchart(depression_matrix, type = "heatmap", label = TRUE) %>%
      hc_xAxis(title=list(text="Days of current month")) %>%
      hc_caption(text = "<b> *Values from the period selected above<b>")
    
  })
  
  ### Heatmap Suicidality ---------------------------------------------------------
  
  output$heat_suicidality <- renderHighchart({
    
    suicidality_matrix <- get_suicidality_selected(safety_melt, input$period_chosen)
    
    heatmap_suicidality <- hchart(suicidality_matrix, type = "heatmap", label = TRUE) %>%
      hc_xAxis(title=list(text="Days of current month")) %>%
      hc_caption(text = "<b> *Values from the period selected above<b>")
    
  })
}


shinyApp(ui, server)

