# Code to explore asthma and other respiratory diseases hospital admission trends

# Part 1 - Extract data from SMRA on admissions
# Part 2 - Bring population and aggregate
# Part 3 - Calculate rates and final formatting 

# TODO:
# Splits: gender and under 10/over 10
# 
# Look at J45(asthma) and J46(status asthmaticus) independtly and together
# Look at J21(bronchiolitis), J20(acute bronchitis) and J22(unspec respiratory infection)
# Look at viral wheeze: primary position B349 and then R062. Check them independtly
# 
# Visualise (shiny?)

###############################################.
# Functions/packages/filepaths ----
###############################################.
# load packages required to run all commands
library(tidyr)
library(dplyr)
library(readr) 
library(odbc) 
library(plotly)

# file path for saved files
data_folder <- "/PHI_conf/ScotPHO/Profiles/Investigations/asthma_work/data/"

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")
))


###############################################.
# Part 1 - Extract data from SMRA on asthma admissions ----
###############################################.
#Looking to admissions with a main diagnosis of asthma, excluding unknown sex, by financial year. 
#Creates one record per CIS and selects only one case per patient/year.
# Excluding unvalid sex cases 
data_asthma <- tbl_df(dbGetQuery(channel, statement=
  "SELECT link_no linkno, AGE_IN_YEARS age, sex, dr_postcode pc7, 
      CASE WHEN extract(month from admission_date) > 3 
          THEN extract(year from admission_date) 
          ELSE extract(year from admission_date) -1 END as year,
      CASE WHEN regexp_like(main_condition, '^J4[56]') THEN 1 ELSE 0 END as J45_J46_main,
      CASE WHEN regexp_like(main_condition, '^J4[56]')
            OR regexp_like(other_condition_1, '^J4[56]') 
            OR regexp_like(other_condition_2, '^J4[56]')   
            OR regexp_like(other_condition_3, '^J4[56]') 
            OR regexp_like(other_condition_4, '^J4[56]')   
            OR regexp_like(other_condition_5, '^J4[56]') THEN 1 ELSE 0 END as J45_J46_all,
      CASE WHEN regexp_like(main_condition, '^J45') THEN 1 ELSE 0 END as J45_main,
      CASE WHEN regexp_like(main_condition, '^J45')
            OR regexp_like(other_condition_1, '^J45') 
            OR regexp_like(other_condition_2, '^J45')   
            OR regexp_like(other_condition_3, '^J45') 
            OR regexp_like(other_condition_4, '^J45')   
            OR regexp_like(other_condition_5, '^J45') THEN 1 ELSE 0 END as J45_all,
      CASE WHEN regexp_like(main_condition, '^J46') THEN 1 ELSE 0 END as J46_main,
      CASE WHEN regexp_like(main_condition, '^J46')
            OR regexp_like(other_condition_1, '^J46') 
            OR regexp_like(other_condition_2, '^J46')   
            OR regexp_like(other_condition_3, '^J46') 
            OR regexp_like(other_condition_4, '^J46')   
            OR regexp_like(other_condition_5, '^J46') THEN 1 ELSE 0 END as J46_all,
      CASE WHEN regexp_like(main_condition, '^J22') THEN 1 ELSE 0 END as J22_main,
      CASE WHEN regexp_like(main_condition, '^J22')
            OR regexp_like(other_condition_1, '^J22') 
            OR regexp_like(other_condition_2, '^J22')   
            OR regexp_like(other_condition_3, '^J22') 
            OR regexp_like(other_condition_4, '^J22')   
            OR regexp_like(other_condition_5, '^J22') THEN 1 ELSE 0 END as J22_all,
      CASE WHEN regexp_like(main_condition, '^J21') THEN 1 ELSE 0 END as J21_main,
      CASE WHEN regexp_like(main_condition, '^J21')
            OR regexp_like(other_condition_1, '^J21') 
            OR regexp_like(other_condition_2, '^J21')   
            OR regexp_like(other_condition_3, '^J21') 
            OR regexp_like(other_condition_4, '^J21')   
            OR regexp_like(other_condition_5, '^J21') THEN 1 ELSE 0 END as J21_all,
      CASE WHEN regexp_like(main_condition, '^J20') THEN 1 ELSE 0 END as J20_main,
      CASE WHEN regexp_like(main_condition, '^J20')
            OR regexp_like(other_condition_1, '^J20') 
            OR regexp_like(other_condition_2, '^J20')   
            OR regexp_like(other_condition_3, '^J20') 
            OR regexp_like(other_condition_4, '^J20')   
            OR regexp_like(other_condition_5, '^J20') THEN 1 ELSE 0 END as J20_all,
      CASE WHEN regexp_like(main_condition, '^B349') THEN 1 ELSE 0 END as B349_main,
      CASE WHEN regexp_like(main_condition, '^R062') THEN 1 ELSE 0 END as R062_main,
      CASE WHEN regexp_like(main_condition, '^B349') 
            AND regexp_like(other_condition_1, '^R062') THEN 1 ELSE 0 END as B349_other1_R062,
      CASE WHEN regexp_like(main_condition, '^B349') 
            AND (regexp_like(other_condition_1, '^R062')
                OR regexp_like(other_condition_2, '^R062')   
                OR regexp_like(other_condition_3, '^R062') 
                OR regexp_like(other_condition_4, '^R062')   
                OR regexp_like(other_condition_5, '^R062')) THEN 1 ELSE 0 END as B349_other_R062,
      CASE WHEN (regexp_like(main_condition, '^B349')
                OR regexp_like(other_condition_1, '^B349') 
                OR regexp_like(other_condition_2, '^B349')   
                OR regexp_like(other_condition_3, '^B349') 
                OR regexp_like(other_condition_4, '^B349')   
                OR regexp_like(other_condition_5, '^B349'))
            AND (regexp_like(main_condition, '^R062') 
                OR regexp_like(other_condition_1, '^R062') 
                OR regexp_like(other_condition_2, '^R062')   
                OR regexp_like(other_condition_3, '^R062') 
                OR regexp_like(other_condition_4, '^R062')   
                OR regexp_like(other_condition_5, '^R062')) THEN 1 ELSE 0 END as anypos_B349_R062,
      CASE WHEN (regexp_like(other_condition_1, '^B349') 
                OR regexp_like(other_condition_2, '^B349')   
                OR regexp_like(other_condition_3, '^B349') 
                OR regexp_like(other_condition_4, '^B349')   
                OR regexp_like(other_condition_5, '^B349'))
            AND (regexp_like(other_condition_1, '^R062') 
                OR regexp_like(other_condition_2, '^R062')   
                OR regexp_like(other_condition_3, '^R062') 
                OR regexp_like(other_condition_4, '^R062')   
                OR regexp_like(other_condition_5, '^R062')) THEN 1 ELSE 0 END as anyotherpos_B349_R062
  FROM ANALYSIS.SMR01_PI z
  WHERE admission_date between '1 April 2002' and '31 March 2019'
      AND sex in ('1','2') 
      AND regexp_like(main_condition, '^J4[5-6]|^J2[012]|^B349|^R062') ")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# recode the age groups
data_asthma <- data_asthma %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)),
  # age groups - over 10 and under 10
  age_grp2 = case_when(age < 10 ~ 1, age > 9 ~ 2, TRUE ~ as.numeric(age)
  ))

# Bringing datazone info to exclude non-Scottish.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2011)

data_asthma <- left_join(data_asthma, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>%  # converting variables into factors
  select(-pc7, -datazone2011)

# Counting only a patient once per year
data_asthma <- data_asthma %>% 
  mutate(sex = as.numeric(sex)) %>% 
  group_by(linkno, year) %>% 
  summarise_if(is.numeric, max) %>% ungroup() 

saveRDS(data_asthma, paste0(data_folder, "asthma_basefile.rds"))

###############################################.
# Part 2 - Bring population and aggregate ----
###############################################.
# bring populations file 
scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds') %>%
  setNames(tolower(names(.))) %>%  # variables to lower case
  subset(year > 2001 & year <= 2018) 

# aggregating to scottish total population
# recode age groups
scottish_population <- scottish_population %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age))) %>%
  group_by(age_grp, sex, year) %>% 
  summarise(pop =sum(pop)) %>% ungroup()

# Aggregate to Scotland totals
data_asthma_scotland <- data_asthma %>% group_by(age_grp, age_grp2, sex, year) %>% 
  summarise_if(is.numeric, sum) %>% ungroup() %>% 
  select(-age, -linkno) 

# Joining data with population (denominator)
data_asthma_scotland <- full_join(data_asthma_scotland, scottish_population, 
                                  c("year", "age_grp", "sex")) %>% 
  rename(denominator = pop) # numerator and denominator used for calculation

# Moving into long format
data_asthma_scotland <- data_asthma_scotland %>% 
  gather(diagnosis, numerator, -c(age_grp, age_grp2, sex, year, denominator)) %>% 
  mutate(epop = recode(as.character(age_grp), # EASR age group pops
                       "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                       "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                       "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                       "16"= 4000, "17"=2500, "18"=1500, "19"=1000)) 

###############################################.
# Part 3 - Calculate rates and final formatting ----
###############################################.
data_asthma_scotland <- data_asthma_scotland %>%
  mutate(easr_first = numerator*epop/denominator, # easr population
         var_dsr = (numerator*epop^2)/denominator^2)  # variance
  
final_data <- bind_rows(
  # obtaining rates for each sex
  data_asthma_scotland %>%  select(-age_grp, -age_grp2) %>%
    group_by(diagnosis, year, sex) %>% summarise_all(sum, na.rm =T) %>% ungroup(),
  # obtaining rates for the total
  data_asthma_scotland %>%  select(-age_grp, -sex, -age_grp2) %>%
    group_by(diagnosis, year) %>% summarise_all(sum, na.rm =T) %>% ungroup(),
  # obtaining rates for each age group
  data_asthma_scotland %>%  select(-age_grp, -sex) %>%
    group_by(diagnosis, year, age_grp2) %>% summarise_all(sum, na.rm =T) %>% ungroup(),
  # Aggregating by each age group and sex
  data_asthma_scotland %>%  select(-age_grp) %>%
    group_by(diagnosis, year, age_grp2, sex) %>% summarise_all(sum, na.rm =T) %>% ungroup()
) 

final_data <- final_data %>% 
  # Formatting labels
  mutate(sex= case_when(is.na(sex) ~ "All",
                        sex == 1 ~ "Male",
                        sex == 2 ~ "Female"),
         age_grp = case_when(is.na(age_grp2) ~ "All",
                             age_grp2 == 1 ~ "Under 10",
                             age_grp2 == 2 ~ "Over 10"),
         year = paste0(year, "/", substr(year+1, 3,4)),
         diagnosis = recode(diagnosis, "anypos_b349_r062" = "B349 and R062 appearing together in any positions",
                            "anyotherpos_B349_R062" = "B349 and R062 appearing together in positions 2 - 6",
                            "b349_anyother_r062"  = "Viral wheeze (B349 first, R602 in pos 2 to 6)", 
                            "b349_main" = "Viral infection first position", 
                            "b349_other1_r062" = "Viral wheeze (B349 followed by R602 in pos 2)", 
                            "j20_all" = "Acute bronchitis (J20) all positions", 
                            "j20_main" = "Acute bronchitis (J20) first position", 
                            "j21_all"  = "Bronchiolitis (J21) all positions", 
                            "j21_main" = "Bronchiolitis (J21) first position", 
                            "j22_all" = "Unspec. respiratory infection (J22) all positions", 
                            "j22_main"  = "Unspec. respiratory infection (J22) first position", 
                            "j45_all"  = "Asthma (J45) all positions", 
                            "j45_j46_all" = "Asthma and Status asthmaticus (J45-J46) all positions", 
                            "j45_j46_main" = "Asthma and Status asthmaticus (J45-J46) first position", 
                            "j45_main" = "Asthma (J45) first position", 
                            "j46_all" = "Status asthmaticus (J46) all positions", 
                            "j46_main"  = "Status asthmaticus (J46) first position",
                            "r602_main" = "Wheeze first position")) %>% 
  select(-age_grp2) %>% # Total EPOP population
  mutate(epop_total = case_when(sex == "All" & age_grp == "All" ~ 200000,
                                sex %in% c("Male", "Female") & age_grp == "All" ~ 100000,
                                sex == "All" & age_grp == "Under 10" ~ 21000,
                                sex %in% c("Male", "Female") & age_grp == "Under 10" ~ 10500,
                                sex == "All" & age_grp == "Over 10" ~ 179000,
                                sex %in% c("Male", "Female") & age_grp == "Over 10" ~ 89500),
         o_lower = numerator*(1-(1/(9*numerator)) - (1.96/(3*sqrt(numerator))))^3,  # Lower CI 1st step
         o_upper = (numerator+1)*(1-(1/(9*(numerator+1))) + (1.96/(3*sqrt(numerator+1))))^3, # Upper CI 1st step
         var = (1/epop_total^2)*var_dsr, #variance
         easr = easr_first/epop_total, # easr calculation
         rate = easr*100000,
         lowci = (easr+sqrt(var/numerator)*(o_lower - numerator))*100000, #CI final step
         upci = (easr+sqrt(var/numerator)*(o_upper - numerator))*100000) %>%  
  select(-c(easr_first, epop_total, easr, denominator, epop, o_lower, o_upper, var, var_dsr))

saveRDS(final_data, paste0(data_folder, "asthma_final.rds"))
final_data <- readRDS(paste0(data_folder, "asthma_final.rds"))

##END