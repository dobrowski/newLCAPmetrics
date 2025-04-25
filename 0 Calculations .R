



library(tidyverse)
library(googlesheets4)
library(here)
library(vroom)
library(readxl)
library(glue)
library(MCOE) 


yr <- 2025

options(scipen = 999)


con <- mcoe_sql_con()



charter.school.codes <- c(
    "0112177", # Monterey Bay Charter
    "0116491", # Open Door Charter
    "0124297", # Bay View Academy
    "2730232" , # Home Charter
    "6119663", # Oasis
    "2730240", # Learning for Life
    "6118962", # International School
    "0118349" # Big Sur Charter
) 

### imports ------


indicators <- read_rds("indicators.rds")
metrics <- read_rds("metrics.rds")
dashboard_mry <- read_rds("dashboard_mry.rds")





dashboard_mry <- tbl(con, "DASH_ALL") %>%
    filter(countyname == "Monterey",
           rtype == "D" | charter_flag == "Y",
           reportingyear == (yr -1) 
    ) %>%  
    collect() 


dash.tbl <- dashboard_mry %>%
    mutate(schoolname = replace_na(schoolname, "Districtwide"),
           districtname = if_else(!is.na(charter_flag), schoolname, districtname),
           indicator = case_match(indicator, 
                                       "CCI" ~ "College-Career Readiness (Dashboard)",
                                       "CHRO" ~ "Chronic Absenteeism (Dashboard)",
                                        "ELA" ~ "ELA (Dashboard)",
                                       "ELPI" ~ "English Lanuage Progress Indicator - ELPI  (Dashboard)", 
                                       "GRAD" ~ "Graduation (Dashboard)", 
                                       "MATH" ~ "Math (Dashboard)",
                                       "SCIENCE" ~ "Science (Dashboard)",
                                       "SUSP" ~ "Suspension (Dashboard)" 
                                       )
    ) %>%
    filter(!is.na(currstatus), !is.na(studentgroup.long)) %>%
    select(cds, districtname, indicator, studentgroup, studentgroup.long, currstatus)


caaspp.full <- tbl(con, "CAASPP") %>%
    filter(# subgroup_id == "1",
           test_year == (yr -1),
           county_code == "27",
           school_code %in% c("0000000",charter.school.codes),
           grade %in% c("11", "13") 
           ) %>%  
    select(county_code:test_id, percentage_standard_met_and_above) %>%
    collect()  





standard.groups <- c(
    1	,	#	All Students	,	All Students	,
    128	,	#	Disability Status	,	Reported disabilities	,
    # 99	,	#	Disability Status	,	No reported disabilities	,
    31	,	#	Economic Status	,	Socioeconomically disadvantaged	,
    # 111	,	#	Economic Status	,	Not socioeconomically disadvantaged	,
    # 6	,	#	English-Language Fluency	,	IFEP, RFEP, and EO (Fluent English proficient and English only)	,
    7	,	#	English-Language Fluency	,	IFEP (Initial fluent English proficient)	,
    8	,	#	English-Language Fluency	,	RFEP (Reclassified fluent English proficient)	,
    # 120	,	#	English-Language Fluency	,	ELs enrolled less than 12 months	,
    # 142	,	#	English-Language Fluency	,	ELs enrolled 12 months or more	,
    160	,	#	English-Language Fluency	,	EL (English learner)	,
    # 243	,	#	English-Language Fluency	,	ADEL (Adult English learner)	,
    180	,	#	English-Language Fluency	,	EO (English only)	,
    # 170	,	#	English-Language Fluency	,	Ever–EL	,
    250	,	#	English-Language Fluency	,	LTEL (Long-Term English learner)	,
    # 251	,	#	English-Language Fluency	,	AR–LTEL (At-Risk of becoming LTEL)	,
    # 252	,	#	English-Language Fluency	,	Never–EL	,
    # 190	,	#	English-Language Fluency	,	TBD (To be determined)	,
    75	,	#	Race and Ethnicity	,	American Indian or Alaska Native	,
    76	,	#	Race and Ethnicity	,	Asian	,
    74	,	#	Race and Ethnicity	,	Black or African American	,
    77	,	#	Race and Ethnicity	,	Filipino	,
    78	,	#	Race and Ethnicity	,	Hispanic or Latino	,
    79	,	#	Race and Ethnicity	,	Native Hawaiian or Pacific Islander	,
    80	,	#	Race and Ethnicity	,	White	,
    144	,	#	Race and Ethnicity	,	Two or more races	,
    # 201	,	#	Ethnicity for Economically Disadvantaged	,	American Indian or Alaska Native	,
    # 202	,	#	Ethnicity for Economically Disadvantaged	,	Asian	,
    # 200	,	#	Ethnicity for Economically Disadvantaged	,	Black or African American	,
    # 203	,	#	Ethnicity for Economically Disadvantaged	,	Filipino	,
    # 204	,	#	Ethnicity for Economically Disadvantaged	,	Hispanic or Latino	,
    # 205	,	#	Ethnicity for Economically Disadvantaged	,	Native Hawaiian or Pacific Islander	,
    # 206	,	#	Ethnicity for Economically Disadvantaged	,	White	,
    # 207	,	#	Ethnicity for Economically Disadvantaged	,	Two or more races	,
    # 221	,	#	Ethnicity for Not Economically Disadvantaged	,	American Indian or Alaska Native	,
    # 222	,	#	Ethnicity for Not Economically Disadvantaged	,	Asian	,
    # 220	,	#	Ethnicity for Not Economically Disadvantaged	,	Black or African American	,
    # 223	,	#	Ethnicity for Not Economically Disadvantaged	,	Filipino	,
    # 224	,	#	Ethnicity for Not Economically Disadvantaged	,	Hispanic or Latino	,
    # 225	,	#	Ethnicity for Not Economically Disadvantaged	,	Native Hawaiian or Pacific Islander	,
    # 226	,	#	Ethnicity for Not Economically Disadvantaged	,	White	,
    # 227	,	#	Ethnicity for Not Economically Disadvantaged	,	Two or more races	,
    4	,	#	Gender	,	Female	,
    3	,	#	Gender	,	Male	,
    28	,	#	Migrant	,	Migrant education	,
    # 29	,	#	Migrant	,	Not migrant education	,
    # 90	,	#	Parent Education	,	Not a high school graduate	,
    # 91	,	#	Parent Education	,	High school graduate	,
    # 92	,	#	Parent Education	,	Some college (includes AA degree)	,
    # 93	,	#	Parent Education	,	College graduate	,
    # 94	,	#	Parent Education	,	Graduate school/Postgraduate	,
    # 121	,	#	Parent Education	,	Declined to state	,
    # 50	,	#	Military Status	,	Armed forces family member	,
    # 51	,	#	Military Status	,	Not armed forces family member	,
    52	,	#	Homeless Status	,	Homeless	,
    # 53	,	#	Homeless Status	,	Not homeless	,
    240		#	Foster Status	,	Foster youth	,
    # 241		#	Foster Status	,	Not foster youth	,
)



caaspp.tbl <- caaspp.full %>%
    filter(subgroup_id %in%  standard.groups) %>%
    mutate(cds = paste0(county_code,district_code,school_code),
           currstatus = as.numeric(percentage_standard_met_and_above),
           subgroup_id = as.character(subgroup_id),
#           district_name = NA
    ) %>%
    left_join_codebook("CAASPP", "subgroup_id") %>%
    rename(studentgroup.long = definition) %>%
    mutate(test_id = as.character(test_id)) %>%
    left_join_codebook("CAASPP", "test_id") %>%
    rename(indicator = definition) %>%
    mutate(indicator = case_when(grade == 13 ~ indicator,
                                 grade == 11 & test_id == 1 ~ "EAP for ELA",
                                 grade == 11 & test_id == 2 ~ "EAP for Math",
                                 
                                 )
           ) %>%
    filter(!is.na(currstatus)) %>%
    select(cds,  indicator, studentgroup.long, currstatus )


#. Need data entry for all student groups 
A_G <- read_sheet("https://docs.google.com/spreadsheets/d/1aX3sSlrWsOSyd9lzjME_fppAEbZE0EiXDkdYgygseAY/edit#gid=0",
                  sheet = "2024 Dashboard") %>%
    mutate(cds = as.character(cds))



### Drop ----


drop.full <- tbl(con, "GRAD_FOUR")  %>% 
    filter( # reporting_category == "TA",
           #  charter_school =="No",
           # dass == "All",
        (aggregate_level == "D" & charter_school == "No" & dass == "All") |  (aggregate_level == "S" & charter_school == "Yes"), 
            county_name == "Monterey",
            academic_year == max(academic_year)) %>%
    collect() 


drop.tbl <- drop.full %>%
    mutate(cds = paste0(county_code,district_code,school_code),
           districtname = if_else(charter_school == "Yes", school_name, district_name)) %>%
    left_join_codebook("GRAD_FOUR","reporting_category") %>%
    rename(studentgroup.long = definition) %>%
    mutate(indicator = "Dropout Rate (HS)") %>%
    filter(!is.na(dropout_rate)) %>%
    select(cds, districtname, indicator, studentgroup.long, currstatus = dropout_rate )

    


exp.full <- tbl(con, "EXP")  %>%
    #    mutate_at(vars(CumulativeEnrollment:ExpulsionCountDefianceOnly), funs(as.numeric) ) %>%
    filter(county_name == "Monterey",
       #    reporting_category == "TA",
           academic_year == max(academic_year),
          ( aggregate_level == "D" & charter_yn == "No") | (aggregate_level == "S" & charter_yn == "Yes" )
    ) %>%  # Charter included Yes/No
    collect() 


exp.tbl <- exp.full %>%   
    mutate(cds = paste0(county_code,district_code,school_code),
           districtname = if_else(charter_yn == "Yes", school_name, district_name),
           currstatus = (unduplicated_count_of_students_expelled_total*1000/cumulative_enrollment)%>% round2(3) 
           ) %>%
    left_join_codebook("EXP","reporting_category") %>%
    rename(studentgroup.long = definition) %>%
    mutate(indicator = "Expulsion Rate") %>%
    filter(!is.na(currstatus)) %>%
    select(cds, districtname, indicator, studentgroup.long, currstatus  )





cred_rate <- tbl(con, "Teaching") %>%
    filter(Academic_Year == max(Academic_Year),
           County_Code == 27,
           #    #   Aggregate_Level %in% c("D", "S" ),
         #  Subject_Area == "TA",
           Teacher_Experience_Level == "ALL",
           #       DASS == "ALL",
           #       School_Grade_Span == "ALL",
           Teacher_Credential_Level == "ALL",
           # #      Charter_School %in% c("No","Yes") 
    ) %>%
    collect() 



cred.tbl <- cred_rate %>%
    filter(  ( Aggregate_Level == "D" &  Charter_School == "N" & DASS == "ALL" & School_Grade_Span == "ALL") |
                 ( Aggregate_Level == "S" &  Charter_School == "Y"   ) 
    ) %>%
    left_join_codebook("TEACHING","Subject_Area") %>%
    rename(studentgroup.long = definition) %>%
    
    mutate(
        School_Code = str_replace(School_Code,"NULL" ,"0000000"),
        districtname = if_else(School_Name == "NULL", District_Name, School_Name),
        cds = paste0(County_Code,as.character(District_Code),School_Code),
        currstatus = Clear_FTE_percent,
        indicator = "Clear FTE Credential Rate")  %>%
    select(cds, districtname, indicator, studentgroup.long, currstatus  )





ag.cte.tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1aX3sSlrWsOSyd9lzjME_fppAEbZE0EiXDkdYgygseAY/edit?gid=294348514#gid=294348514",
                         sheet = "2024 Dashboard - Student Groups"
                         ) %>%
    pivot_longer(cols = ends_with("perc"),
                 names_to = "indicator",
                 values_to = "currstatus"
                 )  %>%
    rename(studentgroup.long = student_group,
           districtname = LEA)  %>%
    
    mutate(indicator = case_match(indicator,
                                  "ag_perc" ~ "A-G Met Percentage",
                                  "cte_perc" ~ "CTE Met Percentage",
                                  "ag_cte_perc" ~ "A-G and CTE Met Percentage",
                                  
                                  
                                  ) ,
           cds = as.character(cds)
           ) # %>%
#    select(cds, districtname, indicator, studentgroup.long, currstatus  )





### Put together -------

indicators2 <- ls(pattern = "tbl$") %>%
    mget(envir = .GlobalEnv) %>%
    reduce(bind_rows)


write_rds(indicators2, here("LCAP_Metric_Look_Up", "indicators2.rds"))




