library(tidyverse)
rm(list=ls())

#### PSIS Data ####

# Key for province codes  http://www.statcan.gc.ca/pub/92-195-x/2011001/geo/prov/tbl/tbl8-eng.htm
# PROVOFSTUDY_levels = c("10", "12", "13", "24", "35", "46", "47", "48", "59" )
# PROVOFSTUDY_labels = c("NL", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" )
# REGION_levels = c("Atlantic", "Atlantic", "Atlantic", "QC", "ON", "Prairies", "Prairies", "Prairies", "BC" )

# Key for Stats Programs
# 26.1102	Biostatistics
# 27.0501	Statistics, General
# 27.0502	Mathematical Statistics and Probability
# 27.0599	Statistics, Other
# 27.9999	Mathematics and Statistics, Other
# 52.1302	Business Statistics

# Enrolment data 
enrol = read_csv("./Data/psis_enrolments.csv", col_names=TRUE) %>%
  drop_na() %>%
  mutate(YEAR = PSIS_REFYEAR_D_CODE) %>% 
  mutate(PROVINCE = factor(PSIS_PROVOFSTUDY_CODE, 
                           levels=c("10", "12", "13", "24", "35", "46", "47", "48", "59" ), 
                           labels=c("NL", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ) )) %>% 
  mutate(REGION = factor(PROVINCE, 
                         levels = c("NL", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ),
                         labels = c("Atlantic", "Atlantic", "Atlantic", "QC", "ON", "Prairies", "Prairies", "Prairies", "BC" ))) %>%
  mutate(PROGRAM = factor(PSIS_PRGCODECIP6_D_CODE, 
                          levels=c("26.1102","27.0501","27.0502","27.0503","27.0599","27.9999","52.1302"), 
                          labels=c("Biostats", "Stats, Gen", "Math-Stats & Prob", "Math & Stats", "Stats, Othr", "Mathematics and Statistics, Other", "Business Statistics") )) %>%
  mutate(LEVEL = factor(PSIS_PCSCE_D_CODE, levels=c("66","76","86"), labels=c("BSc","MSc","PhD") )) %>% 
  mutate(GENDER = factor(PSIS_GENDER_CODE, levels = c(2, 1, 9), labels = c("F", "M", "D") )) %>%
  mutate(SEX = factor(PSIS_GENDER_CODE, levels = c(2, 1), labels = c("F", "M") )) %>% 
  mutate(ENROLMENTS = `_COUNT_`) %>% 
  select(-(1:6), -GENDER, -PROVINCE) 

enrol_15 = read_csv("./Data/RTRA3858008_psisfreq_enrolments.csv", col_names=TRUE) %>%
  drop_na() %>% 
  filter( PSIS_ISCED_D_CODE %in%  c("06","07","08") ) %>%
  mutate(YEAR = as.integer(PSIS_REFYEAR_D_CODE)) %>% 
  filter(YEAR == 2015) %>%
  mutate(PROVINCE = factor(PSIS_PROVOFSTUDY_CODE, 
                           levels=c("10", "12", "13", "24", "35", "46", "47", "48", "59" ), 
                           labels=c("NL", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ) )) %>%
  mutate(REGION = factor(PROVINCE, levels = c( "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ),
                         labels = c("Atlantic", "Atlantic", "Atlantic", "Atlantic", "QC", "ON", "Prairies", "Prairies", "Prairies", "BC" ))) %>%
  mutate(PROGRAM = factor(PSIS_PRGCODECIP6_D_CODE, 
                          levels=c("26.1102","27.0501","27.0502","27.0503","27.0599","27.9999","52.1302"), 
                          labels=c("Biostats", "Stats, Gen", "Math-Stats & Prob", "Math & Stats", "Stats, Othr", "Mathematics and Statistics, Other", "Business Statistics") )) %>%
  mutate(LEVEL = factor(PSIS_ISCED_D_CODE, levels=c("06","07","08"), labels=c("BSc","MSc","PhD") )) %>% 
  mutate(GENDER = factor(PSIS_GENDER_CODE, levels = c(2, 1, 9), labels = c("F", "M", "D") )) %>% 
  mutate(SEX = factor(PSIS_GENDER_CODE, levels = c(2, 1), labels = c("F", "M"))) %>% 
  mutate(ENROLMENTS = `_COUNT_`) %>%
  select(-(1:6), -GENDER, -PROVINCE)

enrol = enrol %>% bind_rows( enrol_15 )

# Graduates

grad = read_csv("./Data/psis_graduates.csv", col_names=TRUE) %>%
  drop_na() %>%
  mutate(YEAR = PSIS_REFYEAR_D_CODE) %>% 
  mutate(PROVINCE = factor(PSIS_PROVOFSTUDY_CODE, 
                           levels=c("10", "12", "13", "24", "35", "46", "47", "48", "59" ), 
                           labels=c("NL", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ) )) %>% 
  mutate(REGION = factor(PROVINCE, 
                         levels = c("NL", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ),
                         labels = c("Atlantic", "Atlantic", "Atlantic", "QC", "ON", "Prairies", "Prairies", "Prairies", "BC" ))) %>%
  mutate(PROGRAM = factor(PSIS_PRGCODECIP6_D_CODE, 
                          levels=c("26.1102","27.0501","27.0502","27.0503","27.0599","27.9999","52.1302"), 
                          labels=c("Biostats", "Stats, Gen", "Math-Stats & Prob", "Math & Stats", "Stats, Othr", "Mathematics and Statistics, Other", "Business Statistics") )) %>%
  mutate(LEVEL = factor(PSIS_PCSCE_D_CODE, levels=c("66","76","86"), labels=c("BSc","MSc","PhD") )) %>% 
  mutate(SEX = factor(PSIS_GENDER_CODE, levels = c(2, 1), labels = c("F", "M") )) %>% 
  mutate(GRADUATES = `_COUNT_`) %>% 
  select(-(1:6), -PROVINCE)

grad_15=read_delim("./Data/RTRA3842343_psisfreq_graduates.csv",delim = ",", col_names=TRUE) %>%
  drop_na() %>% 
  filter( PSIS_ISCED_D_CODE %in%  c("06","07","08") ) %>%
  mutate(YEAR = PSIS_REFYEAR_D_CODE) %>%
  filter(YEAR == 2015) %>%
  mutate(PROVINCE = factor(PSIS_PROVOFSTUDY_CODE, 
                           levels=c("10", "12", "13", "24", "35", "46", "47", "48", "59" ), 
                           labels=c("NL", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ) )) %>%
  mutate(REGION = factor(PROVINCE, levels = c( "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ),
                         labels = c("Atlantic", "Atlantic", "Atlantic", "Atlantic", "QC", "ON", "Prairies", "Prairies", "Prairies", "BC" ))) %>%
  mutate(PROGRAM = factor(PSIS_PRGCODECIP6_D_CODE, 
                          levels=c("26.1102","27.0501","27.0502","27.0503","27.0599","27.9999","52.1302"), 
                          labels=c("Biostats", "Stats, Gen", "Math-Stats & Prob", "Math & Stats", "Stats, Othr", "Mathematics and Statistics, Other", "Business Statistics") )) %>%
  mutate(LEVEL = factor(PSIS_ISCED_D_CODE, levels=c("06","07","08"), labels=c("BSc","MSc","PhD") )) %>% 
  mutate(GENDER = factor(PSIS_GENDER_CODE, levels = c(2, 1, 9), labels = c("F", "M", "D") )) %>% 
  mutate(SEX = factor(PSIS_GENDER_CODE, levels = c(2, 1), labels = c("F", "M"))) %>% 
  mutate(GRADUATES = `_COUNT_`) %>%
  select(-(1:6), -PROVINCE) %>%
  # correct 2015 PhD's for controlled rounding (set to minimum value of 100)
  mutate( GRADUATES = replace(GRADUATES, YEAR == 2015 & PROGRAM == "Stats, Gen" & LEVEL == "PhD" & REGION == "ON", 50) ) 
  
grad = grad %>% bind_rows(grad_15)

#### Open Data ####

# StatsCan open data from https://www150.statcan.gc.ca/n1/en/tbl/csv/37100018-eng.zip

enrol_all=read_csv("./Data/37100018.csv", na='..') %>%
  select(REF_DATE,
         GEO,
         TYPE = `Institution type`,
         RGSTAT = `Registration status`,
         ISCED = `International Standard Classification of Education (ISCED)`,
         CIPPG = `Classification of Instructional Programs, Primary Grouping (CIP_PG)`,
         Sex,
         STSTAT = `Student status`,
         VALUE) %>%
  filter(REF_DATE %in% c("2010/2011","2011/2012","2012/2013","2013/2014","2014/2015","2015/2016"),
         GEO != "Canada",
         TYPE == "University",
         RGSTAT == "Total, registration status",
         ISCED %in% c("Bachelor's or equivalent", "Master's or equivalent", "Doctoral or equivalent"),
         CIPPG %in% c("Total, instructional programs", "Mathematics, computer and information sciences"),
         Sex %in% c("Females","Males"),
         STSTAT == "Total, student status" ) %>%
  mutate(YEAR = as.numeric( str_sub(REF_DATE,1,4)) ) %>%
  mutate(PROVINCE = factor(GEO,
                           levels = c( "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia",
                                       "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan",
                                       "Alberta", "British Columbia"),
                           labels = c( "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ) ) ) %>%
  mutate(REGION = factor(PROVINCE,
                           levels = c( "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ),
                           labels = c("Atlantic", "Atlantic", "Atlantic", "Atlantic", "QC", "ON", "Prairies", "Prairies", "Prairies", "BC" ))) %>%
  mutate(PROGRAM = factor(CIPPG,
                          levels =  c("Total, instructional programs", "Mathematics, computer and information sciences"),
                          labels = c("Total", "MCIS"))) %>%
  mutate(LEVEL = factor(ISCED,
                        levels = c("Bachelor's or equivalent", "Master's or equivalent", "Doctoral or equivalent"),
                        labels = c("BSc","MSc","PhD") )) %>%
  mutate(SEX = factor(Sex,
                      levels = c("Females","Males"), labels = c("F","M"))) %>%
  mutate(ENROLMENTS = VALUE) %>%
  select(-(1:9), -PROVINCE)

enrol = enrol %>% bind_rows( enrol_all)

# StatsCan open data from https://www150.statcan.gc.ca/n1/en/tbl/csv/37100020-eng.zip
grad_all=read_csv("./Data/37100020.csv") %>%
  select(REF_DATE,
         GEO,
         TYPE = `Institution type`,
         ISCED = `International Standard Classification of Education (ISCED)`,
         CIPPG = `Classification of Instructional Programs, Primary Grouping (CIP_PG)`,
         Sex,
         STSTAT = `Student status`,
         VALUE) %>%
  filter(REF_DATE %in% (2010:2015),
         GEO != "Canada",
         TYPE == "University",
         ISCED %in% c("Bachelor's or equivalent", "Master's or equivalent", "Doctoral or equivalent"),
         CIPPG %in% c("Total, instructional programs", "Mathematics, computer and information sciences"),
         Sex %in% c("Females","Males"),
         STSTAT == "Total, student status") %>%
  mutate(YEAR = REF_DATE ) %>%
  mutate(PROVINCE = factor(GEO,
                           levels = c( "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia",
                                       "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan",
                                       "Alberta", "British Columbia"),
                           labels = c( "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ) ) ) %>%
  mutate(REGION = factor(PROVINCE,
                         levels = c( "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ),
                         labels = c("Atlantic", "Atlantic", "Atlantic", "Atlantic", "QC", "ON", "Prairies", "Prairies", "Prairies", "BC" ))) %>%
  mutate(PROGRAM = factor(CIPPG,
                          levels =  c("Total, instructional programs", "Mathematics, computer and information sciences"),
                          labels = c("Total", "MCIS"))) %>%
  mutate(LEVEL = factor(ISCED,
                        levels = c("Bachelor's or equivalent", "Master's or equivalent", "Doctoral or equivalent"),
                        labels = c("BSc","MSc","PhD") )) %>%
  mutate(SEX = factor(Sex, levels = c("Females","Males"), labels = c("F","M"))) %>%
  mutate(GRADUATES = VALUE) %>%
  select(-(1:8))

grad = grad %>% bind_rows( grad_all)


# US open data from https://ncsesdata.nsf.gov/webcaspar/

grad_us = read_csv("./Data/webcaspar_table20180709184416.csv") %>%
  select( YEAR = 1, PROGRAM = 2, LEVEL = 3, GRADUATES = 4 ) %>% 
  mutate( PROGRAM = str_sub(PROGRAM, 1, 7)) %>%
  filter( PROGRAM %in% c("26.1102","27.0501","27.0502","27.0599","27.9999","52.1302") ) %>%
  mutate( PROGRAM = factor(PROGRAM, 
                           levels = c("26.1102","27.0502","27.0501","27.0599","27.9999","52.1302"), 
                           labels = c("Biostats", "Math-Stats & Prob", "Stats, Gen", "Stats, Othr", "Mathematics and Statistics, Other", "Business Statistics") )) %>%
  filter( LEVEL %in% c( "Bachelor's Degrees", "Doctorate Degree-Research/Scholarship", "Master's Degrees" )) %>%
  mutate( LEVEL = factor(LEVEL,
                         levels = c( "Bachelor's Degrees", "Master's Degrees", "Doctorate Degree-Research/Scholarship" ), 
                         labels = c( "BSc", "MSc", "PhD")
  )) %>%
  mutate( GRADUATES = as.numeric(GRADUATES))

#### Program Data ####

# list of programs
lop = read_delim("./Data/Stats Program Data - List of Programs.csv", col_names = TRUE, delim=',') %>%
  mutate(REGION = factor(PROVINCE,
                         levels = c( "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ),
                         labels = c("Atlantic", "Atlantic", "Atlantic", "Atlantic", "QC", "ON", "Prairies", "Prairies", "Prairies", "BC" ))) %>%
  filter(Hons_Spec=="Y") %>% 
  select(c(2:6),REGION) 

# Honours/Specialist programs only
prog=list()
for(i in 1:nrow(lop)){
  print( paste( i, " --- ", lop$UNIVERSITY[i] ) )
  fname=paste("./Data/Stats Program Data - ", lop$UNIVERSITY[i], ".csv", sep="")
  prog[[i]]=read_csv(fname, col_names = TRUE, col_types = cols( 
    Code = col_character(),
    Title = col_character(),
    Description = col_character(),
    Prerequisites = col_character(),
    Exclusions = col_character(),
    Discipline = col_character(),
    Level = col_character(),
    Type = col_character(),
    Category = col_character(),
    Comments = col_character() ) )
}

lop = lop %>% 
  mutate( PROGRAMS = prog )

# all programs tabble 
prog = lop %>% 
  unnest(PROGRAMS) %>% 
  mutate(CATEGORY = str_extract_all(Category, "CS|MT|PT|SM|SP|ST|OT" ) ) %>% 
  mutate(DISCIPLINE = str_extract_all(Discipline, "COMP|MATH|STAT|OTHR") ) %>%
  mutate(LEVEL = str_extract_all( Level, "1|2|3|4")) %>%
  mutate(TYPE = factor(Type, levels=c("R","E","FE"), labels=c("Core","Elect","Free"))) %>%
  mutate(CREDIT = 2*Credits ) %>%   
  filter(TYPE %in% c("Core","Elect") )




#### ------------ 
save.image(file = "all_data.RData")
