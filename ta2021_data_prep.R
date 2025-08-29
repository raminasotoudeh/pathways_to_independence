# PSID Transition to Adulthood (TA) 2021 Data Preparation Script
# This script processes the 2021 TA data

library(dplyr)

# Load 2021 TA data
ta2021 = readRDS("./TAS2021/tas2021.RDS")

ta2021$FID = as.numeric(ta2021$YRID21)
ta2021$IID = as.numeric(ta2021$SN21)
ta2021$UID = paste(ta2021$FID, ta2021$IID, sep = "_")

# parents helped pay house for 2021
ta2021$total_house_value = as.numeric(ta2021$E43)
ta2021$total_house_value = ifelse(ta2021$total_house_value >= 9999998, NA, ta2021$total_house_value)
ta2021$amt_pahouses_paid_house = as.numeric(ta2021$E43Amt)
ta2021$amt_pahouses_paid_house = ifelse(ta2021$amt_pahouses_paid_house >= 9999998, NA, ta2021$amt_pahouses_paid_house)
ta2021$percent_pahouses_paid_house = as.numeric(ta2021$E43Pct)
ta2021$percent_pahouses_paid_house = ifelse(ta2021$percent_pahouses_paid_house > 100, NA, ta2021$percent_pahouses_paid_house)

ta2021$prop_pahouses_paid_house = ifelse(ta2021$amt_pahouses_paid_house == 0 & ta2021$total_house_value == 0, 0, ta2021$amt_pahouses_paid_house / ta2021$total_house_value)
ta2021$prop_pahouses_paid_house = ifelse(ta2021$percent_pahouses_paid_house > 0, ta2021$percent_pahouses_paid_house/100, ta2021$prop_pahouses_paid_house) 

ta2021$house_covered = ifelse(ta2021$prop_pahouses_paid_house == 1, 1, 0)
ta2021$house_covered = ifelse(is.na(ta2021$house_covered), 0, ta2021$house_covered)

ta2021$total_rent_value = as.numeric(ta2021$E44)
ta2021$total_rent_value = ifelse(ta2021$total_rent_value >= 9999998, NA, ta2021$total_rent_value)
ta2021$amt_parents_paid_rent = as.numeric(ta2021$E44Amt)
ta2021$amt_parents_paid_rent = ifelse(ta2021$amt_parents_paid_rent >= 9999998, NA, ta2021$amt_parents_paid_rent)
ta2021$percent_parents_paid_rent = as.numeric(ta2021$E44Pct)
ta2021$percent_parents_paid_rent = ifelse(ta2021$percent_parents_paid_rent > 100, NA, ta2021$percent_parents_paid_rent)

ta2021$prop_parents_paid_rent = ifelse(ta2021$amt_parents_paid_rent == 0 & ta2021$total_rent_value == 0, 0, 
                                       ifelse(ta2021$amt_parents_paid_rent > ta2021$total_rent_value, 1, 
                                              ta2021$amt_parents_paid_rent / ta2021$total_rent_value))

ta2021$prop_parents_paid_rent = ifelse(ta2021$percent_parents_paid_rent > 0, ta2021$percent_parents_paid_rent/100, ta2021$prop_parents_paid_rent) 

ta2021$rent_covered = ifelse(ta2021$prop_parents_paid_rent == 1, 1, 0)
ta2021$rent_covered = ifelse(is.na(ta2021$rent_covered), 0, ta2021$rent_covered)

ta2021$total_vehicle_value = as.numeric(ta2021$E45)
ta2021$total_vehicle_value = ifelse(ta2021$total_vehicle_value >= 9999998, NA, ta2021$total_vehicle_value)
ta2021$amt_parents_paid_vehicle = as.numeric(ta2021$E45Amt)
ta2021$amt_parents_paid_vehicle = ifelse(ta2021$amt_parents_paid_vehicle >= 9999998, NA, ta2021$amt_parents_paid_vehicle)
ta2021$percent_parents_paid_vehicle = as.numeric(ta2021$E45Pct)
ta2021$percent_parents_paid_vehicle = ifelse(ta2021$percent_parents_paid_vehicle > 100, NA, ta2021$percent_parents_paid_vehicle)

ta2021$prop_parents_paid_vehicle = ifelse(ta2021$amt_parents_paid_vehicle == 0 & ta2021$total_vehicle_value == 0, 0, 
                                          ifelse(ta2021$amt_parents_paid_vehicle > ta2021$total_vehicle_value, 1, 
                                                 ta2021$amt_parents_paid_vehicle / ta2021$total_vehicle_value))

ta2021$prop_parents_paid_vehicle = ifelse(ta2021$percent_parents_paid_vehicle > 0, ta2021$percent_parents_paid_vehicle/100, ta2021$prop_parents_paid_vehicle) 

ta2021$vehicle_covered = ifelse(ta2021$prop_parents_paid_vehicle == 1, 1, 0)
ta2021$vehicle_covered = ifelse(is.na(ta2021$vehicle_covered), 0, ta2021$vehicle_covered)

ta2021$total_tuition_value = as.numeric(ta2021$E46)
ta2021$total_tuition_value = ifelse(ta2021$total_tuition_value >= 9999998, NA, ta2021$total_tuition_value)
ta2021$amt_parents_paid_tuition = as.numeric(ta2021$E46Amt)
ta2021$amt_parents_paid_tuition = ifelse(ta2021$amt_parents_paid_tuition >= 9999998, NA, ta2021$amt_parents_paid_tuition)
ta2021$percent_parents_paid_tuition = as.numeric(ta2021$E46Pct)
ta2021$percent_parents_paid_tuition = ifelse(ta2021$percent_parents_paid_tuition > 100, NA, ta2021$percent_parents_paid_tuition)

ta2021$prop_parents_paid_tuition = ifelse(ta2021$amt_parents_paid_tuition == 0 & ta2021$total_tuition_value == 0, 0, 
                                          ifelse(ta2021$amt_parents_paid_tuition > ta2021$total_tuition_value, 1, 
                                                 ta2021$amt_parents_paid_tuition / ta2021$total_tuition_value))

ta2021$prop_parents_paid_tuition = ifelse(ta2021$percent_parents_paid_tuition > 0, ta2021$percent_parents_paid_tuition/100, ta2021$prop_parents_paid_tuition) 

ta2021$tuition_covered = ifelse(ta2021$prop_parents_paid_tuition == 1, 1, 0)
ta2021$tuition_covered = ifelse(is.na(ta2021$tuition_covered), 0, ta2021$tuition_covered)

ta2021$total_bills_value = as.numeric(ta2021$E49)
ta2021$total_bills_value = ifelse(ta2021$total_bills_value >= 9999998, NA, ta2021$total_bills_value)
ta2021$amt_parents_paid_bills = as.numeric(ta2021$E49Amt)
ta2021$amt_parents_paid_bills = ifelse(ta2021$amt_parents_paid_bills >= 9999998, NA, ta2021$amt_parents_paid_bills)
ta2021$percent_parents_paid_bills = as.numeric(ta2021$E49Pct)
ta2021$percent_parents_paid_bills = ifelse(ta2021$percent_parents_paid_bills > 100, NA, ta2021$percent_parents_paid_bills)

ta2021$prop_parents_paid_bills = ifelse(ta2021$amt_parents_paid_bills == 0 & ta2021$total_bills_value == 0, 0, 
                                        ifelse(ta2021$amt_parents_paid_bills > ta2021$total_bills_value, 1, 
                                               ta2021$amt_parents_paid_bills / ta2021$total_bills_value))

ta2021$prop_parents_paid_bills = ifelse(ta2021$percent_parents_paid_bills > 0, ta2021$percent_parents_paid_bills/100, ta2021$prop_parents_paid_bills) 

ta2021$bills_covered = ifelse(ta2021$prop_parents_paid_bills == 1, 1, 0)
ta2021$bills_covered = ifelse(is.na(ta2021$bills_covered), 0, ta2021$bills_covered)

ta2021$total_personal_loan_value = as.numeric(ta2021$E48)
ta2021$total_personal_loan_value = ifelse(ta2021$total_personal_loan_value >= 9999998, NA, ta2021$total_personal_loan_value)

ta2021$parents_gave_gift_inheritence = ta2021$E51_1
ta2021$parents_gave_gift_inheritence = ifelse(ta2021$parents_gave_gift_inheritence == 5, 0, ta2021$parents_gave_gift_inheritence)
ta2021$parents_gave_gift_inheritence = ifelse(ta2021$parents_gave_gift_inheritence == 1 | ta2021$parents_gave_gift_inheritence == 2, 1, ta2021$parents_gave_gift_inheritence)
ta2021$parents_gave_gift_inheritence = ifelse(ta2021$parents_gave_gift_inheritence > 6, NA, ta2021$parents_gave_gift_inheritence)
ta2021$gift_inheritence_value1 = as.numeric(ta2021$E53_1) 
ta2021$gift_inheritence_value1 = ifelse(ta2021$gift_inheritence_value1 >= 9999998, NA, ta2021$gift_inheritence_value1)
ta2021$gift_inheritence_value2 = as.numeric(ta2021$E53_2) 
ta2021$gift_inheritence_value2 = ifelse(ta2021$gift_inheritence_value2 >= 9999998, NA, ta2021$gift_inheritence_value2)
ta2021$gift_inheritence_value3 = as.numeric(ta2021$E53_3)
ta2021$gift_inheritence_value3 = ifelse(ta2021$gift_inheritence_value3 >= 9999998, NA, ta2021$gift_inheritence_value3)
ta2021$total_gift_inheritence_value = ta2021$gift_inheritence_value1 + ta2021$gift_inheritence_value2 + ta2021$gift_inheritence_value3

vars_help = c("amt_pahouses_paid_house", "amt_parents_paid_rent", "amt_parents_paid_vehicle", 
                        "amt_parents_paid_tuition", "amt_parents_paid_bills", "total_personal_loan_value", "total_gift_inheritence_value")

ta2021$total_parent_help_amount = rowSums(ta2021[,vars_help])

ta2021$marital_status = ta2021$C1
ta2021$marital_status = ifelse(ta2021$marital_status >= 8, NA, ta2021$marital_status)
ta2021$cohab_status = ta2021$C9
ta2021$cohab_status = ifelse(ta2021$cohab_status >= 8, NA, ta2021$cohab_status)
ta2021$marriage_cohabitation = ifelse(ta2021$marital_status == 2 & ta2021$cohab_status == 1, "Never Married; Cohabiting", 0)
ta2021$marriage_cohabitation = ifelse(ta2021$marital_status == 2 & ta2021$cohab_status == 5, "Never Married; Not cohabiting", ta2021$marriage_cohabitation)
ta2021$marriage_cohabitation = ifelse(ta2021$marital_status == 2 & ta2021$cohab_status == 5, "Never Married; Not cohabiting", ta2021$marriage_cohabitation)
ta2021$marriage_cohabitation = ifelse(ta2021$marital_status == 1, "Married, spouse present" , ta2021$marriage_cohabitation)
ta2021$marriage_cohabitation = ifelse(ta2021$marital_status == 5, "Separated" , ta2021$marriage_cohabitation)
ta2021$marriage_cohabitation = ifelse(ta2021$marital_status == 4 & ta2021$cohab_status == 1 , "Divorced, cohabiting" , ta2021$marriage_cohabitation)
ta2021$marriage_cohabitation = ifelse(ta2021$marital_status == 4 & ta2021$cohab_status == 5 , "Divorced, not cohabiting" , ta2021$marriage_cohabitation)
ta2021$marriage_cohabitation = ifelse(ta2021$marital_status == 3 , "Widowed" , ta2021$marriage_cohabitation)

ta2021$fall_winter_residence = as.numeric(ta2021$B15)
ta2021$fall_winter_residence = ifelse(ta2021$fall_winter_residence > 90, NA, ta2021$fall_winter_residence)
# spouse parents' owned home is included in number 5
ta2021$fall_winter_residence_parents_help = ifelse(ta2021$fall_winter_residence == 3 | ta2021$fall_winter_residence == 5 | ta2021$fall_winter_residence == 4, 1, 0)
ta2021$fall_winter_residence_cat = dplyr::recode(ta2021$fall_winter_residence, `3` = "Parents' home", `2` = "Own rent apt", `6` = "College housing", `8` = "College housing", `5` = "Home owned by parents", `1` = "Home owned by R", `9` = "Military base", `4` = "Partner's parents' home", `10` = "Other relatives owned/rented")

ta2021$summer_residence = as.numeric(ta2021$B16)
ta2021$summer_residence = ifelse(ta2021$summer_residence == 96, ta2021$fall_winter_residence, ta2021$summer_residence)
ta2021$summer_residence = ifelse(ta2021$summer_residence > 97, NA, ta2021$summer_residence)
ta2021$summer_residence_cat = dplyr::recode(ta2021$summer_residence, `3` = "Parents' home", `2` = "Own rent apt", `6` = "College housing", `8` = "College housing", `5` = "Home owned by parents", `1` = "Home owned by R", `9` = "Military base", `4` = "Partner's parents' home", `10` = "Other relatives owned/rented", `97` = "Other")
ta2021$summer_residence_parents_help = ifelse(ta2021$summer_residence_cat == "Parents' home"| ta2021$summer_residence_cat == "Home owned by parents" | ta2021$summer_residence_cat == "Partner's parents' home", 1, 0)

# In college now
ta2021$in_college_now = ifelse(ta2021$G15B == 1, 1, 0)
ta2021$in_college_now = ifelse(ta2021$G15A > 5, NA, ta2021$in_college_now)
ta2021$in_school_or_coll = ifelse(ta2021$in_college_now == 1, 1, 0)

# working
ta2021$working_now = ifelse(ta2021$D1_1 == "01", 1, 0)
ta2021$working_now = ifelse(ta2021$D1_2 == "01", 1, ta2021$working_now)
ta2021$working_now = ifelse(ta2021$D1_3 == "01", 1, ta2021$working_now)

ta2021$working_now_no_school = ifelse(ta2021$in_school_or_coll == 0 & ta2021$working_now == 1, 1, 0)
ta2021$no_school_no_work = ifelse(ta2021$in_school_or_coll == 0 & ta2021$working_now == 0, 1, 0)

# wtr_health_limits_work
ta2021$wtr_health_limits_work <- ifelse(ta2021$H5a > 0, 1, 0)

library(readxl)
vars2021 = read_excel("./J333806.xlsx")

vars2021$FID_2021 = as.numeric(vars2021$TA210003)
vars2021$IID_2021 = as.numeric(vars2021$TA210004)
vars2021$UID_2021 =  paste(vars2021$FID_2021, vars2021$IID_2021, sep = "_")

vars2021$psid68_FID = vars2021$ER30001
vars2021$psid68_IID = vars2021$ER30002
vars2021$psid68_UID = paste(vars2021$psid68_FID, vars2021$psid68_IID, sep = "_")

vars2021$female = ifelse(vars2021$ER32000 == 2, 1, 0)

vars2021$children_in_household_2021 = vars2021$ER78021

vars2021$fam_income_last_yr_2021 = vars2021$ER81775

vars2021$gpa2021 = ifelse(vars2021$TA210956 > 12 | vars2021$TA210956 == 0, NA, vars2021$TA210956)
vars2021$gpamax2021 = ifelse(vars2021$TA210957 > 12 | vars2021$TA210957 == 0, NA, vars2021$TA210957)
vars2021$gpa_standard_2021 = vars2021$gpa2021 / vars2021$gpamax2021
vars2021$gpa_standard_2021 = ifelse(vars2021$gpa_standard_2021 > 2, NA, vars2021$gpa_standard_2021)

#race
# race is coded as 0 if the race of the individual was reported in a previous wave of the TAS.
# so have to upload ta2019 in order to merge the values

# Data for linking IDs
psid_to_ta = readRDS("./psid68_to_tas_ids.RDS")

# Read in 2019 data
ta2019 = readRDS("./cleaned_ta_data/TA2019.RDS")

# Construct 2019 specific IDs
ta2019$FID = ta2019$TA190003
ta2019$IID = ta2019$TA190004
ta2019$UID = paste(ta2019$FID, ta2019$IID, sep = "_")

# Construct them in linking data set
psid_to_ta$FID_2019 = as.numeric(psid_to_ta$TA190003)
psid_to_ta$IID_2019 = as.numeric(psid_to_ta$TA190004)
psid_to_ta$UID_2019 =  paste(psid_to_ta$FID_2019, psid_to_ta$IID_2019, sep = "_")

# Merge in 1968 FIDs and IIDs to 2019 TA
matched_ids = match(ta2019$UID, psid_to_ta$UID_2019)

ta2019$psid68_FID = as.numeric(psid_to_ta$ER30001[matched_ids])
ta2019$psid68_IID = as.numeric(psid_to_ta$ER30002[matched_ids])
ta2019$psid68_UID = paste(ta2019$psid68_FID, ta2019$psid68_IID, sep = "_")

# Construct race variables
ta2019$race = ta2019$TA192131
ta2019$race = ifelse(ta2019$race > 9, NA, ta2019$race)
ta2019$race_cat = dplyr::recode(ta2019$race, `1` = "White", `2` = "Hispanic", `3` = "Black", `4` = "Asian", `5` = "Other", `6` = "Other", `7` = "Other", `8` = "Other")
ta2019$race = as.numeric(factor(ta2019$race_cat, levels = c("White", "Black", "Other", "Asian", "Hispanic")))

matched_ids = match(vars2021$psid68_UID, ta2019$psid68_UID)
vars2021$race_2019 = ta2019$race[matched_ids]

vars2021$race = vars2021$TA212252
vars2021$race = ifelse(vars2021$race == 0, vars2021$race_2019, vars2021$race)
vars2021$race = ifelse(vars2021$race > 9, NA, vars2021$race)
vars2021$race_cat = dplyr::recode(vars2021$race, `1` = "White", `2` = "Hispanic", `3` = "Black", `4` = "Asian", `5` = "Other", `6` = "Other", `7` = "Other", `8` = "Other")
vars2021$race = as.numeric(factor(vars2021$race_cat, levels = c("White", "Black", "Other", "Asian", "Hispanic")))

ta2021_merged = merge(ta2021, vars2021, by.x = 'UID', by.y = 'UID_2021')

library(readxl)
vars2021ta = read_excel("./J333876.xlsx")

vars2021ta$FID = as.numeric(vars2021ta$ER30001)
vars2021ta$IID = as.numeric(vars2021ta$ER30002)
vars2021ta$psid68_UID = paste(vars2021ta$FID, vars2021ta$IID, sep = "_")

vars2021ta$mother_edu = as.numeric(vars2021ta$TA212387)
vars2021ta$mother_edu = ifelse(vars2021ta$mother_edu > 90, NA, vars2021ta$mother_edu)

vars2021ta$health_level = vars2021ta$TA211104
vars2021ta$health_level = ifelse(vars2021ta$health_level > 6, NA, vars2021ta$health_level)
vars2021ta$health_level = 6 - as.numeric(vars2021ta$health_level)

vars2021ta$birth_order_mother = as.numeric(vars2021ta$ER32013)
vars2021ta$birth_order_mother = ifelse(vars2021ta$birth_order_mother > 10, NA, vars2021ta$birth_order_mother)

vars2021ta$earnings_work_1 = as.numeric(vars2021ta$TA210292) 
vars2021ta$earnings_work_1 = ifelse(vars2021ta$earnings_work_1 == 9999999| vars2021ta$earnings_work_1 < 0, NA, vars2021ta$earnings_work_1)

vars2021ta$earnings_work_2 = as.numeric(vars2021ta$TA210319)
vars2021ta$earnings_work_2 = ifelse(vars2021ta$earnings_work_2 == 9999999 | vars2021ta$earnings_work_2 < 0 , NA, vars2021ta$earnings_work_2)

vars2021ta$earnings_work_3 = as.numeric(vars2021ta$TA210346) 
vars2021ta$earnings_work_3 = ifelse(vars2021ta$earnings_work_3 == 9999999 | vars2021ta$earnings_work_3 < 0, NA, vars2021ta$earnings_work_3)

vars2021ta$earnings_work = rowSums(vars2021ta[ c("earnings_work_1", "earnings_work_2", "earnings_work_3")])

ta2021_merged_2 = merge(ta2021_merged, vars2021ta, by = 'psid68_UID')

saveRDS(ta2021_merged_2, './TAS2021/ta2021_cleaned_for_seq_new.RDS')
