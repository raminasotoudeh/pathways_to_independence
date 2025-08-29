# PSID Transition to Adulthood Variable Construction Script (May 2024)
# This script produces the main data sets used in the analyses

setwd(".")

# Load TA data for all waves
ta2005 = readRDS("./cleaned_ta_data/TA2005.RDS")
ta2007 = readRDS("./cleaned_ta_data/TA2007.RDS")
ta2009 = readRDS("./cleaned_ta_data/TA2009.RDS")
ta2011 = readRDS("./cleaned_ta_data/TA2011.RDS")
ta2013 = readRDS("./cleaned_ta_data/TA2013.RDS")
ta2015 = readRDS("./cleaned_ta_data/TA2015.RDS")
ta2017 = readRDS("./cleaned_ta_data/TA2017.RDS")
ta2019 = readRDS("./cleaned_ta_data/TA2019.RDS")

# packages
library(dplyr)

# Family ID
ta2005$FID = ta2005$TA050003
ta2007$FID = ta2007$TA070003
ta2009$FID = ta2009$TA090003
ta2011$FID = ta2011$TA110003
ta2013$FID = ta2013$TA130003
ta2015$FID = ta2015$TA150003
ta2017$FID = ta2017$TA170003
ta2019$FID = ta2019$TA190003

# Individual ID
ta2005$IID = ta2005$TA050004
ta2007$IID = ta2007$TA070004
ta2009$IID = ta2009$TA090004
ta2011$IID = ta2011$TA110004
ta2013$IID = ta2013$TA130004
ta2015$IID = ta2015$TA150004
ta2017$IID = ta2017$TA170004
ta2019$IID = ta2019$TA190004

# Unique identifier
ta2005$UID = paste(ta2005$FID, ta2005$IID, sep = "_")
ta2007$UID = paste(ta2007$FID, ta2007$IID, sep = "_")
ta2009$UID = paste(ta2009$FID, ta2009$IID, sep = "_")
ta2011$UID = paste(ta2011$FID, ta2011$IID, sep = "_")
ta2013$UID = paste(ta2013$FID, ta2013$IID, sep = "_")
ta2015$UID = paste(ta2015$FID, ta2015$IID, sep = "_")
ta2017$UID = paste(ta2017$FID, ta2017$IID, sep = "_")
ta2019$UID = paste(ta2019$FID, ta2019$IID, sep = "_")

# HOW MUCH RESONSIBLTY EARNG OWN LIVNG
ta2005$earning_responsibility = as.numeric(ta2005$TA050044)
ta2007$earning_responsibility = as.numeric(ta2007$TA070044)
ta2009$earning_responsibility = as.numeric(ta2009$TA090045)
ta2011$earning_responsibility = as.numeric(ta2011$TA110046)
ta2013$earning_responsibility = as.numeric(ta2013$TA130045)
ta2015$earning_responsibility = as.numeric(ta2015$TA150045)
ta2017$earning_responsibility = as.numeric(ta2017$TA170061)
ta2019$earning_responsibility = as.numeric(ta2019$TA190066)

# HOW MUCH RESPONSIBLTY PAYNG OWN RENT
ta2005$rent_responsibility = as.numeric(ta2005$TA050045)
ta2007$rent_responsibility = as.numeric(ta2007$TA070045)
ta2009$rent_responsibility = as.numeric(ta2009$TA090046)
ta2011$rent_responsibility = as.numeric(ta2011$TA110047)
ta2013$rent_responsibility = as.numeric(ta2013$TA130046)
ta2015$rent_responsibility = as.numeric(ta2015$TA150046)
ta2017$rent_responsibility = as.numeric(ta2017$TA170062)
ta2019$rent_responsibility = as.numeric(ta2019$TA190067)

# HOW MUCH RESPONSBLTY FOR OWN BILLS
ta2005$bills_responsibility = as.numeric(ta2005$TA050046)
ta2007$bills_responsibility = as.numeric(ta2007$TA070046)
ta2009$bills_responsibility = as.numeric(ta2009$TA090047)
ta2011$bills_responsibility = as.numeric(ta2011$TA110048)
ta2013$bills_responsibility = as.numeric(ta2013$TA130047)
ta2015$bills_responsibility = as.numeric(ta2015$TA150047)
ta2017$bills_responsibility = as.numeric(ta2017$TA170063)
ta2019$bills_responsibility = as.numeric(ta2019$TA190068)

# HOW MUCH RESPONSIBLTY MANAGING MONEY
ta2005$money_responsibility = as.numeric(ta2005$TA050047)
ta2007$money_responsibility = as.numeric(ta2007$TA070047)
ta2009$money_responsibility = as.numeric(ta2009$TA090048)
ta2011$money_responsibility = as.numeric(ta2011$TA110049)
ta2013$money_responsibility = as.numeric(ta2013$TA130048)
ta2015$money_responsibility = as.numeric(ta2015$TA150048)
ta2017$money_responsibility = as.numeric(ta2017$TA170064)
ta2019$money_responsibility = as.numeric(ta2019$TA190069)

# WTR GIVEN HOUSE/CONDO
ta2005$house_covered = as.numeric(ta2005$TA050559)
ta2005$house_covered = ifelse(ta2005$house_covered == 5, 0, ta2005$house_covered)
ta2005$house_covered = ifelse(ta2005$house_covered > 6, NA, ta2005$house_covered)
ta2005$total_house_value = as.numeric(ta2005$TA050560)
ta2005$total_house_value = ifelse(ta2005$total_house_value >= 9999998, NA, ta2005$total_house_value)

ta2007$house_covered = ta2007$TA070533
ta2007$house_covered = ifelse(ta2007$house_covered == 5, 0, ta2007$house_covered)
ta2007$house_covered = ifelse(ta2007$house_covered > 6, NA, ta2007$house_covered)
ta2007$total_house_value = as.numeric(ta2007$TA070534)
ta2007$total_house_value = ifelse(ta2007$total_house_value >= 9999998, NA, ta2007$total_house_value)

ta2009$house_covered = ta2009$TA090570
ta2009$house_covered = ifelse(ta2009$house_covered == 5, 0, ta2009$house_covered)
ta2009$house_covered = ifelse(ta2009$house_covered > 6, NA, ta2009$house_covered)
ta2009$total_house_value = as.numeric(ta2009$TA090571)
ta2009$total_house_value = ifelse(ta2009$total_house_value >= 9999998, NA, ta2009$total_house_value)

ta2011$house_covered = ta2011$TA110650
ta2011$house_covered = ifelse(ta2011$house_covered == 5, 0, ta2011$house_covered)
ta2011$house_covered = ifelse(ta2011$house_covered > 6, NA, ta2011$house_covered)
ta2011$total_house_value = as.numeric(ta2011$TA110651)
ta2011$total_house_value = ifelse(ta2011$total_house_value >= 9999998, NA, ta2011$total_house_value)

ta2013$house_covered = ta2013$TA130670
ta2013$house_covered = ifelse(ta2013$house_covered == 5, 0, ta2013$house_covered)
ta2013$house_covered = ifelse(ta2013$house_covered > 6, NA, ta2013$house_covered)
ta2013$total_house_value = as.numeric(ta2013$TA130671)
ta2013$total_house_value = ifelse(ta2013$total_house_value >= 9999998, NA, ta2013$total_house_value)

ta2015$house_covered = ta2015$TA150679
ta2015$house_covered = ifelse(ta2015$house_covered == 5, 0, ta2015$house_covered)
ta2015$house_covered = ifelse(ta2015$house_covered > 6, NA, ta2015$house_covered)
ta2015$total_house_value = as.numeric(ta2015$TA150680)
ta2015$total_house_value = ifelse(ta2015$total_house_value >= 9999998, NA, ta2015$total_house_value)


## 2017 and 2019 questions are asked diffehousely; 
## "house_covered" for these two years is whether the amount 
## received is exactly the same as the total price of the car.

ta2017$helped_pay_house = ta2017$TA170634
ta2017$helped_pay_house = ifelse(ta2017$helped_pay_house == 5, 0, ta2017$helped_pay_house)
ta2017$helped_pay_house = ifelse(ta2017$helped_pay_house > 6, NA, ta2017$helped_pay_house)
ta2017$total_house_value = as.numeric(ta2017$TA170635)
ta2017$total_house_value = ifelse(ta2017$total_house_value >= 9999998, NA, ta2017$total_house_value)
ta2017$amt_pahouses_paid_house = as.numeric(ta2017$TA170644)
ta2017$amt_pahouses_paid_house = ifelse(ta2017$amt_pahouses_paid_house >= 9999998, NA, ta2017$amt_pahouses_paid_house)
ta2017$percent_pahouses_paid_house = as.numeric(ta2017$TA170637)
ta2017$percent_pahouses_paid_house = ifelse(ta2017$percent_pahouses_paid_house > 100, NA, ta2017$percent_pahouses_paid_house)

ta2017$prop_pahouses_paid_house = ta2017$amt_pahouses_paid_house/ta2017$total_house_value
ta2017$prop_pahouses_paid_house = ifelse(ta2017$percent_pahouses_paid_house > 0, ta2017$percent_pahouses_paid_house/100, ta2017$prop_pahouses_paid_house) 

# ta2017$house_covered = ifelse(ta2017$prop_pahouses_paid_house == 1, 1, 0)
# ta2017$house_covered = ifelse(is.na(ta2017$house_covered), 0, ta2017$house_covered)

ta2017$house_covered = ta2017$helped_pay_house

ta2019$helped_pay_house = ta2019$TA190839
ta2019$helped_pay_house = ifelse(ta2019$helped_pay_house == 5, 0, ta2019$helped_pay_house)
ta2019$helped_pay_house = ifelse(ta2019$helped_pay_house > 6, NA, ta2019$helped_pay_house)
ta2019$total_house_value = as.numeric(ta2019$TA190840)
ta2019$total_house_value = ifelse(ta2019$total_house_value >= 9999998, NA, ta2019$total_house_value)
ta2019$amt_pahouses_paid_house = as.numeric(ta2019$TA190841)
ta2019$amt_pahouses_paid_house = ifelse(ta2019$amt_pahouses_paid_house >= 9999998, NA, ta2019$amt_pahouses_paid_house)
ta2019$percent_pahouses_paid_house = as.numeric(ta2019$TA190842)
ta2019$percent_pahouses_paid_house = ifelse(ta2019$percent_pahouses_paid_house > 100, NA, ta2019$percent_pahouses_paid_house)

ta2019$prop_pahouses_paid_house = ifelse(ta2019$amt_pahouses_paid_house == 0 & ta2019$total_house_value == 0 ,0, ta2019$amt_pahouses_paid_house/ta2019$total_house_value)
ta2019$prop_pahouses_paid_house = ifelse(ta2019$percent_pahouses_paid_house > 0, ta2019$percent_pahouses_paid_house/100, ta2019$prop_pahouses_paid_house) 

#ta2019$house_covered = ifelse(ta2019$prop_pahouses_paid_house == 1, 1, 0)
#ta2019$house_covered = ifelse(is.na(ta2019$house_covered), 0, ta2019$house_covered)
ta2019$house_covered = ta2019$helped_pay_house

# PARENTS COVERED MORTGAGE OR RENT
ta2005$rent_covered = ta2005$TA050561
ta2005$rent_covered = ifelse(ta2005$rent_covered == 5, 0, ta2005$rent_covered)
ta2005$rent_covered = ifelse(ta2005$rent_covered > 6, NA, ta2005$rent_covered)
ta2005$total_rent_value = as.numeric(ta2005$TA050562)
ta2005$total_rent_value = ifelse(ta2005$total_rent_value >= 9999998, NA, ta2005$total_rent_value)

ta2007$rent_covered = ta2007$TA070535
ta2007$rent_covered = ifelse(ta2007$rent_covered == 5, 0, ta2007$rent_covered)
ta2007$rent_covered = ifelse(ta2007$rent_covered > 6, NA, ta2007$rent_covered)
ta2007$total_rent_value = as.numeric(ta2007$TA070536)
ta2007$total_rent_value = ifelse(ta2007$total_rent_value >= 9999998, NA, ta2007$total_rent_value)

ta2009$rent_covered = ta2009$TA090572
ta2009$rent_covered = ifelse(ta2009$rent_covered == 5, 0, ta2009$rent_covered)
ta2009$rent_covered = ifelse(ta2009$rent_covered > 6, NA, ta2009$rent_covered)
ta2009$total_rent_value = as.numeric(ta2009$TA090573)
ta2009$total_rent_value = ifelse(ta2009$total_rent_value >= 9999998, NA, ta2009$total_rent_value)

ta2011$rent_covered = ta2011$TA110652
ta2011$rent_covered = ifelse(ta2011$rent_covered == 5, 0, ta2011$rent_covered)
ta2011$rent_covered = ifelse(ta2011$rent_covered > 6, NA, ta2011$rent_covered)
ta2011$total_rent_value = as.numeric(ta2011$TA110653)
ta2011$total_rent_value = ifelse(ta2011$total_rent_value >= 9999998, NA, ta2011$total_rent_value)

ta2013$rent_covered = ta2013$TA130672
ta2013$rent_covered = ifelse(ta2013$rent_covered == 5, 0, ta2013$rent_covered)
ta2013$rent_covered = ifelse(ta2013$rent_covered > 6, NA, ta2013$rent_covered)
ta2013$total_rent_value = as.numeric(ta2013$TA130673)
ta2013$total_rent_value = ifelse(ta2013$total_rent_value >= 9999998, NA, ta2013$total_rent_value)

ta2015$rent_covered = ta2015$TA150681
ta2015$rent_covered = ifelse(ta2015$rent_covered == 5, 0, ta2015$rent_covered)
ta2015$rent_covered = ifelse(ta2015$rent_covered > 6, NA, ta2015$rent_covered)
ta2015$total_rent_value = as.numeric(ta2015$TA150682)
ta2015$total_rent_value = ifelse(ta2015$total_rent_value >= 9999998, NA, ta2015$total_rent_value)


## 2017 and 2019 questions are asked differently; 
## "rent_covered" for these two years is whether the amount 
## received is exactly the same as the total price of the car.

ta2017$helped_pay_rent = ta2017$TA170638
ta2017$helped_pay_rent = ifelse(ta2017$helped_pay_rent == 5, 0, ta2017$helped_pay_rent)
ta2017$helped_pay_rent = ifelse(ta2017$helped_pay_rent > 6, NA, ta2017$helped_pay_rent)
ta2017$total_rent_value = as.numeric(ta2017$TA170639)
ta2017$total_rent_value = ifelse(ta2017$total_rent_value >= 9999998, NA, ta2017$total_rent_value)
ta2017$amt_parents_paid_rent = as.numeric(ta2017$TA170640)
ta2017$amt_parents_paid_rent = ifelse(ta2017$amt_parents_paid_rent >= 9999998, NA, ta2017$amt_parents_paid_rent)
ta2017$percent_parents_paid_rent = as.numeric(ta2017$TA170641)
ta2017$percent_parents_paid_rent = ifelse(ta2017$percent_parents_paid_rent > 100, NA, ta2017$percent_parents_paid_rent)

ta2017$prop_parents_paid_rent = ifelse(ta2017$amt_parents_paid_rent == 0 & ta2017$total_rent_value == 0, 0, 
                                       ifelse(ta2017$amt_parents_paid_rent > ta2017$total_rent_value, 1, 
                                              ta2017$amt_parents_paid_rent / ta2017$total_rent_value))

ta2017$prop_parents_paid_rent = ifelse(ta2017$percent_parents_paid_rent > 0, ta2017$percent_parents_paid_rent/100, ta2017$prop_parents_paid_rent) 

# ta2017$rent_covered = ifelse(ta2017$prop_parents_paid_rent == 1, 1, 0)
# ta2017$rent_covered = ifelse(is.na(ta2017$rent_covered), 0, ta2017$rent_covered)
ta2017$rent_covered = ta2017$helped_pay_rent

ta2019$helped_pay_rent = ta2019$TA190843
ta2019$helped_pay_rent = ifelse(ta2019$helped_pay_rent == 5, 0, ta2019$helped_pay_rent)
ta2019$helped_pay_rent = ifelse(ta2019$helped_pay_rent > 6, NA, ta2019$helped_pay_rent)
ta2019$total_rent_value = as.numeric(ta2019$TA190844)
ta2019$total_rent_value = ifelse(ta2019$total_rent_value >= 9999998, NA, ta2019$total_rent_value)
ta2019$amt_parents_paid_rent = as.numeric(ta2019$TA190845)
ta2019$amt_parents_paid_rent = ifelse(ta2019$amt_parents_paid_rent >= 9999998, NA, ta2019$amt_parents_paid_rent)
ta2019$percent_parents_paid_rent = as.numeric(ta2019$TA190846)
ta2019$percent_parents_paid_rent = ifelse(ta2019$percent_parents_paid_rent > 100, NA, ta2019$percent_parents_paid_rent)

ta2019$prop_parents_paid_rent = ifelse(ta2019$amt_parents_paid_rent == 0 & ta2019$total_rent_value == 0, 0, 
                                       ifelse(ta2019$amt_parents_paid_rent > ta2019$total_rent_value, 1, 
                                              ta2019$amt_parents_paid_rent / ta2019$total_rent_value))

ta2019$prop_parents_paid_rent = ifelse(ta2019$percent_parents_paid_rent > 0, ta2019$percent_parents_paid_rent/100, ta2019$prop_parents_paid_rent) 

#ta2019$rent_covered = ifelse(ta2019$prop_parents_paid_rent == 1, 1, 0)
#ta2019$rent_covered = ifelse(is.na(ta2019$rent_covered), 0, ta2019$rent_covered)
ta2019$rent_covered = ta2019$helped_pay_rent

# PARENTS GAVE VEHICLE
ta2005$vehicle_covered = ta2005$TA050563
ta2005$vehicle_covered = ifelse(ta2005$vehicle_covered == 5, 0, ta2005$vehicle_covered)
ta2005$vehicle_covered = ifelse(ta2005$vehicle_covered > 6, NA, ta2005$vehicle_covered)
ta2005$total_vehicle_value = as.numeric(ta2005$TA050564)
ta2005$total_vehicle_value = ifelse(ta2005$total_vehicle_value >= 9999998, NA, ta2005$total_vehicle_value)

ta2007$vehicle_covered = ta2007$TA070537
ta2007$vehicle_covered = ifelse(ta2007$vehicle_covered == 5, 0, ta2007$vehicle_covered)
ta2007$vehicle_covered = ifelse(ta2007$vehicle_covered > 6, NA, ta2007$vehicle_covered)
ta2007$total_vehicle_value = as.numeric(ta2007$TA070538)
ta2007$total_vehicle_value = ifelse(ta2007$total_vehicle_value >= 9999998, NA, ta2007$total_vehicle_value)

ta2009$vehicle_covered = ta2009$TA090574
ta2009$vehicle_covered = ifelse(ta2009$vehicle_covered == 5, 0, ta2009$vehicle_covered)
ta2009$vehicle_covered = ifelse(ta2009$vehicle_covered > 6, NA, ta2009$vehicle_covered)
ta2009$total_vehicle_value = as.numeric(ta2009$TA090575)
ta2009$total_vehicle_value = ifelse(ta2009$total_vehicle_value >= 9999998, NA, ta2009$total_vehicle_value)

ta2011$vehicle_covered = ta2011$TA110654
ta2011$vehicle_covered = ifelse(ta2011$vehicle_covered == 5, 0, ta2011$vehicle_covered)
ta2011$vehicle_covered = ifelse(ta2011$vehicle_covered > 6, NA, ta2011$vehicle_covered)
ta2011$total_vehicle_value = as.numeric(ta2011$TA110655)
ta2011$total_vehicle_value = ifelse(ta2011$total_vehicle_value >= 9999998, NA, ta2011$total_vehicle_value)

ta2013$vehicle_covered = ta2013$TA130674
ta2013$vehicle_covered = ifelse(ta2013$vehicle_covered == 5, 0, ta2013$vehicle_covered)
ta2013$vehicle_covered = ifelse(ta2013$vehicle_covered > 6, NA, ta2013$vehicle_covered)
ta2013$total_vehicle_value = as.numeric(ta2013$TA130675)
ta2013$total_vehicle_value = ifelse(ta2013$total_vehicle_value >= 9999998, NA, ta2013$total_vehicle_value)

ta2015$vehicle_covered = ta2015$TA150683
ta2015$vehicle_covered = ifelse(ta2015$vehicle_covered == 5, 0, ta2015$vehicle_covered)
ta2015$vehicle_covered = ifelse(ta2015$vehicle_covered > 6, NA, ta2015$vehicle_covered)
ta2015$total_vehicle_value = as.numeric(ta2015$TA150684)
ta2015$total_vehicle_value = ifelse(ta2015$total_vehicle_value >= 9999998, NA, ta2015$total_vehicle_value)


## 2017 and 2019 questions are asked differently; 
## "vehicle_covered" for these two years is whether the amount 
## received is exactly the same as the total price of the car.

ta2017$helped_pay_vehicle = ta2017$TA170642
ta2017$helped_pay_vehicle = ifelse(ta2017$helped_pay_vehicle == 5, 0, ta2017$helped_pay_vehicle)
ta2017$helped_pay_vehicle = ifelse(ta2017$helped_pay_vehicle > 6, NA, ta2017$helped_pay_vehicle)
ta2017$total_vehicle_value = as.numeric(ta2017$TA170643)
ta2017$total_vehicle_value = ifelse(ta2017$total_vehicle_value >= 9999998, NA, ta2017$total_vehicle_value)
ta2017$amt_parents_paid_vehicle = as.numeric(ta2017$TA170644)
ta2017$amt_parents_paid_vehicle = ifelse(ta2017$amt_parents_paid_vehicle >= 9999998, NA, ta2017$amt_parents_paid_vehicle)
ta2017$percent_parents_paid_vehicle = as.numeric(ta2017$TA170645)
ta2017$percent_parents_paid_vehicle = ifelse(ta2017$percent_parents_paid_vehicle > 100, NA, ta2017$percent_parents_paid_vehicle)

ta2017$prop_parents_paid_vehicle = ta2017$amt_parents_paid_vehicle/ta2017$total_vehicle_value
ta2017$prop_parents_paid_vehicle = ifelse(ta2017$percent_parents_paid_vehicle > 0, ta2017$percent_parents_paid_vehicle/100, ta2017$prop_parents_paid_vehicle) 

#ta2017$vehicle_covered = ifelse(ta2017$prop_parents_paid_vehicle == 1, 1, 0)
#ta2017$vehicle_covered = ifelse(is.na(ta2017$vehicle_covered), 0, ta2017$vehicle_covered)
ta2017$vehicle_covered = ta2017$helped_pay_vehicle 
  
ta2019$helped_pay_vehicle = ta2019$TA190847
ta2019$helped_pay_vehicle = ifelse(ta2019$helped_pay_vehicle == 5, 0, ta2019$helped_pay_vehicle)
ta2019$helped_pay_vehicle = ifelse(ta2019$helped_pay_vehicle > 6, NA, ta2019$helped_pay_vehicle)
ta2019$total_vehicle_value = as.numeric(ta2019$TA190848)
ta2019$total_vehicle_value = ifelse(ta2019$total_vehicle_value >= 9999998, NA, ta2019$total_vehicle_value)
ta2019$amt_parents_paid_vehicle = as.numeric(ta2019$TA190849)
ta2019$amt_parents_paid_vehicle = ifelse(ta2019$amt_parents_paid_vehicle >= 9999998, NA, ta2019$amt_parents_paid_vehicle)
ta2019$percent_parents_paid_vehicle = as.numeric(ta2019$TA190850)
ta2019$percent_parents_paid_vehicle = ifelse(ta2019$percent_parents_paid_vehicle > 100, NA, ta2019$percent_parents_paid_vehicle)

ta2019$prop_parents_paid_vehicle = ta2019$amt_parents_paid_vehicle/ta2019$total_vehicle_value
ta2019$prop_parents_paid_vehicle = ifelse(ta2019$percent_parents_paid_vehicle > 0, ta2019$percent_parents_paid_vehicle/100, ta2019$prop_parents_paid_vehicle) 

#ta2019$vehicle_covered = ifelse(ta2019$prop_parents_paid_vehicle == 1, 1, 0)
#ta2019$vehicle_covered = ifelse(is.na(ta2019$vehicle_covered), 0, ta2019$vehicle_covered)
ta2019$vehicle_covered = ta2019$helped_pay_vehicle 

# PARENTS GAVE TUITION
ta2005$tuition_covered = ta2005$TA050565
ta2005$tuition_covered = ifelse(ta2005$tuition_covered == 5, 0, ta2005$tuition_covered)
ta2005$tuition_covered = ifelse(ta2005$tuition_covered > 6, NA, ta2005$tuition_covered)
ta2005$total_tuition_value = as.numeric(ta2005$TA050566)
ta2005$total_tuition_value = ifelse(ta2005$total_tuition_value >= 9999998, NA, ta2005$total_tuition_value)

ta2007$tuition_covered = ta2007$TA070539
ta2007$tuition_covered = ifelse(ta2007$tuition_covered == 5, 0, ta2007$tuition_covered)
ta2007$tuition_covered = ifelse(ta2007$tuition_covered > 6, NA, ta2007$tuition_covered)
ta2007$total_tuition_value = as.numeric(ta2007$TA070540)
ta2007$total_tuition_value = ifelse(ta2007$total_tuition_value >= 9999998, NA, ta2007$total_tuition_value)

ta2009$tuition_covered = ta2009$TA090576
ta2009$tuition_covered = ifelse(ta2009$tuition_covered == 5, 0, ta2009$tuition_covered)
ta2009$tuition_covered = ifelse(ta2009$tuition_covered > 6, NA, ta2009$tuition_covered)
ta2009$total_tuition_value = as.numeric(ta2009$TA090577)
ta2009$total_tuition_value = ifelse(ta2009$total_tuition_value >= 9999998, NA, ta2009$total_tuition_value)

ta2011$tuition_covered = ta2011$TA110656
ta2011$tuition_covered = ifelse(ta2011$tuition_covered == 5, 0, ta2011$tuition_covered)
ta2011$tuition_covered = ifelse(ta2011$tuition_covered > 6, NA, ta2011$tuition_covered)
ta2011$total_tuition_value = as.numeric(ta2011$TA110657)
ta2011$total_tuition_value = ifelse(ta2011$total_tuition_value >= 9999998, NA, ta2011$total_tuition_value)

ta2013$tuition_covered = ta2013$TA130676
ta2013$tuition_covered = ifelse(ta2013$tuition_covered == 5, 0, ta2013$tuition_covered)
ta2013$tuition_covered = ifelse(ta2013$tuition_covered > 6, NA, ta2013$tuition_covered)
ta2013$total_tuition_value = as.numeric(ta2013$TA130677)
ta2013$total_tuition_value = ifelse(ta2013$total_tuition_value >= 9999998, NA, ta2013$total_tuition_value)

ta2015$tuition_covered = ta2015$TA150685
ta2015$tuition_covered = ifelse(ta2015$tuition_covered == 5, 0, ta2015$tuition_covered)
ta2015$tuition_covered = ifelse(ta2015$tuition_covered > 6, NA, ta2015$tuition_covered)
ta2015$total_tuition_value = as.numeric(ta2015$TA150686)
ta2015$total_tuition_value = ifelse(ta2015$total_tuition_value >= 9999998, NA, ta2015$total_tuition_value)

## 2017 and 2019 questions are asked differently; 
## "tuition_covered" for these two years is whether the amount 
## received is exactly the same as the total price of the tuition.

ta2017$helped_pay_tuition = ta2017$TA170646
ta2017$helped_pay_tuition = ifelse(ta2017$helped_pay_tuition == 5, 0, ta2017$helped_pay_tuition)
ta2017$helped_pay_tuition = ifelse(ta2017$helped_pay_tuition > 6, NA, ta2017$helped_pay_tuition)
ta2017$total_tuition_value = as.numeric(ta2017$TA170647)
ta2017$total_tuition_value = ifelse(ta2017$total_tuition_value >= 9999998, NA, ta2017$total_tuition_value)
ta2017$amt_parents_paid_tuition = as.numeric(ta2017$TA170648)
ta2017$amt_parents_paid_tuition = ifelse(ta2017$amt_parents_paid_tuition >= 9999998, NA, ta2017$amt_parents_paid_tuition)
ta2017$percent_parents_paid_tuition = as.numeric(ta2017$TA170649)
ta2017$percent_parents_paid_tuition = ifelse(ta2017$percent_parents_paid_tuition > 100, NA, ta2017$percent_parents_paid_tuition)

ta2017$prop_parents_paid_tuition = ta2017$amt_parents_paid_tuition/ta2017$total_tuition_value
ta2017$prop_parents_paid_tuition = ifelse(ta2017$percent_parents_paid_tuition > 0, ta2017$percent_parents_paid_tuition/100, ta2017$prop_parents_paid_tuition) 

#ta2017$tuition_covered = ifelse(ta2017$prop_parents_paid_tuition == 1, 1, 0)
#ta2017$tuition_covered = ifelse(is.na(ta2017$tuition_covered), 0, ta2017$tuition_covered)
ta2017$tuition_covered = ta2017$helped_pay_tuition 

ta2019$helped_pay_tuition = ta2019$TA190851
ta2019$helped_pay_tuition = ifelse(ta2019$helped_pay_tuition == 5, 0, ta2019$helped_pay_tuition)
ta2019$helped_pay_tuition = ifelse(ta2019$helped_pay_tuition > 6, NA, ta2019$helped_pay_tuition)
ta2019$total_tuition_value = as.numeric(ta2019$TA190852)
ta2019$total_tuition_value = ifelse(ta2019$total_tuition_value >= 9999998, NA, ta2019$total_tuition_value)
ta2019$amt_parents_paid_tuition = as.numeric(ta2019$TA190853)
ta2019$amt_parents_paid_tuition = ifelse(ta2019$amt_parents_paid_tuition >= 9999998, NA, ta2019$amt_parents_paid_tuition)
ta2019$percent_parents_paid_tuition = as.numeric(ta2019$TA190854)
ta2019$percent_parents_paid_tuition = ifelse(ta2019$percent_parents_paid_tuition > 100, NA, ta2019$percent_parents_paid_tuition)

ta2019$prop_parents_paid_tuition = ta2019$amt_parents_paid_tuition/ta2019$total_tuition_value
ta2019$prop_parents_paid_tuition = ifelse(ta2019$percent_parents_paid_tuition > 0, ta2019$percent_parents_paid_tuition/100, ta2019$prop_parents_paid_tuition) 

#ta2019$tuition_covered = ifelse(ta2019$prop_parents_paid_tuition == 1, 1, 0)
#ta2019$tuition_covered = ifelse(is.na(ta2019$tuition_covered), 0, ta2019$tuition_covered)

ta2019$tuition_covered = ta2019$helped_pay_tuition 


# PARENTS GAVE EXPENSES/BILLS
ta2005$bills_covered = ta2005$TA050567
ta2005$bills_covered = ifelse(ta2005$bills_covered == 5, 0, ta2005$bills_covered)
ta2005$bills_covered = ifelse(ta2005$bills_covered > 6, NA, ta2005$bills_covered)
ta2005$total_bills_value = as.numeric(ta2005$TA050568)
ta2005$total_bills_value = ifelse(ta2005$total_bills_value >= 9999998, NA, ta2005$total_bills_value)

ta2007$bills_covered = ta2007$TA070541
ta2007$bills_covered = ifelse(ta2007$bills_covered == 5, 0, ta2007$bills_covered)
ta2007$bills_covered = ifelse(ta2007$bills_covered > 6, NA, ta2007$bills_covered)
ta2007$total_bills_value = as.numeric(ta2007$TA070542)
ta2007$total_bills_value = ifelse(ta2007$total_bills_value >= 9999998, NA, ta2007$total_bills_value)

ta2009$bills_covered = ta2009$TA090578
ta2009$bills_covered = ifelse(ta2009$bills_covered == 5, 0, ta2009$bills_covered)
ta2009$bills_covered = ifelse(ta2009$bills_covered > 6, NA, ta2009$bills_covered)
ta2009$total_bills_value = as.numeric(ta2009$TA090579)
ta2009$total_bills_value = ifelse(ta2009$total_bills_value >= 9999998, NA, ta2009$total_bills_value)

ta2011$bills_covered = ta2011$TA110658
ta2011$bills_covered = ifelse(ta2011$bills_covered == 5, 0, ta2011$bills_covered)
ta2011$bills_covered = ifelse(ta2011$bills_covered > 6, NA, ta2011$bills_covered)
ta2011$total_bills_value = as.numeric(ta2011$TA110659)
ta2011$total_bills_value = ifelse(ta2011$total_bills_value >= 9999998, NA, ta2011$total_bills_value)

ta2013$bills_covered = ta2013$TA130678
ta2013$bills_covered = ifelse(ta2013$bills_covered == 5, 0, ta2013$bills_covered)
ta2013$bills_covered = ifelse(ta2013$bills_covered > 6, NA, ta2013$bills_covered)
ta2013$total_bills_value = as.numeric(ta2013$TA130679)
ta2013$total_bills_value = ifelse(ta2013$total_bills_value >= 9999998, NA, ta2013$total_bills_value)

ta2015$bills_covered = ta2015$TA150687
ta2015$bills_covered = ifelse(ta2015$bills_covered == 5, 0, ta2015$bills_covered)
ta2015$bills_covered = ifelse(ta2015$bills_covered > 6, NA, ta2015$bills_covered)
ta2015$total_bills_value = as.numeric(ta2015$TA150688)
ta2015$total_bills_value = ifelse(ta2015$total_bills_value >= 9999998, NA, ta2015$total_bills_value)

## 2017 and 2019 questions are asked differently; 
## "bills_covered" for these two years is whether the amount 
## received is exactly the same as the total price of the bills.

ta2017$helped_pay_bills = ta2017$TA170656
ta2017$helped_pay_bills = ifelse(ta2017$helped_pay_bills == 5, 0, ta2017$helped_pay_bills)
ta2017$helped_pay_bills = ifelse(ta2017$helped_pay_bills > 6, NA, ta2017$helped_pay_bills)
ta2017$total_bills_value = as.numeric(ta2017$TA170657)
ta2017$total_bills_value = ifelse(ta2017$total_bills_value >= 9999998, NA, ta2017$total_bills_value)
ta2017$amt_parents_paid_bills = as.numeric(ta2017$TA170658)
ta2017$amt_parents_paid_bills = ifelse(ta2017$amt_parents_paid_bills >= 9999998, NA, ta2017$amt_parents_paid_bills)
ta2017$percent_parents_paid_bills = as.numeric(ta2017$TA170659)
ta2017$percent_parents_paid_bills = ifelse(ta2017$percent_parents_paid_bills > 100, NA, ta2017$percent_parents_paid_bills)

ta2017$prop_parents_paid_bills = ta2017$amt_parents_paid_bills/ta2017$total_bills_value
ta2017$prop_parents_paid_bills = ifelse(ta2017$percent_parents_paid_bills > 0, ta2017$percent_parents_paid_bills/100, ta2017$prop_parents_paid_bills) 

#ta2017$bills_covered = ifelse(ta2017$prop_parents_paid_bills == 1, 1, 0)
#ta2017$bills_covered = ifelse(is.na(ta2017$bills_covered), 0, ta2017$bills_covered)
ta2017$bills_covered = ta2017$helped_pay_bills 

ta2019$helped_pay_bills = ta2019$TA190859
ta2019$helped_pay_bills = ifelse(ta2019$helped_pay_bills == 5, 0, ta2019$helped_pay_bills)
ta2019$helped_pay_bills = ifelse(ta2019$helped_pay_bills > 6, NA, ta2019$helped_pay_bills)
ta2019$total_bills_value = as.numeric(ta2019$TA190860)
ta2019$total_bills_value = ifelse(ta2019$total_bills_value >= 9999998, NA, ta2019$total_bills_value)
ta2019$amt_parents_paid_bills = as.numeric(ta2019$TA190861)
ta2019$amt_parents_paid_bills = ifelse(ta2019$amt_parents_paid_bills >= 9999998, NA, ta2019$amt_parents_paid_bills)
ta2019$percent_parents_paid_bills = as.numeric(ta2019$TA190862)
ta2019$percent_parents_paid_bills = ifelse(ta2019$percent_parents_paid_bills > 100, NA, ta2019$percent_parents_paid_bills)

ta2019$prop_parents_paid_bills = ta2019$amt_parents_paid_bills/ta2019$total_bills_value
ta2019$prop_parents_paid_bills = ifelse(ta2019$percent_parents_paid_bills > 0, ta2019$percent_parents_paid_bills/100, ta2019$prop_parents_paid_bills) 

#ta2019$bills_covered = ifelse(ta2019$prop_parents_paid_bills == 1, 1, 0)
#ta2019$bills_covered = ifelse(is.na(ta2019$bills_covered), 0, ta2019$bills_covered)
ta2019$bills_covered = ta2019$helped_pay_bills 

# PARENTS GAVE PERSONAL LOAN
ta2005$personal_loan_covered = ta2005$TA050569
ta2005$personal_loan_covered = ifelse(ta2005$personal_loan_covered == 5, 0, ta2005$personal_loan_covered)
ta2005$personal_loan_covered = ifelse(ta2005$personal_loan_covered > 6, NA, ta2005$personal_loan_covered)
ta2005$total_personal_loan_value = as.numeric(ta2005$TA050570)
ta2005$total_personal_loan_value = ifelse(ta2005$total_personal_loan_value >= 9999998, NA, ta2005$total_personal_loan_value)

ta2007$personal_loan_covered = ta2007$TA070543
ta2007$personal_loan_covered = ifelse(ta2007$personal_loan_covered == 5, 0, ta2007$personal_loan_covered)
ta2007$personal_loan_covered = ifelse(ta2007$personal_loan_covered > 6, NA, ta2007$personal_loan_covered)
ta2007$total_personal_loan_value = as.numeric(ta2007$TA070544)
ta2007$total_personal_loan_value = ifelse(ta2007$total_personal_loan_value >= 9999998, NA, ta2007$total_personal_loan_value)

ta2009$personal_loan_covered = ta2009$TA090580
ta2009$personal_loan_covered = ifelse(ta2009$personal_loan_covered == 5, 0, ta2009$personal_loan_covered)
ta2009$personal_loan_covered = ifelse(ta2009$personal_loan_covered > 6, NA, ta2009$personal_loan_covered)
ta2009$total_personal_loan_value = as.numeric(ta2009$TA090581)
ta2009$total_personal_loan_value = ifelse(ta2009$total_personal_loan_value >= 9999998, NA, ta2009$total_personal_loan_value)

ta2011$personal_loan_covered = ta2011$TA110660
ta2011$personal_loan_covered = ifelse(ta2011$personal_loan_covered == 5, 0, ta2011$personal_loan_covered)
ta2011$personal_loan_covered = ifelse(ta2011$personal_loan_covered > 6, NA, ta2011$personal_loan_covered)
ta2011$total_personal_loan_value = as.numeric(ta2011$TA110661)
ta2011$total_personal_loan_value = ifelse(ta2011$total_personal_loan_value >= 9999998, NA, ta2011$total_personal_loan_value)

ta2013$personal_loan_covered = ta2013$TA130680
ta2013$personal_loan_covered = ifelse(ta2013$personal_loan_covered == 5, 0, ta2013$personal_loan_covered)
ta2013$personal_loan_covered = ifelse(ta2013$personal_loan_covered > 6, NA, ta2013$personal_loan_covered)
ta2013$total_personal_loan_value = as.numeric(ta2013$TA130681)
ta2013$total_personal_loan_value = ifelse(ta2013$total_personal_loan_value >= 9999998, NA, ta2013$total_personal_loan_value)

ta2015$personal_loan_covered = ta2015$TA150689
ta2015$personal_loan_covered = ifelse(ta2015$personal_loan_covered == 5, 0, ta2015$personal_loan_covered)
ta2015$personal_loan_covered = ifelse(ta2015$personal_loan_covered > 6, NA, ta2015$personal_loan_covered)
ta2015$total_personal_loan_value = as.numeric(ta2015$TA150690)
ta2015$total_personal_loan_value = ifelse(ta2015$total_personal_loan_value >= 9999998, NA, ta2015$total_personal_loan_value)

ta2017$personal_loan_covered = ta2017$TA170654
ta2017$personal_loan_covered = ifelse(ta2017$personal_loan_covered == 5, 0, ta2017$personal_loan_covered)
ta2017$personal_loan_covered = ifelse(ta2017$personal_loan_covered > 6, NA, ta2017$personal_loan_covered)
ta2017$total_personal_loan_value = as.numeric(ta2017$TA170655)
ta2017$total_personal_loan_value = ifelse(ta2017$total_personal_loan_value >= 9999998, NA, ta2017$total_personal_loan_value)

ta2019$personal_loan_covered = ta2019$TA190837
ta2019$personal_loan_covered = ifelse(ta2019$personal_loan_covered == 5, 0, ta2019$personal_loan_covered)
ta2019$personal_loan_covered = ifelse(ta2019$personal_loan_covered > 6, NA, ta2019$personal_loan_covered)
ta2019$total_personal_loan_value = as.numeric(ta2019$TA190838)
ta2019$total_personal_loan_value = ifelse(ta2019$total_personal_loan_value >= 9999998, NA, ta2019$total_personal_loan_value)



# PARENTS GAVE GIFT / INHERITENCE
ta2005$parents_gave_gift_inheritence = ta2005$TA050571
ta2005$parents_gave_gift_inheritence = ifelse(ta2005$parents_gave_gift_inheritence == 5, 0, ta2005$parents_gave_gift_inheritence)
ta2005$parents_gave_gift_inheritence = ifelse(ta2005$parents_gave_gift_inheritence > 6, NA, ta2005$parents_gave_gift_inheritence)
ta2005$total_gift_inheritence_value = as.numeric(ta2005$TA050572)
ta2005$total_gift_inheritence_value = ifelse(ta2005$total_gift_inheritence_value >= 9999998, NA, ta2005$total_gift_inheritence_value)

ta2007$parents_gave_gift_inheritence = ta2007$TA070546
ta2007$parents_gave_gift_inheritence = ifelse(ta2007$parents_gave_gift_inheritence == 5, 0, ta2007$parents_gave_gift_inheritence)
ta2007$parents_gave_gift_inheritence = ifelse(ta2007$parents_gave_gift_inheritence > 6, NA, ta2007$parents_gave_gift_inheritence)
ta2007$total_gift_inheritence_value = as.numeric(ta2007$TA070547)
ta2007$total_gift_inheritence_value = ifelse(ta2007$total_gift_inheritence_value >= 9999998, NA, ta2007$total_gift_inheritence_value)

ta2009$parents_gave_gift_inheritence = ta2009$TA090583
ta2009$parents_gave_gift_inheritence = ifelse(ta2009$parents_gave_gift_inheritence == 5, 0, ta2009$parents_gave_gift_inheritence)
ta2009$parents_gave_gift_inheritence = ifelse(ta2009$parents_gave_gift_inheritence > 6, NA, ta2009$parents_gave_gift_inheritence)
ta2009$gift_inheritence_value1 = as.numeric(ta2009$TA090584) 
ta2009$gift_inheritence_value1 = ifelse(ta2009$gift_inheritence_value1 >= 9999998, NA, ta2009$gift_inheritence_value1)
ta2009$gift_inheritence_value2 = as.numeric(ta2009$TA090586) 
ta2009$gift_inheritence_value2 = ifelse(ta2009$gift_inheritence_value2 >= 9999998, NA, ta2009$gift_inheritence_value2)
ta2009$gift_inheritence_value3 = as.numeric(ta2009$TA090588)
ta2009$gift_inheritence_value3 = ifelse(ta2009$gift_inheritence_value3 >= 9999998, NA, ta2009$gift_inheritence_value3)
ta2009$total_gift_inheritence_value = ta2009$gift_inheritence_value1 + ta2009$gift_inheritence_value2 + ta2009$gift_inheritence_value3

ta2011$parents_gave_gift_inheritence = ta2011$TA110664
ta2011$parents_gave_gift_inheritence = ifelse(ta2011$parents_gave_gift_inheritence == 5, 0, ta2011$parents_gave_gift_inheritence)
ta2011$parents_gave_gift_inheritence = ifelse(ta2011$parents_gave_gift_inheritence > 6, NA, ta2011$parents_gave_gift_inheritence)
ta2011$gift_inheritence_value1 = as.numeric(ta2011$TA110665) 
ta2011$gift_inheritence_value1 = ifelse(ta2011$gift_inheritence_value1 >= 9999998, NA, ta2011$gift_inheritence_value1)
ta2011$gift_inheritence_value2 = as.numeric(ta2011$TA110667) 
ta2011$gift_inheritence_value2 = ifelse(ta2011$gift_inheritence_value2 >= 9999998, NA, ta2011$gift_inheritence_value2)
ta2011$gift_inheritence_value3 = as.numeric(ta2011$TA110669)
ta2011$gift_inheritence_value3 = ifelse(ta2011$gift_inheritence_value3 >= 9999998, NA, ta2011$gift_inheritence_value3)
ta2011$total_gift_inheritence_value = ta2011$gift_inheritence_value1 + ta2011$gift_inheritence_value2 + ta2011$gift_inheritence_value3

ta2013$parents_gave_gift_inheritence = ta2013$TA130684
ta2013$parents_gave_gift_inheritence = ifelse(ta2013$parents_gave_gift_inheritence == 5, 0, ta2013$parents_gave_gift_inheritence)
ta2013$parents_gave_gift_inheritence = ifelse(ta2013$parents_gave_gift_inheritence > 6, NA, ta2013$parents_gave_gift_inheritence)
ta2013$gift_inheritence_value1 = as.numeric(ta2013$TA130685) 
ta2013$gift_inheritence_value1 = ifelse(ta2013$gift_inheritence_value1 >= 9999998, NA, ta2013$gift_inheritence_value1)
ta2013$gift_inheritence_value2 = as.numeric(ta2013$TA130687) 
ta2013$gift_inheritence_value2 = ifelse(ta2013$gift_inheritence_value2 >= 9999998, NA, ta2013$gift_inheritence_value2)
ta2013$gift_inheritence_value3 = as.numeric(ta2013$TA130689)
ta2013$gift_inheritence_value3 = ifelse(ta2013$gift_inheritence_value3 >= 9999998, NA, ta2013$gift_inheritence_value3)
ta2013$total_gift_inheritence_value = ta2013$gift_inheritence_value1 + ta2013$gift_inheritence_value2 + ta2013$gift_inheritence_value3

ta2015$parents_gave_gift_inheritence = ta2015$TA150693
ta2015$parents_gave_gift_inheritence = ifelse(ta2015$parents_gave_gift_inheritence == 5, 0, ta2015$parents_gave_gift_inheritence)
ta2015$parents_gave_gift_inheritence = ifelse(ta2015$parents_gave_gift_inheritence > 6, NA, ta2015$parents_gave_gift_inheritence)
ta2015$gift_inheritence_value1 = as.numeric(ta2015$TA150695) 
ta2015$gift_inheritence_value1 = ifelse(ta2015$gift_inheritence_value1 >= 9999998, NA, ta2015$gift_inheritence_value1)
ta2015$gift_inheritence_value2 = as.numeric(ta2015$TA150697) 
ta2015$gift_inheritence_value2 = ifelse(ta2015$gift_inheritence_value2 >= 9999998, NA, ta2015$gift_inheritence_value2)
ta2015$gift_inheritence_value3 = as.numeric(ta2015$TA150699)
ta2015$gift_inheritence_value3 = ifelse(ta2015$gift_inheritence_value3 >= 9999998, NA, ta2015$gift_inheritence_value3)
ta2015$total_gift_inheritence_value = ta2015$gift_inheritence_value1 + ta2015$gift_inheritence_value2 + ta2015$gift_inheritence_value3

ta2017$parents_gave_gift_inheritence = ta2017$TA170662
ta2017$parents_gave_gift_inheritence = ifelse(ta2017$parents_gave_gift_inheritence == 5, 0, ta2017$parents_gave_gift_inheritence)
ta2017$parents_gave_gift_inheritence = ifelse(ta2017$parents_gave_gift_inheritence > 6, NA, ta2017$parents_gave_gift_inheritence)
ta2017$gift_inheritence_value1 = as.numeric(ta2017$TA170667) 
ta2017$gift_inheritence_value1 = ifelse(ta2017$gift_inheritence_value1 >= 9999998, NA, ta2017$gift_inheritence_value1)
ta2017$gift_inheritence_value2 = as.numeric(ta2017$TA170672) 
ta2017$gift_inheritence_value2 = ifelse(ta2017$gift_inheritence_value2 >= 9999998, NA, ta2017$gift_inheritence_value2)
ta2017$gift_inheritence_value3 = as.numeric(ta2017$TA170677)
ta2017$gift_inheritence_value3 = ifelse(ta2017$gift_inheritence_value3 >= 9999998, NA, ta2017$gift_inheritence_value3)
ta2017$total_gift_inheritence_value = ta2017$gift_inheritence_value1 + ta2017$gift_inheritence_value2 + ta2017$gift_inheritence_value3

### note this changed!!! 2019 is different 
ta2019$parents_gave_gift_inheritence = ta2019$TA190863
ta2019$parents_gave_gift_inheritence = ifelse(ta2019$parents_gave_gift_inheritence == 5, 0, ta2019$parents_gave_gift_inheritence)
ta2019$parents_gave_gift_inheritence = ifelse(ta2019$parents_gave_gift_inheritence == 1 | ta2019$parents_gave_gift_inheritence == 2, 1, ta2019$parents_gave_gift_inheritence)
ta2019$parents_gave_gift_inheritence = ifelse(ta2019$parents_gave_gift_inheritence > 6, NA, ta2019$parents_gave_gift_inheritence)
ta2019$gift_inheritence_value1 = as.numeric(ta2019$TA190869) 
ta2019$gift_inheritence_value1 = ifelse(ta2019$gift_inheritence_value1 >= 9999998, NA, ta2019$gift_inheritence_value1)
ta2019$gift_inheritence_value2 = as.numeric(ta2019$TA190875) 
ta2019$gift_inheritence_value2 = ifelse(ta2019$gift_inheritence_value2 >= 9999998, NA, ta2019$gift_inheritence_value2)
ta2019$gift_inheritence_value3 = as.numeric(ta2019$TA190881)
ta2019$gift_inheritence_value3 = ifelse(ta2019$gift_inheritence_value3 >= 9999998, NA, ta2019$gift_inheritence_value3)
ta2019$total_gift_inheritence_value = ta2019$gift_inheritence_value1 + ta2019$gift_inheritence_value2 + ta2019$gift_inheritence_value3

# GRADUATED HIGH SCHOOL OR GED
ta2005$graduated_high_school = ta2005$TA050573
ta2005$graduated_high_school = ifelse(ta2005$graduated_high_school > 7 | ta2005$graduated_high_school == 0, NA, ta2005$graduated_high_school)
ta2005$graduated_high_school = ifelse(ta2005$graduated_high_school == 1 | ta2005$graduated_high_school == 2, 1, 0)

ta2007$graduated_high_school = ta2007$TA070548
ta2007$graduated_high_school = ifelse(ta2007$graduated_high_school > 7 | ta2007$graduated_high_school == 0, NA, ta2007$graduated_high_school)
ta2007$graduated_high_school = ifelse(ta2007$graduated_high_school == 1 | ta2007$graduated_high_school == 2, 1, 0)

ta2009$graduated_high_school = ta2009$TA090590
ta2009$graduated_high_school = ifelse(ta2009$graduated_high_school > 7 | ta2009$graduated_high_school == 0, NA, ta2009$graduated_high_school)
ta2009$graduated_high_school = ifelse(ta2009$graduated_high_school == 1 | ta2009$graduated_high_school == 2, 1, 0)

ta2011$graduated_high_school = ta2011$TA110671
ta2011$graduated_high_school = ifelse(ta2011$graduated_high_school > 7 | ta2011$graduated_high_school == 0, NA, ta2011$graduated_high_school)
ta2011$graduated_high_school = ifelse(ta2011$graduated_high_school == 1 | ta2011$graduated_high_school == 2, 1, 0)

ta2013$graduated_high_school = ta2013$TA130691
ta2013$graduated_high_school = ifelse(ta2013$graduated_high_school > 7 | ta2013$graduated_high_school == 0, NA, ta2013$graduated_high_school)
ta2013$graduated_high_school = ifelse(ta2013$graduated_high_school == 1 | ta2013$graduated_high_school == 2, 1, 0)

ta2015$graduated_high_school = ta2015$TA150701
ta2015$graduated_high_school = ifelse(ta2015$graduated_high_school > 7 | ta2015$graduated_high_school == 0, NA, ta2015$graduated_high_school)
ta2015$graduated_high_school = ifelse(ta2015$graduated_high_school == 1 | ta2015$graduated_high_school == 2, 1, 0)

ta2017$graduated_high_school = ta2017$TA170781
ta2017$graduated_high_school = ifelse(ta2017$graduated_high_school > 7 | ta2017$graduated_high_school == 0, NA, ta2017$graduated_high_school)
ta2017$graduated_high_school = ifelse(ta2017$graduated_high_school == 1 | ta2017$graduated_high_school == 2, 1, 0)

ta2019$graduated_high_school = ta2019$TA190918
ta2019$graduated_high_school = ifelse(ta2019$graduated_high_school > 7 | ta2019$graduated_high_school == 0, NA, ta2019$graduated_high_school)
ta2019$graduated_high_school = ifelse(ta2019$graduated_high_school == 1 | ta2019$graduated_high_school == 2, 1, 0)

# GRADE COMPLETED IF DID NOT FINISH HIGH SCHOOL OR GED
ta2005$grade_completed = ta2005$TA050581
ta2005$grade_completed = ifelse(ta2005$grade_completed > 90 | ta2005$grade_completed == 0, NA, ta2005$grade_completed)

ta2007$grade_completed = ta2007$TA070556
ta2007$grade_completed = ifelse(ta2007$grade_completed > 90 | ta2007$grade_completed == 0, NA, ta2007$grade_completed)

ta2009$grade_completed = ta2009$TA090598
ta2009$grade_completed = ifelse(ta2009$grade_completed > 90 | ta2009$grade_completed == 0, NA, ta2009$grade_completed)

ta2011$grade_completed = ta2011$TA110679
ta2011$grade_completed = ifelse(ta2011$grade_completed > 90 | ta2011$grade_completed == 0, NA, ta2011$grade_completed)

ta2013$grade_completed = ta2013$TA130699
ta2013$grade_completed = ifelse(ta2013$grade_completed > 90 | ta2013$grade_completed == 0, NA, ta2013$grade_completed)

ta2015$grade_completed = ta2015$TA150709
ta2015$grade_completed = ifelse(ta2015$grade_completed > 90 | ta2015$grade_completed == 0, NA, ta2015$grade_completed)

ta2017$grade_completed = ta2017$TA170780
ta2017$grade_completed = ifelse(ta2017$grade_completed > 90 | ta2017$grade_completed == 0, NA, ta2017$grade_completed)

ta2019$grade_completed = ta2019$TA190917
ta2019$grade_completed = ifelse(ta2019$grade_completed > 90 | ta2019$grade_completed == 0, NA, ta2019$grade_completed)

# EDUCATIONAL ASPIRATION
ta2005$educational_aspirations = ta2005$TA050586
ta2005$educational_aspirations = ifelse(ta2005$educational_aspirations == 7, 0, ta2005$educational_aspirations)
ta2005$educational_aspirations = ifelse(ta2005$educational_aspirations > 7, NA, ta2005$educational_aspirations)

ta2007$educational_aspirations = ta2007$TA070561
ta2007$educational_aspirations = ifelse(ta2007$educational_aspirations == 7, 0, ta2007$educational_aspirations)
ta2007$educational_aspirations = ifelse(ta2007$educational_aspirations > 7, NA, ta2007$educational_aspirations)

ta2009$educational_aspirations = ta2009$TA090603
ta2009$educational_aspirations = ifelse(ta2009$educational_aspirations == 7, 0, ta2009$educational_aspirations)
ta2009$educational_aspirations = ifelse(ta2009$educational_aspirations > 7, NA, ta2009$educational_aspirations)

ta2011$educational_aspirations = ta2011$TA110690
ta2011$educational_aspirations = ifelse(ta2011$educational_aspirations == 7, 0, ta2011$educational_aspirations)
ta2011$educational_aspirations = ifelse(ta2011$educational_aspirations > 7, NA, ta2011$educational_aspirations)

ta2013$educational_aspirations = ta2013$TA130710
ta2013$educational_aspirations = ifelse(ta2013$educational_aspirations == 7, 0, ta2013$educational_aspirations)
ta2013$educational_aspirations = ifelse(ta2013$educational_aspirations > 7, NA, ta2013$educational_aspirations)

ta2015$educational_aspirations = ta2015$TA150722
ta2015$educational_aspirations = ifelse(ta2015$educational_aspirations == 7, 0, ta2015$educational_aspirations)
ta2015$educational_aspirations = ifelse(ta2015$educational_aspirations > 7, NA, ta2015$educational_aspirations)

ta2017$educational_aspirations = ta2017$TA170776
ta2017$educational_aspirations = ifelse(ta2017$educational_aspirations == 7, 0, ta2017$educational_aspirations)
ta2017$educational_aspirations = ifelse(ta2017$educational_aspirations > 7, NA, ta2017$educational_aspirations)

ta2019$educational_aspirations = ta2019$TA190913
ta2019$educational_aspirations = ifelse(ta2019$educational_aspirations == 7, 0, ta2019$educational_aspirations)
ta2019$educational_aspirations = ifelse(ta2019$educational_aspirations > 7, NA, ta2019$educational_aspirations)

# EDUCATIONAL EXPECTATIONS
ta2005$educational_expectations = ta2005$TA050588
ta2005$educational_expectations = ifelse(ta2005$educational_expectations == 7, 0, ta2005$educational_expectations)
ta2005$educational_expectations = ifelse(ta2005$educational_expectations > 7, NA, ta2005$educational_expectations)

ta2007$educational_expectations = ta2007$TA070563
ta2007$educational_expectations = ifelse(ta2007$educational_expectations == 7, 0, ta2007$educational_expectations)
ta2007$educational_expectations = ifelse(ta2007$educational_expectations > 7, NA, ta2007$educational_expectations)

ta2009$educational_expectations = ta2009$TA090605
ta2009$educational_expectations = ifelse(ta2009$educational_expectations == 7, 0, ta2009$educational_expectations)
ta2009$educational_expectations = ifelse(ta2009$educational_expectations > 7, NA, ta2009$educational_expectations)

ta2011$educational_expectations = ta2011$TA110692
ta2011$educational_expectations = ifelse(ta2011$educational_expectations == 7, 0, ta2011$educational_expectations)
ta2011$educational_expectations = ifelse(ta2011$educational_expectations > 7, NA, ta2011$educational_expectations)

ta2013$educational_expectations = ta2013$TA130712
ta2013$educational_expectations = ifelse(ta2013$educational_expectations == 7, 0, ta2013$educational_expectations)
ta2013$educational_expectations = ifelse(ta2013$educational_expectations > 7, NA, ta2013$educational_expectations)

ta2015$educational_expectations = ta2015$TA150724
ta2015$educational_expectations = ifelse(ta2015$educational_expectations == 7, 0, ta2015$educational_expectations)
ta2015$educational_expectations = ifelse(ta2015$educational_expectations > 7, NA, ta2015$educational_expectations)

ta2017$educational_expectations = ta2017$TA170778
ta2017$educational_expectations = ifelse(ta2017$educational_expectations == 7, 0, ta2017$educational_expectations)
ta2017$educational_expectations = ifelse(ta2017$educational_expectations > 7, NA, ta2017$educational_expectations)

ta2019$educational_expectations = ta2019$TA190915
ta2019$educational_expectations = ifelse(ta2019$educational_expectations == 7, 0, ta2019$educational_expectations)
ta2019$educational_expectations = ifelse(ta2019$educational_expectations > 7, NA, ta2019$educational_expectations)

## whether you ever attended college

ta2005$ever_college = ta2005$TA050594
ta2005$ever_college = ifelse(ta2005$ever_college == 5, 0, ta2005$ever_college)
ta2005$ever_college = ifelse(ta2005$ever_college > 6, NA, ta2005$ever_college)

ta2007$ever_college = ta2007$TA070569
ta2007$ever_college = ifelse(ta2007$ever_college == 5, 0, ta2007$ever_college)
ta2007$ever_college = ifelse(ta2007$ever_college > 6, NA, ta2007$ever_college)

ta2009$ever_college = ta2009$TA090611
ta2009$ever_college = ifelse(ta2009$ever_college == 5, 0, ta2009$ever_college)
ta2009$ever_college = ifelse(ta2009$ever_college > 6, NA, ta2009$ever_college)

ta2011$ever_college = ta2011$TA110698
ta2011$ever_college = ifelse(ta2011$ever_college == 5, 0, ta2011$ever_college)
ta2011$ever_college = ifelse(ta2011$ever_college > 6, NA, ta2011$ever_college)

ta2013$ever_college = ta2013$TA130718
ta2013$ever_college = ifelse(ta2013$ever_college == 5, 0, ta2013$ever_college)
ta2013$ever_college = ifelse(ta2013$ever_college > 6, NA, ta2013$ever_college)

ta2015$ever_college = ta2015$TA150730
ta2015$ever_college = ifelse(ta2015$ever_college == 5, 0, ta2015$ever_college)
ta2015$ever_college = ifelse(ta2015$ever_college > 6, NA, ta2015$ever_college)

# For2017 and 2019, they asked whether they are currently attending college/grad school
# if no, then asked whether they had ever attended college

ta2017$ever_college = ta2017$TA170790
ta2017$ever_college = ifelse(ta2017$ever_college == 3, 1, ta2017$ever_college)
ta2017$ever_college = ifelse(ta2017$ever_college == 5, 0, ta2017$ever_college)
ta2017$ever_college = ifelse(ta2017$ever_college > 6, NA, ta2017$ever_college)

ta2019$ever_college = ta2019$TA190927
ta2019$ever_college = ifelse(ta2019$ever_college == 3, 1, ta2019$ever_college)
ta2019$ever_college = ifelse(ta2019$ever_college == 5, 0, ta2019$ever_college)
ta2019$ever_college = ifelse(ta2019$ever_college > 6, NA, ta2019$ever_college)

### Whether respondent is in college now

ta2005$in_college_now = ta2005$TA050595
ta2005$in_college_now = ifelse(ta2005$in_college_now == 5, 0, ta2005$in_college_now)
ta2005$in_college_now = ifelse(ta2005$in_college_now > 6, NA, ta2005$in_college_now)

ta2007$in_college_now = ta2007$TA070570
ta2007$in_college_now = ifelse(ta2007$in_college_now == 5, 0, ta2007$in_college_now)
ta2007$in_college_now = ifelse(ta2007$in_college_now > 6, NA, ta2007$in_college_now)

ta2009$in_college_now = ta2009$TA090612
ta2009$in_college_now = ifelse(ta2009$in_college_now == 5, 0, ta2009$in_college_now)
ta2009$in_college_now = ifelse(ta2009$in_college_now > 6, NA, ta2009$in_college_now)

ta2011$in_college_now = ta2011$TA110699
ta2011$in_college_now = ifelse(ta2011$in_college_now == 5, 0, ta2011$in_college_now)
ta2011$in_college_now = ifelse(ta2011$in_college_now > 6, NA, ta2011$in_college_now)

ta2013$in_college_now = ta2013$TA130719
ta2013$in_college_now = ifelse(ta2013$in_college_now == 5, 0, ta2013$in_college_now)
ta2013$in_college_now = ifelse(ta2013$in_college_now > 6, NA, ta2013$in_college_now)

ta2015$in_college_now = ta2015$TA150731
ta2015$in_college_now = ifelse(ta2015$in_college_now == 5, 0, ta2015$in_college_now)
ta2015$in_college_now = ifelse(ta2015$in_college_now > 6, NA, ta2015$in_college_now)

# For2017 and 2019, they asked whether they are currently attending college/grad school
# if no, then asked whether they had ever attended college (same vars are used for ever_college and in_college_now)

ta2017$in_college_now = ta2017$TA170790
ta2017$in_college_now = ifelse(ta2017$in_college_now > 6, NA, ta2017$in_college_now)
ta2017$in_college_now = ifelse(ta2017$in_college_now > 2, 0, ta2017$in_college_now)

ta2019$in_college_now = ta2019$TA190927
ta2019$in_college_now = ifelse(ta2019$in_college_now > 6, NA, ta2019$in_college_now)
ta2019$in_college_now = ifelse(ta2019$in_college_now > 2, 0, ta2019$in_college_now)

### Health levels "Now I have a few questions about your health. 
# Would you say your health in general is excellent, very good, good, fair, or poor?
# coded so that excellent health is highest

ta2005$health_level = ta2005$TA050676
ta2005$health_level = ifelse(ta2005$health_level > 6, NA, ta2005$health_level)
ta2005$health_level = 6 - as.numeric(ta2005$health_level)

ta2007$health_level = ta2007$TA070647
ta2007$health_level = ifelse(ta2007$health_level > 6, NA, ta2007$health_level)
ta2007$health_level = 6 - as.numeric(ta2007$health_level)

ta2009$health_level = ta2009$TA090700
ta2009$health_level = ifelse(ta2009$health_level > 6, NA, ta2009$health_level)
ta2009$health_level = 6 - as.numeric(ta2009$health_level)

ta2011$health_level = ta2011$TA110788
ta2011$health_level = ifelse(ta2011$health_level > 6, NA, ta2011$health_level)
ta2011$health_level = 6 - as.numeric(ta2011$health_level)

ta2013$health_level = ta2013$TA130808
ta2013$health_level = ifelse(ta2013$health_level > 6, NA, ta2013$health_level)
ta2013$health_level = 6 - as.numeric(ta2013$health_level)

ta2015$health_level = ta2015$TA150821
ta2015$health_level = ifelse(ta2015$health_level > 6, NA, ta2015$health_level)
ta2015$health_level = 6 - as.numeric(ta2015$health_level)

ta2017$health_level = ta2017$TA170861
ta2017$health_level = ifelse(ta2017$health_level > 6, NA, ta2017$health_level)
ta2017$health_level = 6 - as.numeric(ta2017$health_level)

ta2019$health_level = ta2019$TA191004
ta2019$health_level = ifelse(ta2019$health_level > 6, NA, ta2019$health_level)
ta2019$health_level = 6 - as.numeric(ta2019$health_level)


# Ever Diagnosed with psychiatric problems
# Has a doctor or other health professional ever told you that you have or had any
# emotional, nervous, or psychiatric problems?

ta2005$ever_psych_problems = ta2005$TA050708
ta2005$ever_psych_problems = ifelse(ta2005$ever_psych_problems == 5, 0, ta2005$ever_psych_problems)
ta2005$ever_psych_problems = ifelse(ta2005$ever_psych_problems > 6, NA, ta2005$ever_psych_problems)

ta2007$ever_psych_problems = ta2007$TA070679
ta2007$ever_psych_problems = ifelse(ta2007$ever_psych_problems == 5, 0, ta2007$ever_psych_problems)
ta2007$ever_psych_problems = ifelse(ta2007$ever_psych_problems > 6, NA, ta2007$ever_psych_problems)

ta2009$ever_psych_problems = ta2009$TA090735
ta2009$ever_psych_problems = ifelse(ta2009$ever_psych_problems == 5, 0, ta2009$ever_psych_problems)
ta2009$ever_psych_problems = ifelse(ta2009$ever_psych_problems > 6, NA, ta2009$ever_psych_problems)

ta2011$ever_psych_problems = ta2011$TA110825
ta2011$ever_psych_problems = ifelse(ta2011$ever_psych_problems == 5, 0, ta2011$ever_psych_problems)
ta2011$ever_psych_problems = ifelse(ta2011$ever_psych_problems > 6, NA, ta2011$ever_psych_problems)

ta2013$ever_psych_problems = ta2013$TA130848
ta2013$ever_psych_problems = ifelse(ta2013$ever_psych_problems == 5, 0, ta2013$ever_psych_problems)
ta2013$ever_psych_problems = ifelse(ta2013$ever_psych_problems > 6, NA, ta2013$ever_psych_problems)

ta2015$ever_psych_problems = ta2015$TA150865
ta2015$ever_psych_problems = ifelse(ta2015$ever_psych_problems == 5, 0, ta2015$ever_psych_problems)
ta2015$ever_psych_problems = ifelse(ta2015$ever_psych_problems > 6, NA, ta2015$ever_psych_problems)

ta2017$ever_psych_problems = ta2017$TA170905
ta2017$ever_psych_problems = ifelse(ta2017$ever_psych_problems == 0, 0, ta2017$ever_psych_problems)
ta2017$ever_psych_problems = ifelse(ta2017$ever_psych_problems > 6, NA, ta2017$ever_psych_problems)

ta2019$ever_psych_problems = ta2019$TA191049
ta2019$ever_psych_problems = ifelse(ta2019$ever_psych_problems == 5, 0, ta2019$ever_psych_problems)
ta2019$ever_psych_problems = ifelse(ta2019$ever_psych_problems > 6, NA, ta2019$ever_psych_problems)

# Do you have a health problem that limits work?

ta2005$wtr_health_limits_work = ta2005$TA050677
ta2005$wtr_health_limits_work = ifelse(ta2005$wtr_health_limits_work > 6, NA, ta2005$wtr_health_limits_work)
ta2005$wtr_health_limits_work = ifelse(ta2005$wtr_health_limits_work == 5, 0, ta2005$wtr_health_limits_work)

ta2007$wtr_health_limits_work = ta2007$TA070648
ta2007$wtr_health_limits_work = ifelse(ta2007$wtr_health_limits_work > 6, 0, ta2007$wtr_health_limits_work)
ta2007$wtr_health_limits_work = ifelse(ta2007$wtr_health_limits_work == 5, 0, ta2007$wtr_health_limits_work)

ta2009$wtr_health_limits_work = ta2009$TA090704
ta2009$wtr_health_limits_work = ifelse(ta2009$wtr_health_limits_work > 6, 0, ta2009$wtr_health_limits_work)
ta2009$wtr_health_limits_work = ifelse(ta2009$wtr_health_limits_work == 5, 0, ta2009$wtr_health_limits_work)

ta2011$wtr_health_limits_work = ta2011$TA110792
ta2011$wtr_health_limits_work = ifelse(ta2011$wtr_health_limits_work > 6, 0, ta2011$wtr_health_limits_work)
ta2011$wtr_health_limits_work = ifelse(ta2011$wtr_health_limits_work == 5, 0, ta2011$wtr_health_limits_work)

ta2013$wtr_health_limits_work = ta2013$TA130812
ta2013$wtr_health_limits_work = ifelse(ta2013$wtr_health_limits_work > 6, 0, ta2013$wtr_health_limits_work)
ta2013$wtr_health_limits_work = ifelse(ta2013$wtr_health_limits_work == 5, 0, ta2013$wtr_health_limits_work)

ta2015$wtr_health_limits_work = ta2015$TA150825
ta2015$wtr_health_limits_work = ifelse(ta2015$wtr_health_limits_work > 6, 0, ta2015$wtr_health_limits_work)
ta2015$wtr_health_limits_work = ifelse(ta2015$wtr_health_limits_work == 5, 0, ta2015$wtr_health_limits_work)

ta2017$wtr_health_limits_work = ta2017$TA170865
ta2017$wtr_health_limits_work = ifelse(ta2017$wtr_health_limits_work > 6, 0, ta2017$wtr_health_limits_work)
ta2017$wtr_health_limits_work = ifelse(ta2017$wtr_health_limits_work == 5, 0, ta2017$wtr_health_limits_work)

ta2019$wtr_health_limits_work = ta2019$TA191008
ta2019$wtr_health_limits_work = ifelse(ta2019$wtr_health_limits_work > 6, 0, ta2019$wtr_health_limits_work)
ta2019$wtr_health_limits_work = ifelse(ta2019$wtr_health_limits_work == 5, 0, ta2019$wtr_health_limits_work)

# how much does the problem (COULD BE ANY HEALTH PROBLEM) limits normal activies?

ta2005$problem_limits_activities = ta2005$TA050719
ta2005$problem_limits_activities = ifelse(ta2005$problem_limits_activities > 7, NA, ta2005$problem_limits_activities)
ta2005$problem_limits_activities = ifelse(ta2005$problem_limits_activities == 0, 5, ta2005$problem_limits_activities)
ta2005$problem_limits_activities = ifelse(ta2005$problem_limits_activities == 3, 2, ta2005$problem_limits_activities)
ta2005$problem_limits_activities = ifelse(ta2005$problem_limits_activities == 5, 3, ta2005$problem_limits_activities)
ta2005$problem_limits_activities = ifelse(ta2005$problem_limits_activities == 7, 4, ta2005$problem_limits_activities)
ta2005$problem_limits_activities = 5 - as.numeric(ta2005$problem_limits_activities)

ta2007$problem_limits_activities = ta2007$TA070690
ta2007$problem_limits_activities = ifelse(ta2007$problem_limits_activities > 7, NA, ta2007$problem_limits_activities)
ta2007$problem_limits_activities = ifelse(ta2007$problem_limits_activities == 0, 5, ta2007$problem_limits_activities)
ta2007$problem_limits_activities = ifelse(ta2007$problem_limits_activities == 3, 2, ta2007$problem_limits_activities)
ta2007$problem_limits_activities = ifelse(ta2007$problem_limits_activities == 5, 3, ta2007$problem_limits_activities)
ta2007$problem_limits_activities = ifelse(ta2007$problem_limits_activities == 7, 4, ta2007$problem_limits_activities)
ta2007$problem_limits_activities = 5 - as.numeric(ta2007$problem_limits_activities)

ta2009$problem_limits_activities = ta2009$TA090746
ta2009$problem_limits_activities = ifelse(ta2009$problem_limits_activities > 7, NA, ta2009$problem_limits_activities)
ta2009$problem_limits_activities = ifelse(ta2009$problem_limits_activities == 0, 5, ta2009$problem_limits_activities)
ta2009$problem_limits_activities = ifelse(ta2009$problem_limits_activities == 3, 2, ta2009$problem_limits_activities)
ta2009$problem_limits_activities = ifelse(ta2009$problem_limits_activities == 5, 3, ta2009$problem_limits_activities)
ta2009$problem_limits_activities = ifelse(ta2009$problem_limits_activities == 7, 4, ta2009$problem_limits_activities)
ta2009$problem_limits_activities = 5 - as.numeric(ta2009$problem_limits_activities)

ta2011$problem_limits_activities = ta2011$TA110837
ta2011$problem_limits_activities = ifelse(ta2011$problem_limits_activities > 7, NA, ta2011$problem_limits_activities)
ta2011$problem_limits_activities = ifelse(ta2011$problem_limits_activities == 0, 5, ta2011$problem_limits_activities)
ta2011$problem_limits_activities = ifelse(ta2011$problem_limits_activities == 3, 2, ta2011$problem_limits_activities)
ta2011$problem_limits_activities = ifelse(ta2011$problem_limits_activities == 5, 3, ta2011$problem_limits_activities)
ta2011$problem_limits_activities = ifelse(ta2011$problem_limits_activities == 7, 4, ta2011$problem_limits_activities)
ta2011$problem_limits_activities = 5 - as.numeric(ta2011$problem_limits_activities)

ta2013$problem_limits_activities = ta2013$TA130861
ta2013$problem_limits_activities = ifelse(ta2013$problem_limits_activities > 7, NA, ta2013$problem_limits_activities)
ta2013$problem_limits_activities = ifelse(ta2013$problem_limits_activities == 0, 5, ta2013$problem_limits_activities)
ta2013$problem_limits_activities = ifelse(ta2013$problem_limits_activities == 3, 2, ta2013$problem_limits_activities)
ta2013$problem_limits_activities = ifelse(ta2013$problem_limits_activities == 5, 3, ta2013$problem_limits_activities)
ta2013$problem_limits_activities = ifelse(ta2013$problem_limits_activities == 7, 4, ta2013$problem_limits_activities)
ta2013$problem_limits_activities = 5 - as.numeric(ta2013$problem_limits_activities)

ta2015$problem_limits_activities = ta2015$TA150878
ta2015$problem_limits_activities = ifelse(ta2015$problem_limits_activities > 7, NA, ta2015$problem_limits_activities)
ta2015$problem_limits_activities = ifelse(ta2015$problem_limits_activities == 0, 5, ta2015$problem_limits_activities)
ta2015$problem_limits_activities = ifelse(ta2015$problem_limits_activities == 3, 2, ta2015$problem_limits_activities)
ta2015$problem_limits_activities = ifelse(ta2015$problem_limits_activities == 5, 3, ta2015$problem_limits_activities)
ta2015$problem_limits_activities = ifelse(ta2015$problem_limits_activities == 7, 4, ta2015$problem_limits_activities)
ta2015$problem_limits_activities = 5 - as.numeric(ta2015$problem_limits_activities)

ta2017$problem_limits_activities = ta2017$TA170918
ta2017$problem_limits_activities = ifelse(ta2017$problem_limits_activities > 7, NA, ta2017$problem_limits_activities)
ta2017$problem_limits_activities = 5 - as.numeric(ta2017$problem_limits_activities) 

ta2019$problem_limits_activities = ta2019$TA191065
ta2019$problem_limits_activities = ifelse(ta2019$problem_limits_activities > 7, NA, ta2019$problem_limits_activities)
ta2019$problem_limits_activities = 5 - as.numeric(ta2019$problem_limits_activities)

# race/ ethnicity -- this is the combination of two variables; one for HISPANICITY the other FIRST MENTION OF RACE
ta2005$hispanic = ta2005$TA050883
ta2005$hispanic = ifelse(ta2005$hispanic > 7, NA, ta2005$hispanic)
ta2005$hispanic = ifelse(ta2005$hispanic > 0, 1, ta2005$hispanic)

ta2005$race = ta2005$TA050884
ta2005$race = ifelse(ta2005$race > 7, NA, ta2005$race)
ta2005$race = ifelse(ta2005$race == 5 | ta2005$race == 7, 3, ta2005$race) # collapsing Other, Native/Alaskan, and pacific islander into one category
ta2005$race = ifelse(ta2005$hispanic == 1, 5, ta2005$race) # assigning hispanics 5

ta2005$race_cat = dplyr::recode(ta2005$race, `1` = "White", `2` = "Black", `3` = "Other", `4` = "Asian", `5` = "Hispanic")

ta2007$hispanic = ta2007$TA070864
ta2007$hispanic = ifelse(ta2007$hispanic > 7, NA, ta2007$hispanic)
ta2007$hispanic = ifelse(ta2007$hispanic > 0, 1, ta2007$hispanic)

ta2007$race = ta2007$TA070865
ta2007$race = ifelse(ta2007$race > 7, NA, ta2007$race)
ta2007$race = ifelse(ta2007$race == 5 | ta2007$race == 7, 3, ta2007$race) # collapsing Other, Native/Alaskan, and pacific islander into one category
ta2007$race = ifelse(ta2007$hispanic == 1, 5, ta2007$race) # assigning hispanics 5

ta2007$race_cat = dplyr::recode(ta2007$race, `1` = "White", `2` = "Black", `3` = "Other", `4` = "Asian", `5` = "Hispanic")

ta2009$hispanic = ta2009$TA090924
ta2009$hispanic = ifelse(ta2009$hispanic > 7, NA, ta2009$hispanic)
ta2009$hispanic = ifelse(ta2009$hispanic > 0, 1, ta2009$hispanic)

ta2009$race = ta2009$TA090925
ta2009$race = ifelse(ta2009$race > 7, NA, ta2009$race)
ta2009$race = ifelse(ta2009$race == 5 | ta2009$race == 7, 3, ta2009$race) # collapsing Other, Native/Alaskan, and pacific islander into one category
ta2009$race = ifelse(ta2009$hispanic == 1, 5, ta2009$race) # assigning hispanics 5

ta2009$race_cat = dplyr::recode(ta2009$race, `1` = "White", `2` = "Black", `3` = "Other", `4` = "Asian", `5` = "Hispanic")

ta2011$hispanic = ta2011$TA111056
ta2011$hispanic = ifelse(ta2011$hispanic > 7, NA, ta2011$hispanic)
ta2011$hispanic = ifelse(ta2011$hispanic > 0, 1, ta2011$hispanic)

ta2011$race = ta2011$TA111057
ta2011$race = ifelse(ta2011$race > 7, NA, ta2011$race)
ta2011$race = ifelse(ta2011$race == 5 | ta2011$race == 7, 3, ta2011$race) # collapsing Other, Native/Alaskan, and pacific islander into one category
ta2011$race = ifelse(ta2011$hispanic == 1, 5, ta2011$race) # assigning hispanics 5

ta2011$race_cat = dplyr::recode(ta2011$race, `1` = "White", `2` = "Black", `3` = "Other", `4` = "Asian", `5` = "Hispanic")

ta2013$hispanic = ta2013$TA131091
ta2013$hispanic = ifelse(ta2013$hispanic > 7, NA, ta2013$hispanic)
ta2013$hispanic = ifelse(ta2013$hispanic > 0, 1, ta2013$hispanic)

ta2013$race = ta2013$TA131092
ta2013$race = ifelse(ta2013$race > 7, NA, ta2013$race)
ta2013$race = ifelse(ta2013$race == 5 | ta2013$race == 7, 3, ta2013$race) # collapsing Other, Native/Alaskan, and pacific islander into one category
ta2013$race = ifelse(ta2013$hispanic == 1, 5, ta2013$race) # assigning hispanics 5

ta2013$race_cat = dplyr::recode(ta2013$race, `1` = "White", `2` = "Black", `3` = "Other", `4` = "Asian", `5` = "Hispanic")

ta2015$hispanic = ta2015$TA151131
ta2015$hispanic = ifelse(ta2015$hispanic > 7, NA, ta2015$hispanic)
ta2015$hispanic = ifelse(ta2015$hispanic > 0, 1, ta2015$hispanic)

ta2015$race = ta2015$TA151132
ta2015$race = ifelse(ta2015$race > 7, NA, ta2015$race)
ta2015$race = ifelse(ta2015$race == 5 | ta2015$race == 7, 3, ta2015$race) # collapsing Other, Native/Alaskan, and pacific islander into one category
ta2015$race = ifelse(ta2015$hispanic == 1, 5, ta2015$race) # assigning hispanics 5

ta2015$race_cat = dplyr::recode(ta2015$race, `1` = "White", `2` = "Black", `3` = "Other", `4` = "Asian", `5` = "Hispanic")

## for 2017 and 2019 hispanicity is asked within the question of race

ta2017$race = ta2017$TA171955
ta2017$race = ifelse(ta2017$race > 9, NA, ta2017$race)
ta2017$race_cat = dplyr::recode(ta2017$race, `1` = "White", `2` = "Hispanic", `3` = "Black", `4` = "Asian", `5` = "Other", `6` = "Other", `7` = "Other", `8` = "Other")
ta2017$race = as.numeric(factor(ta2017$race_cat, levels = c("White", "Black", "Other", "Asian", "Hispanic")))

ta2019$race = ta2019$TA192131
ta2019$race = ifelse(ta2019$race > 9, NA, ta2019$race)
ta2019$race_cat = dplyr::recode(ta2019$race, `1` = "White", `2` = "Hispanic", `3` = "Black", `4` = "Asian", `5` = "Other", `6` = "Other", `7` = "Other", `8` = "Other")
ta2019$race = as.numeric(factor(ta2019$race_cat, levels = c("White", "Black", "Other", "Asian", "Hispanic")))


# Marital / Cohabiation status

ta2005$marriage_cohabitation = ta2005$TA050951
ta2005$marriage_cohabitation = ifelse(ta2005$marriage_cohabitation > 8, NA, ta2005$marriage_cohabitation)
ta2005$marriage_cohabitation = dplyr::recode(ta2005$marriage_cohabitation, `1` = "Never Married; Cohabiting", `2` = "Never Married; Not cohabiting", `3` = "Married, spouse present", `4` = "Married, spouse not present", `5` = "Separated", `6` = "Divorced, cohabiting", `7` = "Divorced, not cohabiting", `8` = "Widowed")

ta2007$marriage_cohabitation = ta2007$TA070932
ta2007$marriage_cohabitation = ifelse(ta2007$marriage_cohabitation > 8, NA, ta2007$marriage_cohabitation)
ta2007$marriage_cohabitation = dplyr::recode(ta2007$marriage_cohabitation, `1` = "Never Married; Cohabiting", `2` = "Never Married; Not cohabiting", `3` = "Married, spouse present", `4` = "Married, spouse not present", `5` = "Separated", `6` = "Divorced, cohabiting", `7` = "Divorced, not cohabiting", `8` = "Widowed")

ta2009$marriage_cohabitation = ta2009$TA090996
ta2009$marriage_cohabitation = ifelse(ta2009$marriage_cohabitation > 8, NA, ta2009$marriage_cohabitation)
ta2009$marriage_cohabitation = dplyr::recode(ta2009$marriage_cohabitation, `1` = "Never Married; Cohabiting", `2` = "Never Married; Not cohabiting", `3` = "Married, spouse present", `4` = "Married, spouse not present", `5` = "Separated", `6` = "Divorced, cohabiting", `7` = "Divorced, not cohabiting", `8` = "Widowed")

ta2011$marriage_cohabitation = ta2011$TA111138
ta2011$marriage_cohabitation = ifelse(ta2011$marriage_cohabitation > 8, NA, ta2011$marriage_cohabitation)
ta2011$marriage_cohabitation = dplyr::recode(ta2011$marriage_cohabitation, `1` = "Never Married; Cohabiting", `2` = "Never Married; Not cohabiting", `3` = "Married, spouse present", `4` = "Married, spouse not present", `5` = "Separated", `6` = "Divorced, cohabiting", `7` = "Divorced, not cohabiting", `8` = "Widowed")

ta2013$marriage_cohabitation = ta2013$TA131230
ta2013$marriage_cohabitation = ifelse(ta2013$marriage_cohabitation > 8, NA, ta2013$marriage_cohabitation)
ta2013$marriage_cohabitation = dplyr::recode(ta2013$marriage_cohabitation, `1` = "Never Married; Cohabiting", `2` = "Never Married; Not cohabiting", `3` = "Married, spouse present", `4` = "Married, spouse not present", `5` = "Separated", `6` = "Divorced, cohabiting", `7` = "Divorced, not cohabiting", `8` = "Widowed")

ta2015$marriage_cohabitation = ta2015$TA151290
ta2015$marriage_cohabitation = ifelse(ta2015$marriage_cohabitation > 8, NA, ta2015$marriage_cohabitation)
ta2015$marriage_cohabitation = dplyr::recode(ta2015$marriage_cohabitation, `1` = "Never Married; Cohabiting", `2` = "Never Married; Not cohabiting", `3` = "Married, spouse present", `4` = "Married, spouse not present", `5` = "Separated", `6` = "Divorced, cohabiting", `7` = "Divorced, not cohabiting", `8` = "Widowed")

ta2017$marriage_cohabitation = ta2017$TA171985
ta2017$marriage_cohabitation = ifelse(ta2017$marriage_cohabitation > 8, NA, ta2017$marriage_cohabitation)
ta2017$marriage_cohabitation = dplyr::recode(ta2017$marriage_cohabitation, `1` = "Never Married; Cohabiting", `2` = "Never Married; Not cohabiting", `3` = "Married, spouse present", `4` = "Married, spouse not present", `5` = "Separated", `6` = "Divorced, cohabiting", `7` = "Divorced, not cohabiting", `8` = "Widowed")

ta2019$marriage_cohabitation = ta2019$TA192158
ta2019$marriage_cohabitation = ifelse(ta2019$marriage_cohabitation > 8, NA, ta2019$marriage_cohabitation)
ta2019$marriage_cohabitation = dplyr::recode(ta2019$marriage_cohabitation, `1` = "Never Married; Cohabiting", `2` = "Never Married; Not cohabiting", `3` = "Married, spouse present", `4` = "Married, spouse not present", `5` = "Separated", `6` = "Divorced, cohabiting", `7` = "Divorced, not cohabiting", `8` = "Widowed")

## Earnings last year
ta2005$earnings_work = as.numeric(ta2005$TA050954)
ta2005$earnings_work = ifelse(ta2005$earnings_work == 9999999, NA, ta2005$earnings_work)

ta2007$earnings_work = as.numeric(ta2007$TA070935)
ta2007$earnings_work = ifelse(ta2007$earnings_work == 9999999, NA, ta2007$earnings_work)

ta2009$earnings_work = as.numeric(ta2009$TA090999)
ta2009$earnings_work = ifelse(ta2009$earnings_work == 9999999, NA, ta2009$earnings_work)

ta2011$earnings_work = as.numeric(ta2011$TA111141)
ta2011$earnings_work = ifelse(ta2011$earnings_work == 9999999, NA, ta2011$earnings_work)

ta2013$earnings_work = as.numeric(ta2013$TA131232)
ta2013$earnings_work = ifelse(ta2013$earnings_work == 9999999, NA, ta2013$earnings_work)

ta2015$earnings_work = as.numeric(ta2015$TA151292)
ta2015$earnings_work = ifelse(ta2015$earnings_work == 9999999, NA, ta2015$earnings_work)

# for 2017 and 2019, respondents were asked about earnings from their three jobs. 
# I summed these to produce  "earnings_work" variable for 2017 and 2019

ta2017$earnings_work_1 = as.numeric(ta2017$TA170238) 
ta2017$earnings_work_1 = ifelse(ta2017$earnings_work_1 == 9999999, NA, ta2017$earnings_work_1)

ta2017$earnings_work_2 = as.numeric(ta2017$TA170253)
ta2017$earnings_work_2 = ifelse(ta2017$earnings_work_2 == 9999999, NA, ta2017$earnings_work_2)

ta2017$earnings_work_3 = as.numeric(ta2017$TA170268) 
ta2017$earnings_work_3 = ifelse(ta2017$earnings_work_3 == 9999999, NA, ta2017$earnings_work_3)

ta2017$earnings_work = rowSums(ta2017[ c("earnings_work_1", "earnings_work_2", "earnings_work_3")])

ta2019$earnings_work_1 = as.numeric(ta2019$TA190292) 
ta2019$earnings_work_1 = ifelse(ta2019$earnings_work_1 == 9999999, NA, ta2019$earnings_work_1)

ta2019$earnings_work_2 = as.numeric(ta2019$TA190319) 
ta2019$earnings_work_2 = ifelse(ta2019$earnings_work_2 == 9999999, NA, ta2019$earnings_work_2)

ta2019$earnings_work_3 = as.numeric(ta2019$TA190346)
ta2019$earnings_work_3 = ifelse(ta2019$earnings_work_3 == 9999999, NA, ta2019$earnings_work_3)

ta2019$earnings_work = rowSums(ta2019[ c("earnings_work_1", "earnings_work_2", "earnings_work_3")])


##Amount in all bank accounts
# 0 could mean no money or no bank account

ta2005$amt_all_accounts = as.numeric(ta2005$TA050913)
ta2005$amt_all_accounts = ifelse(ta2005$amt_all_accounts == -999999, NA, ta2005$amt_all_accounts)
ta2005$amt_all_accounts = ifelse(ta2005$amt_all_accounts > 9999997, NA, ta2005$amt_all_accounts)

ta2007$amt_all_accounts = as.numeric(ta2007$TA070894)
ta2007$amt_all_accounts = ifelse(ta2007$amt_all_accounts == -999999, NA, ta2007$amt_all_accounts)
ta2007$amt_all_accounts = ifelse(ta2007$amt_all_accounts > 9999997, NA, ta2007$amt_all_accounts)

ta2009$amt_all_accounts = as.numeric(ta2009$TA090954)
ta2009$amt_all_accounts = ifelse(ta2009$amt_all_accounts == -999999, NA, ta2009$amt_all_accounts)
ta2009$amt_all_accounts = ifelse(ta2009$amt_all_accounts > 9999997, NA, ta2009$amt_all_accounts)

ta2011$amt_all_accounts = as.numeric(ta2011$TA111086)
ta2011$amt_all_accounts = ifelse(ta2011$amt_all_accounts == -999999, NA, ta2011$amt_all_accounts)
ta2011$amt_all_accounts = ifelse(ta2011$amt_all_accounts > 9999997, NA, ta2011$amt_all_accounts)

ta2013$amt_all_accounts = as.numeric(ta2013$TA131137)
ta2013$amt_all_accounts = ifelse(ta2013$amt_all_accounts == -999999, NA, ta2013$amt_all_accounts)
ta2013$amt_all_accounts = ifelse(ta2013$amt_all_accounts > 9999997, NA, ta2013$amt_all_accounts)

ta2015$amt_all_accounts = as.numeric(ta2015$TA151177)
ta2015$amt_all_accounts = ifelse(ta2015$amt_all_accounts == -999999, NA, ta2015$amt_all_accounts)
ta2015$amt_all_accounts = ifelse(ta2015$amt_all_accounts > 9999997, NA, ta2015$amt_all_accounts)

# in 2017 the max amount that they can report changed from 9999997 to 999999997
ta2017$amt_all_accounts = as.numeric(ta2017$TA170691)
ta2017$amt_all_accounts = ifelse(ta2017$amt_all_accounts == -99999999, NA, ta2017$amt_all_accounts)
ta2017$amt_all_accounts = ifelse(ta2017$amt_all_accounts > 999999997, NA, ta2017$amt_all_accounts)

# in 2019 the max amount increased to 9,999,999,997
# in 2019 they have a question about what's your best estimate if the person said DK --TA190894 (not used here)
ta2019$amt_all_accounts = as.numeric(ta2019$TA190893)
ta2019$amt_all_accounts = ifelse(ta2019$amt_all_accounts == -999999999, NA, ta2019$amt_all_accounts)
ta2019$amt_all_accounts = ifelse(ta2019$amt_all_accounts > 9999999997, NA, ta2019$amt_all_accounts)

#Fall/Winter primary residence

ta2005$fall_winter_residence = as.numeric(ta2005$TA050042)
ta2005$fall_winter_residence = ifelse(ta2005$fall_winter_residence > 7, NA, ta2005$fall_winter_residence)
ta2005$fall_winter_residence_parents_help = ifelse(ta2005$fall_winter_residence == 1 | ta2005$fall_winter_residence == 5, 1, 0)
ta2005$fall_winter_residence_cat = dplyr::recode(ta2005$fall_winter_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other")

ta2007$fall_winter_residence = as.numeric(ta2007$TA070042)
ta2007$fall_winter_residence = ifelse(ta2007$fall_winter_residence > 7, NA, ta2007$fall_winter_residence)
ta2007$fall_winter_residence_parents_help = ifelse(ta2007$fall_winter_residence == 1 | ta2007$fall_winter_residence == 5, 1, 0)
ta2007$fall_winter_residence_cat = dplyr::recode(ta2007$fall_winter_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other")

ta2009$fall_winter_residence = as.numeric(ta2009$TA090043)
ta2009$fall_winter_residence = ifelse(ta2009$fall_winter_residence > 7, NA, ta2009$fall_winter_residence)
ta2009$fall_winter_residence_parents_help = ifelse(ta2009$fall_winter_residence == 1 | ta2009$fall_winter_residence == 5, 1, 0)
ta2009$fall_winter_residence_cat = dplyr::recode(ta2009$fall_winter_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other")

# in 2011 they added overseas and military base
ta2011$fall_winter_residence = as.numeric(ta2011$TA110044)
ta2011$fall_winter_residence = ifelse(ta2011$fall_winter_residence > 90, NA, ta2011$fall_winter_residence)
ta2011$fall_winter_residence_parents_help = ifelse(ta2011$fall_winter_residence == 1 | ta2011$fall_winter_residence == 5, 1, 0)
ta2011$fall_winter_residence_cat = dplyr::recode(ta2011$fall_winter_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other", `8` = "Military base", `9` = "Overseas")

# they added military base and spouse/partner's parents' home
ta2013$fall_winter_residence = as.numeric(ta2013$TA130043)
ta2013$fall_winter_residence = ifelse(ta2013$fall_winter_residence > 90, NA, ta2013$fall_winter_residence)
# I have included partners' parents' home in the "parents help category
ta2013$fall_winter_residence_parents_help = ifelse(ta2013$fall_winter_residence == 1 | ta2013$fall_winter_residence == 5 | ta2013$fall_winter_residence == 9, 1, 0)
ta2013$fall_winter_residence_cat = dplyr::recode(ta2013$fall_winter_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other", `8` = "Military base", `9` = "Partner's parents' home")

# they added military base and spouse/partner's parents' home
ta2015$fall_winter_residence = as.numeric(ta2015$TA150043)
ta2015$fall_winter_residence = ifelse(ta2015$fall_winter_residence > 90, NA, ta2015$fall_winter_residence)
# I have included partners' parents' home in the "parents help category
ta2015$fall_winter_residence_parents_help = ifelse(ta2015$fall_winter_residence == 1 | ta2015$fall_winter_residence == 5 | ta2015$fall_winter_residence == 9, 1, 0)
ta2015$fall_winter_residence_cat = dplyr::recode(ta2015$fall_winter_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other", `8` = "Military base", `9` = "Partner's parents' home")

# in 2017 & 2019 they added other relatives OR FRIEND help rented as category 10
# work related housing / military base are both nubmer 8
ta2017$fall_winter_residence = as.numeric(ta2017$TA170058)
ta2017$fall_winter_residence = ifelse(ta2017$fall_winter_residence > 90, NA, ta2017$fall_winter_residence)
# spouse parents' owned home is included in number 5
ta2017$fall_winter_residence_parents_help = ifelse(ta2017$fall_winter_residence == 1 | ta2017$fall_winter_residence == 5 | ta2017$fall_winter_residence == 9, 1, 0)
ta2017$fall_winter_residence_cat = dplyr::recode(ta2017$fall_winter_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other", `8` = "Military base", `9` = "Partner's parents' home", `10` = "Other relatives owned/rented")

# in 2019 they totally changed the responses
ta2019$fall_winter_residence = as.numeric(ta2019$TA190063)
ta2019$fall_winter_residence = ifelse(ta2019$fall_winter_residence > 90, NA, ta2019$fall_winter_residence)
# spouse parents' owned home is included in number 5
ta2019$fall_winter_residence_parents_help = ifelse(ta2019$fall_winter_residence == 3 | ta2019$fall_winter_residence == 5 | ta2019$fall_winter_residence == 4, 1, 0)
ta2019$fall_winter_residence_cat = dplyr::recode(ta2019$fall_winter_residence, `3` = "Parents' home", `2` = "Own rent apt", `6` = "College housing", `8` = "College housing", `5` = "Home owned by parents", `1` = "Home owned by R", `9` = "Military base", `4` = "Partner's parents' home", `10` = "Other relatives owned/rented")

###
# Summer primary residence
ta2005$summer_residence = as.numeric(ta2005$TA050043)
ta2005$summer_residence = ifelse(ta2005$summer_residence == 96, ta2005$fall_winter_residence, ta2005$summer_residence)
ta2005$summer_residence = ifelse(ta2005$summer_residence > 97, NA, ta2005$summer_residence)
ta2005$summer_residence_cat = dplyr::recode(ta2005$summer_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other", `97` = "Other")
ta2005$summer_residence_parents_help = ifelse(ta2005$summer_residence_cat == "Parents' home" | ta2005$summer_residence_cat =="Home owned by parents", 1, 0)

ta2007$summer_residence = as.numeric(ta2007$TA070043)
ta2007$summer_residence = ifelse(ta2007$summer_residence == 96, ta2007$fall_winter_residence, ta2007$summer_residence)
ta2007$summer_residence = ifelse(ta2007$summer_residence > 97, NA, ta2007$summer_residence)
ta2007$summer_residence_cat = dplyr::recode(ta2007$summer_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other",  `97` = "Other")
ta2007$summer_residence_parents_help = ifelse(ta2007$summer_residence_cat == "Parents' home" | ta2007$summer_residence_cat =="Home owned by parents" , 1, 0)

ta2009$summer_residence = as.numeric(ta2009$TA090044)
ta2009$summer_residence = ifelse(ta2009$summer_residence == 96, ta2009$fall_winter_residence, ta2009$summer_residence)
ta2009$summer_residence = ifelse(ta2009$summer_residence > 97, NA, ta2009$summer_residence)
ta2009$summer_residence_cat = dplyr::recode(ta2009$summer_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other",  `97` = "Other")
ta2009$summer_residence_parents_help = ifelse(ta2009$summer_residence_cat == "Parents' home" | ta2009$summer_residence_cat =="Home owned by parents", 1, 0)

ta2011$summer_residence = as.numeric(ta2011$TA110045)
ta2011$summer_residence = ifelse(ta2011$summer_residence == 96, ta2011$fall_winter_residence, ta2011$summer_residence)
ta2011$summer_residence = ifelse(ta2011$summer_residence > 97, NA, ta2011$summer_residence)
ta2011$summer_residence_cat = dplyr::recode(ta2011$summer_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other", `8` = "Military base", `9` = "Overseas", `97` = "Other")
ta2011$summer_residence_parents_help = ifelse(ta2011$summer_residence_cat == "Parents' home" | ta2011$summer_residence_cat =="Home owned by parents" , 1, 0)

ta2013$summer_residence = as.numeric(ta2013$TA130044)
ta2013$summer_residence = ifelse(ta2013$summer_residence == 96, ta2013$fall_winter_residence, ta2013$summer_residence)
ta2013$summer_residence = ifelse(ta2013$summer_residence > 97, NA, ta2013$summer_residence)
ta2013$summer_residence_cat = dplyr::recode(ta2013$summer_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other", `8` = "Military base", `9` = "Partner's parents' home", `97` = "Other")    
ta2013$summer_residence_parents_help = ifelse(ta2013$summer_residence_cat == "Parents' home" | ta2013$summer_residence_cat =="Home owned by parents" | ta2013$summer_residence_cat == "Partner's parents' home", 1, 0)

ta2015$summer_residence = as.numeric(ta2015$TA150044)
ta2015$summer_residence = ifelse(ta2015$summer_residence == 96, ta2015$fall_winter_residence, ta2015$summer_residence)
ta2015$summer_residence = ifelse(ta2015$summer_residence > 97, NA, ta2015$summer_residence)
ta2015$summer_residence_cat = dplyr::recode(ta2015$summer_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `8` = "Military base",`9` = "Partner's parents' home", `7` = "Other", `97` = "Other")
ta2015$summer_residence_parents_help = ifelse(ta2015$summer_residence_cat == "Parents' home"| ta2015$summer_residence_cat == "Home owned by parents" | ta2015$summer_residence_cat == "Partner's parents' home", 1, 0)

ta2017$summer_residence = as.numeric(ta2017$TA170059)
ta2017$summer_residence = ifelse(ta2017$summer_residence == 96, ta2017$fall_winter_residence, ta2017$summer_residence)
ta2017$summer_residence = ifelse(ta2017$summer_residence > 97, NA, ta2017$summer_residence)
ta2017$summer_residence_cat = dplyr::recode(ta2017$summer_residence, `1` = "Parents' home", `2` = "Own rent apt", `3` = "College housing", `4` = "College housing", `5` = "Home owned by parents", `6` = "Home owned by R", `7` = "Other", `8` = "Military base", `9` = "Partner's parents' home", `10` = "Other relatives owned/rented",  `97` = "Other")  
ta2017$summer_residence_parents_help = ifelse(ta2017$summer_residence_cat == "Parents' home"| ta2017$summer_residence_cat == "Home owned by parents" | ta2017$summer_residence_cat == "Partner's parents' home", 1, 0)

ta2019$summer_residence = as.numeric(ta2019$TA190064)
ta2019$summer_residence = ifelse(ta2019$summer_residence == 96, ta2019$fall_winter_residence, ta2019$summer_residence)
ta2019$summer_residence = ifelse(ta2019$summer_residence > 97, NA, ta2019$summer_residence)
ta2019$summer_residence_cat = dplyr::recode(ta2019$summer_residence, `3` = "Parents' home", `2` = "Own rent apt", `6` = "College housing", `8` = "College housing", `5` = "Home owned by parents", `1` = "Home owned by R", `9` = "Military base", `4` = "Partner's parents' home", `10` = "Other relatives owned/rented", `97` = "Other")
ta2019$summer_residence_parents_help = ifelse(ta2019$summer_residence_cat == "Parents' home"| ta2019$summer_residence_cat == "Home owned by parents" | ta2019$summer_residence_cat == "Partner's parents' home", 1, 0)

# Enrollment Status

ta2005$enrollment_status = ta2005$TA050946
ta2005$enrollment_status = ifelse(ta2005$enrollment_status > 90, NA, ta2005$enrollment_status)
ta2005$enrollment_status_cat = dplyr::recode(ta2005$enrollment_status, `1` = "No High School", `2` = "GED (no HS)", `3`= "High School diploma", `4` = "Not enrolled; some College", `5` = "Not enrolled; 2 yr college", `6` = "Not enrolled; 4 year college", `7` = "Not enrolled; graduate degree", `9` = "Enrolled; no degree", `10` = "Enrolled; prior degree", `11` = "Enrolled; graduate program") 
ta2005$current_student = ifelse(ta2005$enrollment_status_cat == "Enrolled; no degree" | ta2005$enrollment_status_cat == "Enrolled; prior degree" | ta2005$enrollment_status_cat == "Enrolled; graduate program", 1, 0)

ta2007$enrollment_status = ta2007$TA070927
ta2007$enrollment_status = ifelse(ta2007$enrollment_status > 90, NA, ta2007$enrollment_status)
ta2007$enrollment_status_cat = dplyr::recode(ta2007$enrollment_status, `1` = "No High School", `2` = "GED (no HS)", `3`= "High School diploma", `4` = "Not enrolled; some College", `5` = "Not enrolled; 2 yr college", `6` = "Not enrolled; 4 year college", `7` = "Not enrolled; graduate degree", `9` = "Enrolled; no degree", `10` = "Enrolled; prior degree", `11` = "Enrolled; graduate program") 
ta2007$current_student = ifelse(ta2007$enrollment_status_cat == "Enrolled; no degree" | ta2007$enrollment_status_cat == "Enrolled; prior degree" | ta2007$enrollment_status_cat == "Enrolled; graduate program", 1, 0)

ta2009$enrollment_status = ta2009$TA090991
ta2009$enrollment_status = ifelse(ta2009$enrollment_status > 90, NA, ta2009$enrollment_status)
ta2009$enrollment_status_cat = dplyr::recode(ta2009$enrollment_status, `1` = "No High School", `2` = "GED (no HS)", `3`= "High School diploma", `4` = "Not enrolled; some College", `5` = "Not enrolled; 2 yr college", `6` = "Not enrolled; 4 year college", `7` = "Not enrolled; graduate degree", `9` = "Enrolled; no degree", `10` = "Enrolled; prior degree", `11` = "Enrolled; graduate program") 
ta2009$current_student = ifelse(ta2009$enrollment_status_cat == "Enrolled; no degree" | ta2009$enrollment_status_cat == "Enrolled; prior degree" | ta2009$enrollment_status_cat == "Enrolled; graduate program", 1, 0)

ta2011$enrollment_status = ta2011$TA111133
ta2011$enrollment_status = ifelse(ta2011$enrollment_status > 90, NA, ta2011$enrollment_status)
ta2011$enrollment_status_cat = dplyr::recode(ta2011$enrollment_status, `1` = "No High School", `2` = "GED (no HS)", `3`= "High School diploma", `4` = "Not enrolled; some College", `5` = "Not enrolled; 2 yr college", `6` = "Not enrolled; 4 year college", `7` = "Not enrolled; graduate degree", `9` = "Enrolled; no degree", `10` = "Enrolled; prior degree", `11` = "Enrolled; graduate program") 
ta2011$current_student = ifelse(ta2011$enrollment_status_cat == "Enrolled; no degree" | ta2011$enrollment_status_cat == "Enrolled; prior degree" | ta2011$enrollment_status_cat == "Enrolled; graduate program", 1, 0)

ta2013$enrollment_status = ta2013$TA131225
ta2013$enrollment_status = ifelse(ta2013$enrollment_status > 90, NA, ta2013$enrollment_status)
ta2013$enrollment_status_cat = dplyr::recode(ta2013$enrollment_status, `1` = "No High School", `2` = "GED (no HS)", `3`= "High School diploma", `4` = "Not enrolled; some College", `5` = "Not enrolled; 2 yr college", `6` = "Not enrolled; 4 year college", `7` = "Not enrolled; graduate degree", `9` = "Enrolled; no degree", `10` = "Enrolled; prior degree", `11` = "Enrolled; graduate program") 
ta2013$current_student = ifelse(ta2013$enrollment_status_cat == "Enrolled; no degree" | ta2013$enrollment_status_cat == "Enrolled; prior degree" | ta2013$enrollment_status_cat == "Enrolled; graduate program", 1, 0)

ta2015$enrollment_status = ta2015$TA151285
ta2015$enrollment_status = ifelse(ta2015$enrollment_status > 90, NA, ta2015$enrollment_status)
ta2015$enrollment_status_cat = dplyr::recode(ta2015$enrollment_status, `1` = "No High School", `2` = "GED (no HS)", `3`= "High School diploma", `4` = "Not enrolled; some College", `5` = "Not enrolled; 2 yr college", `6` = "Not enrolled; 4 year college", `7` = "Not enrolled; graduate degree", `9` = "Enrolled; no degree", `10` = "Enrolled; prior degree", `11` = "Enrolled; graduate program") 
ta2015$current_student = ifelse(ta2015$enrollment_status_cat == "Enrolled; no degree" | ta2015$enrollment_status_cat == "Enrolled; prior degree" | ta2015$enrollment_status_cat == "Enrolled; graduate program", 1, 0)

ta2017$enrollment_status = ta2017$TA171980
ta2017$enrollment_status = ifelse(ta2017$enrollment_status > 90, NA, ta2017$enrollment_status)
ta2017$enrollment_status_cat = dplyr::recode(ta2017$enrollment_status, `1` = "No High School", `2` = "GED (no HS)", `3`= "High School diploma", `4` = "Not enrolled; some College", `5` = "Not enrolled; 2 yr college", `6` = "Not enrolled; 4 year college", `7` = "Not enrolled; graduate degree", `9` = "Enrolled; no degree", `10` = "Enrolled; prior degree", `11` = "Enrolled; graduate program") 
ta2017$current_student = ifelse(ta2017$enrollment_status_cat == "Enrolled; no degree" | ta2017$enrollment_status_cat == "Enrolled; prior degree" | ta2017$enrollment_status_cat == "Enrolled; graduate program", 1, 0)

ta2019$enrollment_status = ta2019$TA192190
ta2019$enrollment_status = ifelse(ta2019$enrollment_status > 90, NA, ta2019$enrollment_status)
ta2019$enrollment_status_cat = dplyr::recode(ta2019$enrollment_status, `1` = "No High School", `2` = "GED (no HS)", `3`= "High School diploma", `4` = "Not enrolled; some College", `5` = "Not enrolled; 2 yr college", `6` = "Not enrolled; 4 year college", `7` = "Not enrolled; graduate degree", `9` = "Enrolled; no degree", `10` = "Enrolled; prior degree", `11` = "Enrolled; graduate program") 
ta2019$current_student = ifelse(ta2019$enrollment_status_cat == "Enrolled; no degree" | ta2019$enrollment_status_cat == "Enrolled; prior degree" | ta2019$enrollment_status_cat == "Enrolled; graduate program", 1, 0)

# Births
# 2005
ta2005$number_births = as.numeric(ta2005$TA050091)
ta2005$number_births = ifelse(ta2005$number_births %in% c(98, 99), NA, ta2005$number_births)

# 2007
ta2007$number_births = as.numeric(ta2007$TA070091)
ta2007$number_births = ifelse(ta2007$number_births %in% c(98, 99), NA, ta2007$number_births)

# 2009
ta2009$number_births = as.numeric(ta2009$TA090100)
ta2009$number_births = ifelse(ta2009$number_births %in% c(98, 99), NA, ta2009$number_births)

# 2011
ta2011$number_births = as.numeric(ta2011$TA110101)
ta2011$number_births = ifelse(ta2011$number_births %in% c(98, 99), NA, ta2011$number_births)

# 2013
ta2013$number_births = as.numeric(ta2013$TA130100)
ta2013$number_births = ifelse(ta2013$number_births %in% c(98, 99), NA, ta2013$number_births)

# 2015
ta2015$number_births = as.numeric(ta2015$TA150092)
ta2015$number_births = ifelse(ta2015$number_births %in% c(98, 99), NA, ta2015$number_births)

# 2017
ta2017$number_births = as.numeric(ta2017$TA170176)
ta2017$number_births = ifelse(ta2017$number_births %in% c(98, 99), NA, ta2017$number_births)

 # 2019
ta2019$number_births = as.numeric(ta2019$TA190218)
ta2019$number_births = ifelse(ta2019$number_births %in% c(98, 99), NA, ta2019$number_births)
 
# Current state
# 0 here means INAP: U.S. territory or foreign country. I coded them as NA right now but we can change it to "Foreign"		
# codes here http://psidonline.isr.umich.edu/data/Documentation/FIPSStateCodes.pdf

ta2005$state = as.numeric(ta2005$TA050005)
ta2005$state = ifelse(ta2005$state > 90, NA, ta2005$state)
ta2007$state = as.numeric(ta2007$TA070005)
ta2007$state = ifelse(ta2007$state > 90, NA, ta2007$state)
ta2009$state = as.numeric(ta2009$TA090005)
ta2009$state = ifelse(ta2009$state > 90, NA, ta2009$state)
ta2011$state = as.numeric(ta2011$TA110005)
ta2011$state = ifelse(ta2011$state > 90, NA, ta2011$state)
ta2013$state = as.numeric(ta2013$TA130005)
ta2013$state = ifelse(ta2013$state > 90, NA, ta2013$state)
ta2015$state = as.numeric(ta2015$TA150005)
ta2015$state = ifelse(ta2015$state > 90, NA, ta2015$state)
ta2017$state = as.numeric(ta2017$TA170005)
ta2017$state = ifelse(ta2017$state > 90, NA, ta2017$state)
ta2019$state = as.numeric(ta2019$TA192196)
ta2019$state = ifelse(ta2019$state > 90, NA, ta2019$state)

recode_state = function(x){
  x$state_cat = dplyr::recode(x$state, `0` = "Foreign Country",
                              `1`= "Alabama",
                              `2`= "Alaska",
                              `4` = "Arizona",
                              `5` = "Arkansas",
                              `6` = "California",
                              `8` = "Colorado",
                              `9` = "Connecticut",
                              `10` = "Delaware",
                              `11` = "District of Columbia",
                              `12` = "Florida",
                              `13` = "Georgia",
                              `15` = "Hawaii",
                              `16` = "Idaho",
                              `17` = "Illinois",
                              `18` = "Indiana",
                              `19` = "Iowa",
                              `20` = "Kansas",
                              `21` = "Kentucky",
                              `22` = "Louisiana",
                              `23` = "Maine",
                              `24` = "Maryland",
                              `25` = "Massachusetts",
                              `26` = "Michigan",
                              `27` = "Minnesota",
                              `28` = "Mississippi", 
                              `29` = "Missouri", 
                              `30` = "Montana", 
                              `31` = "Nebraska", 
                              `32` = "Nevada",
                              `33` = "New Hampshire", 
                              `34` = "New Jersey",
                              `35` = "New Mexico",
                              `36` = "New York",
                              `37` = "North Carolina", 
                              `38` = "North Dakota", 
                              `39` = "Ohio",
                              `40` = "Oklahoma",
                              `41` = "Oregon",
                              `42` = "Pennsylvania", 
                              `44` = "Rhode Island",
                              `45` = "South Carolina", 
                              `46` = "South Dakota", 
                              `47` = "Tennessee",
                              `48` = "Texas",
                              `49` = "Utah",
                              `50` = "Vermont",
                              `51` = "Virginia",
                              `53` = "Washington",
                              `54` = "West Virginia",
                              `55` = "Wisconsin",
                              `56` = "Wyoming")
  return(x)
}

ta2005 = recode_state(ta2005)
ta2007 = recode_state(ta2007)
ta2009 = recode_state(ta2009)
ta2011 = recode_state(ta2011)
ta2013 = recode_state(ta2013)
ta2015 = recode_state(ta2015)
ta2017 = recode_state(ta2017)
ta2019 = recode_state(ta2019)


# COMPLETED EDUCATION OF MOTHER	
# 17 is at least some graduate school. It is the highest level in this variable 

ta2005$mother_edu = as.numeric(ta2005$TA050947)
ta2005$mother_edu = ifelse(ta2005$mother_edu > 90, NA, ta2005$mother_edu)

ta2007$mother_edu = as.numeric(ta2007$TA070928)
ta2007$mother_edu = ifelse(ta2007$mother_edu > 90, NA, ta2007$mother_edu)

ta2009$mother_edu = as.numeric(ta2009$TA090992)
ta2009$mother_edu = ifelse(ta2009$mother_edu > 90, NA, ta2009$mother_edu)

ta2011$mother_edu = as.numeric(ta2011$TA111134)
ta2011$mother_edu = ifelse(ta2011$mother_edu > 90, NA, ta2011$mother_edu)

ta2013$mother_edu = as.numeric(ta2013$TA131226)
ta2013$mother_edu = ifelse(ta2013$mother_edu > 90, NA, ta2013$mother_edu)

ta2015$mother_edu = as.numeric(ta2015$TA151286)
ta2015$mother_edu = ifelse(ta2015$mother_edu > 90, NA, ta2015$mother_edu)

ta2017$mother_edu = as.numeric(ta2017$TA171981)
ta2017$mother_edu = ifelse(ta2017$mother_edu > 90, NA, ta2017$mother_edu)

ta2019$mother_edu = as.numeric(ta2019$TA192192)
ta2019$mother_edu = ifelse(ta2019$mother_edu > 90, NA, ta2019$mother_edu)


# COMPLETED EDUCATION OF FATHER										

ta2005$father_edu = as.numeric(ta2005$TA050949)
ta2005$father_edu = ifelse(ta2005$father_edu > 90, NA, ta2005$father_edu)

ta2007$father_edu = as.numeric(ta2007$TA070930)
ta2007$father_edu = ifelse(ta2007$father_edu > 90, NA, ta2007$father_edu)

ta2009$father_edu = as.numeric(ta2009$TA090994)
ta2009$father_edu = ifelse(ta2009$father_edu > 90, NA, ta2009$father_edu)

ta2011$father_edu = as.numeric(ta2011$TA111136)
ta2011$father_edu = ifelse(ta2011$father_edu > 90, NA, ta2011$father_edu)

ta2013$father_edu = as.numeric(ta2013$TA131228)
ta2013$father_edu = ifelse(ta2013$father_edu > 90, NA, ta2013$father_edu)

ta2015$father_edu = as.numeric(ta2015$TA151288)
ta2015$father_edu = ifelse(ta2015$father_edu > 90, NA, ta2015$father_edu)

ta2017$father_edu = as.numeric(ta2017$TA171983)
ta2017$father_edu = ifelse(ta2017$father_edu > 90, NA, ta2017$father_edu)

ta2019$father_edu = as.numeric(ta2019$TA192194)
ta2019$father_edu = ifelse(ta2019$father_edu > 90, NA, ta2019$father_edu)

## coding whether they are currently in high school
ta2005$year_last_in_high_school = ifelse(ta2005$TA050583 == "9999" | ta2005$TA050583 == "9998" | ta2005$TA050583 == "0", NA, ta2005$TA050583)
ta2007$year_last_in_high_school = ifelse(ta2007$TA070558 == "9999" | ta2007$TA070558 == "9998" | ta2007$TA070558 == "0", NA, ta2007$TA070558)
ta2009$year_last_in_high_school = ifelse(ta2009$TA090600  == "9999" | ta2009$TA090600  == "9998" | ta2009$TA090600  == "0", NA, ta2009$TA090600 )
ta2011$year_last_in_high_school = ifelse(ta2011$TA110681  == "9999" | ta2011$TA110681  == "9998" | ta2011$TA110681  == "0", NA, ta2011$TA110681 )
ta2013$year_last_in_high_school = ifelse(ta2013$TA130701  == "9999" | ta2013$TA130701  == "9998" | ta2013$TA130701  == "0", NA, ta2013$TA130701 )
ta2015$year_last_in_high_school = ifelse(ta2015$TA150711   == "9999" | ta2015$TA150711   == "9998" | ta2015$TA150711   == "0", NA, ta2015$TA150711)

# if the year they were last in high school = current year, then we assume they are still in school
ta2005$still_in_high_school = ifelse(ta2005$year_last_in_high_school== 2005, 1, 0)
ta2007$still_in_high_school = ifelse(ta2007$year_last_in_high_school== 2007, 1, 0)
ta2009$still_in_high_school = ifelse(ta2009$year_last_in_high_school== 2009, 1, 0)
ta2011$still_in_high_school = ifelse(ta2011$year_last_in_high_school== 2011, 1, 0)
ta2013$still_in_high_school = ifelse(ta2013$year_last_in_high_school== 2013, 1, 0)
ta2015$still_in_high_school = ifelse(ta2015$year_last_in_high_school== 2015, 1, 0)

ta2017$still_in_high_school = ifelse(ta2017$TA170792 == 3, 1, 0)
ta2017$still_in_high_school = ifelse(ta2017$TA170793 == 3, 1, ta2017$still_in_high_school)
ta2017$still_in_high_school = ifelse(ta2017$TA170794 == 3, 1, ta2017$still_in_high_school)

ta2019$still_in_high_school = ifelse(ta2019$TA190929 == 3, 1, 0)
ta2019$still_in_high_school = ifelse(ta2019$TA190930 == 3, 1, ta2019$still_in_high_school)
ta2019$still_in_high_school = ifelse(ta2019$TA190931 == 3, 1, ta2019$still_in_high_school)

# employment status

ta2005$working_now = ifelse(ta2005$TA050127 == 1, 1, 0)
ta2005$working_now = ifelse(ta2005$TA050128 == 1, 1, ta2005$working_now)
ta2005$working_now = ifelse(ta2005$TA050129 == 1, 1, ta2005$working_now)

ta2007$working_now = ifelse(ta2007$TA070127 == 1, 1, 0)
ta2007$working_now = ifelse(ta2007$TA070128 == 1, 1, ta2007$working_now)
ta2007$working_now = ifelse(ta2007$TA070129 == 1, 1, ta2007$working_now)

ta2009$working_now = ifelse(ta2009$TA090136 == 1, 1, 0)
ta2009$working_now = ifelse(ta2009$TA090137 == 1, 1, ta2009$working_now)
ta2009$working_now = ifelse(ta2009$TA090138 == 1, 1, ta2009$working_now)

ta2011$working_now = ifelse(ta2011$TA110137 == 1, 1, 0)
ta2011$working_now = ifelse(ta2011$TA110138 == 1, 1, ta2011$working_now)
ta2011$working_now = ifelse(ta2011$TA110139 == 1, 1, ta2011$working_now)

ta2013$working_now = ifelse(ta2013$TA130136 == 1, 1, 0)
ta2013$working_now = ifelse(ta2013$TA130137 == 1, 1, ta2013$working_now)
ta2013$working_now = ifelse(ta2013$TA130138 == 1, 1, ta2013$working_now)

ta2015$working_now = ifelse(ta2015$TA150128 == 1, 1, 0)
ta2015$working_now = ifelse(ta2015$TA150129 == 1, 1, ta2015$working_now)
ta2015$working_now = ifelse(ta2015$TA150130 == 1, 1, ta2015$working_now)

ta2017$working_now = ifelse(ta2017$TA170183 == 1, 1, 0)
ta2017$working_now = ifelse(ta2017$TA170184 == 1, 1, ta2017$working_now)
ta2017$working_now = ifelse(ta2017$TA170185 == 1, 1, ta2017$working_now)

ta2019$working_now = ifelse(ta2019$TA190225 == 1, 1, 0)
ta2019$working_now = ifelse(ta2019$TA190226 == 1, 1, ta2019$working_now)
ta2019$working_now = ifelse(ta2019$TA190227 == 1, 1, ta2019$working_now)

#################################################
########## Measures of Indpendence #############
################################################

# make a total help amount for each year
vars_help = c("total_house_value", "total_rent_value", "total_vehicle_value", "total_tuition_value", "total_bills_value", "total_personal_loan_value")

vars_help_2017_2019 = c("amt_pahouses_paid_house", "amt_parents_paid_rent", "amt_parents_paid_vehicle", 
                        "amt_parents_paid_tuition", "amt_parents_paid_bills", "total_personal_loan_value", "total_gift_inheritence_value")

ta2005$total_parent_help_amount = rowSums(ta2005[,vars_help])
ta2007$total_parent_help_amount = rowSums(ta2007[,vars_help])
ta2009$total_parent_help_amount = rowSums(ta2009[,vars_help])
ta2011$total_parent_help_amount = rowSums(ta2011[,vars_help])
ta2013$total_parent_help_amount = rowSums(ta2013[,vars_help])
ta2015$total_parent_help_amount = rowSums(ta2015[,vars_help])
ta2017$total_parent_help_amount = rowSums(ta2017[,vars_help_2017_2019])
ta2019$total_parent_help_amount = rowSums(ta2019[,vars_help_2017_2019])

# make a dataset of only the variables we recoded
ta2005$year = 2005
ta2007$year = 2007
ta2009$year = 2009
ta2011$year = 2011
ta2013$year = 2013
ta2015$year = 2015
ta2017$year = 2017
ta2019$year = 2019

# count number of IIDs that are the same over the whole dataset?
psid_to_ta = readRDS("./psid68_to_tas_ids.RDS")

## ER30001 1968 INTERVIEW NUMBER
## ER30002 PERSON NUMBER 68

# 2005
psid_to_ta$FID_2005 = as.numeric(psid_to_ta$TA050003)
psid_to_ta$IID_2005 = as.numeric(psid_to_ta$TA050004)
psid_to_ta$UID_2005 =  paste(psid_to_ta$FID_2005, psid_to_ta$IID_2005, sep = "_")

ta2005$psid68_FID = psid_to_ta$ER30001[match(ta2005$UID, psid_to_ta$UID_2005)]
ta2005$psid68_IID = psid_to_ta$ER30002[match(ta2005$UID, psid_to_ta$UID_2005)]
ta2005$psid68_UID = paste(as.numeric(ta2005$psid68_FID), as.numeric(ta2005$psid68_IID), sep = "_")

# 2007
psid_to_ta$FID_2007 = as.numeric(psid_to_ta$TA070003)
psid_to_ta$IID_2007 = as.numeric(psid_to_ta$TA070004)
psid_to_ta$UID_2007 =  paste(psid_to_ta$FID_2007, psid_to_ta$IID_2007, sep = "_")

ta2007$psid68_FID = psid_to_ta$ER30001[match(ta2007$UID, psid_to_ta$UID_2007)]
ta2007$psid68_IID = psid_to_ta$ER30002[match(ta2007$UID, psid_to_ta$UID_2007)]
ta2007$psid68_UID = paste(as.numeric(ta2007$psid68_FID), as.numeric(ta2007$psid68_IID), sep = "_")

# 2009
psid_to_ta$FID_2009 = as.numeric(psid_to_ta$TA090003)
psid_to_ta$IID_2009 = as.numeric(psid_to_ta$TA090004)
psid_to_ta$UID_2009 =  paste(psid_to_ta$FID_2009, psid_to_ta$IID_2009, sep = "_")

ta2009$psid68_FID = psid_to_ta$ER30001[match(ta2009$UID, psid_to_ta$UID_2009)]
ta2009$psid68_IID = psid_to_ta$ER30002[match(ta2009$UID, psid_to_ta$UID_2009)]
ta2009$psid68_UID = paste(as.numeric(ta2009$psid68_FID), as.numeric(ta2009$psid68_IID), sep = "_")

# 2011
psid_to_ta$FID_2011 = as.numeric(psid_to_ta$TA110003)
psid_to_ta$IID_2011 = as.numeric(psid_to_ta$TA110004)
psid_to_ta$UID_2011 =  paste(psid_to_ta$FID_2011, psid_to_ta$IID_2011, sep = "_")

ta2011$psid68_FID = psid_to_ta$ER30001[match(ta2011$UID, psid_to_ta$UID_2011)]
ta2011$psid68_IID = psid_to_ta$ER30002[match(ta2011$UID, psid_to_ta$UID_2011)]
ta2011$psid68_UID = paste(as.numeric(ta2011$psid68_FID), as.numeric(ta2011$psid68_IID), sep = "_")

# 2013
psid_to_ta$FID_2013 = as.numeric(psid_to_ta$TA130003)
psid_to_ta$IID_2013 = as.numeric(psid_to_ta$TA130004)
psid_to_ta$UID_2013 =  paste(psid_to_ta$FID_2013, psid_to_ta$IID_2013, sep = "_")

ta2013$psid68_FID = psid_to_ta$ER30001[match(ta2013$UID, psid_to_ta$UID_2013)]
ta2013$psid68_IID = psid_to_ta$ER30002[match(ta2013$UID, psid_to_ta$UID_2013)]
ta2013$psid68_UID = paste(as.numeric(ta2013$psid68_FID), as.numeric(ta2013$psid68_IID), sep = "_")

# 2015
psid_to_ta$FID_2015 = as.numeric(psid_to_ta$TA150003)
psid_to_ta$IID_2015 = as.numeric(psid_to_ta$TA150004)
psid_to_ta$UID_2015 =  paste(psid_to_ta$FID_2015, psid_to_ta$IID_2015, sep = "_")

ta2015$psid68_FID = psid_to_ta$ER30001[match(ta2015$UID, psid_to_ta$UID_2015)]
ta2015$psid68_IID = psid_to_ta$ER30002[match(ta2015$UID, psid_to_ta$UID_2015)]
ta2015$psid68_UID = paste(as.numeric(ta2015$psid68_FID), as.numeric(ta2015$psid68_IID), sep = "_")

# 2017
psid_to_ta$FID_2017 = as.numeric(psid_to_ta$TA170003)
psid_to_ta$IID_2017 = as.numeric(psid_to_ta$TA170004)
psid_to_ta$UID_2017 =  paste(psid_to_ta$FID_2017, psid_to_ta$IID_2017, sep = "_")

ta2017$psid68_FID = psid_to_ta$ER30001[match(ta2017$UID, psid_to_ta$UID_2017)]
ta2017$psid68_IID = psid_to_ta$ER30002[match(ta2017$UID, psid_to_ta$UID_2017)]
ta2017$psid68_UID = paste(as.numeric(ta2017$psid68_FID), as.numeric(ta2017$psid68_IID), sep = "_")

# 2019
psid_to_ta$FID_2019 = as.numeric(psid_to_ta$TA190003)
psid_to_ta$IID_2019 = as.numeric(psid_to_ta$TA190004)
psid_to_ta$UID_2019 =  paste(psid_to_ta$FID_2019, psid_to_ta$IID_2019, sep = "_")

ta2019$psid68_FID = psid_to_ta$ER30001[match(ta2019$UID, psid_to_ta$UID_2019)]
ta2019$psid68_IID = psid_to_ta$ER30002[match(ta2019$UID, psid_to_ta$UID_2019)]
ta2019$psid68_UID = paste(as.numeric(ta2019$psid68_FID), as.numeric(ta2019$psid68_IID), sep = "_")

# 2021
ta2021_merged <- readRDS('./TAS2021/ta2021_cleaned_for_seq_new.RDS')
ta2021_merged$year = 2021

# Births
ta2021_merged$number_births = as.numeric(ta2021_merged$C45)
ta2021_merged$number_births = ifelse(ta2021_merged$number_births %in% c(98, 99), NA, ta2021_merged$number_births)

ta2005$keeping_home = ifelse(ta2005$TA050127 == 6, 1, 0)
ta2007$keeping_home = ifelse(ta2007$TA070127 == 6, 1, 0)
ta2009$keeping_home = ifelse(ta2009$TA090136 == 6, 1, 0)
ta2011$keeping_home = ifelse(ta2011$TA110137 == 6, 1, 0)
ta2013$keeping_home = ifelse(ta2013$TA130136 == 6, 1, 0)
ta2015$keeping_home = ifelse(ta2015$TA150128 == 6, 1, 0)
ta2017$keeping_home = ifelse(ta2017$TA170183 == 6, 1, 0)
ta2019$keeping_home = ifelse(ta2019$TA190225 == 6, 1, 0)
ta2021_merged$keeping_home = ifelse(ta2021_merged$employment == 6, 1, 0)

ta2005$unemployed = ifelse(ta2005$TA050127 == 3, 1, 0)
ta2007$unemployed = ifelse(ta2007$TA070127 == 3, 1, 0)
ta2009$unemployed = ifelse(ta2009$TA090136 == 3, 1, 0)
ta2011$unemployed = ifelse(ta2011$TA110137 == 3, 1, 0)
ta2013$unemployed = ifelse(ta2013$TA130136 == 3, 1, 0)
ta2015$unemployed = ifelse(ta2015$TA150128 == 3, 1, 0)
ta2017$unemployed = ifelse(ta2017$TA170183 == 3, 1, 0)
ta2019$unemployed = ifelse(ta2019$TA190225 == 3, 1, 0)
# the data for this is different - it's in a file called "ta2021_data_prep.R". If this doesn't work, you can try D1_1 == 3
ta2021_merged$unemployed = ifelse(ta2021_merged$employment == 3, 1, 0)


vars_coded = c("psid68_UID",
               "psid68_FID",
               "psid68_IID",
               "year",
               "FID", 
               "IID", 
               "UID",
               "earning_responsibility",
               "rent_responsibility",
               "bills_responsibility",
               "money_responsibility",
               "house_covered",
               "total_house_value",
               "rent_covered",
               "total_rent_value",
               "vehicle_covered",
               "total_vehicle_value",
               "tuition_covered",
               "total_tuition_value",
               "bills_covered",
               "total_bills_value",
               "personal_loan_covered",
               "total_personal_loan_value",
               "parents_gave_gift_inheritence",
               "total_gift_inheritence_value",
               "graduated_high_school",
               "grade_completed",
               "educational_aspirations",
               "educational_expectations",
               "ever_college",
               "in_college_now",
               "health_level",
               "ever_psych_problems",
               "problem_limits_activities",
               "race",
               "race_cat",
               "amt_all_accounts",
               "enrollment_status",
               "mother_edu",
               "father_edu",
               "marriage_cohabitation",
               "earnings_work",
               "state_cat",
               "fall_winter_residence_cat",
               "summer_residence_cat",
               "fall_winter_residence_parents_help",
               "summer_residence_parents_help",
               "current_student", 
               "still_in_high_school",
               "working_now",
               "total_parent_help_amount", 
               "keeping_home", 
               "unemployed", 
               "wtr_health_limits_work", 
               "number_births")

vars_coded[!vars_coded %in% colnames(ta2021_merged)]

# Limit to shared variables
vars_coded_shared = vars_coded[vars_coded %in% colnames(ta2021_merged)]

# Put all of the different files in a list
tas = list(ta2005, ta2007, ta2009, ta2011, ta2013, ta2015, ta2017, ta2019, ta2021_merged)

# Loop through and select the above variables
tas_long = lapply(tas, FUN = function(x) x[,vars_coded_shared])

# Rbind them all into one long-form data set
tas_long_df = do.call("rbind", tas_long)

#test = tas_long_df[,c("psid68_UID", "year", "earning_responsibility")]
#test[order(test$psid68_UID, decreasing = T),]

# Now recast from long to wide
library(tidyr)
ta_wide_df_via_cast = pivot_wider(tas_long_df, 
                                  id_cols = c("psid68_UID"),
                                  names_from = "year",
                                  values_from = vars_coded_shared[6:length(vars_coded_shared)])

saveRDS(ta_wide_df_via_cast, "ta_coded_wideform_v3.RDS")

saveRDS(tas_long_df, "ta_coded_longform_v3.RDS")

# Read in individual-level data
ta_wide_df_via_cast = readRDS("ta_coded_wideform_v3.RDS")
tas_long_df = readRDS("ta_coded_longform_v3.RDS")

individual_data = readRDS("./cleaned_ind_data/psid_individual_data_6821_w_column_names.RDS")

# Family ID
individual_data$FID = individual_data$ER30001_1968_interview_number__all
individual_data$FID = ifelse(nchar(individual_data$FID) == 3, paste0(0, individual_data$FID), individual_data$FID)
individual_data$FID = ifelse(nchar(individual_data$FID) == 2, paste0("00", individual_data$FID), individual_data$FID)
individual_data$FID = ifelse(nchar(individual_data$FID) == 1, paste0("000", individual_data$FID), individual_data$FID)

# Individual ID
individual_data$IID = individual_data$ER30002_person_number_1968
individual_data$IID = ifelse(nchar(individual_data$IID) == 2, paste0("0", individual_data$IID), individual_data$IID)
individual_data$IID = ifelse(nchar(individual_data$IID) == 1, paste0("00", individual_data$IID), individual_data$IID)

# Unique identifier
individual_data$UID = paste(as.numeric(individual_data$FID), as.numeric(individual_data$IID), sep = "_")

ta_wide_df_via_cast_w_ind = merge(x = ta_wide_df_via_cast, y = individual_data, by.x = "psid68_UID", by.y = "UID", all.x = T, fill = NA)

# Example of how to code age for 2005 -- 0s = NA
# grep("_age_", colnames(ta_wide_df_via_cast_w_ind), value = T)
ta_wide_df_via_cast_w_ind$age_in_2005 = as.numeric(ta_wide_df_via_cast_w_ind$ER33804_age_of_individual_2005)
ta_wide_df_via_cast_w_ind$age_in_2005 = ifelse(ta_wide_df_via_cast_w_ind$age_in_2005 == 0, NA, ta_wide_df_via_cast_w_ind$age_in_2005)

ta_wide_df_via_cast_w_ind$age_in_2021 = as.numeric(ta_wide_df_via_cast_w_ind$ER34904_age_of_individual_2021)
ta_wide_df_via_cast_w_ind$age_in_2021 = ifelse(ta_wide_df_via_cast_w_ind$age_in_2021 == 0, NA, ta_wide_df_via_cast_w_ind$age_in_2021)

# Merge into long data
tas_long_df_w_ind = merge(x = tas_long_df, y = individual_data, by.x = "psid68_UID", by.y = "UID", all.x = T, fill = NA)
tas_long_df_w_ind$age_in_2021 = as.numeric(tas_long_df_w_ind$ER34904_age_of_individual_2021)
tas_long_df_w_ind$age_in_2021 = ifelse(tas_long_df_w_ind$age_in_2021 == 0, NA, tas_long_df_w_ind$age_in_2021)
tas_long_df_w_ind$year_of_birth = 2021-tas_long_df_w_ind$age_in_2021

tas_long_df_w_ind$year_at_ta_sample = tas_long_df_w_ind$year-tas_long_df_w_ind$year_of_birth

saveRDS(tas_long_df_w_ind, "psid_long_with_ta_and_ind_v3.RDS")
saveRDS(ta_wide_df_via_cast_w_ind, "psid_wide_with_ta_and_ind_v3.RDS")

#######
all = readRDS("psid_long_with_ta_and_ind_v3.RDS")

# create independence measures
# first, residential independence
all$residentially_ind = ifelse(all$fall_winter_residence_cat %in% c("Own rent apt", "College housing", "Home owned by R", "Overseas", "Military base", "Other"), 0, all$fall_winter_residence_cat)
all$residentially_ind = ifelse(all$residentially_ind %in% c("Parents' home", "Home owned by parents","Partner's parents' home", "Other relatives owned/rented"), 1, all$residentially_ind)
all$residentially_ind = abs(as.numeric(all$residentially_ind) - 1)

## Create string of independence Sequence
all$IndSeq <- paste(
  all$bills_covered,
  all$tuition_covered,
  all$rent_covered,
  all$parents_gave_gift_inheritence)

## Delete rows which contain NA in any one of "independent variables"
table(all$IndSeq)
all <- all[-grep("NA",all$IndSeq) ,]
table(all$IndSeq) # here are all the permutations 

## Create final independence column
all$financially_ind <- NA
all$financially_ind = ifelse(grepl("1",all$IndSeq), "1", "0") # IF there is atleast one occurrence of dependent ("1"), then row is automatically dependent ("1"). Everything else is 0 because no NAs. 

## flip independence so 1=Independent; 0=Dependent now. 
all$financially_ind = ifelse(grepl("0",all$financially_ind), 1, 0) # IF there is atleast one occurrence of dependent ("1"), then row is automatically dependent ("1"). Everything else is 0 because no NAs. 

# save all that has independence measures in it
saveRDS(all, "psid_long_with_ta_and_ind_v4.RDS")

# in school or college
all$in_school_or_coll = ifelse(all$in_college_now == 1, 1, 0)

# working
all$working_now_no_school = ifelse(all$in_school_or_coll == 0 & all$working_now == 1, 1, 0)
all$no_school_no_work = ifelse(all$in_school_or_coll == 0 & all$working_now == 0, 1, 0)

#making sure they are mutually exclusive
table(all$in_school_or_coll, all$working_now_no_school )
table(all$in_school_or_coll, all$no_school_no_work )

# creating mutually exclusive categories for independence
all$totally_ind = ifelse(all$residentially_ind == 1 & all$financially_ind == 1, 1, 0)
all$F_dep_R_ind = ifelse(all$residentially_ind == 1 & all$financially_ind == 0, 1, 0)
all$F_ind_R_dep = ifelse(all$residentially_ind == 0 & all$financially_ind == 1, 1, 0)
all$totally_dep = ifelse(all$residentially_ind == 0 & all$financially_ind == 0, 1, 0)

all$independence_res_fin = NA
all$independence_res_fin = ifelse(all$financially_ind == 1 & all$residentially_ind == 1, "Totally Independent", all$independence_res_fin)
all$independence_res_fin = ifelse(all$financially_ind == 0 & all$residentially_ind == 0, "Totally Dependent", all$independence_res_fin)
all$independence_res_fin = ifelse(all$financially_ind == 1 & all$residentially_ind == 0, "F ind, R dep", all$independence_res_fin)
all$independence_res_fin = ifelse(all$financially_ind == 0 & all$residentially_ind == 1, "F dep, R ind", all$independence_res_fin)

# marital status single and married or cohabiting 

all$marriage_cohabitation_cat = dplyr::recode(all$marriage_cohabitation, `Divorced, cohabiting` = "married or cohab", `Divorced, not cohabiting` = "not married or cohab",
                                              `Never Married; Not cohabiting` = "not married or cohab", `Separated` = "not married or cohab", `Widowed` = "not married or cohab",
                                              `Never Married; Cohabiting` = "married or cohab", `Separated` = "not married or cohab",
                                              `Widowed` = "not married or cohab", `Married, spouse present` = "married or cohab")

all$marriage_cohabitation_binary = ifelse(all$marriage_cohabitation_cat == "married or cohab", 1, 0)

all$married_binary = ifelse(all$marriage_cohabitation == "Married, spouse present", 1, 0)
all$cohab_binary = ifelse(all$marriage_cohabitation == "Divorced, cohabiting" | all$marriage_cohabitation == "Never Married; Cohabiting", 1, 0)

all$birth_order_mother = as.numeric(all$ER32013_order_of_birth_to_mother__all)
all$birth_order_mother = ifelse(all$birth_order_mother > 10, NA, all$birth_order_mother)

all$female = ifelse(all$ER32000_sex_of_individual__all == 2, 1, 0)
all$first_born = ifelse(all$birth_order_mother == 1, 1, 0)

all$UID_2 = paste(all$ER30001_1968_interview_number__all, all$ER30002_person_number_1968, sep = "_")

saveRDS(all, "./psid_long_with_ta_and_ind_v5.RDS")

all = readRDS('./psid_long_with_ta_and_ind_v5.RDS')

all$gpa2005 = ifelse(all$TA050584 > 6 | all$TA050584 == 0, NA, all$TA050584)
all$gpamax2005 = ifelse(all$TA050585 > 6 | all$TA050585 == 0, NA, all$TA050585)
all$gpa_standard_2005 = all$gpa2005 / all$gpamax2005

all$gpa2007 = ifelse(all$TA070559 > 12 | all$TA070559 == 0, NA, all$TA070559)
all$gpamax2007 = ifelse(all$TA070560 > 12 | all$TA070560 == 0, NA, all$TA070560)
all$gpa_standard_2007 = all$gpa2007 / all$gpamax2007

all$gpa2009 = ifelse(all$TA090601 > 12 | all$TA090601 == 0, NA, all$TA090601)
all$gpamax2009 = ifelse(all$TA090602 > 12 | all$TA090602 == 0, NA, all$TA090602)
all$gpa_standard_2009 = all$gpa2009 / all$gpamax2009

all$gpa2011 = ifelse(all$TA110682 > 12 | all$TA110682 == 0, NA, all$TA110682)
all$gpamax2011 = ifelse(all$TA110683 > 12 | all$TA110683 == 0, NA, all$TA110683)
all$gpa_standard_2011 = all$gpa2011 / all$gpamax2011

all$gpa2013 = ifelse(all$TA130702 > 12 | all$TA130702 == 0, NA, all$TA130702)
all$gpamax2013 = ifelse(all$TA130703 > 12 | all$TA130703 == 0, NA, all$TA130703)
all$gpa_standard_2013 = all$gpa2013 / all$gpamax2013

all$gpa2015 = ifelse(all$TA150712 > 12 | all$TA150712 == 0, NA, all$TA150712)
all$gpamax2015 = ifelse(all$TA150713 > 12 | all$TA150713 == 0, NA, all$TA150713)
all$gpa_standard_2015 = all$gpa2015 / all$gpamax2015

all$gpa2017 = ifelse(all$TA170782 > 12 | all$TA170782 == 0, NA, all$TA170782)
all$gpamax2017 = ifelse(all$TA170783 > 12 | all$TA170783 == 0, NA, all$TA170783)
all$gpa_standard_2017 = all$gpa2017 / all$gpamax2017

all$gpa2019 = ifelse(all$TA190919 > 12 | all$TA190919 == 0, NA, all$TA190919)
all$gpamax2019 = ifelse(all$TA190920 > 12 | all$TA190920 == 0, NA, all$TA190920)
all$gpa_standard_2019 = all$gpa2019 / all$gpamax2019
all$gpa_standard_2019 = ifelse(all$gpa_standard_2019 > 2, NA, all$gpa_standard_2019)

all$standardized_gpa[all$year == 2021] = ta2021_merged$gpa_standard_2021[match(all$psid68_UID[all$year == 2021], ta2021_merged$psid68_UID)]

## income
all$fam_income_last_yr_2005 = all$ER28037
all$fam_income_last_yr_2007 = all$ER41027
all$fam_income_last_yr_2009 = all$ER46935
all$fam_income_last_yr_2011 = all$ER52343
all$fam_income_last_yr_2013 = all$ER58152
all$fam_income_last_yr_2015 = all$ER65349
all$fam_income_last_yr_2017 = all$ER71426
all$fam_income_last_yr_2019 = all$ER77448
all$fam_income_last_yr_2021 = all$ER81775

# wealth
all$wealth_no_equity_2005 = all$S716
all$wealth_w_equity_2005 = all$S717

all$wealth_no_equity_2007 = all$S816
all$wealth_w_equity_2007 = all$S817

all$wealth_no_equity_2009 = all$ER46968
all$wealth_w_equity_2009 = all$ER46970

all$wealth_no_equity_2011 = all$ER52392
all$wealth_w_equity_2011 = all$ER52394

all$wealth_no_equity_2013 = all$ER58209
all$wealth_w_equity_2013 = all$ER58211

all$wealth_no_equity_2015 = all$ER65406
all$wealth_w_equity_2015 = all$ER65408

all$wealth_no_equity_2017 = all$ER71483
all$wealth_w_equity_2017 = all$ER71485

all$wealth_no_equity_2019 = all$ER77509
all$wealth_w_equity_2019 = all$ER77511

all$wealth_no_equity_2021 = all$ER81836
all$wealth_w_equity_2021 = all$ER81838

# first merge in parents identifiers
library(readxl)
parents = read_excel("./fim14088_gid_BA_2_UBL_wide.xlsx")

parents$child_UID = paste(parents$ER30001, parents$ER30002, sep = "_")

parents$mom_FID = ifelse(!is.na(parents$ER30001_P_AM), parents$ER30001_P_AM, parents$ER30001_P_M )
parents$mom_IID = ifelse(!is.na(parents$ER30002_P_AM), parents$ER30002_P_AM, parents$ER30002_P_M )
parents$mom_UID = paste(parents$mom_FID, parents$mom_IID, sep = "_")

parents$dad_FID = ifelse(!is.na(parents$ER30001_P_AF), parents$ER30001_P_AF, parents$ER30001_P_F )
parents$dad_IID = ifelse(!is.na(parents$ER30002_P_AF), parents$ER30002_P_AF, parents$ER30002_P_F )
parents$dad_UID = paste(parents$dad_FID, parents$dad_IID, sep = "_")

# 2005
parents$average_parental_income_2005 = rowMeans(parents[,c('dad_income_2005', 'mom_income_2005')], na.rm = T)
parents$average_parental_income_2005 = ifelse(is.nan(parents$average_parental_income_2005), NA, parents$average_parental_income_2005)
parents$average_parental_wealth_no_equity_2005 = rowMeans(parents[,c('dad_wealth_no_equity_2005', 'mom_wealth_no_equity_2005')], na.rm = T)
parents$average_parental_wealth_no_equity_2005 = ifelse(is.nan(parents$average_parental_wealth_no_equity_2005), NA, parents$average_parental_wealth_no_equity_2005)
parents$average_parental_wealth_w_equity_2005 = rowMeans(parents[,c('dad_wealth_w_equity_2005', 'mom_wealth_w_equity_2005')], na.rm = T)
parents$average_parental_wealth_w_equity_2005 = ifelse(is.nan(parents$average_parental_wealth_w_equity_2005), NA, parents$average_parental_wealth_w_equity_2005)

# 2007
parents$average_parental_income_2007 = rowMeans(parents[,c('dad_income_2007', 'mom_income_2007')], na.rm = T)
parents$average_parental_income_2007 = ifelse(is.nan(parents$average_parental_income_2007), NA, parents$average_parental_income_2007)
parents$average_parental_wealth_no_equity_2007 = rowMeans(parents[,c('dad_wealth_no_equity_2007', 'mom_wealth_no_equity_2007')], na.rm = T)
parents$average_parental_wealth_no_equity_2007 = ifelse(is.nan(parents$average_parental_wealth_no_equity_2007), NA, parents$average_parental_wealth_no_equity_2007)
parents$average_parental_wealth_w_equity_2007 = rowMeans(parents[,c('dad_wealth_w_equity_2007', 'mom_wealth_w_equity_2007')], na.rm = T)
parents$average_parental_wealth_w_equity_2007 = ifelse(is.nan(parents$average_parental_wealth_w_equity_2007), NA, parents$average_parental_wealth_w_equity_2007)

# 2009
parents$average_parental_income_2009 = rowMeans(parents[,c('dad_income_2009', 'mom_income_2009')], na.rm = T)
parents$average_parental_income_2009 = ifelse(is.nan(parents$average_parental_income_2009), NA, parents$average_parental_income_2009)
parents$average_parental_wealth_no_equity_2009 = rowMeans(parents[,c('dad_wealth_no_equity_2009', 'mom_wealth_no_equity_2009')], na.rm = T)
parents$average_parental_wealth_no_equity_2009 = ifelse(is.nan(parents$average_parental_wealth_no_equity_2009), NA, parents$average_parental_wealth_no_equity_2009)
parents$average_parental_wealth_w_equity_2009 = rowMeans(parents[,c('dad_wealth_w_equity_2009', 'mom_wealth_w_equity_2009')], na.rm = T)
parents$average_parental_wealth_w_equity_2009 = ifelse(is.nan(parents$average_parental_wealth_w_equity_2009), NA, parents$average_parental_wealth_w_equity_2009)

#2011
parents$average_parental_income_2011 = rowMeans(parents[,c('dad_income_2011', 'mom_income_2011')], na.rm = T)
parents$average_parental_income_2011 = ifelse(is.nan(parents$average_parental_income_2011), NA, parents$average_parental_income_2011)
parents$average_parental_wealth_no_equity_2011 = rowMeans(parents[,c('dad_wealth_no_equity_2011', 'mom_wealth_no_equity_2011')], na.rm = T)
parents$average_parental_wealth_no_equity_2011 = ifelse(is.nan(parents$average_parental_wealth_no_equity_2011), NA, parents$average_parental_wealth_no_equity_2011)
parents$average_parental_wealth_w_equity_2011 = rowMeans(parents[,c('dad_wealth_w_equity_2011', 'mom_wealth_w_equity_2011')], na.rm = T)
parents$average_parental_wealth_w_equity_2011 = ifelse(is.nan(parents$average_parental_wealth_w_equity_2011), NA, parents$average_parental_wealth_w_equity_2011)

# 2013
parents$average_parental_income_2013 = rowMeans(parents[,c('dad_income_2013', 'mom_income_2013')], na.rm = T)
parents$average_parental_income_2013 = ifelse(is.nan(parents$average_parental_income_2013), NA, parents$average_parental_income_2013)
parents$average_parental_wealth_no_equity_2013 = rowMeans(parents[,c('dad_wealth_no_equity_2013', 'mom_wealth_no_equity_2013')], na.rm = T)
parents$average_parental_wealth_no_equity_2013 = ifelse(is.nan(parents$average_parental_wealth_no_equity_2013), NA, parents$average_parental_wealth_no_equity_2013)
parents$average_parental_wealth_w_equity_2013 = rowMeans(parents[,c('dad_wealth_w_equity_2013', 'mom_wealth_w_equity_2013')], na.rm = T)
parents$average_parental_wealth_w_equity_2013 = ifelse(is.nan(parents$average_parental_wealth_w_equity_2013), NA, parents$average_parental_wealth_w_equity_2013)

# 2015
parents$average_parental_income_2015 = rowMeans(parents[,c('dad_income_2015', 'mom_income_2015')], na.rm = T)
parents$average_parental_income_2015 = ifelse(is.nan(parents$average_parental_income_2015), NA, parents$average_parental_income_2015)
parents$average_parental_wealth_no_equity_2015 = rowMeans(parents[,c('dad_wealth_no_equity_2015', 'mom_wealth_no_equity_2015')], na.rm = T)
parents$average_parental_wealth_no_equity_2015 = ifelse(is.nan(parents$average_parental_wealth_no_equity_2015), NA, parents$average_parental_wealth_no_equity_2015)
parents$average_parental_wealth_w_equity_2015 = rowMeans(parents[,c('dad_wealth_w_equity_2015', 'mom_wealth_w_equity_2015')], na.rm = T)
parents$average_parental_wealth_w_equity_2015 = ifelse(is.nan(parents$average_parental_wealth_w_equity_2015), NA, parents$average_parental_wealth_w_equity_2015)

# 2017
parents$average_parental_income_2017 = rowMeans(parents[,c('dad_income_2017', 'mom_income_2017')], na.rm = T)
parents$average_parental_income_2017 = ifelse(is.nan(parents$average_parental_income_2017), NA, parents$average_parental_income_2017)
parents$average_parental_wealth_no_equity_2017 = rowMeans(parents[,c('dad_wealth_no_equity_2017', 'mom_wealth_no_equity_2017')], na.rm = T)
parents$average_parental_wealth_no_equity_2017 = ifelse(is.nan(parents$average_parental_wealth_no_equity_2017), NA, parents$average_parental_wealth_no_equity_2017)
parents$average_parental_wealth_w_equity_2017 = rowMeans(parents[,c('dad_wealth_w_equity_2017', 'mom_wealth_w_equity_2017')], na.rm = T)
parents$average_parental_wealth_w_equity_2017 = ifelse(is.nan(parents$average_parental_wealth_w_equity_2017), NA, parents$average_parental_wealth_w_equity_2017)

# 2019
parents$average_parental_income_2019 = rowMeans(parents[,c('dad_income_2019', 'mom_income_2019')], na.rm = T)
parents$average_parental_income_2019 = ifelse(is.nan(parents$average_parental_income_2019), NA, parents$average_parental_income_2019)
parents$average_parental_wealth_no_equity_2019 = rowMeans(parents[,c('dad_wealth_no_equity_2019', 'mom_wealth_no_equity_2019')], na.rm = T)
parents$average_parental_wealth_no_equity_2019 = ifelse(is.nan(parents$average_parental_wealth_no_equity_2019), NA, parents$average_parental_wealth_no_equity_2019)
parents$average_parental_wealth_w_equity_2019 = rowMeans(parents[,c('dad_wealth_w_equity_2019', 'mom_wealth_w_equity_2019')], na.rm = T)
parents$average_parental_wealth_w_equity_2019 = ifelse(is.nan(parents$average_parental_wealth_w_equity_2019), NA, parents$average_parental_wealth_w_equity_2019)

# 2021
parents$average_parental_income_2021 = rowMeans(parents[,c('dad_income_2021', 'mom_income_2021')], na.rm = T)
parents$average_parental_income_2021 = ifelse(is.nan(parents$average_parental_income_2021), NA, parents$average_parental_income_2021)
parents$average_parental_wealth_no_equity_2021 = rowMeans(parents[,c('dad_wealth_no_equity_2021', 'mom_wealth_no_equity_2021')], na.rm = T)
parents$average_parental_wealth_no_equity_2021 = ifelse(is.nan(parents$average_parental_wealth_no_equity_2021), NA, parents$average_parental_wealth_no_equity_2021)
parents$average_parental_wealth_w_equity_2021 = rowMeans(parents[,c('dad_wealth_w_equity_2021', 'mom_wealth_w_equity_2021')], na.rm = T)
parents$average_parental_wealth_w_equity_2021 = ifelse(is.nan(parents$average_parental_wealth_w_equity_2021), NA, parents$average_parental_wealth_w_equity_2021)

parents_sub = parents[,c("child_UID", "dad_UID", "mom_UID", 
                         "average_parental_income_2005", "average_parental_income_2007", "average_parental_income_2009", 
                         "average_parental_income_2011", "average_parental_income_2013", "average_parental_income_2015", 
                         "average_parental_income_2017", "average_parental_income_2019", "average_parental_income_2021",
                         "average_parental_wealth_w_equity_2005", "average_parental_wealth_w_equity_2007", "average_parental_wealth_w_equity_2009", 
                         "average_parental_wealth_w_equity_2011", "average_parental_wealth_w_equity_2013", "average_parental_wealth_w_equity_2015", 
                         "average_parental_wealth_w_equity_2017", "average_parental_wealth_w_equity_2019", "average_parental_wealth_w_equity_2021",
                         "average_parental_wealth_no_equity_2005", "average_parental_wealth_no_equity_2007", "average_parental_wealth_no_equity_2009", 
                         "average_parental_wealth_no_equity_2011", "average_parental_wealth_no_equity_2013", "average_parental_wealth_no_equity_2015", 
                         "average_parental_wealth_no_equity_2017", "average_parental_wealth_no_equity_2019", "average_parental_wealth_no_equity_2021")]

library(reshape2)
parents_m = melt(parents_sub, id.vars = c("child_UID", "dad_UID", "mom_UID"))

parents_m$year = gsub("average_parental_income_", "", parents_m$variable) # remove gpa_standard
parents_m$year = gsub("average_parental_wealth_no_equity_", "", parents_m$year) # remove gpa_standard
parents_m$year = gsub("average_parental_wealth_w_equity_", "", parents_m$year) # remove gpa_standard

parents_m$year = as.numeric(parents_m$year) # convert to numeric

parents_m1 = subset(parents_m, grepl('average_parental_income_', variable))
parents_m2 = subset(parents_m, grepl('average_parental_wealth_no_equity', variable))
parents_m3 = subset(parents_m, grepl('average_parental_wealth_w_equity', variable))

parents_m1 = parents_m1[,c('child_UID', 'value', 'year')]
colnames(parents_m1) = c('UID_2', 'average_parental_income', 'year')

parents_m2 = parents_m2[,c('child_UID', 'value', 'year')]
colnames(parents_m2) = c('UID_2', 'average_parental_wealth_no_equity', 'year')

parents_m3 = parents_m3[,c('child_UID', 'value', 'year')]
colnames(parents_m3) = c('UID_2', 'average_parental_wealth_w_equity', 'year')

parents_m_final = left_join(parents_m1, parents_m2)
parents_m_final = left_join(parents_m_final, parents_m3)

all_gpa_income1 = merge(all, parents_m_final, by = c("UID_2", "year"), all.x = T)

wealth_data = readRDS("./morningstar_wealth_data_long.RDS")
wealth_data$head_married = ifelse(wealth_data$martialStatusR == "Married", 1, 0)
wealth_data$head_divorced = ifelse(wealth_data$martialStatusR == "Divorced/Annulled", 1, 0)
wealth_data$householdStructure_P = ifelse(wealth_data$householdStructure == "Partners", 1, 0)
wealth_data$householdStructure_FA = ifelse(wealth_data$householdStructure == "Female Adult", 1, 0)
wealth_data$householdStructure_MA = ifelse(wealth_data$householdStructure == "Male Adult", 1, 0)
wealth_data$head_ageI = wealth_data$ageI
wealth_data$head_working = wealth_data$IsWorkingR_Simple

vars = c("constantIndividualID", "year", 
         "isMetroH", "inflatedNetWorthWithHomeRecalc_as_2019",
         "inflatedNetWorthWithHome_as_2019", "inflatedPreTaxIncome_as_2019", "inflatedAfterTaxIncome_as_2019",
         "totalIncomeHH", "afterTaxIncomeHH", "PovertyThreshold")

vars_to_mean = c("isMetroH", 
                 "inflatedNetWorthWithHomeRecalc_as_2019",
                 "inflatedNetWorthWithHome_as_2019", 
                 "inflatedPreTaxIncome_as_2019", 
                 "inflatedAfterTaxIncome_as_2019",
                 "totalIncomeHH", 
                 "afterTaxIncomeHH", 
                 "PovertyThreshold")
mom_data = wealth_data[,vars]
dad_data = mom_data

colnames(mom_data)[3:length(mom_data)] = paste0(colnames(mom_data)[3:length(mom_data)], "_mom")
colnames(dad_data)[3:length(dad_data)] = paste0(colnames(dad_data)[3:length(dad_data)], "_dad")

library(dplyr)
dad_joined = left_join(mom_data, dad_data)

for ( i in vars_to_mean ){
  dad_joined[,i] = rowMeans(dad_joined[,c(paste0(i, "_dad"), paste0(i, "_mom"))], na.rm = T)
}

parents_morningstar = dad_joined[,c("constantIndividualID", "year", vars_to_mean)] # delete variable column since we don’t need it anymore
colnames(parents_morningstar)[1] = c("UID_2") # rename value column to standardized gpa

all_gpa_income = left_join(all_gpa_income1, parents_morningstar)

# add in number of people in household and number of children
all$children_in_household_2005 = all$ER25020
all$children_in_household_2007 = all$ER36020
all$children_in_household_2009 = all$ER42020
all$children_in_household_2011 = all$ER47320
all$children_in_household_2013 = all$ER53020
all$children_in_household_2015 = all$ER60021
all$children_in_household_2017 = all$ER66021
all$children_in_household_2019 = all$ER72021
all$children_in_household_2021 = all$ER78021

all$earnings_work = ifelse(all$earnings_work < 0, NA, all$earnings_work)

all$in_college_now = as.numeric(all$in_college_now)

all$race_cat = ifelse(all$race_cat == 6, NA, all$race_cat)
all$race_cat = factor(all$race_cat, levels = c('White', 'Asian', 'Black', "Hispanic", "Other"))

all$marriage_cohabitation_cat = factor(all$marriage_cohabitation_cat, levels = c('not married or cohab', 'married or cohab'))

all$total_parent_help_amount_log = log(all$total_parent_help_amount + 1)
all$earnings_work_log = log(all$earnings_work + 1)
all$average_parental_income_log = ifelse(all$average_parental_income < 0, 0, all$average_parental_income)
all$average_parental_income_log = log(all$average_parental_income_log + 1)

# creating mutually exclusive categories for independence
all$totally_ind = ifelse(all$residentially_ind == 1 & all$financially_ind == 1, 1, 0)
all$F_dep_R_ind = ifelse(all$residentially_ind == 1 & all$financially_ind == 0, 1, 0)
all$F_ind_R_dep = ifelse(all$residentially_ind == 0 & all$financially_ind == 1, 1, 0)
all$totally_dep = ifelse(all$residentially_ind == 0 & all$financially_ind == 0, 1, 0)

all$independence_res_fin = NA
all$independence_res_fin = ifelse(all$financially_ind == 1 & all$residentially_ind == 1, "Totally Independent", all$independence_res_fin)
all$independence_res_fin = ifelse(all$financially_ind == 0 & all$residentially_ind == 0, "Totally Dependent", all$independence_res_fin)
all$independence_res_fin = ifelse(all$financially_ind == 1 & all$residentially_ind == 0, "F ind, R dep", all$independence_res_fin)
all$independence_res_fin = ifelse(all$financially_ind == 0 & all$residentially_ind == 1, "F dep, R ind", all$independence_res_fin)

# MERGE IN DIVORCE OUTCOMES
marital_history_for_head = read.csv('~/marital_history.csv')

marital_history_for_head$psid68_FID = marital_history_for_head$ER30001
marital_history_for_head$psid68_IID = marital_history_for_head$ER30002
marital_history_for_head$psid68_UID = paste0(marital_history_for_head$psid68_FID, "_", marital_history_for_head$psid68_IID)

marital_history_for_head$is_head_2003 = ifelse(marital_history_for_head$ER33703 == 10, 1, 0)
marital_history_for_head$head_marital_2003 = marital_history_for_head$ER21023
marital_history_for_head$head_divorced_2003 = ifelse(marital_history_for_head$head_marital_2003 %in% c(4, 5), 1, 0)

marital_history_for_head$is_head_2005 = ifelse(marital_history_for_head$ER33803 == 10, 1, 0)
marital_history_for_head$head_marital_2005 = marital_history_for_head$ER25023
marital_history_for_head$head_divorced_2005 = ifelse(marital_history_for_head$head_marital_2005 %in% c(4, 5), 1, 0)

marital_history_for_head$is_head_2007 = ifelse(marital_history_for_head$ER33903 == 10, 1, 0)
marital_history_for_head$head_marital_2007 = marital_history_for_head$ER36023
marital_history_for_head$head_divorced_2007 = ifelse(marital_history_for_head$head_marital_2007 %in% c(4, 5), 1, 0)

marital_history_for_head$is_head_2009 = ifelse(marital_history_for_head$ER34003 == 10, 1, 0)
marital_history_for_head$head_marital_2009 = marital_history_for_head$ER42023
marital_history_for_head$head_divorced_2009 = ifelse(marital_history_for_head$head_marital_2009 %in% c(4, 5), 1, 0)

marital_history_for_head$is_head_2011 = ifelse(marital_history_for_head$ER34103 == 10, 1, 0)
marital_history_for_head$head_marital_2011 = marital_history_for_head$ER47323
marital_history_for_head$head_divorced_2011 = ifelse(marital_history_for_head$head_marital_2011 %in% c(4, 5), 1, 0)

marital_history_for_head$is_head_2013 = ifelse(marital_history_for_head$ER34203 == 10, 1, 0)
marital_history_for_head$head_marital_2013 = marital_history_for_head$ER53023
marital_history_for_head$head_divorced_2013 = ifelse(marital_history_for_head$head_marital_2013 %in% c(4, 5), 1, 0)

marital_history_for_head$is_head_2015 = ifelse(marital_history_for_head$ER34303 == 10, 1, 0)
marital_history_for_head$head_marital_2015 = marital_history_for_head$ER60024
marital_history_for_head$head_divorced_2015 = ifelse(marital_history_for_head$head_marital_2015 %in% c(4, 5), 1, 0)

marital_history_for_head$is_head_2017 = ifelse(marital_history_for_head$ER34503 == 10, 1, 0)
marital_history_for_head$head_marital_2017 = marital_history_for_head$ER66024
marital_history_for_head$head_divorced_2017 = ifelse(marital_history_for_head$head_marital_2017 %in% c(4, 5), 1, 0)

marital_history_for_head$is_head_2019 = ifelse(marital_history_for_head$ER34703 == 10, 1, 0)
marital_history_for_head$head_marital_2019 = marital_history_for_head$ER72024
marital_history_for_head$head_divorced_2019 = ifelse(marital_history_for_head$head_marital_2019 %in% c(4, 5), 1, 0)

marital_history_for_head$is_head_2021 = ifelse(marital_history_for_head$ER34903 == 10, 1, 0)
marital_history_for_head$head_marital_2021 = marital_history_for_head$ER78025
marital_history_for_head$head_divorced_2021 = ifelse(marital_history_for_head$head_marital_2021 %in% c(4, 5), 1, 0)

to_melt_2003 = marital_history_for_head[,c("psid68_UID", "is_head_2003", "head_marital_2003", "head_divorced_2003")]
colnames(to_melt_2003) = c("psid68_UID", "is_head", "head_marital", "head_divorced")
to_melt_2003$year = 2003
to_melt_2003 = subset(to_melt_2003, is_head == 1)

to_melt_2005 = marital_history_for_head[,c("psid68_UID", "is_head_2005", "head_marital_2005", "head_divorced_2005")]
colnames(to_melt_2005) = c("psid68_UID", "is_head", "head_marital", "head_divorced")
to_melt_2005$year = 2005
to_melt_2005 = subset(to_melt_2005, is_head == 1)

to_melt_2007 = marital_history_for_head[,c("psid68_UID", "is_head_2007", "head_marital_2007", "head_divorced_2007")]
colnames(to_melt_2007) = c("psid68_UID", "is_head", "head_marital", "head_divorced")
to_melt_2007$year = 2007
to_melt_2007 = subset(to_melt_2007, is_head == 1)

to_melt_2009 = marital_history_for_head[,c("psid68_UID", "is_head_2009", "head_marital_2009", "head_divorced_2009")]
colnames(to_melt_2009) = c("psid68_UID", "is_head", "head_marital", "head_divorced")
to_melt_2009$year = 2009
to_melt_2009 = subset(to_melt_2009, is_head == 1)

to_melt_2011 = marital_history_for_head[,c("psid68_UID", "is_head_2011", "head_marital_2011", "head_divorced_2011")]
colnames(to_melt_2011) = c("psid68_UID", "is_head", "head_marital", "head_divorced")
to_melt_2011$year = 2011
to_melt_2011 = subset(to_melt_2011, is_head == 1)

to_melt_2013 = marital_history_for_head[,c("psid68_UID", "is_head_2013", "head_marital_2013", "head_divorced_2013")]
colnames(to_melt_2013) = c("psid68_UID", "is_head", "head_marital", "head_divorced")
to_melt_2013$year = 2013
to_melt_2013 = subset(to_melt_2013, is_head == 1)

to_melt_2015 = marital_history_for_head[,c("psid68_UID", "is_head_2015", "head_marital_2015", "head_divorced_2015")]
colnames(to_melt_2015) = c("psid68_UID", "is_head", "head_marital", "head_divorced")
to_melt_2015$year = 2015
to_melt_2015 = subset(to_melt_2015, is_head == 1)

to_melt_2017 = marital_history_for_head[,c("psid68_UID", "is_head_2017", "head_marital_2017", "head_divorced_2017")]
colnames(to_melt_2017) = c("psid68_UID", "is_head", "head_marital", "head_divorced")
to_melt_2017$year = 2017
to_melt_2017 = subset(to_melt_2017, is_head == 1)

to_melt_2019 = marital_history_for_head[,c("psid68_UID", "is_head_2019", "head_marital_2019", "head_divorced_2019")]
colnames(to_melt_2019) = c("psid68_UID", "is_head", "head_marital", "head_divorced")
to_melt_2019$year = 2019
to_melt_2019 = subset(to_melt_2019, is_head == 1)

to_melt_2021 = marital_history_for_head[,c("psid68_UID", "is_head_2021", "head_marital_2021", "head_divorced_2021")]
colnames(to_melt_2021) = c("psid68_UID", "is_head", "head_marital", "head_divorced")
to_melt_2021$year = 2021
to_melt_2021 = subset(to_melt_2021, is_head == 1)

parents$dad_divorced_2003 = to_melt_2003$head_divorced[match(parents$dad_UID, to_melt_2003$psid68_UID)]
parents$dad_divorced_2005 = to_melt_2005$head_divorced[match(parents$dad_UID, to_melt_2005$psid68_UID)]
parents$dad_divorced_2007 = to_melt_2007$head_divorced[match(parents$dad_UID, to_melt_2007$psid68_UID)]
parents$dad_divorced_2009 = to_melt_2009$head_divorced[match(parents$dad_UID, to_melt_2009$psid68_UID)]
parents$dad_divorced_2011 = to_melt_2011$head_divorced[match(parents$dad_UID, to_melt_2011$psid68_UID)]
parents$dad_divorced_2013 = to_melt_2013$head_divorced[match(parents$dad_UID, to_melt_2013$psid68_UID)]
parents$dad_divorced_2015 = to_melt_2015$head_divorced[match(parents$dad_UID, to_melt_2015$psid68_UID)]
parents$dad_divorced_2017 = to_melt_2017$head_divorced[match(parents$dad_UID, to_melt_2017$psid68_UID)]
parents$dad_divorced_2019 = to_melt_2019$head_divorced[match(parents$dad_UID, to_melt_2019$psid68_UID)]
parents$dad_divorced_2021 = to_melt_2021$head_divorced[match(parents$dad_UID, to_melt_2021$psid68_UID)]

parents$mom_divorced_2003 = to_melt_2003$head_divorced[match(parents$mom_UID, to_melt_2003$psid68_UID)]
parents$mom_divorced_2005 = to_melt_2005$head_divorced[match(parents$mom_UID, to_melt_2005$psid68_UID)]
parents$mom_divorced_2007 = to_melt_2007$head_divorced[match(parents$mom_UID, to_melt_2007$psid68_UID)]
parents$mom_divorced_2009 = to_melt_2009$head_divorced[match(parents$mom_UID, to_melt_2009$psid68_UID)]
parents$mom_divorced_2011 = to_melt_2011$head_divorced[match(parents$mom_UID, to_melt_2011$psid68_UID)]
parents$mom_divorced_2013 = to_melt_2013$head_divorced[match(parents$mom_UID, to_melt_2013$psid68_UID)]
parents$mom_divorced_2015 = to_melt_2015$head_divorced[match(parents$mom_UID, to_melt_2015$psid68_UID)]
parents$mom_divorced_2017 = to_melt_2017$head_divorced[match(parents$mom_UID, to_melt_2017$psid68_UID)]
parents$mom_divorced_2019 = to_melt_2019$head_divorced[match(parents$mom_UID, to_melt_2019$psid68_UID)]
parents$mom_divorced_2021 = to_melt_2021$head_divorced[match(parents$mom_UID, to_melt_2021$psid68_UID)]

parents$divorced_2003 = ifelse(parents$mom_divorced_2003 %in% 1 | parents$mom_divorced_2003 %in% 1, 1, 0)
parents$divorced_2003 = ifelse(is.na(parents$mom_divorced_2003) & is.na(parents$dad_divorced_2003), NA, parents$divorced_2003)

parents$divorced_2005 = ifelse(parents$mom_divorced_2005 %in% 1 | parents$dad_divorced_2005 %in% 1, 1, 0)
parents$divorced_2005 = ifelse(is.na(parents$mom_divorced_2005) & is.na(parents$dad_divorced_2005), NA, parents$divorced_2005)

parents$divorced_2007 = ifelse(parents$mom_divorced_2007 %in% 1 | parents$dad_divorced_2007 %in% 1, 1, 0)
parents$divorced_2007 = ifelse(is.na(parents$mom_divorced_2007) & is.na(parents$dad_divorced_2007), NA, parents$divorced_2007)

parents$divorced_2009 = ifelse(parents$mom_divorced_2009 %in% 1 | parents$dad_divorced_2009 %in% 1, 1, 0)
parents$divorced_2009 = ifelse(is.na(parents$mom_divorced_2009) & is.na(parents$dad_divorced_2009), NA, parents$divorced_2009)

parents$divorced_2011 = ifelse(parents$mom_divorced_2011 %in% 1 | parents$dad_divorced_2011 %in% 1, 1, 0)
parents$divorced_2011 = ifelse(is.na(parents$mom_divorced_2011) & is.na(parents$dad_divorced_2011), NA, parents$divorced_2011)

parents$divorced_2013 = ifelse(parents$mom_divorced_2013 %in% 1 | parents$dad_divorced_2013 %in% 1, 1, 0)
parents$divorced_2013 = ifelse(is.na(parents$mom_divorced_2013) & is.na(parents$dad_divorced_2013), NA, parents$divorced_2013)

parents$divorced_2015 = ifelse(parents$mom_divorced_2015 %in% 1 | parents$dad_divorced_2015 %in% 1, 1, 0)
parents$divorced_2015 = ifelse(is.na(parents$mom_divorced_2015) & is.na(parents$dad_divorced_2015), NA, parents$divorced_2015)

parents$divorced_2017 = ifelse(parents$mom_divorced_2017 %in% 1 | parents$dad_divorced_2017 %in% 1, 1, 0)
parents$divorced_2017 = ifelse(is.na(parents$mom_divorced_2017) & is.na(parents$dad_divorced_2017), NA, parents$divorced_2017)

parents$divorced_2019 = ifelse(parents$mom_divorced_2019 %in% 1 | parents$dad_divorced_2019 %in% 1, 1, 0)
parents$divorced_2019 = ifelse(is.na(parents$mom_divorced_2019) & is.na(parents$dad_divorced_2019), NA, parents$divorced_2019)

parents$divorced_2021 = ifelse(parents$mom_divorced_2021 %in% 1 | parents$mom_divorced_2021 %in% 1, 1, 0)
parents$divorced_2021 = ifelse(is.na(parents$mom_divorced_2021) & is.na(parents$dad_divorced_2021), NA, parents$divorced_2021)

parents_sub = parents[,c("child_UID", "divorced_2003", "divorced_2005", "divorced_2007", "divorced_2009",
                         "divorced_2011", "divorced_2013", "divorced_2015", "divorced_2017", "divorced_2019", "divorced_2021")]

library(reshape2)
parents_m = melt(parents_sub, id.vars = c("child_UID"))
parents_m$variable = gsub("divorced_", "", parents_m$variable)
parents_m$year = as.numeric(parents_m$variable)
parents_m = parents_m[,c("child_UID", "year", "value")]
colnames(parents_m)[3] = "currently_divorced_from_ind"

all = left_join(all, parents_m, by = c("psid68_UID" = "child_UID", "year" = "year"))
all$currently_divorced = ifelse(is.na(all$currently_divorced), all$currently_divorced_from_ind, all$currently_divorced)
all = all[,!is.na(colnames(all))]

first_year <- all %>%
  group_by(UID_2) %>%
  summarise(join_year = min(year)) %>%
  ungroup()

all$join_year = first_year$join_year[match(all$UID_2, first_year$UID_2)]

saveRDS(all, "./psid_long_with_ta_and_ind_v8.RDS")

# Add in child development module CDS 
library(readxl)
cds = read_excel("./J334230.xlsx")
cds$UID = paste(cds$ER30001, cds$ER30002, sep = "_")

sum(cds$UID %in% all$UID_2)
sum(all$UID_2 %in% cds$UID)

cds$disobedient_2002 = ifelse(cds$Q21B29J > 3, NA, cds$Q21B29J)
cds$helped_sib_2002 = ifelse(cds$Q21B37A > 5, NA, cds$Q21B37A)
cds$kind_sib_2002 = ifelse(cds$Q21B37B > 5, NA, cds$Q21B37B)
cds$cooporate_sib_2002 = ifelse(cds$Q21B37C > 5, NA, cds$Q21B37C)
cds$child_bothers_me_2002 = ifelse(cds$Q21E8A > 5, NA, cds$Q21E8A)
cds$ever_spanked_2002 = ifelse(cds$Q21E10 > 5, NA, cds$Q21E10)
cds$ever_spanked_2002 = ifelse(cds$ever_spanked_2002 == 5, 0, cds$ever_spanked_2002)
cds$age_first_spanked_2002 = ifelse(cds$Q21E11A1 > 96, NA, cds$Q21E11A1)
cds$age_first_spanked_2002 = ifelse(cds$age_first_spanked_2002 == 0, NA, cds$age_first_spanked_2002)  
cds$freq_said_love_you_2002 = ifelse(cds$Q21E13A > 5, NA, cds$Q21E13A)
cds$activities_child_likes_2002 = ifelse(cds$Q21E13B > 5, NA, cds$Q21E13B)

cds$in_tutoring_2002 = ifelse(cds$Q21G5 == 1 & cds$Q21G5A == 1, 1, 0)
cds$in_tutoring_2002 = ifelse(cds$Q21H5 == 1 & cds$Q21H5A == 1, 1, cds$in_tutoring_2002)
cds$in_tutoring_2002 = ifelse(cds$Q23K2 == 1, 1, cds$in_tutoring_2002)

cds$cost_tutoring_2002 = ifelse(cds$Q21H5C > 9997, NA, cds$Q21H5C)
cds$cost_tutoring_2002 = ifelse(cds$cost_tutoring_2002 == 0, NA, cds$cost_tutoring_2002)

cds$independence_girls_boys_2002 = ifelse(cds$Q26B16F > 4, NA, cds$Q26B16F)

# questions asked from children
cds$kid_feels_close_mom_2002 = ifelse(cds$Q23H5A > 4, NA, cds$Q23H5A)
cds$kid_feels_close_dad_2002 = ifelse(cds$Q23H5B > 4, NA, cds$Q23H5B)
cds$kid_feels_close_stepmom_2002 = ifelse(cds$Q23H5D > 4, NA, cds$Q23H5D)
cds$kid_feels_close_stepdad_2002 = ifelse(cds$Q23H5C > 4, NA, cds$Q23H5C)
cds$kid_feels_close_sibling_2002 = ifelse(cds$Q23H5F > 4, NA, cds$Q23H5F)
cds$kid_feels_close_sibling_2002 = ifelse(cds$kid_feels_close_sibling_2002 == 0, NA, cds$kid_feels_close_sibling_2002)

cds$kid_income_by_30_2002 = ifelse(cds$Q23J34E > 5, NA, cds$Q23J34E)
cds$kid_income_by_30_2002 = ifelse(cds$kid_income_by_30_2002  == 0 , NA, cds$kid_income_by_30_2002)

cds$freq_did_homework_2002 = ifelse(cds$Q23K1 > 5, NA, cds$Q23K1)
cds$freq_did_homework_2002 = ifelse(cds$freq_did_homework_2002  == 0 , NA, cds$freq_did_homework_2002)
cds$hours_on_homework_2002 = ifelse(cds$Q23K1A > 97, NA, cds$Q23K1A) # could also be did no homework, but also kids under 10..

cds$friends_encourage_disobedience_2002 = ifelse(cds$Q23K25A > 5, NA, cds$Q23K25A)
cds$friends_encourage_disobedience_2002 = ifelse(cds$friends_encourage_disobedience_2002  == 0 , NA, cds$friends_encourage_disobedience_2002 )
cds$friends_encourage_obedience_2002 = ifelse(cds$Q23K25D > 5, NA, cds$Q23K25D)
cds$friends_encourage_obedience_2002 = ifelse(cds$friends_encourage_obedience_2002  == 0 , NA, cds$friends_encourage_obedience_2002 )

cds$kid_doing_best_2002 = ifelse(cds$Q23K26D > 5, NA, cds$Q23K26D)
cds$kid_doing_best_2002 = ifelse(cds$kid_doing_best_2002 == 0 , NA, cds$kid_doing_best_2002)
# not changing 0 in arrested because it could indicate 'never'
cds$kid_arrested_2002 = ifelse(cds$Q23L11J > 97, NA, cds$Q23L11J)

cds$my_mom_enjoys_time_w_me_2002 = ifelse(cds$Q23L17C > 3, NA, cds$Q23L17C)
cds$my_mom_enjoys_time_w_me_2002 = ifelse(cds$my_mom_enjoys_time_w_me_2002 == 0, NA, cds$my_mom_enjoys_time_w_me_2002 )
cds$my_mom_criticizes_me_2002 = ifelse(cds$Q23L17D > 3, NA, cds$Q23L17D) 
cds$my_mom_criticizes_me_2002 = ifelse(cds$my_mom_criticizes_me_2002 == 0, NA, cds$my_mom_criticizes_me_2002)
cds$my_dad_enjoys_time_w_me_2002 = ifelse(cds$Q23L18C > 3, NA, cds$Q23L18C)
cds$my_dad_enjoys_time_w_me_2002 = ifelse(cds$my_dad_enjoys_time_w_me_2002 == 0, NA, cds$my_dad_enjoys_time_w_me_2002)
cds$my_dad_criticizes_me_2002 = ifelse(cds$Q23L18D > 3, NA, cds$Q23L18D) 
cds$my_dad_criticizes_me_2002 = ifelse(cds$my_dad_criticizes_me_2002 == 0, NA, cds$my_dad_criticizes_me_2002)

cds$kid_hides_things_parents_2002 = ifelse(cds$Q23L27 > 5, NA, cds$Q23L27)
cds$kid_hides_things_parents_2002 = ifelse(cds$kid_hides_things_parents_2002 == 0, NA, cds$kid_hides_things_parents_2002)
cds$kid_tells_parents_2002 = ifelse(cds$Q23L28 > 5, NA, cds$Q23L28) 
cds$kid_tells_parents_2002 = ifelse(cds$kid_tells_parents_2002 == 0, NA, cds$kid_tells_parents_2002)

######## 2007 ########

cds$disobedient_2007 = ifelse(cds$Q31B29J > 3, NA, cds$Q31B29J) 
cds$helped_sib_2007 = ifelse(cds$Q31B37A > 5, NA, cds$Q31B37A)  
cds$kind_sib_2007 = ifelse(cds$Q21B37B > 5, NA, cds$Q21B37B)  

cds$cooporate_sib_2007 = ifelse(cds$Q21B37C > 5, NA, cds$Q21B37C)

cds$child_bothers_me_2007 = ifelse(cds$Q31E8A > 5, NA, cds$Q31E8A)  

cds$ever_spanked_2007 = ifelse(cds$Q31E10 > 5, NA, cds$Q31E10)   
cds$ever_spanked_2007 = ifelse(cds$ever_spanked_2007 == 5, 0, cds$ever_spanked_2007)  

cds$age_first_spanked_2007 = ifelse(cds$Q31E11A1 > 18, NA, cds$Q31E11A1)  
cds$age_first_spanked_2007 = ifelse(cds$age_first_spanked_2007 == 0, NA, cds$age_first_spanked_2007)

cds$freq_said_love_you_2007 = ifelse(cds$Q31E13A > 5, NA, cds$Q31E13A)  

cds$activities_child_likes_2007 = ifelse(cds$Q31E13B > 5, NA, cds$Q31E13B)  

cds$in_tutoring_2007 = ifelse(cds$Q33K2 == 1 | cds$Q21G5A == 1, 1, 0)  

cds$in_tutoring_2007 = ifelse(cds$Q21H5 == 1 & cds$Q21H5A == 1, 1, cds$in_tutoring_2007)

cds$cost_tutoring_2007 = ifelse(cds$Q31H5C > 9997, NA, cds$Q21H5C)   #but only for 10+ 
cds$cost_tutoring_2007 = ifelse(cds$cost_tutoring_2007  == 0, NA, cds$cost_tutoring_2007)

cds$independence_girls_boys_2007 = ifelse(cds$Q36B16F > 4, NA, cds$Q36B16F)

## questions asked from children 2007

cds$kid_feels_close_mom_2007 = ifelse(cds$Q33H5A > 4, NA, cds$Q33H5A)  
cds$kid_feels_close_dad_2007 = ifelse(cds$Q33H5B > 4, NA, cds$Q33H5B)  
cds$kid_feels_close_stepmom_2007 = ifelse(cds$Q33H5D > 4, NA, cds$Q33H5D)  
cds$kid_feels_close_stepdad_2007 = ifelse(cds$Q33H5C > 4, NA, cds$Q33H5C)  
cds$kid_feels_close_sibling_2007 = ifelse(cds$Q33H5F > 4, NA, cds$Q33H5F)
cds$kid_feels_close_sibling_2007 = ifelse(cds$kid_feels_close_sibling_2007 == 0, NA, cds$kid_feels_close_sibling_2007)  

cds$kid_income_by_30_2007 = ifelse(cds$Q33J34E > 5, NA, cds$Q33J34E)   
cds$kid_income_by_30_2007 = ifelse(cds$kid_income_by_30_2007  == 0 , NA, cds$kid_income_by_30_2007)  

cds$freq_did_homework_2007 = ifelse(cds$Q33K1 > 5, NA, cds$Q33K1)  
cds$freq_did_homework_2007 = ifelse(cds$freq_did_homework_2007  == 0 , NA, cds$freq_did_homework_2007)
cds$hours_on_homework_2007 = ifelse(cds$Q33K1A > 97, NA, cds$Q33K1A)   #could also be did no homework, but also kids under 10..

cds$friends_encourage_disobedience_2007 = ifelse(cds$Q33K25A > 5, NA, cds$Q33K25A)  
cds$friends_encourage_disobedience_2007 = ifelse(cds$friends_encourage_disobedience_2007  == 0 , NA, cds$friends_encourage_disobedience_2007 )
cds$friends_encourage_obedience_2007 = ifelse(cds$Q33K25D > 5, NA, cds$Q33K25D)  
cds$friends_encourage_obedience_2007 = ifelse(cds$friends_encourage_obedience_2007  == 0 , NA, cds$friends_encourage_obedience_2007 )

cds$kid_doing_best_2007 = ifelse(cds$Q33K26D > 5, NA, cds$Q33K26D)  
cds$kid_doing_best_2007 = ifelse(cds$kid_doing_best_2007 == 0 , NA, cds$kid_doing_best_2007)

cds$kid_arrested_2007 = ifelse(cds$Q33L11J > 97, NA, cds$Q33L11J)  # not changing 0 in arrested because it could indicate 'never'

cds$my_mom_enjoys_time_w_me_2007 = ifelse(cds$Q33L17C > 3, NA, cds$Q33L17C)  
cds$my_mom_enjoys_time_w_me_2007 = ifelse(cds$my_mom_enjoys_time_w_me_2007 == 0, NA, cds$my_mom_enjoys_time_w_me_2007 )
cds$my_mom_criticizes_me_2007 = ifelse(cds$Q33L17D > 3, NA, cds$Q33L17D)  
cds$my_mom_criticizes_me_2007 = ifelse(cds$my_mom_criticizes_me_2007 == 0, NA, cds$my_mom_criticizes_me_2007)
cds$my_dad_enjoys_time_w_me_2007 = ifelse(cds$Q33L18C > 3, NA, cds$Q33L18C)
cds$my_dad_enjoys_time_w_me_2007 = ifelse(cds$my_dad_enjoys_time_w_me_2007 == 0, NA, cds$my_dad_enjoys_time_w_me_2007)
cds$my_dad_criticizes_me_2007 = ifelse(cds$Q33L18D > 3, NA, cds$Q33L18D) 
cds$my_dad_criticizes_me_2007 = ifelse(cds$my_dad_criticizes_me_2007 == 0, NA, cds$my_dad_criticizes_me_2007)

cds$kid_hides_things_parents_2007 = ifelse(cds$Q33L27 > 5, NA, cds$Q33L27)
cds$kid_hides_things_parents_2007 = ifelse(cds$kid_hides_things_parents_2007 == 0, NA, cds$kid_hides_things_parents_2007)
cds$kid_tells_parents_2007 = ifelse(cds$Q33L28 > 5, NA, cds$Q33L28) 
cds$kid_tells_parents_2007 = ifelse(cds$kid_tells_parents_2007 == 0, NA, cds$kid_tells_parents_2007)

######## 2014 #######

cds$disobedient_2014 = ifelse(cds$P14B10 > 3, NA, cds$P14B10) 
cds$helped_sib_2014 = ifelse(cds$P14B48 > 5, NA, cds$P14B48)  

cds$ever_spanked_2014 = ifelse(cds$P14C14 > 5, NA, cds$P14C14)   
cds$ever_spanked_2014 = ifelse(cds$ever_spanked_2014 == 5, 0, cds$ever_spanked_2014)  

cds$age_first_spanked_2014 = ifelse(cds$P14C15A > 18, NA, cds$P14C15A)  
cds$age_first_spanked_2014 = ifelse(cds$age_first_spanked_2014 == 0, NA, cds$age_first_spanked_2014)  
cds$age_spanked_weeks_years_2014 = ifelse(cds$P14C15B > 3, NA, cds$P14C15B)

cds$in_tutoring_2014 = ifelse(cds$P14E9 == 1 , 1, 0)  

cds$cost_tutoring_2014 = ifelse(cds$P14E13 > 9997, NA, cds$P14E13)   # zero may be that tutoring was free...
cds$cost_tutoring_2014 = ifelse(cds$cost_tutoring_2014  == 0, NA, cds$cost_tutoring_2014)

## questions asked from children 2014

cds$kid_feels_close_mom_2014 = ifelse(cds$C14D1A > 4, NA, cds$C14D1A)  
cds$kid_feels_close_dad_2014 = ifelse(cds$C14D1B > 4, NA, cds$C14D1B)  
cds$kid_feels_close_stepmom_2014 = ifelse(cds$Q33H5D > 4, NA, cds$Q33H5D)  
cds$kid_feels_close_stepdad_2014 = ifelse(cds$C14D1D > 4, NA, cds$C14D1D)  
cds$kid_feels_close_sibling_2014 = ifelse(cds$C14D1F > 4, NA, cds$C14D1F)
cds$kid_feels_close_sibling_2014 = ifelse(cds$kid_feels_close_sibling_2014 == 0, NA, cds$kid_feels_close_sibling_2014)  

cds$kid_doing_best_2014 = ifelse(cds$C14E9 > 5, NA, cds$C14E9)  
cds$kid_doing_best_2014 = ifelse(cds$kid_doing_best_2014 == 0 , NA, cds$kid_doing_best_2014) 
cds$kid_arrested_2014 = ifelse(cds$C14J29 > 97, NA, cds$C14J29)  # not changing 0 in arrested because it could indicate 'never'


## Match in earliest CDS for these children
all$disobedient = NA
all$disobedient = cds$disobedient_2014[match(all$UID_2, cds$UID)]
all$disobedient = ifelse(is.na(all$disobedient), cds$disobedient_2007[match(all$UID_2, cds$UID)], all$disobedient)
all$disobedient = ifelse(is.na(all$disobedient), cds$disobedient_2002[match(all$UID_2, cds$UID)], all$disobedient)

all$helped_sib = NA
all$helped_sib = cds$helped_sib_2014[match(all$UID_2, cds$UID)]
all$helped_sib = ifelse(is.na(all$helped_sib), cds$helped_sib_2007[match(all$UID_2, cds$UID)], all$helped_sib)
all$helped_sib = ifelse(is.na(all$helped_sib), cds$helped_sib_2002[match(all$UID_2, cds$UID)], all$helped_sib)

all$ever_spanked = NA
all$ever_spanked = cds$ever_spanked_2014[match(all$UID_2, cds$UID)]
all$ever_spanked = ifelse(is.na(all$ever_spanked), cds$ever_spanked_2007[match(all$UID_2, cds$UID)], all$ever_spanked)
all$ever_spanked = ifelse(is.na(all$ever_spanked), cds$ever_spanked_2002[match(all$UID_2, cds$UID)], all$ever_spanked)

all$in_tutoring = NA
all$in_tutoring = cds$in_tutoring_2014[match(all$UID_2, cds$UID)]
all$in_tutoring = ifelse(is.na(all$in_tutoring), cds$in_tutoring_2007[match(all$UID_2, cds$UID)], all$in_tutoring)
all$in_tutoring = ifelse(is.na(all$in_tutoring), cds$in_tutoring_2002[match(all$UID_2, cds$UID)], all$in_tutoring)

all$cost_tutoring = NA
all$cost_tutoring = cds$cost_tutoring_2014[match(all$UID_2, cds$UID)]
all$cost_tutoring = ifelse(is.na(all$cost_tutoring), cds$cost_tutoring_2007[match(all$UID_2, cds$UID)], all$cost_tutoring)
all$cost_tutoring = ifelse(is.na(all$cost_tutoring), cds$cost_tutoring_2002[match(all$UID_2, cds$UID)], all$cost_tutoring)

all$kid_feels_close_mom = NA
all$kid_feels_close_mom = cds$kid_feels_close_mom_2014[match(all$UID_2, cds$UID)]
all$kid_feels_close_mom = ifelse(is.na(all$kid_feels_close_mom), cds$kid_feels_close_mom_2007[match(all$UID_2, cds$UID)], all$kid_feels_close_mom)
all$kid_feels_close_mom = ifelse(is.na(all$kid_feels_close_mom), cds$kid_feels_close_mom_2002[match(all$UID_2, cds$UID)], all$kid_feels_close_mom)

all$kid_feels_close_dad = NA
all$kid_feels_close_dad = cds$kid_feels_close_dad_2014[match(all$UID_2, cds$UID)]
all$kid_feels_close_dad = ifelse(is.na(all$kid_feels_close_dad), cds$kid_feels_close_dad_2007[match(all$UID_2, cds$UID)], all$kid_feels_close_dad)
all$kid_feels_close_dad = ifelse(is.na(all$kid_feels_close_dad), cds$kid_feels_close_dad_2002[match(all$UID_2, cds$UID)], all$kid_feels_close_dad)

all$kid_feels_close_stepmom = NA
all$kid_feels_close_stepmom = cds$kid_feels_close_stepmom_2014[match(all$UID_2, cds$UID)]
all$kid_feels_close_stepmom = ifelse(is.na(all$kid_feels_close_stepmom), cds$kid_feels_close_stepmom_2007[match(all$UID_2, cds$UID)], all$kid_feels_close_stepmom)
all$kid_feels_close_stepmom = ifelse(is.na(all$kid_feels_close_stepmom), cds$kid_feels_close_stepmom_2002[match(all$UID_2, cds$UID)], all$kid_feels_close_stepmom)

all$kid_feels_close_stepdad = NA
all$kid_feels_close_stepdad = cds$kid_feels_close_stepdad_2014[match(all$UID_2, cds$UID)]
all$kid_feels_close_stepdad = ifelse(is.na(all$kid_feels_close_stepdad), cds$kid_feels_close_stepdad_2007[match(all$UID_2, cds$UID)], all$kid_feels_close_stepdad)
all$kid_feels_close_stepdad = ifelse(is.na(all$kid_feels_close_stepdad), cds$kid_feels_close_stepdad_2002[match(all$UID_2, cds$UID)], all$kid_feels_close_stepdad)

all$kid_feels_close_sibling = NA
all$kid_feels_close_sibling = cds$kid_feels_close_sibling_2014[match(all$UID_2, cds$UID)]
all$kid_feels_close_sibling = ifelse(is.na(all$kid_feels_close_sibling), cds$kid_feels_close_sibling_2007[match(all$UID_2, cds$UID)], all$kid_feels_close_sibling)
all$kid_feels_close_sibling = ifelse(is.na(all$kid_feels_close_sibling), cds$kid_feels_close_sibling_2002[match(all$UID_2, cds$UID)], all$kid_feels_close_sibling)

all$kid_doing_best = NA
all$kid_doing_best = cds$kid_doing_best_2014[match(all$UID_2, cds$UID)]
all$kid_doing_best = ifelse(is.na(all$kid_doing_best), cds$kid_doing_best_2007[match(all$UID_2, cds$UID)], all$kid_doing_best)
all$kid_doing_best = ifelse(is.na(all$kid_doing_best), cds$kid_doing_best_2002[match(all$UID_2, cds$UID)], all$kid_doing_best)

all$kid_arrested = NA
all$kid_arrested = cds$kid_arrested_2014[match(all$UID_2, cds$UID)]
all$kid_arrested = ifelse(is.na(all$kid_arrested), cds$kid_arrested_2007[match(all$UID_2, cds$UID)], all$kid_arrested)
all$kid_arrested = ifelse(is.na(all$kid_arrested), cds$kid_arrested_2002[match(all$UID_2, cds$UID)], all$kid_arrested)

# Fix mother's edu for 2013
parents$mother_edu = individual_data$ER34230[match(parents$mom_UID, individual_data$UID)]
parents$mother_edu = ifelse(parents$mother_edu == 99, NA, parents$mother_edu)
all$mother_edu[all$year == 2013] = as.numeric(parents$mother_edu[match(all$UID_2[all$year == 2013], parents$child_UID)])

num_children_data = read_excel("./number_children/J335270.xlsx")
num_children_data$UID_2 = paste0(num_children_data$ER30001, "_", num_children_data$ER30002)

all$number_children_mother = num_children_data$ER32012[match(all$UID_2, num_children_data$UID_2)]
all$number_children_mother = ifelse(all$number_children_mother > 20, NA, all$number_children_mother)

all$number_children_father = num_children_data$ER32019[match(all$UID_2, num_children_data$UID_2)]
all$number_children_father = ifelse(all$number_children_father > 20, NA, all$number_children_mother)

all$number_children_parents_avg = (all$number_children_mother + all$number_children_father)/2

# Metro
all$metro_2005 = ifelse(all$ER28043A == 0, NA, all$ER28043A)
all$metro_2005 = ifelse(all$metro_2005 < 4, 1, 0)

all$metro_2007 = ifelse(all$ER41033A == 0, NA, all$ER41033A)
all$metro_2007 = ifelse(all$metro_2007 < 4, 1, 0)

all$metro_2009 = ifelse(all$ER46975A == 0, NA, all$ER46975A)
all$metro_2009 = ifelse(all$metro_2009 < 4, 1, 0)

all$metro_2011 = ifelse(all$ER52399A == 0, NA, all$ER52399A)
all$metro_2011 = ifelse(all$metro_2011 < 4, 1, 0)

all$metro_2013 = ifelse(all$ER58216 == 0, NA, all$ER58216)
all$metro_2013 = ifelse(all$metro_2013 < 4, 1, 0)

all$metro_2015 = ifelse(all$ER65452 == 0 | all$ER77592 == 9, NA, all$ER65452)
all$metro_2015 = ifelse(all$metro_2015 == 1, 1, 0)

all$metro_2017 = ifelse(all$ER71531 == 0 | all$ER71531 == 9, NA, all$ER71531)
all$metro_2017 = ifelse(all$metro_2017 == 1, 1, 0)

all$metro_2019 = ifelse(all$ER77592 == 0 | all$ER77592 == 9, NA, all$ER77592)
all$metro_2019 = ifelse(all$metro_2019 == 1, 1, 0)

all$metro_2021 = ifelse(all$ER81919 == 0 | all$ER81919 == 9, NA, all$ER81919)
all$metro_2021 = ifelse(all$metro_2021 == 1, 1, 0)

saveRDS(all, "./psid_long_with_ta_and_ind_v8_w_cds.RDS")

# version for sibling analyses
library(readxl)
sib = read_excel(".//sibling data/fims/fim13980_sib_0.xlsx")
all = readRDS("./psid_long_with_ta_and_ind_v8_w_cds.RDS")

sib$UID_2 = paste(sib$ID68, sib$PN, sep = "_")
sib$UID_2_S = paste(sib$ID68S, sib$PNS, sep = "_")
sib$has_sib = 1

sib_full = subset(sib, SIBTYPE == 1)

library(igraph)
sib_el = sib_full[,c('UID_2', 'UID_2_S')]
sib_net = graph.edgelist(as.matrix(sib_el), directed = F)
sib_groups = cluster_louvain(sib_net)

sib_group_df = data.frame(UID_2 = V(sib_net)$name, sib_group = membership(sib_groups))

sib_full = sib_full[,c('ID68', 'UID_2', 'SIBNUM')]
sib_full = unique(sib_full)

sib_full_max = sib_full %>% 
  group_by(ID68, UID_2) %>%
  filter(SIBNUM == max(SIBNUM)) %>%
  arrange(ID68, UID_2, SIBNUM)

sib_full_max$has_sib = 1
sib_full_max$sib_group = sib_group_df$sib_group[match(sib_full_max$UID_2, sib_group_df$UID_2)]

sibs_data = merge(all, sib_full_max, by = "UID_2", all.x = T)

sibs_data$has_sib = ifelse(is.na(sibs_data$has_sib), 0, sibs_data$has_sib)

saveRDS(sibs_data, "./psid_long_with_ta_and_ind_v9.RDS")

