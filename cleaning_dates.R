## loading packages ####
library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)



## combining all the extracted data ####
new_notices_blackouts = readRDS("new_notices_blackouts.rds")
new_renamed_blackouts = readRDS("new_renamed_blackouts.rds")
different_blackouts = readRDS("different_blackouts.rds")
blackouts = rbind(new_notices_blackouts, new_renamed_blackouts, different_blackouts)
blackouts = blackouts %>% distinct()
write.csv(blackouts, "raw_blackouts.csv", row.names = F)
saveRDS(blackouts, "raw_blackouts.rds")

## cleaning the data ####
blackouts = readRDS("raw_blackouts.rds")
blackouts$area = stri_trans_general(blackouts$area, id = "Title")
#blackouts$area = gsub(",", " and", blackouts$area)
blackouts$area = gsub("Part Of", "", blackouts$area)
blackouts$area = gsub("Whole Of", "", blackouts$area)
blackouts$area = gsub("Parts Of", "", blackouts$area)
blackouts$area = gsub("Areas: ", "", blackouts$area)

blackouts$date_time = gsub("\\s+", " ", blackouts$date_time) # removing more than one space
blackouts$date_time =  gsub("–", "-", blackouts$date_time)
#blackouts$date = strptime(blackouts$date_time, format = "%A %d.%m.%Y")
#naDates = blackouts[is.na(blackouts$date),]

blackouts$date_time = str_replace_all(blackouts$date_time,
                                      c("DATES: " = "", 
                                        "DATE : " = "", 
                                        "ATE: " = "", 
                                        "," = "", 
                                        "Date " = "", 
                                        "DATE " = "",
                                        "Date: " = "",
                                        "DATE:" = "",
                                        "Tuesdays" = "Tuesday",
                                        "Thursday." = "Thursday",
                                        "10 2016" = "10.2016",
                                        "05 2017" = "05.2017", 
                                        "06 .10" = "06.10",
                                        "10 .2015" = "10.2015"))

long_datetimes_blackouts = blackouts[nchar(blackouts$date_time) > 50, ]
blackouts$pdf_name = as.character(blackouts$pdf_name)
blackouts[67,] = list("Elgeyo Marakwet", "24.09.2015 to 06.10.2015 7.00 A.M. – 6.00 P.M.", "6C33eTsZ89ol_Special Interruption - Elgeyo Marakwet")
blackouts[537,] = list("SECTION BETWEEN KISUMU INTERNATIONAL
AIRPORT AND KISIAN JUCTION", "Monday 30.03.2015 TIME: 7.00. A.M. – 1.00 P.M.", "32.0500-2015-03-26(cut)")
blackouts[nrow(blackouts) + 1,] = list("National Hsng Pamba Rd Cables & Plastics Kensalt Msa Salt KIE", "Wednesday 02.04.2014 9.00 A.M. - 5.00 P.M.")
blackouts[nrow(blackouts) + 1,] = list("Taru Township Taru Sec Mgalani Meli Kubwa Macknon Sec", "Wednesday 02.04.2014 9.00 A.M. - 5.00 P.M.")

#grepl("TO", blackouts$date_time, ignore.case = TRUE) == TRUE 


multiple_dates_notices = blackouts[grepl("&", blackouts$date_time, ignore.case = TRUE) == TRUE |
                          grepl("AND", blackouts$date_time, ignore.case = TRUE) == TRUE,] # 15 rows
#multiple_dates_notices_indices1 = grep("&", blackouts$date_time, ignore.case = TRUE)
#multiple_dates_notices_indices2 = grep("AND", blackouts$date_time, ignore.case = TRUE)
#multiple_dates_indices = rbind(multiple_dates_botices_indices1, multiple_dates_notices_indices2)
multiple_dates_notices_day1 = multiple_dates_notices
multiple_dates_notices_day1$date_time = gsub("&.*", "", multiple_dates_notices_day1$date_time)
multiple_dates_notices_day1$date_time = gsub("and.*", "", multiple_dates_notices_day1$date_time)
multiple_dates_notices_day1$date_time = gsub("AND.*", "", multiple_dates_notices_day1$date_time)


multiple_dates_notices_day2 = multiple_dates_notices
multiple_dates_notices_day2$date_time = gsub(".*&", "", multiple_dates_notices_day2$date_time)
multiple_dates_notices_day2$date_time = gsub(".*and", "", multiple_dates_notices_day2$date_time)
multiple_dates_notices_day2$date_time = gsub(".*AND", "", multiple_dates_notices_day2$date_time)
multiple_dates_notices_day2$date_time[1] = "Sunday 02.11.2014 TIME: 6.00 A.M. – 6.00P.M."
multiple_dates_notices_day2$date_time[2] = "Sunday 09.11.2014 6.00 A.M. – 6.00 P.M."
multiple_dates_notices_day2$date_time[14] = "Thursday 28.04.2016 TIME: 7.00 A.M. – 5.00 P.M."

multiple_dates_notices_filled = rbind(multiple_dates_notices_day1, multiple_dates_notices_day2)

# replace multiple dates_notices with multiple_dates_notices_filled
blackouts$multiple_dates = ifelse(grepl("&",blackouts$date_time, ignore.case = TRUE) == TRUE|
                                    grepl("AND", blackouts$date_time, ignore.case = TRUE) == TRUE, TRUE, FALSE)

blackouts = blackouts %>% filter(multiple_dates == FALSE) %>% select(-multiple_dates)# remove entries with multiple blackouts
blackouts = rbind(blackouts, multiple_dates_notices_filled)



# split different areas into different columns
blackouts$area = trimws(blackouts$area, "both")
blackouts$area = str_replace_all(blackouts$area, c(" &" = ",", " And" = ","))
#blackouts$area = gsub(", ", ",", blackouts$area)
#blackouts$area = gsub(" ", "_", blackouts$area)
#blackouts$area = gsub(",", ", ", blackouts$area)

blackouts$new_date = strptime(blackouts$date_time, format = "%A %d.%m.%Y")
blackouts$date_time[is.na(blackouts$new_date)] = trimws(blackouts$date_time[is.na(blackouts$new_date)], "both")
blackouts$date_time[is.na(blackouts$new_date)] = str_replace_all(blackouts$date_time[is.na(blackouts$new_date)],
                                      c("Sunday " = "",
                                        "Saturday " = "",
                                        "Friday " = "",
                                        "Thursday" = "",
                                        "Sunday " = ""))

blackouts$new_date[is.na(blackouts$new_date)] = strptime(blackouts$date_time[is.na(blackouts$new_date)], format = "%d.%m.%Y")                                       
blackouts$new_date[blackouts$date_time == "32.0690-2015-11-30"] = "2015-11-30 EAT"                                  
blackouts$new_date[blackouts$date_time == "32.0790-2015-06-18"] = "2015-06-18 EAT" 

blackouts$date_time[is.na(blackouts$new_date)] = str_sub(blackouts$pdf_name[is.na(blackouts$new_date)], start = 9)
blackouts$new_date[is.na(blackouts$new_date)] = strptime(blackouts$date_time[is.na(blackouts$new_date)], format = "%Y-%m-%d")                                       
blackouts$new_date[blackouts$date_time == "Vsxj_Interruptions 02 06 2016"] = "2016-06-02 EAT" 
blackouts$new_date[blackouts$date_time == "Vsxj_Interruptions 02 06 2016"] = "2016-06-02 EAT"
blackouts$new_date[blackouts$new_date == "0015-02-11 LMT"] = "2015-02-11 EAT"
blackouts$new_date[blackouts$new_date == "0015-02-12 LMT"] = "2015-02-12 EAT"
blackouts$new_date[blackouts$new_date == "0015-11-16 LMT"] = "2015-11-16 EAT"
blackouts$new_date[blackouts$new_date == "0020-06-21 LMT"] = "2016-06-21 EAT"
blackouts$new_date[blackouts$new_date == "0020-08-17 LMT"] = "2017-08-17 EAT"
blackouts$new_date[blackouts$new_date == "0201-01-21 LMT"] = "2016-01-21 EAT"
blackouts$new_date[blackouts$new_date == "0201-10-18 LMT"] = "2015-10-18 EAT"
blackouts$new_date[blackouts$new_date == "2104-09-23 EAT"] = "2014-09-23 EAT"

blackouts$new_date = as.POSIXct(blackouts$new_date)

saveRDS(blackouts, "clean_dates_blackouts.rds")


