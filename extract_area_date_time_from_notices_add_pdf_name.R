library(pdftools)
library(purrr)
library(textreadr)
# notices from new_notices folder(a combination of notices from notices folder and those of similar 
# structure from Renamed folder)

#setwd("C:/Users/Chance/Dropbox/My Projects/Elvis/KPLC/notices/new_notices")
#notices = file.path("C:/Users/Chance/Dropbox/My Projects/Elvis/KPLC/notices/new_notices")
announcements = list.files(notices, ".pdf")

# function to read an announcement, extract area and date-time 
extract_area_date_time = function(y){
  #y = "18Um0M6UhiIr_Special Interruption - Turkana County.pdf" # new
  x = pdf_text(y) # reading an announcement into R from .pdf
  x = unlist(strsplit(x, "\n")) # breaking the announcements into lines and unlisting the lines
  area = x[startsWith(x, "AREA")] # getting the line that contains the area of blackout
  area = gsub("AREA: ", "", area) # cleaning the area
  area = gsub("\r", "", area) # cleaning the area
  
  dateTime = x[startsWith(x, "DATE")] # getting the line that contains date and time of blackout
  dateTime = gsub("DATE: ", "", dateTime) # cleaning the date
  dateTime = gsub("TIME: ", "", dateTime) # cleaning the date
  dateTime = gsub("\r", "", dateTime) # cleaning the date
  
  
  blackoutEntry = data.frame(cbind(area, dateTime))
  blackoutEntry$pdf_name = rep(gsub(".pdf", "", y), nrow(blackoutEntry)) # new
  
  write.table(blackoutEntry, "new_notices_blackouts.csv", sep = ",", append = T, row.names = F, col.names = F )
  
}

map(announcements, extract_area_date_time)

blackouts = read.csv("new_notices_blackouts.csv")
saveRDS(blackouts, "new_notices_blackouts.rds")


# notices from big fails folder
#setwd("C:/Users/Chance/Dropbox/My Projects/Elvis/KPLC/notices/bigfails")
#notices = file.path("C:/Users/Chance/Dropbox/My Projects/Elvis/KPLC/notices/bigfails")
#announcements = list.files(notices, ".pdf")

#x = read_pdf("s1lGJZH4xkaC_Special Interruption - Parts of Kiambu County.pdf" )# returns empty


# notices from new_renamed folder(notices from Renamed folder that have been modified such that 
# each notice is on its own line)

setwd("C:/Users/Chance/Dropbox/My Projects/Elvis/KPLC/Renamed/new_renamed")
notices = file.path("C:/Users/Chance/Dropbox/My Projects/Elvis/KPLC/Renamed/new_renamed")
announcements = list.files(notices, ".pdf")


extract_area_date_time = function(y){
x = read_pdf(y) # from package textreadr
x = x$text

areas = which(startsWith(x, "AREA"))
areas1 = x[areas]
areas1 = gsub("AREA: ", "", areas1)
dates = areas+1
dates1 = x[dates]
dates1 = gsub("DATE: ", "", dates1)
dates1 = gsub("TIME: ", "", dates1)

blackoutEntry = data.frame(cbind(areas1, dates1))

blackoutEntry$pdf_name = rep(gsub(".pdf", "", y), nrow(blackoutEntry)) # new
write.table(blackoutEntry, "new_renamed_blackouts.csv", sep = ",", append = T, row.names = F, col.names = F) 

}

map(announcements, extract_area_date_time) 

blackouts = read.csv("new_renamed_blackouts.csv")
saveRDS(blackouts, "new_renamed_blackouts.rds")

# one notice had a completely different format from the rest
different_blackouts = data.frame(area = c("Parts of South C and Lang'ata", "Parts of Embakasi"),
                                    date_time = c("Saturday 26.11.2016  8.00 A.M. – 5.00 P.M.", 
                                                  "Sunday 27.11.2016  8.00 A.M. – 5.00 P.M."),
                                 pdf_name = "32.0800-2016-11-26.pdf")
saveRDS(different_blackouts, "different_blackouts.rds")
