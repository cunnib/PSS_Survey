#### pss_scrape.R ###
# Purpose: Download tables from PSS web page
# Author: Brandon Cunningham (brandon.cunningham.20@gmail.com)
#####################

library(data.table)
library(XML)
setwd("C:/Users/brand/Documents/PSS_Survey/")

pss19 <- readHTMLTable(readLines("https://hr.uw.edu/comp/professional-staff/2019-professional-staff-salary-survey/"))[2:8]
pss17 <- readHTMLTable(readLines("https://hr.uw.edu/comp/professional-staff/2017-professional-staff-salary-survey/"))

df <- pss19

for(i in 1:length(df)){ df[[i]] <- data.table(df[i][[1]]) }
###Salary by classification, table 4
t4new <- data.table()
t4 <- df[[4]]
##Split into sub tables
##First, identify blank rows between sub-tables
blanks <- c(0,(1:nrow(t4))[is.na(t4[,2])], nrow(t4))
#loop through each sub-table between two blank rows
for(r in 1:(length(blanks)-1)){
  temp <- t4[(blanks[r]+1):(blanks[r+1]-1),] #subset table
  #Read the job category - 
  #in the first row for most sub-tables, in the column names for the first sub-table
  if(r==1){ 
    temp[,category := names(temp)[3]] 
  } else {
    temp[,category := temp[1,3]]
    temp <- temp[2:nrow(temp),]
  }
  #omit the category percentages - we can get these from table 5 easier
  if(r != length(blanks)-1) {temp <- temp[1:(nrow(temp)-1)]} 
  #R friendly column names
  names(temp) <- c("job_family", "uw_job_codes_2019", "title", 
                   "uw_ees_2019", "uw_market_50th_tcc_2019", "job_category")
  t4new <- rbind(t4new,temp,fill=T)
}
titles19 <- copy(t4new)
##Table 5: uw-market_50th_tcc by job category
yrs <- c("y2012", "y2015", "y2017", "y2019")
t5 <- df[[5]]
t5 <- t5[2:nrow(t5)]
names(t5) <- c("category", yrs[1:(ncol(t5)-1)])

df <- pss17
for(i in 1:length(df)){ df[[i]] <- data.table(df[i][[1]]) }
###Salary by classification, table 4
t4new <- data.table()
t4 <- df[[4]]
##Split into sub tables
##First, identify blank rows between sub-tables
blanks <- c(0,(1:nrow(t4))[is.na(t4[,2])], nrow(t4))
#loop through each sub-table between two blank rows
for(r in 1:(length(blanks)-1)){
  temp <- t4[(blanks[r]+1):(blanks[r+1]-1),] #subset table
  #Read the job category - 
  #in the first row for most sub-tables, in the column names for the first sub-table
  if(r==1){ 
    temp[,category := names(temp)[3]] 
  } else {
    temp[,category := temp[1,3]]
    temp <- temp[2:nrow(temp),]
  }
  #omit the category percentages - we can get these from table 5 easier
  if(r != length(blanks)-1) {temp <- temp[1:(nrow(temp)-1)]} 
  #R friendly column names
  names(temp) <- c("job_family", "uw_job_codes_2017", "title", 
                   "uw_ees_2017", "uw_market_50th_tcc_2017", "job_category")
  t4new <- rbind(t4new,temp,fill=T)
}
titles17 <- copy(t4new)
comparison <- merge(titles17, titles19, by = c("title", "job_category", 
                                               "job_family"), all=T)
comparison$title <- gsub("-", "-", comp$title)
fwrite(comparison, paste0("data/comparison_17_19.csv"))
fwrite(t5, paste0("data/comparison_categories.csv"))



                    
                    