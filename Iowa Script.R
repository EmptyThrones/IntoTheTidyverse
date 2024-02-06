library(openxlsx)
library(tidyverse)
library(NLP)
# load the file
fb = read.xlsx('FPI_IA.xlsx')

# rename the variables to align with our internal documentation
colnames(fb)[1] <- "STANDARD_Unique_Donor_ID"
colnames(fb)[2] <- "STANDARD_Gift_Date"
colnames(fb)[3] <- "STANDARD_Gift_Amount"
colnames(fb)[4] <- "STANDARD_Donor_Last_Name"
colnames(fb)[5] <- "STANDARD_Donor_First_Name"
colnames(fb)[9] <- "STANDARD_Donor_ZIP_Code"
colnames(fb)[10] <- "STANDARD_Source_of_Donation"
colnames(fb)[11] <- "STANDARD_Method_of_Solicitation"
colnames(fb)[12] <- "STANDARD_Purpose_of_Donation"
fb$STANDARD_Gift_Fiscal_Year <- ""
fb$STANDARD_Organization_Name <- ""

#create a data set that keeps the variables we want while putting them in the correct order
fb2 <- fb[,c(1,2,13,3,4,5,14,9,10,11,12)]

#First data cleaning step. Remove the refunds from the donation list
fb2 <- subset(fb2,fb2$STANDARD_Gift_Amount > 0)

#merge their misc category into our misc category
fb2$STANDARD_Method_of_Solicitation <- dplyr::recode(fb2$STANDARD_Method_of_Solicitation, "Due to Others" = "Other Private Support", "Sponsorship - DO NOT USE" = "Cause Marketing")

#alright let us put all of the purposes of donation in the right place
fb2$STANDARD_Purpose_of_Donation <- dplyr::recode(fb2$STANDARD_Purpose_of_Donation, "4090 Event Income" = "Restricted Programs", 
"4040 Program Sponsorship" = "Restricted Programs", "4300 Restricted Grant" = "Restricted Programs", "4010 Restricted Donation"="Restricted Programs", 
"4000 Unrestricted Donation"="Unrestricted", "4200 Unrestricted Grant"="Unrestricted", "4100 Capital Campaign"="Capital Campaign")

#source of donation
fb2$STANDARD_Source_of_Donation <- dplyr::recode(fb2$STANDARD_Source_of_Donation, "Business/Company/Corporate Giving" = "Corporation",
"Individual Giving"="Individual", "Foundations, including family foundations" = "Foundation", "Other Organizations (schools, PTO, service organization)" = "Organization")





fb2$STANDARD_Gift_Date  <-  as.Date(fb2$STANDARD_Gift_Date, origin = "1899-12-30")
fb2$STANDARD_Gift_Fiscal_Year <- quarter(fb2$STANDARD_Gift_Date, type = "year.quarter", fiscal_start = 7)
fb2$STANDARD_Gift_Fiscal_Year <- substring((as.String(fb2$STANDARD_Gift_Fiscal_Year)),1,4)

write.xlsx(fb2, file = "FPI_Data_IA_Processed.xlsx") 

#declare a space to print alerts
fb$DATA_VALIDATION_Questions_from_FANO = ''

#flag missing values
print(paste('There are ', 
            nrow(fb[which(is.na(fb$STANDARD_Unique_Donor_ID)),]), 
            ' row(s) with no Donor ID', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(is.na(fb$STANDARD_Unique_Donor_ID), 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO, 
                                                      'Donor ID is missing. Please correct || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)
print(paste('There are ', 
            nrow(fb[which(is.na(fb$STANDARD_Gift_Date)),]), 
            ' row(s) with no gift date', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(is.na(fb$STANDARD_Gift_Date), 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO,
                                                      'Gift Date is missing. Please correct || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)
print(paste('There are ', 
            nrow(fb[which(is.na(fb$STANDARD_Gift_Amount)),]), 
            ' row(s) with no gift amount', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(is.na(fb$STANDARD_Gift_Amount), 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO,
                                                      'Gift Amount is missing. Please correct || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)
print(paste('There are ', 
            nrow(fb[which(fb$STANDARD_Gift_Amount<=0),]), 
            ' row(s) with a gift amount of $0 or less', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(fb$STANDARD_Gift_Amount <= 0, 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO,
                                                      'Gift Amount is either $0 or a negative amount. Is this a refund? || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)
print(paste('There are ', 
            nrow(fb[which(is.na(fb$STANDARD_Source_of_Donation)),]), 
            ' row(s) with no source of donation', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(is.na(fb$STANDARD_Source_of_Donation), 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO,
                                                      'Source of Donation is missing. Please correct || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)
print(paste('There are ', 
            nrow(fb[which(is.na(fb$STANDARD_Method_of_Solicitation)),]), 
            ' row(s) with no method of solicitation', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(is.na(fb$STANDARD_Method_of_Solicitation), 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO,
                                                      'Method of Solicitation is missing. Please correct || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)
print(paste('There are ', 
            nrow(fb[which(is.na(fb$STANDARD_Purpose_of_Donation)),]), 
            ' row(s) with no method of solicitation', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(is.na(fb$STANDARD_Purpose_of_Donation), 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO,
                                                      'Purpose of Donation is missing. Please correct || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)

# now for something different. Lets flag all entries that are inconsistent with our data definitions

print(paste('There are ', 
            nrow(fb[which(fb$STANDARD_Method_of_Solicitation=='Due to Others'),]), 
            ' row(s) with an entry that does not align with our data definitions', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(fb$STANDARD_Method_of_Solicitation == 'Due to Others', 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO,
                                                      'Is this miscellaneous private support? || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)

print(paste('There are ', 
            nrow(fb[which(fb$STANDARD_Purpose_of_Donation=='4090 Event Income'),]), 
            ' row(s) with an entry that does not align with our data definitions', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(fb$STANDARD_Purpose_of_Donation == '4090 Event Income', 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO,
                                                      'Can the Purpose of Donation be reclassified to align with the data memo? || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)
print(paste('There are ', 
            nrow(fb[which(fb$STANDARD_Purpose_of_Donation=='4010 Restricted Donation'),]), 
            ' row(s) with an entry that does not align with our data definitions', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(fb$STANDARD_Purpose_of_Donation == '4010 Restricted Donation', 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO,
                                                      'Can the Purpose of Donation be reclassified to align with the data memo? || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)
print(paste('There are ', 
            nrow(fb[which(fb$STANDARD_Purpose_of_Donation=='4040 Program Sponsorship'),]), 
            ' row(s) with an entry that does not align with our data definitions', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(fb$STANDARD_Purpose_of_Donation == '4040 Program Sponsorship', 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO,
                                                      'Can the Purpose of Donation be reclassified to align with the data memo? || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)
print(paste('There are ', 
            nrow(fb[which(fb$STANDARD_Purpose_of_Donation=='4300 Restricted Grant'),]), 
            ' row(s) with an entry that does not align with our data definitions', sep=''))
fb$DATA_VALIDATION_Questions_from_FANO = ifelse(fb$STANDARD_Purpose_of_Donation == '4300 Restricted Grant', 
                                                paste(fb$DATA_VALIDATION_Questions_from_FANO,
                                                      'Can the Purpose of Donation be reclassified to align with the data memo? || ', sep=''), 
                                                fb$DATA_VALIDATION_Questions_from_FANO)

write.xlsx(fb, file = "FPI_Data_IA.xlsx") 