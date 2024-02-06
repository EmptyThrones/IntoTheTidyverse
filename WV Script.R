library(openxlsx)
library(tidyverse)
library(NLP)
# load the file
fb = read.csv('FPI_FHFB.csv')

#column 12 is empty so lets get rid of that
fb <- fb[ -c(12)]

#rename variables
colnames(fb)[1] <- "STANDARD_Unique_Donor_ID"
colnames(fb)[2] <- "STANDARD_Gift_Date"
colnames(fb)[3] <- "STANDARD_Gift_Fiscal_Year"
colnames(fb)[4] <- "STANDARD_Gift_Amount"
colnames(fb)[5] <- "STANDARD_Donor_Last_Name"
colnames(fb)[6] <- "STANDARD_Donor_First_Name"
colnames(fb)[7] <- "STANDARD_Organization_Name"
colnames(fb)[8] <- "STANDARD_Donor_ZIP_Code"
colnames(fb)[9] <- "STANDARD_Source_of_Donation"
colnames(fb)[10] <- "STANDARD_Method_of_Solicitation"
colnames(fb)[11] <- "STANDARD_Purpose_of_Donation"

fb$STANDARD_Donor_ZIP_Code <- substring(fb$STANDARD_Donor_ZIP_Code,1,5)

fb$STANDARD_Gift_Date  <-  as.Date(fb$STANDARD_Gift_Date, format = "%m/%d/%Y")

fb$STANDARD_Gift_Fiscal_Year <- year(fb$STANDARD_Gift_Date)

fb2 <- fb
#I am about to do a few recodings. I want to be able to restore unmodified values so I make a second dataframe to mess with and just reload the old one when I need it

fb2$STANDARD_Purpose_of_Donation <- dplyr::recode(fb2$STANDARD_Purpose_of_Donation, "FA KY Boyd" = "Restricted - Programs", 
"FA KY Greenup" = "Restricted - Programs", "Government Shutdown"="Restricted - Programs","Restricted- Programs"="Restricted - Programs", "Restricted Programs"="Restricted - Programs")

fb2$STANDARD_Method_of_Solicitation <- fb2$STANDARD_Method_of_Solicitation %>% replace_na('Grant Applications - Corporations')


fb2$STANDARD_Method_of_Solicitation <- dplyr::recode(fb2$STANDARD_Method_of_Solicitation, "YetiRaffle" = "Special Events",
"Grant Applications-Foundations"="Grant Applications - Foundations", "Grant Applications-Other"= "Grant Applications - Other", 
"Grant Applications- Corporationa" = "Grant Applications - Corporations")

fb2$STANDARD_Source_of_Donation <- dplyr::recode(fb2$STANDARD_Source_of_Donation, "Company"="Corporation",
"Corporations and Businesses"="Corporation", "Church"="Organization")