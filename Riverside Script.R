library(openxlsx)
library(tidyverse)
library(NLP)
# load the file
fb = read.xlsx('FPI_RSB.xlsx')
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
#cleaning up the purpose
fb$STANDARD_Purpose_of_Donation <- dplyr::recode(fb$STANDARD_Purpose_of_Donation, "Personal Solicitation" = "Unrestricted")

fb2 <- fb[ -c(12:14)]

fb2$STANDARD_Gift_Date <- as.numeric(fb2$STANDARD_Gift_Date)
fb2$STANDARD_Gift_Date  <-  as.Date(fb2$STANDARD_Gift_Date, origin = "1899-12-30")
fb2$STANDARD_Gift_Fiscal_Year <- quarter(fb2$STANDARD_Gift_Date, type = "year.quarter", fiscal_start = 7)

fb2$STANDARD_Gift_Fiscal_Year <- substring((as.String(fb2$STANDARD_Gift_Fiscal_Year)),1,4)

write.xlsx(fb2, file = "FPI_Data_RSB_Processed.xlsx") 