## Analyzing DOSPERT from Qualtrics (Note: only uses one response)

## First, load the neccessary packages that will be needed for the analyses and set the Working Directory.

library(XML)
library(plyr)
library(stringr)
library(reshape2)
setwd("C:/Users/World/Desktop/Joe/Research/CDS - Columbia/DOSPERT")

## Next read the file in from Qualtrics.

dataQ <- xmlToDataFrame("DOSPERT.xml", stringsAsFactors = F)

## This is how the head of your columns should look after uploading the file.

head(dataQ)

## Eliminating the location columns and melting the data into the right structure.

dataQ <- dataQ[, -101:-103]  ## Skip this step if you need the location columns for further analysis
dataQm <- melt(dataQ, id = c("ResponseID", "ResponseSet", "Name", "ExternalDataReference", 
    "EmailAddress", "IPAddress", "Status", "StartDate", "EndDate", "Finished"))

## This is how your data should look after being melted.

head(dataQm)

## Split the variable column into 3 columns.

dataQm <- mutate(dataQm, Scale = str_extract(variable, "(RiskTaking|RiskPerceptions|ExpectedBenefits)"), 
    Domain = str_replace(variable, "[A-Za-z]*_([A-Za-z]{1,2})\\d+", "\\1"), 
    Question = str_extract(variable, "\\d{1,2}"))

## This is how the new columns should look.

head(dataQm[, 13:15])

## The sums have to be calculated of each domain for each subject.

dataQm$value <- as.numeric(dataQm$value)
ddply(dataQm, .(ResponseID, Domain), summarize, sum = round(sum(value)))

## The data will be casted into the right format to conduct a linear regression.

dataQc <- dcast(dataQm, ResponseID + ResponseSet + Name + ExternalDataReference + 
    EmailAddress + IPAddress + Status + StartDate + EndDate + Finished + Domain + 
    Question ~ Scale, value.var = "value")

## This is how the last 5 columns of your casted data frame should look

head(dataQc[, 11:15])

## Next we will regress Expected Benefits and Risk Perceptions on Risk Taking for each item on the scale to calculate risk-attitude.

model <- function(dataQc) {
    lm(RiskTaking ~ RiskPerceptions + ExpectedBenefits, data = dataQc)
}

dataQc.lm <- dlply(dataQc, .(Domain, ResponseID), model)

## Your regression results should look like these:

head(dataQc.lm)
