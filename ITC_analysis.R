## Load the neccessary packages

library(plyr)
library(stringr)

## Set the working directory and import the data files And naming the columns when group is equal to 1 = OCPD, 3 = HC and 4 = OCD

setwd("C:/Users/World/Desktop/Joe/Research/CDS - Columbia/ITC/DD")
ITC = ldply(list.files(pattern = "[0-9]+__[0-9]+_[0-9]+_[0-9]+_[0-9]+_[0-9]+_[0-9]+_[A-Z]+_TMS_Outcomes.txt"), 
    function(filename) {
        dum = read.table(filename)
        dum$filename = filename
        dum$ID = str_extract(dum$filename, "^[0-9]{4}")
        dum$group = str_extract(dum$filename, "^[0-9]")
        dum <- rename(dum, c(V1 = "TrialN", V2 = "SSd", V3 = "LLd", V4 = "SSa", 
            V5 = "LLa", V6 = "Choice"))
        return(dum)
    })

ITC$check <- ifelse(str_extract(ITC$filename, "^[0-9]{1}") %in% 1 | str_extract(ITC$filename, 
    "^[0-9]{1}") %in% 3:4, "1", "2")

ITC <- ITC[ITC[, 10] != 2, ]

head(ITC)

## To check the Ns in each group divide the following by 58 and then by 2 (or 116)

N <- table(ITC$group)
N/116

## Seperating between Accelerate and Delay. If the file is completed first it is accelerate if later it is delay. Make time column and data column

ITC$time <- str_replace(ITC$filename, "[0-9]+__[0-9]+_[0-9]+_[0-9]+_[0-9]+_([0-9]+)_([0-9]+)_[A-Z]+_TMS_Outcomes.txt", 
    "\\1\\2")
ITC$hour <- as.numeric(str_replace(ITC$filename, "[0-9]+__[0-9]+_[0-9]+_[0-9]+_([0-9]+)_[0-9]+_[0-9]+_[A-Z]+_TMS_Outcomes.txt", 
    "\\1"))
ITC$hour <- ITC$hour + 12
ITC$time <- paste(ITC$hour, ITC$time, sep = "")
ITC$hour <- NULL

ITC$date <- str_replace(ITC$filename, "[0-9]+__([0-9]+)_([0-9]+)_([0-9]+)_[0-9]+_[0-9]+_[0-9]+_[A-Z]+_TMS_Outcomes.txt", 
    "\\1\\2\\3")
## If the date and id are equal and the time is less than the next i+58th question it is accelerate(1), if not it is delay(2). Column 11 is equal to ITC$time. Column 8 is equal to ITC$ID and column 12 is equal to ITC$date. Renaming the column

for (i in 1:nrow(ITC)) {
    a = ITC[i, 8]
    b = ITC[i + 58, 8]
    x = ITC[i, 11]
    y = ITC[i + 58, 11]
    w = ITC[i, 12]
    z = ITC[i + 58, 12]
    ITC[i, 13] <- ifelse(a == b & x < y & w == z, 1, 2)
}

colnames(ITC)[13] <- "Acc.D"
## Now that we have ID, time, date, group and accelerate vs delay, we can start the analysis Setting the delays in years Starting with the titrator

ITC$SSd <- ifelse(ITC[, 1] %in% 1:36, ITC$SSd/52, ITC$SSd/12)
ITC$LLd <- ifelse(ITC[, 1] %in% 1:36, ITC$LLd/52, ITC$LLd/12)

head(ITC)

dummy.T <- subset(ITC, !(TrialN %in% 1:36))

for (i in 1:nrow(dummy.T)) {
    x = dummy.T[i, 6]
    y = dummy.T[i + 1, 6]
    dummy.T[i, 14] <- ifelse(x < y, (dummy.T[i, 5] + dummy.T[i + 1, 5])/2, "NA")
}

colnames(dummy.T)[14] <- "IP"

head(dummy.T)

## Calculating the k and delta

dummy.T[, 14] <- as.numeric(dummy.T[, 14])
## Warning: NAs introduced by coercion

for (i in 1:nrow(dummy.T)) {
    x = dummy.T[i, 6]
    y = dummy.T[i + 1, 6]
    dummy.T[i, 15] <- ifelse(x < y, (dummy.T[i, 4]/dummy.T[i, 14])^(dummy.T[i, 
        3] - dummy.T[i, 2]), "NA")
    dummy.T[i, 16] <- ifelse(x < y, ((dummy.T[i, 14]/dummy.T[i, 4]) - 1)^4, 
        "NA")
}

dummy.T <- rename(dummy.T, c(V15 = "T.d", V16 = "T.k"))
head(dummy.T)

## Calculating group differences

dummy.T <- na.omit(dummy.T)
summary(aov(T.k ~ group, dummy.T))

## The non titrator Calculating the implied k Recode 0 as impatient and 1 as patient

dummy.NT <- subset(ITC, TrialN %in% 1:36)
dummy.NT$NT.k <- (dummy.NT$LLa - dummy.NT$SSa)/((dummy.NT$SSa * dummy.NT$LLd) - 
    (dummy.NT$LLa * dummy.NT$SSd))
dummy.NT$Choice <- ifelse(dummy.NT$Choice == 2, 1, 0)

dummy.NT.glm <- function(dummy.NT) {
    glm(dummy.NT$Choice ~ dummy.NT$NT.k, family = binomial)
}
dummy.NT.AIC <- function(dummy.NT) {
    extractAIC(glm(dummy.NT$Choice ~ dummy.NT$NT.k, family = binomial))
}

dummy.NT.glm <- dlply(dummy.NT, .(ID, Acc.D), dummy.NT.glm)
dummy.NT.AIC <- dlply(dummy.NT, .(ID, Acc.D), dummy.NT.AIC)

intercept <- sapply(dummy.NT.glm, `[[`, 1)

k <- data.frame((-intercept[1, ])/intercept[2, ])

## After k is calculated we need to calculate the difference in k values between groups First I resutrcture a dataframe to fit the data

AIC <- data.frame(sapply(dummy.NT.AIC, `[[`, 2))
AIC$ID <- rownames(AIC)
AIC$ID <- str_extract(AIC$ID, "^[0-9]{4}")
AIC$group <- str_extract(AIC$ID, "^[0-9]{1}")

ITC.n <- cbind(AIC, k)
ITC.n <- rename(ITC.n, c(sapply.dummy.NT.AIC........2. = "AIC", X..intercept.1.....intercept.2... = "k"))

head(ITC.n)

## Now I do an ANOVA to see if there are group differences

ITC.n$group <- as.factor(ITC.n$group)

summary(aov(k ~ group, ITC.n))
