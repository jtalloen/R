setwd("C:/Users/Joachim/Box Sync/Joe/Research/CMU/Luxi Shen/UR Model/")
Pay <- read.csv("1711_UR_Model_11_Bonus.csv")

if((Pay$pay == .12 & Pay$day==0) | (Pay$pay == .15 & Pay$day == 7)){
  Pay$new <-.1
}
##################################################
########## getting data from previous survey ######
###################################################

library(stringr)

data$array <- "array('iD'=>'x','stage1'=>y,'stage2'=>z,'benefit'=>s,'Com1'=>r,'Com2'=>s,'Com3'=>t,'Com4'=>u, 'Com5'=>v, 'Com6'=>w, 'Com7'=>a,'Com8'=>b, 'Com9'=>c, 'Com10'=>d),"

Pay$array <- str_replace(Pay$batch, "y", Pay[,1])
Pay$array <- str_replace(Pay$batch, "x", Pay[,2])
Pay$array <- str_replace(Pay$batch, "z", Pay[,3])

##############################################
###### sending message to MTurkers ###########
##############################################

library(MTurkR)

credentials(keypair=c("AKIAIIWIJFZTUMLQ3PZQ","IsWtdkS6/kIFUXbu+hYkaME3Yff+SSzZrEJjX10A"))
# credentials(keypair=c("AKIAJV2FWUCARJMCSIWQ","e8nOc7aCu7LIUQoerya9FvUuc7WSGYBMyIRkQei9"))

AccountBalance()

a <- "Wrong Bonus"

b <- "Dear MTurker,

We just paid you a bonus and made a calculation error.  We would really appreciate it if you could refund this bonus so that we can pay you your correct bonus!

Thansk for participating in our survey and we hope to have you participate again in the future!"

c <- "A2X9TCHOAZHJYZ"

ContactWorker(subjects = a,
              msgs = b,
              workers = Pay$mtwid,
              batch = TRUE)

###############################
####### Grant bonus ###########
###############################


library(MTurkR)
credentials(keypair=c("AKIAIIWIJFZTUMLQ3PZQ","IsWtdkS6/kIFUXbu+hYkaME3Yff+SSzZrEJjX10A"))

## Gretchen
credentials(keypair=c("AKIAI3HP2HDEGLDGBFTQ","ajuG0/V8VqB7LoAPODJh1elXxcJX/46b63Vnm1R5"))

## Luxi
credentials(keypair=c("AKIAJEJMHJ4DI3UJN2PQ","cU1Y+URk1h0CjtdUeRvf6I/CgYBuUrdWYLut71Za"))


## Ebru
# credentials(keypair=c("AKIAJV2FWUCARJMCSIWQ","e8nOc7aCu7LIUQoerya9FvUuc7WSGYBMyIRkQei9"))
## 

## Barry
# credentials(keypair=c("AKIAIJ34DFLFGLB7EGEA","9/kzVAA2FvUPjY1oeVy20JArfSNRm2Id61xmq4K8"))


AccountBalance()

Pay$pay <- round(Pay$pay, 2)

Pay <- Pay[-c(3,12,13,18,19,28,30,39,45,53,65), ]

1.20*(sum(Pay$pay)) 
(sum(Pay$pay)) 

newpay <- Pay[ which(Pay$day ==14), ]

a <- Pay$mtwid
b <- Pay$assignid
c <- Pay$pay
d <- "Thanks for your great work on my HIT!  This is the bonus payment you earned."
GrantBonus(workers=a, assignments=b, amounts=c, reasons=d)

## how to add comma for php file 
library(stringr)

Pay$comma <- "y,"

Pay$comma <- str_replace(Pay$comma, "y", Pay[,1])

write.csv(Pay, file = "Pred_Stock2_Comma.csv", row.names = FALSE)


###########################
##### Qualification #######
###########################

library(MTurkR)
credentials(keypair=c("AKIAIIWIJFZTUMLQ3PZQ","IsWtdkS6/kIFUXbu+hYkaME3Yff+SSzZrEJjX10A"))

## Gretchen
credentials(keypair=c("AKIAI3HP2HDEGLDGBFTQ","ajuG0/V8VqB7LoAPODJh1elXxcJX/46b63Vnm1R5"))

## Luxi
credentials(keypair=c("AKIAJEJMHJ4DI3UJN2PQ","cU1Y+URk1h0CjtdUeRvf6I/CgYBuUrdWYLut71Za"))


qual  <- CreateQualificationType(name = "Work Done Before",
                                 description= "This qualification is for people who have worked for me before",
                                 status = "Active",
                                 keywords="Worked for me before")

for(i in 1:nrow(Pay)){
  x <- Pay[i, "mtwid"]
  AssignQualification(qual = "33YW3NK3807SZCK2O4US5E2CBNBS2O", workers = x, value = "1")
}


