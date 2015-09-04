setwd("C:/Users/Joachim/OneDrive/Joe/Research/Behavioral Economics Lab - Rutgers/")
Pay <- read.csv("Wage_Experiment_1.csv")


## getting data from previous survey
library(stringr)

Pay$array <- "array('iD'=>'x','stage1'=>y,'stage2'=>z,'benefit'=>s,'Com1'=>r,'Com2'=>s,'Com3'=>t,'Com4'=>u, 'Com5'=>v, 'Com6'=>w, 'Com7'=>a,'Com8'=>b, 'Com9'=>c, 'Com10'=>d),"

Pay$array <- str_replace(Pay$batch, "y", Pay[,1])
Pay$array <- str_replace(Pay$batch, "x", Pay[,2])
Pay$array <- str_replace(Pay$batch, "z", Pay[,3]) ## do this for all variables

## sending message to MTurkers

library(MTurkR)

credentials(keypair=c("enter personal AWS Access Key ID ","enter AWS Secret Access Key"))

AccountBalance()

a <- "This is the follow up HIT to the Choice Task you did a week ago"

b <- "Thanks for completing my HIT!
I will pay a $.50 bonus if you complete this follow-up survey.
The survey can be completed at
{link}"

c <- "A2X9TCHOAZHJYZ"

ContactWorker(subjects = a,
              msgs = b,
              workers = Pay$mtwid,
              batch = TRUE)


## Grant bonus

library(MTurkR)

credentials(keypair=c("enter personal AWS Access Key ID ","enter AWS Secret Access Key"))

AccountBalance()

setwd("C:/Users/Joachim/OneDrive/Joe/Research/Behavioral Economics Lab - Rutgers/")
data <- read.csv("roulette pay1.csv", header=T)

1.10*(sum(data$realpay))

a <- Pay$mtwid
b <- Pay$assign_id
c <- Pay$realpay
d <- "Thanks for your great work on my HIT!  This is the bonus payment for the roulette study."
GrantBonus(workers=a, assignments=b, amounts=c, reasons=d)