## Load packages and data into R

library("ggplot2")
library("reshape2")
library("stringr")
library("plyr")
library("dplyr")
setwd("C:/Users/World/Desktop/Joe/Research/MDM - Rutgers/Partitioning Study/MTurk")
Pdata <- read.csv("partition default svo data EDIT.csv")

## Calculate the mean of svo_angle and make new column that has svo_angle centered to the mean

mean <- mean(Pdata[,4])

Pdata$svo_mean <- Pdata$svo_angle - mean

## Now dichotomize the consume (not cooperate is 1 and cooperate is 0)

Pdata$coop <- ifelse(Pdata$consume > 10, 1, 0)

## Some graphs (-1 is no partition, 1 is partition)

Coop.1 <- subset(Pdata, Pdata$coop == 0)

frequency1 <- ddply(Coop.1, .(partition), summarize, length=length(coop))

count(Pdata$partition==-1)

frequency1$percentage <- ifelse(frequency1$partition==-1, frequency1$length / 253, frequency1$length / 252) * 100

frequency1$partition <- ifelse(frequency1$partition==-1, "No Partition", "Partition")

ggplot(frequency1, aes(y=length, x=factor(partition))) + 
  geom_bar(stat="identity", position="dodge", fill="salmon") + 
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,40)) + 
  ggtitle("Number of people that cooperated by partition") + 
  xlab("Partition") + ylab("# of people that cooperated") + 
  geom_path(x=c(1,1), y=c(185,190)) + 
  geom_path(x=c(1,2), y=c(190,190)) +
  geom_path(x=c(2,2), y=c(185,190)) + 
  annotate("text",x=1.5,y=195,label="***")

 ggplot(frequency1, aes(y=percentage, x=factor(partition))) + 
  geom_bar(stat="identity", position="dodge", fill="salmon") + 
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,20)) + 
  ggtitle("Percentage of people that cooperated by partition") + 
  xlab("Partition") + ylab("% of people that cooperated") + 
  geom_path(x=c(1,1), y=c(70,75)) + 
  geom_path(x=c(1,2), y=c(75,75)) + 
  geom_path(x=c(2,2), y=c(75,70)) + 
  annotate("text",x=1.5,y=80,label="***")

twobytwo <- ddply(Pdata, .(partition, coop), summarize, length = length(coop))

twobytwo$percentage <- (twobytwo$length/505) * 100

ggplot(twobytwo, aes(y = percentage, x = factor(partition), fill = factor(coop))) + 
    geom_bar(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) + 
    ggtitle("Percentage of people that cooperated by partition and cooperation") + 
    xlab("Partition") + 
    ylab("% of people that cooperated") + 
    scale_fill_discrete(name = "Cooperation", breaks = c("1", "0"), labels = c("No cooperation", "Cooperation"))

ggplot(Coop.1, aes(x=svo_mean)) + 
  geom_step(stat="ecdf", color="salmon") + 
  ggtitle("Cumulative density distribution for svo_mean for people that cooperated")  

ggplot(Pdata, aes(x=svo_mean, colour=factor(coop))) + 
  geom_step(stat="ecdf") + 
  ggtitle("Cumulative density distribution for \n cooperation vs no cooperation") + 
  scale_colour_discrete(name="Cooperation", breaks=c("1", "0"), labels=c("No cooperation", "Cooperation")) + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) + 
  annotate("text",x=0,y=.8,label="***")

mean.df <- ddply(Pdata, .(coop), summarize, mean=mean(svo_mean))

ggplot(mean.df, aes(x=factor(coop), y=mean)) + 
  geom_bar(stat="identity", position="dodge", fill="salmon") + 
  scale_y_continuous(limits=c(-.1,.1), breaks=seq(-.1,.1,.02)) + 
  geom_path(x=c(1,1), y=c(.08,.09)) + 
  geom_path(x=c(1,2), y=c(.09,.09)) + 
  geom_path(x=c(2,2), y=c(.09,.08)) + 
  annotate("text", x=1.5,y=.095, label="***") + 
  ggtitle("Mean SVO score for cooparation(0) and \n no cooperation(1)") + 
  xlab("Cooperation") + 
  ylab("mean svo score") + 
  theme(plot.title=element_text(face="bold"))

## After looking at some the dichotomized data we will now graph the continuous variable data

mean_consume <- ddply(Pdata, .(partition), summarize, mean.c=mean(consume))

ggplot(mean_consume, aes(x=factor(partition), y=mean.c)) + 
  geom_bar(stat="identity", position="dodge", fill="salmon") + 
  scale_y_continuous(limits=c(0, 40), breaks=seq(0,40,10)) + 
  geom_path(x=c(1,1), y=c(30,32)) + 
  geom_path(x=c(1,2), y=c(32,32)) + 
  geom_path(x=c(2,2), y=c(32,30)) + 
  annotate("text", x=1.5,y=34, label="**") + 
  ggtitle("Mean consumption for partition(1) and \n no partition(-1)") + 
  xlab("Partition") + 
  ylab("mean consumption") + 
  theme(plot.title=element_text(face="bold"))

mean_svo <- ddply(Pdata, .(svo_mean, partition), summarize, mean=mean(consume))

ggplot(mean_svo, aes(y=mean, x=svo_mean, colour=factor(partition))) + 
  geom_line() + 
  ggtitle("Mean consumption for SVO") + 
  xlab("linear transformation of SVO ") + 
  ylab("Mean consumption for respective SVO") + 
  theme(plot.title=element_text(face="bold")) + 
  scale_colour_discrete(name="Partition", breaks=c("-1", "1"), labels=c("No partition", "Partition")) +
  geom_smooth(method=lm)

ggplot(mean_svo, aes(y=mean, x=svo_mean, colour=factor(partition))) + 
  geom_point(shape=1) + 
  ggtitle("Mean consumption for SVO") + 
  xlab("linear transformation of SVO ") + 
  ylab("Mean consumption for respective SVO") + 
  theme(plot.title=element_text(face="bold")) + 
  scale_colour_discrete(name="Partition", breaks=c("-1", "1"), labels=c("No partition", "Partition")) +geom_smooth(method=lm)

## Analyzing the data

summary(glm(Pdata$coop ~ Pdata$partition*Pdata$svo_mean, family=binomial))

summary(glm(Pdata$consume ~ Pdata$partition*Pdata$svo_mean))

write.csv(Pdata, file="Default svo Data after Analysis.csv")