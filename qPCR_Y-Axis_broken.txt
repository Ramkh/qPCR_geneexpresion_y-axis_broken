library(ggplot2)
library(ggExtra)
library(grid)
library(reshape2)
library(ggplot2)
library(Rmisc)
library(plyr)
#dat1 <-read.table(file="clipboard", sep = "\t", header=TRUE)

dat<-read.table(file = "clipboard", sep = "\t", header=TRUE)
attach(dat)
dat
names(dat)
#for PINI
trans <- function(x){pmin(x,5) + 0.08*pmax(x-8,0)}
yticks <- c(0, 1, 2,10, 20, 30, 40,50)

tgc <- summarySE(dat, measurevar= "PINI.1", groupvars=c("Treatment"),na.rm = T)
tgc
tgc$mean_t <- trans(tgc$PINI.1)
tgc$sd_up_t <- trans(tgc$PINI.1 + tgc$se)
tgc$sd_low_t <- pmax(trans(tgc$PINI.1 - tgc$se),1) 

o<- ggplot(data=tgc, aes(x=Treatment, y=mean_t, group=Treatment,fill=Treatment)) +
  geom_errorbar(aes(ymin=sd_low_t, ymax=sd_up_t),width=.2, position="dodge") + 
  geom_col(position="dodge") +
  geom_rect(aes(xmin=0.1, xmax=6.8, ymin=4, ymax=4.2), fill="white") +
  scale_y_continuous(limits=c(0,NA), breaks=trans(yticks), labels=yticks) +
  labs(y="PINI (fold)")

p<-o+theme(legend.position="none")+theme(axis.text.x = element_text("angle" = 90, vjust = 0.5, hjust = 1))
p
#for PINII
trans <- function(x){pmin(x,3) + 0.08*pmax(x-5,0)}
yticks <- c(0, 1, 2, 10, 20, 30, 40,50, 60, 70, 80, 90, 100)

tgc <- summarySE(dat, measurevar= "PINII.1", groupvars=c("Treatment"),na.rm = T)
tgc

tgc$mean_t <- trans(tgc$PINII.1)
tgc$sd_up_t <- trans(tgc$PINII.1 + tgc$se)
tgc$sd_low_t <- pmax(trans(tgc$PINII.1 - tgc$se),1) 

o<- ggplot(data=tgc, aes(x=Treatment, y=mean_t, group=Treatment,fill=Treatment)) +
  geom_errorbar(aes(ymin=sd_low_t, ymax=sd_up_t),width=.2, position="dodge") + 
  geom_col(position="dodge") +
  geom_rect(aes(xmin=0.1, xmax=6.8, ymin=3, ymax=3.5), fill="white") +
  scale_y_continuous(limits=c(0,NA), breaks=trans(yticks), labels=yticks) +
  labs(y="PINII (fold)")

p<-o+theme(legend.position="none")+theme(axis.text.x = element_text("angle" = 90, vjust = 0.5, hjust = 1))
p




#For PR1b 48 hr
trans <- function(x){pmin(x,2) + 0.05*pmax(x-1,0)}
yticks <- c(0, 1, 2, 50, 60,70,80,90,100)

tgc <- summarySE(dat, measurevar= "PR1b", groupvars=c("Treatment"),na.rm = T)
tgc

tgc$mean_t <- trans(tgc$PR1b)
tgc$sd_up_t <- trans(tgc$PR1b + tgc$se)
tgc$sd_low_t <- pmax(trans(tgc$PR1b - tgc$se),1) 

o<- ggplot(data=tgc, aes(x=Treatment, y=mean_t, group=Treatment,fill=Treatment)) +
  geom_errorbar(aes(ymin=sd_low_t, ymax=sd_up_t),width=.2, position="dodge") + 
  geom_col(position="dodge") +
  geom_rect(aes(xmin=0.1, xmax=6.8, ymin=3, ymax=3.5), fill="white") +
  scale_y_continuous(limits=c(0,NA), breaks=trans(yticks), labels=yticks) +
  labs(y="PR1b (fold)")

p<-o+theme(legend.position="none")+theme(axis.text.x = element_text("angle" = 90, vjust = 0.5, hjust = 1))
p


#for PR2b 48hr

trans <- function(x){pmin(x,2) + 0.05*pmax(x-1,0)}
yticks <- c(0, 1, 2,50,60, 70,80,90,100)

tgc <- summarySE(dat, measurevar= "PR2b", groupvars=c("Treatment"),na.rm = T)
tgc

tgc$mean_t <- trans(tgc$PR2b)
tgc$sd_up_t <- trans(tgc$PR2b + tgc$se)
tgc$sd_low_t <- pmax(trans(tgc$PR2b - tgc$se),1) 

o<- ggplot(data=tgc, aes(x=Treatment, y=mean_t, group=Treatment,fill=Treatment)) +
  geom_errorbar(aes(ymin=sd_low_t, ymax=sd_up_t),width=.2, position="dodge") + 
  geom_col(position="dodge") +
  geom_rect(aes(xmin=0.1, xmax=6.8, ymin=3, ymax=3.2), fill="white") +
  scale_y_continuous(limits=c(0,NA), breaks=trans(yticks), labels=yticks) +
  labs(y="PINII (fold)")

p<-o+theme(legend.position="none")+theme(axis.text.x = element_text("angle" = 90, vjust = 0.5, hjust = 1))
p




#For PR1b 24 hr
trans <- function(x){pmin(x,4) + 0.08*pmax(x-5,0)}
yticks <- c(0,1,2,3,4,20, 25, 30, 35, 40, 45,50, 55,60,65)

tgc <- summarySE(dat, measurevar= "PR1b", groupvars=c("Treatment"),na.rm = T)
tgc

tgc$mean_t <- trans(tgc$PR1b)
tgc$sd_up_t <- trans(tgc$PR1b + tgc$se)
tgc$sd_low_t <- pmax(trans(tgc$PR1b - tgc$se),1) 

o<- ggplot(data=tgc, aes(x=Treatment, y=mean_t, group=Treatment,fill=Treatment)) +
  geom_errorbar(aes(ymin=sd_low_t, ymax=sd_up_t),width=.2, position="dodge") + 
  geom_col(position="dodge") +
  geom_rect(aes(xmin=0.1, xmax=6.8, ymin=4, ymax=4.2), fill="white") +
  scale_y_continuous(limits=c(0,NA), breaks=trans(yticks), labels=yticks) +
  labs(y="PR1b (fold)")

p<-o+theme(legend.position="none")+theme(axis.text.x = element_text("angle" = 90, vjust = 0.5, hjust = 1))
p


#for PR2b 24hr

trans <- function(x){pmin(x,2) + 0.08*pmax(x-2,0)}
yticks <- c(0, 1, 2, 30, 35, 40, 45, 50, 55, 60, 65)


tgc <- summarySE(dat, measurevar= "PR2b", groupvars=c("Treatment"),na.rm = T)
tgc

tgc$mean_t <- trans(tgc$PR2b)
tgc$sd_up_t <- trans(tgc$PR2b + tgc$se)
tgc$sd_low_t <- pmax(trans(tgc$PR2b - tgc$se),1) 

o<- ggplot(data=tgc, aes(x=Treatment, y=mean_t, group=Treatment,fill=Treatment)) +
  geom_errorbar(aes(ymin=sd_low_t, ymax=sd_up_t),width=.2, position="dodge") + 
  geom_col(position="dodge") +
  geom_rect(aes(xmin=0.1, xmax=6.8, ymin=2, ymax=2.2), fill="white") +
  scale_y_continuous(limits=c(0,NA), breaks=trans(yticks), labels=yticks) +
  labs(y="PR2b (fold)")

p<-o+theme(legend.position="none")+theme(axis.text.x = element_text("angle" = 90, vjust = 0.5, hjust = 1))
p





tgc <- summarySE(dat, measurevar= "LoxA", groupvars=c("Treatment"),na.rm = T)
tgc
o <- ggplot(tgc, aes(Treatment, LoxA, fill=  Treatment)) + geom_bar(stat="identity", color="black", 
                                                                    position=position_dodge())

p <-o + geom_errorbar(aes(ymin=LoxA, ymax=LoxA+se), width=.2,
                      position=position_dodge(.9)) +labs(x="Treatments ", y="TomLoxA (fold)")+theme(legend.position="none")+theme(legend.position="none")+theme(axis.text.x = element_text("angle" = 90, vjust = 0.5, hjust = 1))

p










pd <- position_dodge(0.7) # move them .05 to the left and right


#Function to transform data to y positions
trans <- function(x){pmin(x,2) + 0.05*pmax(x-2,0)}

yticks <- c(0, 1, 2, 10,15,20,25,30)

#Transform the data onto the display scale
dat$mean_t <- trans(dat$value)
dat$sd_up_t <- trans(dat$value + dat$sd)
dat$sd_low_t <- pmax(trans(dat$value - dat$sd),1) #

png("test.png", units="in", family="Times",  width=6, height=4, res=300) #pointsize is font size| increase image size to see the key

ggplot(data=dat, aes(x=gene, y=mean_t, group=treatment, fill=treatment)) +
  geom_errorbar(aes(ymin=sd_low_t, ymax=sd_up_t),size=.5,  
                width=.2,
                position=position_dodge(0.7)) + 
geom_col(position="dodge") +
geom_rect(aes(xmin=0, xmax=4, ymin=2.2, ymax=2.3), fill="white") +
scale_y_continuous(limits=c(0,NA),expand = c(0,0), breaks=trans(yticks), labels=yticks) +
labs(y="Relative titer of CLas")+
theme_classic()  
