library(haven)
library(psy)
library(ggplot2)
#library(dplyr)
library(scales)
library(car)
library(Rmisc)


df <- read_dta("clergy.dta")

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
df[is.nan(df)] <- 0

##Making the relcon measure
df$relcon <- df$q68_1 + df$q68_2 + df$q68_3 + df$q68_4 + df$q68_5 + df$q68_6
df$relcon <- 31 - df$relcon
df$relcon <- Recode(df$relcon, "31=30")
df$relcon <- df$relcon/30
df$relcon <- round(df$relcon, 2)
ggplot(df, aes(relcon)) +geom_bar()

cronbach(df[,331:336])



##Making the democratic values measure
df$dem1 <- df$q40_1
df$dem2 <- df$q40_2
df$dem3 <- df$q40_3
df$dem4 <- df$q40_4


df$dem <- df$dem1 + df$dem2 + df$dem3 + df$dem4
df$dem <- 14 - df$dem
df$dem <- Recode(df$dem, "14=0")

df$dem <- df$dem/10


##Making the deliberative values measure
df$delib1 = 5- df$q47_1
df$delib2 = 5- df$q47_2
df$delib3 = 5- df$q47_3
df$delib4 = 5- df$q47_4
df$delib5 = 5- df$q47_5
df$delib <- df$delib1 + df$delib2 + df$delib3 + df$delib4 + df$delib5
df$delib <- df$delib/max(df$delib)


##Making the RAV measure

df$rav <- df$q26_2 + df$q26_3 + df$q26_5 + df$q26_7 + df$q26_9
df$rav <- df$rav/25
df <- filter(df, rav >0)

##Making inclusive vs exclusive values
df$incl1 <- df$q7_1  + df$q8_1 + df$q17_1 + df$q23_1
df$incl1 <- 6-df$incl1
df$incl1 <- Recode(df$incl1, "6=0")
df$incl2 <- df$q7_2  + df$q8_2 + df$q17_2 + df$q23_2
df$incl2 <- 6-df$incl2
df$incl2 <- Recode(df$incl2, "6=0")
df$incl3 <- df$incl1 + df$incl2

df$inclusive <- df$incl3/10

df$exc1 <- df$q7_3  + df$q12_3 + df$q13_1 + df$q23_3
df$exc1 <- 6 - df$exc1
df$exc1 <- Recode(df$exc1, "6=0")
df$exc2 <- df$q7_4  + df$q12_4 + df$q13_2 + df$q23_4
df$exc2 <- 6 - df$exc2
df$exc2 <- Recode(df$exc2, "6=0")
df$exc3 <- df$exc1 + df$exc2
df$exclusive <- df$exc3/10


##Authoritarianism

df$authority <- df$q30_1 + df$q30_2 + df$q30_3
df$authority <- df$authority/6

##Subsetting based on Approval of ECM
library(dplyr)

emergent <- filter(df, q25 == 1 | q25==2)
n_emergent <- filter(df, q25 ==3 | q25 ==4 | q25==5)

##Adding labels then merging together
emergent$label <- c("Emergent")
n_emergent$label <- c("Non Emergent")
df.plot <- rbind(n_emergent, emergent)

##Three attempts at the same plot
ggplot(emergent, aes(relcon)) +geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent)


ggplot(df, aes(relcon)) + geom_bar(aes(y = (..count..)/sum(..count..)), data = emergent, fill = "white", color = "gray", alpha = 0.6) +  geom_bar(aes(y = (..count..)/sum(..count..)), data = n_emergent, fill = "gray",  alpha = 0.6) + scale_y_continuous(labels=percent) + ylab("Percent of Sample") + xlab("Religious Conservatism Scale") + theme(text=element_text(size=16, family="Roboto")) 


ggplot(data=subset(df.plot, !is.na(relcon)), aes(x=factor(relcon), fill=label)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position="dodge") + theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels=percent) + ylab("Percent of Sample") + xlab("Religious Conservatism Scale") +
  theme(text=element_text(size=16, family="Roboto")) +    theme(legend.position="bottom") + 
  theme(legend.title = element_blank())



