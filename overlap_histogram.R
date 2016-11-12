library(haven)
library(psy)
library(ggplot2)
#library(dplyr)
library(scales)
library(car)


df <- read_dta("clergy.dta")

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
df[is.nan(df)] <- 0

##Making the relcon measure
df$relcon <- df$q68_1 + df$q68_2 + df$q68_3 + df$q68_4 + df$q68_5 + df$q68_6
df$relcon <- 31 - df$relcon
df$relcon <- recode(df$relcon, "31=30")
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
df$dem <- recode(df$dem, "14=0")

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



