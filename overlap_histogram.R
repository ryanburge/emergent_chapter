library(haven)
library(psy)
library(ggplot2)
#library(dplyr)
library(scales)
library(car)
library(Rmisc)


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



