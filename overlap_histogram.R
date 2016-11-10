df <- read_dta("clergy.dta")
df$relcon <- df$q68_1 + df$q68_2 + df$q68_3 + df$q68_4 + df$q68_5 + df$q68_6
ggplot(df, aes(relcon)) +geom_bar()
install.packages("psy")
cronbach(df[,331:336])

emergent <- filter(df, q25 == 1 | q25==2)

ggplot(emergent, aes(relcon)) +geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent)


ggplot(df, aes(relcon)) + geom_bar(aes(y = (..count..)/sum(..count..)), data = emergent, fill = "white", color = "gray", alpha = 0.6) +  geom_bar(aes(y = (..count..)/sum(..count..)), data = n_emergent, fill = "gray",  alpha = 0.6) + scale_y_continuous(labels=percent) + ylab("Percent of Sample") + xlab("Religious Conservatism Scale") + theme(text=element_text(size=16, family="Roboto")) 


ggplot(df.plot,aes(x=relcon/30, y = (..count..)/sum(..count..)))+ 
  geom_histogram(data=subset(df.plot,label=='Emergent'),aes(fill=label),alpha=0.6)+
  geom_histogram(data=subset(df.plot,label=='Entire Sample'),aes(fill=label),alpha=0.6)+
  scale_fill_manual(name="label", values=c("black","gray"),labels=c("Non-Emergent","Emergent")) +  scale_y_continuous(labels=percent) + ylab("Percent of Sample") + xlab("Religious Conservatism Scale") + theme(text=element_text(size=16, family="Roboto")) +    theme(legend.position="bottom") + theme(legend.title = element_blank())