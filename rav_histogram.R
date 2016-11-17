library(ggplot2)
library(Rmisc)
library(extrafont)
library(extrafontdb)

evangelical <- filter(rav, q66_1 == 1)
fundie <-filter(rav, q66_2 == 1)
liberal <- filter(rav, q66_3 ==1)
orthodox <- filter(rav, q66_4 ==1)
ecumenical <- filter(rav, q66_5 ==1)
pentecostal <- filter(rav, q66_6 ==1)
conservative <- filter(rav, q66_7 ==1)
emergent <- filter(rav, q66_8 ==1)

CI(evangelical$rav)
CI(fundie$rav)
CI(liberal$rav)
CI(orthodox$rav)
CI(ecumenical$rav)
CI(pentecostal$rav)
CI(conservative$rav)
CI(emergent$rav)

rav <- read.csv("rav.csv")

limits <- aes(ymax = rav$hi, ymin= rav$lo)

rav <- filter(df, rav > 0)
ggplot(rav, aes(x=reorder(factor, mean), y=mean)) + geom_bar(stat = "identity") + coord_flip() + geom_errorbar(limits, width=0.30 ) + xlab("Religious Label") + ylab("Religious Authority") + theme(text=element_text(size=16, family="Garamond")) 

ggplot(df %>% filter(rav > 0), aes(x=relcon, y=rav)) + geom_point(shape =1, position=position_jitter(width=.1,height=.1)) + geom_smooth(method=lm) + xlab("Religious Conservative") + ylab("Religious Authority") + theme(text=element_text(size=16, family="Garamond")) 

delib <- read.csv("delib.csv")
limits <- aes(ymax = delib$hi, ymin= delib$lo)
ggplot(delib, aes(x=reorder(factor, mean), y=mean)) + geom_bar(stat = "identity") + coord_flip() + geom_errorbar(limits, width=0.30 ) + xlab("Religious Label") + ylab("Deliberative Values") + theme(text=element_text(size=16, family="Garamond")) 





