#ecm <- filter(df, q66_8 ==1 | q24 == 1)
#not.ecm <- filter(df, q66_8 !=1 & q24 != 1)
#not.ecm <- filter(not.ecm, q68_1 >0)

ecm <- filter(df, q66_8 ==1)
not.ecm <- filter(df, q66_8 !=1)
not.ecm <- filter(not.ecm, q68_1 >0)


mean(ecm$q68_1)
mean(not.ecm$q68_1)
mean(ecm$q68_2)
mean(not.ecm$q68_2)
mean(ecm$q68_3)
mean(not.ecm$q68_3)
mean(ecm$q68_4)
mean(not.ecm$q68_4)
mean(ecm$q68_5)
mean(not.ecm$q68_5)
mean(ecm$q68_6)
mean(not.ecm$q68_6)

dotplot <- read.csv("dotplot.csv")
dotplot$mean <- 6- dotplot$mean

dotplot$question <- factor(dotplot$question, levels=unique(dotplot$question))


p1 <- ggplot(dotplot, aes(x = mean, y = question))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(label))) + 
  scale_fill_manual(values=c("gray", "black")) +  theme(legend.title=element_blank()) + 
  theme(legend.position = "none") +xlab("Mean Score") + ylab("Theology Questions") + xlim(1,5) + theme(text=element_text(size=16, family="Garamond"))



mean(ecm$q47_1)
mean(not.ecm$q47_1)
mean(ecm$q47_2)
mean(not.ecm$q47_2)
mean(ecm$q47_3)
mean(not.ecm$q47_3)
mean(ecm$q47_4)
mean(not.ecm$q47_4)
mean(ecm$q47_5)
mean(not.ecm$q47_5)

delibplot <- read.csv("delibdot.csv")
delibplot$mean <- 6- delibplot$mean

delibplot$question <- factor(delibplot$question, levels=unique(delibplot$question))

p2 <- ggplot(delibplot, aes(x = mean, y = question))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(label))) +  theme(legend.title=element_blank()) + 
  theme(legend.position = "bottom") +xlab("Mean Score") + ylab("Deliberative Values") + xlim(1,5) + theme(text=element_text(size=16, family="Garamond")) 
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))



