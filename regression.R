##Recoding the ECM Approval variable
df$ecm_sup <- df$q25
df$ecm_sup <- 6- df$ecm_sup
ggplot(df, aes(ecm_sup)) +geom_bar()


## Recoding controls
df$male <- recode(df$q62, "1=1; else=0")
df$white <- recode(df$q65_1, "1=1; else=0")
df$repubid <- recode(df$q63, "8=0")
df$repubid <- df$repubid/max(df$repubid)


df$years <- gsub('years', '', df$q69_1_text)
df$years <- gsub('year', '', df$years)
df$years <- gsub('yrs', '', df$years)
df$years <- gsub('YEARS', '', df$years)
df$years <- recode(df$years, "1984=30")
df$years <- gsub('\\+', '', df$years)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
df$years <- trim(df$years)
df$years <- as.numeric(df$years)
df$years <- df$years/70

df$educ <- df$q71 -1
df$educ <- recode(df$educ, "-1=0")
df$educ <- df$educ/max(df$educ)

df$size <- df$q52
df$size <- df$size/max(df$size)


## Regression and dotwhisker

library(dotwhisker)
reg1 <- lm(ecm_sup ~ male + white + relcon + years + educ + size + repubid + rav, data =df)

dwplot(reg1)  + geom_vline(xintercept = 0, colour = "grey50", linetype = 2)

dwplot(reg1)  + geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  relabel_y_axis(c("Male", "White", "Religious Conservative", 
                   "Years in Ministry", "Education", "Size of community", "Republican ID", "Religious Authority")) + xlab("Predicting Support for the ECM")

