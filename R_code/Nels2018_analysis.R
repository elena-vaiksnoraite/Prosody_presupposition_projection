setwd('/Users/elenavaiksnoraite/Dropbox/Research_Projects/NSF_Project/')
library(ggplot2)
library(tidyverse)
library(xtable)
library(dplyr)
library(lme4)
library(grid)
library(lme4)
library(tidyr)
library(dplyr)
library(plotly)
library(reshape2)
library(lmerTest)
library(languageR)
library(tidyverse)
library(wesanderson)
source("helpers.R")

# Read into the data frame 
data7 = read.csv("data7.csv", header=T, sep = ",")
summary(data7)


############### ANALYSES USED IN NELS

## Normalized duration of the last content word

m_lcw_duration_norm <- lmer(lcw_dur_norm ~ cond + PA_LCW_Movement + (1| talker) + (1| utt) , data = data7)
summary(m_lcw_duration_norm)


## Mean utterance F0 (centered)
data7$F0mean_c<-scale(data7$Utterance_mean, center=TRUE, scale=TRUE)
summary(data7)

m_Utt_mean_c <- lmer(F0mean_c ~ cond + gender + PA_LCW_Height + PA_PRED_Height  + (1| talker) + (1| utt) , data = data7)
summary(m_Utt_mean_c)


# Pitch accent on the last content word

data7$lcw_PA_Focus <- ifelse(data7$lcw_PA == "H*" | data7$lcw_PA == "L+H*", "1",
                             "0")
data7$lcw_PA_Focus <- as.factor(data7$lcw_PA_Focus)

lcw_Focus <- glmer(lcw_PA_Focus ~ cond + (1| talker) + (1| utt), family = "binomial", data = data7)
summary(lcw_Focus)


############### GRAPHS USED IN NELS

# 1. Duration of the last content word
agr = aggregate(lcw_dur_norm ~ cond , data=data7, FUN="mean")

lcw_dur_graph = ggplot(data7, aes(cond,lcw_dur_norm, fill=cond)) + geom_violin() + theme_bw(base_size=12) 
lcw_dur_graph = lcw_dur_graph + labs( y = "Normalized last content word duration", x = "Condition", fill = "Condition") + scale_fill_manual(values = wes_palette(n=2, name = "GrandBudapest1")) + guides(fill=FALSE) + scale_x_discrete(labels=c("c" = "projecting", "nc" = "non-projecting")) + theme(axis.text=element_text(size=14,face="bold"),
                                                                                                                                                                                                                                                                                                       axis.title=element_text(size=16,face="bold"))

lcw_dur_graph = lcw_dur_graph + stat_summary(fun.y=mean, geom="point")

#Add median and quartile
lcw_dur_graph= lcw_dur_graph + geom_boxplot(width=0.1)

errbar_lims <- group_by(data7, cond) %>% 
  summarize(mean=mean(lcw_dur_norm), se=sd(lcw_dur_norm)/sqrt(n()), 
            upper=mean+se, lower=mean-se)

mean_se_violin <- ggplot() +
  geom_violin(data=data7, aes(x=cond, y=lcw_dur_norm)) +
  geom_point(data=errbar_lims, aes(x=cond, y=mean), size=3) +
  geom_errorbar(aes(x=errbar_lims$cond, ymax=errbar_lims$upper, 
                    ymin=errbar_lims$lower), stat='identity', width=.25) +
  theme_minimal()

mean_se_violin = mean_se_violin + theme_bw(base_size=12) 
mean_se_violin = mean_se_violin + labs( y = "Normalized duration of the last content word", x = "Condition") + guides(fill=FALSE) + scale_x_discrete(labels=c("c" = "projecting", "nc" = "non-projecting")) + theme(axis.text=element_text(size=20,face="bold"),
                                                                                                                                                                                                                    axis.title=element_text(size=20,face="bold"))

print(mean_se_violin)




# 3. F0 mean


f0_mean_graph <- ggplot(data7, aes(gender, F0mean_c, fill=cond)) + geom_violin() + theme_bw(base_size=12)
f0_mean_graph = f0_mean_graph + labs( y = "z-score normalized mean f0", x = "Gender") + scale_x_discrete(labels=c("male" = "male", "female" = "female")) + theme(axis.text=element_text(size=20,face="bold"),axis.title=element_text(size=20,face="bold"))
f0_mean_graph = f0_mean_graph + scale_fill_grey(name = "Condition", labels = c("Projecting", "Non-projecting"))
f0_mean_graph = f0_mean_graph + theme(plot.title = element_text(size = 30, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=20))
f0_mean_graph = f0_mean_graph + stat_summary(fun.y=mean, geom="point", shape=23, size=2, color = "white", position=position_dodge(0.9))


