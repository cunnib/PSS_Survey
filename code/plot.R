#### plot.R ###
# Purpose: Visualize differences btw 2017 and 2019 by job class
# Author: Brandon Cunningham (brandon.cunningham.20@gmail.com)
#####################

library(data.table)
library(XML)
library(ggplot2)
setwd("C:/Users/brand/Documents/PSS_Survey/")

comp <- fread(paste0("data/comparison_17_19.csv"))

comp$uw_market_50th_tcc_2017 <- gsub("-", "-", comp$uw_market_50th_tcc_2017)
comp$uw_market_50th_tcc_2019 <- gsub("-", "-", comp$uw_market_50th_tcc_2019)
comp$uw_market_50th_tcc_2017 <- gsub("%", "", comp$uw_market_50th_tcc_2017)
comp$uw_market_50th_tcc_2019 <- gsub("%", "", comp$uw_market_50th_tcc_2019)
comp[,uw_market_50th_tcc_2017 := as.numeric(as.character(uw_market_50th_tcc_2017))]
comp[,uw_market_50th_tcc_2019 := as.numeric(as.character(uw_market_50th_tcc_2019))]
bound <- max ( abs(min(comp$uw_market_50th_tcc_2017, na.rm=T)),
               abs(min(comp$uw_market_50th_tcc_2019, na.rm=T)), 
               abs(max(comp$uw_market_50th_tcc_2017, na.rm=T)),
               abs(max(comp$uw_market_50th_tcc_2019, na.rm=T)) )
lab = (bound %/% 10) * 10
labs <- seq(-1 * lab, lab, by = 5)

ggplot(data = comp, 
       aes(x = uw_market_50th_tcc_2017, y = uw_market_50th_tcc_2019, 
           color = job_category, size = uw_ees_2019)) +
  xlab("2017 UW - Market") + ylab("2019 UW - Market") +
  ylim((-1*bound), bound) + xlim((-1*bound), bound) +
  # scale_x_discrete("2017 UW - Market", labs, labs, c(-1*bound), bound) +
  # scale_y_discrete("2019 UW - Market", labs, labs, c(-1*bound), bound) +
  geom_point() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_abline(linetype = "dashed", alpha = 0.3, color = "black") + 
  ggtitle("Percentage difference between UW and Market Rates by Job Title -- 2017 vs 2019") +
  guides(color=guide_legend(title="Job Category")) +
  guides(size=guide_legend(title="Survey Sample Size")) + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

categories <- fread("data/comparison_categories.csv")
ggplot() +
  # add a line segment that goes from men to women for each discipline
  geom_segment(data = categories, aes(x = 1, xend = 2, 
                   y = y2017, 
                   yend = y2019,
                   group = category,
                   col = category), 
               size = 1.2) +
  # set the colors
  #scale_color_manual(values = c("#468189", "#9DBEBB"), guide = "none")  +
  # remove all axis stuff
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  # add the label and success rate for each discipline next the men axis
  geom_text(data = categories, aes(x = 1 - 0.03, 
                y = y2017, 
                label = y2017),
            col = "grey30", hjust = "right") +
  # add the success rate next to each point on the women axis
  geom_text(data = categories, aes(x = 2 + 0.08, 
                y = y2019, 
                label = y2019),
            col = "grey30") +
  # set the limits of the x-axis so that the labels are not cut off
  scale_x_continuous(limits = c(0.5, 2.1)) 

##Reshape wide
categories <- melt(categories, id.vars='category')
categories$variable <- gsub("y", "", categories$variable)

ggplot(categories, aes(category, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +  
  xlab("Job Category") + ylab("Percantage Difference, UW - Market") +
  ggtitle("Percentage difference between UW and Market Rates by Job Category") +
  geom_hline(yintercept = 0) +
  guides(fill=guide_legend(title="Survey Year")) + theme_bw() + 
  #geom_text(aes(label=value, fill=variable), vjust=0) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

