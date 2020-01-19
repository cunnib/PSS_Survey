#### plot.R ###
# Purpose: Visualize differences btw 2017 and 2019 by job class
# Author: Brandon Cunningham (brandon.cunningham.20@gmail.com)
#####################

library(data.table)
library(XML)
library(ggplot2)
setwd("C:/Users/brand/Documents/PSS_Survey/")

comp <- fread(paste0("data/comparison_17_19.csv"))
comp$title <- gsub("â???"", "-", comp$title)

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
  geom_abline(linetype = "dashed", alpha = 0.3, color = "black") + theme_bw() + 
  ggtitle("Percentage difference between UW and Market Rates by Job Title -- 2017 vs 2019") +
  guides(color=guide_legend(title="Job Category")) +
  guides(size=guide_legend(title="Survey Sample Size")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
                    