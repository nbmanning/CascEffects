# Title: 2_4_SI_figure_USBR_ExchangeRate.R
# Author: Nick Manning
# Purpose: Plot US (USD) and Brazil (BR Real) Exchange Rate over time
# Creation Date: 10/18/23
# Last Updated: October 2023


# 0: Load Libraries & Constants ---------
library(dplyr)
library(lubridate)
library(ggplot2)


# 1: Load & Clean USD / BR Real Exchange Rate CSV ---------
getwd()
t <- read.csv("../Data_Source/ExchangeRate_1USDtoBRR_20002017.csv")

# filter 
t2 <- t %>% 
  select(Date, Price) %>%
  mutate(Date = mdy(Date),
         Price = as.numeric(Price)) %>% 
  filter(year(Date) >= 2000,  year(Date) <= 2018)

str(t2)
# 2: Plot --------
# plot 
ggplot(t2, aes(x = Date, y = Price))+
  #geom_point()+
  geom_line()+
  scale_x_date(date_breaks = "year",
               date_labels = "%Y")+
  theme_bw()+
  geom_vline(aes(xintercept = date("2012-07-01")), color = "red",
             linetype="dashed", linewidth=0.5)+
  labs(
    title = "Exchange Rate between Brazilian Real and US Dollar",
    y = "Real : Dollar Rate",
    x = "")
  

# save
ggsave("../Figures/ExchangeRate_USBR_20002018.png", 
       width = 8, height = 4)
