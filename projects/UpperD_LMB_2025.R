library(car)
library(sciplot)
library(ggplot2)
library(gdata)
library(gtools)
library(MASS)
library(ez)
library(dplyr)
library(reshape2)
library(devtools)
library(roxygen2)
library(usethis)
library(BassPackage)

ltm1 = read.csv('UD_LMB_2025.csv', na.strings = '.')
ltm1$log_tl = log10(ltm1$TL..mm.)
ltm1$log_ws = -5.528 + (3.273 * ltm1$log_tl)
ltm1$WS = 10^ltm1$log_ws
ltm1$WR = round(100 * (ltm1$Wt..mm./ltm1$WS), 2)
head(ltm1)

unique(ltm1$Date)

lmb = data.frame(Lake = ltm1$Waterbody, Basin = ltm1$Sub.Basin, Year = ltm1$Year, Tran = ltm1$Site, Effort = ltm1$Effort, TL = ltm1$TL..mm., WT = ltm1$Wt..mm., WR = ltm1$WR, Count = 1)
head(lmb)

#write.csv(lmb, file = 'lmb_2025.csv', row.names = F)
wr = lmb
wr$Keep = ifelse(wr$TL <= 150 | wr$WR <= 60, "Remove", "Keep")
wr = wr[wr$Keep == "Keep",]
head(wr)

round(tapply(wr$WR, list(wr$Year), mean, na.rm = T), 1)
round(tapply(wr$WR, list(wr$Year), FUN = se, na.rm = T), 1)


############ CPUE All LMB

#sum
tapply(lmb$Count, lmb$Year, sum)

## Fish/min

head(lmb)

cpen = lmb %>% group_by(Lake, Year, Tran, Effort) %>% summarise(sum(Count))
names(cpen)[5] = "Count"
cpen$CPUE=(cpen$Count/(cpen$Effort/60))
head(cpen)

round(tapply(cpen$CPUE, list(cpen$Year), mean, na.rm= T), 2)
round(tapply(cpen$CPUE, cpen$Year, FUN = se), 2)

## Grams/min

cpew = lmb %>% group_by(Lake, Year, Tran, Effort) %>% summarise(sum(WT))
names(cpew)[5] = "WT"
cpew$CPUE=(cpew$WT/(cpew$Effort/60))
head(cpew)

round(tapply(cpew$CPUE, list(cpew$Year), mean, na.rm= T), 2)
round(tapply(cpew$CPUE, cpew$Year, FUN = se), 2)

###### Length Mectrics

head(lmb)

round(tapply(lmb$TL, lmb$Year, mean, na.rm = T), 2)
round(tapply(lmb$TL, lmb$Year, median, na.rm = T), 2)
round(tapply(lmb$TL, lmb$Year, max, na.rm = T), 2)
round(tapply(lmb$TL, lmb$Year, min, na.rm = T), 2)

###### Stock Denisty 

## psd = (number >= minimum quality length / number >=  minimum stock length) * 100
## rsd = same just change the numerator to the size group you are concerned about

head(lmb)

lmb$Stock = ifelse(lmb$TL >= 610, 'Trophy', 
                   ifelse(lmb$TL >= 510 & lmb$TL < 610, 'Memorable',
                          ifelse(lmb$TL >= 380 & lmb$TL < 510, 'Prefered',
                                 ifelse(lmb$TL >= 300 & lmb$TL < 380, 'Quality', 
                                        ifelse(lmb$TL >= 200 & lmb$TL <300, 'Stock', 'Less')))))
head(lmb)

#### calculate in Excel

table(lmb$Year, lmb$Stock)

table(lmb$LG, lmb$Year)
head(lmb)

lmbg = ggplot(data = lmb, aes(x=Year, y=LG)) +
  geom_bar(colour="black", stat="identity") +
  facet_grid(. ~ Sample_f) + # select long panel type grid or wrap
  #geom_line(size = 1.2) + # line size
  #geom_point(size = 2) +  # point size
  theme_bw(base_size = 18)+ # base text size
  theme(panel.margin = unit(0.0, 'lines'))+ # facet gap size
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),line = element_line(size = 2)) + # remove grid lines, remove grid lines, increase border thickness
  theme(panel.border = element_rect(colour = "black", size = 2), strip.background = element_rect(size = 2, colour = 'black')) + # change border to black and increase size, increase row label border thickness and color
  theme(strip.text.y = element_text(face = 'bold', angle = 360), axis.text = element_text(size = 12)) + # row label text bold and rotate, adjust axis text size
  theme(axis.title.y = element_text(size = 20, face = 'bold'), axis.title.x = element_text(size = 20, face = 'bold'), plot.title = element_text(face = "bold"))+ # adjust axis label size and face X3 
  #scale_x_continuous(breaks = c(0.29, 0.38, 0.46, 0.54), labels = c('7:00', '9:00', '11:00', '1:00')) + # set tick locations and labels
  #scale_x_continuous(breaks = c(0.29, 0.33, 0.38, 0.42, 0.46, 0.5, 0.54, 0.58), labels = c('7:00', '8:00', '9:00', '10:00', '11:00', '12:00', '1:00', '2:00')) + # set tick locations and labels
  #scale_y_continuous(limits = c(70, 90), breaks = c(72, 77, 82, 87)) + # set tick locations and labels
  #scale_x_continuous(breaks = c(0.29, 0.44, 0.58), labels = c('7:00', '10:30', '2:00')) + # set tick locations and labels
  #scale_y_continuous(breaks = seq(70, 95, 3)) + # set tick locations and labels
  ggtitle("Temperature by Time for Boaters and the Environment") + # graph title
  labs(x = "", y = 'Temperature ?F') + # x and y labels
  #theme(legend.position="none")  # No legend (redundant in this graph)
#ggsave('lmb.png', width= 20, height = 11, dpi = 100) # save high resolution image
windows()
lmbg





