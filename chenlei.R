
D = read.csv('V:/DNOX_v2/L2/csv/records/2018-07-07_1_Morning.csv')

D$Time = lubridate::ymd_hms(D$Time)

D$Seg = cut(D$Time, '5 mins')

library(dplyr)

S = D  %>% group_by(MAE, Seg) %>% summarise(N = n())

library(ggplot2)

S$MAE = as.character(S$MAE)

P = ggplot(S, aes(x = Seg, y = N, color = MAE, group = MAE)) + 
  geom_step() +
  geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~MAE, scales = 'free', ncol = 1)
P

png('C:/daten/chenlei.png', height = 9000, width = 1000, res = 90)
P
dev.off()
