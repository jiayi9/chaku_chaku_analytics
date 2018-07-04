


D = read.csv('V:/DNOX_v2/L2/csv/daily_summary/2018-06-28_summary_L2.csv')



head(D)

D$MAE = as.character(D$MAE)

library(dplyr)
library(ggplot2)

D2 = D %>% filter(MAE %in% c(10.2, 20, 26.1, 30, 40, 50)) %>% select(MAE, Time, Ts, Tmin)


head(D2)
D2$Time = lubridate::ymd_hms(D2$Time)

D2$Seg = cut(D2$Time, '20 min')

table(D2$Seg)

S = D2 %>% group_by(MAE, Seg) %>% dplyr::summarise(MEAN = mean(Ts), N = n(), MEAN_2 = mean(Tmin))

ggplot(data = S, aes(x = Seg, y = MAE)) + 
  geom_point(aes(color = MEAN, size = MEAN))+#,colour = "white") + 
  scale_color_gradient(low = "white", high = "red", name = 'Cycle time[s]') +
  ylab('Station') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

S = S %>% arrange(MAE, Seg)

ggplot(data = S, aes(x = Seg, y = MEAN, color = MAE, group = MAE)) + 
  geom_point() +#,colour = "white") +
  geom_path() +
  ylab('Cycle time') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = 50, linetype = 'dashed')

ggplot(data = S, aes(x = Seg, y = MEAN_2, color = MAE, group = MAE)) + 
  geom_point() +#,colour = "white") +
  geom_path() +
  ylab('Conditional Cycle time') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = 50, linetype = 'dashed')
