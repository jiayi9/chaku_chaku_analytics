setwd('C:/DNOX_buffer/NEW_BASE_2/')

START = '2018-07-07 07:30:00'
END = '2018-07-07 14:30:00'

source('connection.R', local = TRUE)
source('functions.R', local = TRUE)

MES = read.csv('V:/DNOX_v2/L2/csv/records/2018-07-07_1_Morning.csv') %>%
  mutate(Time = ymd_hms(Time)) %>%
  filter(MAE == '26.1') %>%
  filter(Time > ymd_hms(START) & Time < ymd_hms(END))

head(RAW)
head(MES)

#----------------------------------------------------------------------

RAW_OP_1_EVENTS = c("SM26 Buffer_1", 
                "SM10 Buffer_1", 
                "SM10 Buffer_2", 
                "SM10 Start button", 
                "SM20 Buffer_1", 
                "SM20 Start Button")

X1 = RAW %>% filter(Buffer_Name %in% RAW_OP_1_EVENTS) %>% 
  mutate(SIGNAL = paste(Buffer_Name, Station_Status)) %>% 
  filter(!SIGNAL %in% c('SM26 Buffer_1 0', 'SM10 Start button 0', 'SM20 Start Button 0')) %>%
  arrange(Date_Time) %>% 
  select(SIGNAL = SIGNAL, Time = Date_Time)

X2 = data.frame(SIGNAL = 'SM26_MES_DONE', Time = MES$Time)

D = rbind(X1, X2) %>% arrange(Time)

print(sort(unique(D$SIGNAL)))

#-----------------------------------------------------------------------

ANCHOR = 'SM26 Buffer_1 1'
ANCHOR_INDEX = with(D, which(SIGNAL == ANCHOR))
START_INDEX = ANCHOR_INDEX[1]
END_INDEX = ANCHOR_INDEX[length(ANCHOR_INDEX)]
D_trim = D[START_INDEX:END_INDEX, ]

#-----------------------------------------------------------------------

DATA = D_trim
N = nrow(DATA)
L = list()

TMP = data.frame(SIGNAL = ANCHOR, TIME = DATA$Time[1])

#--- find loop by anchor 
for(i in 2:N){
  if(DATA[i,'SIGNAL'] != ANCHOR){
    NEW_ROW = data.frame(SIGNAL = DATA$SIGNAL[i], TIME = DATA$Time[i])
    TMP = rbind(TMP, NEW_ROW)
  } else{
    NEW_ROW = data.frame(SIGNAL = DATA$SIGNAL[i], TIME = DATA$Time[i])
    TMP = rbind(TMP, NEW_ROW)
    L = rlist::list.append(L, TMP)
    TMP = data.frame(SIGNAL = DATA$SIGNAL[i], TIME = DATA$Time[i])
    
  }
}

#--- delete irregular loops (output: L2)
L_LEN = length(L)
L2 = list()
for(i in 1:L_LEN){
  NROW = nrow(L[[i]]) 
  if(NROW >= 10 & NROW <= 20){
    L2 = rlist::list.append(L2, L[[i]])
  }
}


#--- piecewise timing
LL = lapply(L2, function(df){
  
  SIGNAL = df$SIGNAL
  TIME = df$TIME
  
  SIGNAL = df$SIGNAL
  TIME = df$TIME
  START = TIME[1]
  END = TIME[length(TIME)]
  FIRST_MOVEMENT = TIME[2]
  SM10_START = TIME[SIGNAL == 'SM10 Start button 1'][1]
  SM20_START = TIME[SIGNAL == 'SM20 Start Button 1'][1]
  SM26_MES_DONE = TIME[SIGNAL == 'SM26_MES_DONE'][1]

  Unloading26_to_SM10Buffer = as.numeric(as.duration(FIRST_MOVEMENT - START))
  SM10Buffer_to_SM10Start = as.numeric(as.duration(SM10_START - FIRST_MOVEMENT))
  SM10Start_to_SM20Start = as.numeric(as.duration(SM20_START - SM10_START))
  SM20Start_to_SM26Done = as.numeric(as.duration(SM26_MES_DONE - SM20_START))
  SM26Done_to_Unloading26 = as.numeric(as.duration(END - SM26_MES_DONE))
  TOTAL = as.numeric(as.duration(END - START))
  
  R = data.frame(
    Unloading26_to_SM10Buffer,
    SM10Buffer_to_SM10Start,
    SM10Start_to_SM20Start,
    SM20Start_to_SM26Done,
    SM26Done_to_Unloading26,
    TOTAL
  ) 
  R
})



DDD = do.call(rbind, LL)

DDD_2 = na.omit(DDD)


library(reshape2)

MELT = melt(DDD_2)

S = MELT %>% group_by(variable) %>% summarise(
  MEAN = round(mean(value),1),
  MEDIAN = round(median(value),1)
) %>% mutate(
  MEAN_LABEL = paste0('Mean: ', MEAN,'s'),
  MEDIAN_LABEL = paste0('Median: ', MEDIAN,'s')
)

ggplot(MELT, aes(x = value)) + 
  geom_histogram(fill = 'lightblue', color = 'grey', alpha = 1, bins = 200) + 
  facet_wrap(~variable, ncol=1, scales = 'free_y') + 
  xlim(0,100) +
  geom_text(data = S, aes(label = MEAN_LABEL), x= Inf, y = Inf, hjust = 1, vjust = 3,color = 'orange') +
  geom_text(data = S, aes(label = MEDIAN_LABEL), x= Inf, y = Inf, hjust = 1, vjust = 5, color = 'blue') +
  geom_vline(data = S, aes(xintercept = MEAN), linetype ='dashed', color = 'orange', size = 1) +
  geom_vline(data = S, aes(xintercept = MEDIAN), linetype ='dashed', color = 'blue', size = 1)
