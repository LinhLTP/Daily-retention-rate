setwd("/Users/admin/Documents/Linh-R Studio/38 user product launch test")

library(tidyverse)
library("readxl")
library("writexl")
library(tidyr)
library(dplyr)
library(lubridate)

### ------- DAU 
Aug30 <- read_excel("/Users/admin/Documents/Linh-R Studio/38 user product launch test/300821_usagetime.xlsx")
Active <- Aug30

Active <- Active %>% group_by(UserID,ActiveDate) %>% summarise(totalusage = sum(`Usage Time (minute)`))

Aug30_a <- Aug30 %>% select(UserID,RedeemedAt_text) 
Aug30_a <- Aug30_a[!duplicated(Aug30_a$UserID), ]

Active_m <- merge(Active,Aug30_a)

Active_m$RedeemedAt_text <- as.Date(Active_m$RedeemedAt_text)
Active_m <- Active_m %>% mutate(A0 = Active_m$RedeemedAt_text + 0)
Active_m <- Active_m %>% mutate(A1 = Active_m$RedeemedAt_text + 1)
Active_m <- Active_m %>% mutate(A7 = Active_m$RedeemedAt_text + 7)
Active_m <- Active_m %>% mutate(A14 = Active_m$RedeemedAt_text + 14)
Active_m <- Active_m %>% mutate(A30 = Active_m$RedeemedAt_text + 30)

Active_m <- Active_m %>% mutate(A0_True = if_else(A0 == ActiveDate,TRUE,FALSE))
Active_m <- Active_m %>% mutate(A1_True = if_else(A1 == ActiveDate,TRUE,FALSE))
Active_m <- Active_m %>% mutate(A7_True = if_else(A7 == ActiveDate,TRUE,FALSE))
Active_m <- Active_m %>% mutate(A14_True = if_else(A14 == ActiveDate,TRUE,FALSE))
Active_m <- Active_m %>% mutate(A30_True = if_else(A30 == ActiveDate,TRUE,FALSE))


Active_a0 <- Active_m %>% filter(A0_True == "TRUE")%>% group_by(RedeemedAt_text)%>% summarise(count_a0= n_distinct(UserID))
Active_a1 <- Active_m %>% filter(A1_True == "TRUE")%>% group_by(RedeemedAt_text)%>% summarise(count_a1 = n_distinct(UserID))
Active_a7 <- Active_m %>% filter(A7_True == "TRUE")%>% group_by(RedeemedAt_text)%>% summarise(count_a7 = n_distinct(UserID))
Active_a14 <- Active_m %>% filter(A14_True == "TRUE")%>% group_by(RedeemedAt_text)%>% summarise(count_14 = n_distinct(UserID))
Active_a30 <- Active_m %>% filter(A30_True == "TRUE")%>% group_by(RedeemedAt_text)%>% summarise(count_30 = n_distinct(UserID))

fullJoinDf <- full_join(Active_a0,Active_a1,by="RedeemedAt_text")
fullJoinDf <- full_join(fullJoinDf,Active_a7,by="RedeemedAt_text")
fullJoinDf <- full_join(fullJoinDf,Active_a14,by="RedeemedAt_text")
fullJoinDf <- full_join(fullJoinDf,Active_a30,by="RedeemedAt_text")

Active_a <- Active_m %>% group_by(RedeemedAt_text)%>% summarise(count = n_distinct(UserID))

fullJoinDf <- full_join(fullJoinDf,Active_a,by="RedeemedAt_text")

write_xlsx(fullJoinDf,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/fullJoinDf.xlsx")

### Daily Retention Calculation 
dat <- c(0:60)
dat <- data.frame(dat)

library(lubridate)
test <- Aug30
test$ActiveDate <- as.Date(test$ActiveDate)
test$RedeemedAt_text <- as.Date(test$RedeemedAt_text)
test1 <- test %>% mutate(diff = as.numeric(test$ActiveDate - test$RedeemedAt_text))
test2 <- test1 %>% select(RedeemedAt_text,UserID,diff)

test3 <- test2 %>% group_by(RedeemedAt_text,diff) %>% summarise(n=n_distinct(UserID))
test4 <- test3 %>% select(RedeemedAt_text)
test5 <- test4[!duplicated(test4$RedeemedAt_text),]
test6 <- merge(test5,dat)
test7 <- left_join(test6,test3, by = c("RedeemedAt_text" = "RedeemedAt_text", "dat" = "diff") )
test7$n[is.na(test7$n)] <- 0
result <- spread(test7, dat, n)

test8 <- test %>% group_by(RedeemedAt_text) %>% summarise(nru = n_distinct(UserID))

result <- merge(result, test8)

# calculate percentage for all columns 
result_pct <- data.frame(
  result$RedeemedAt_text,
  result[,2:ncol(result)]/result[["nru"]]
)

# rounded value 
result_pct4 <- result_pct %>% 
  mutate_at(vars(-result.RedeemedAt_text), list(~ round(., 2)))

# rounded value and add % 
myfun <- function(x) {
  if(is.numeric(x)){ 
    ifelse(is.na(x), x, paste0(round(x*100L, 1), "%")) 
  } else x 
}

result_pct5 <- result_pct %>% mutate_each(funs(myfun))
result_pct6 <- result_pct5 %>% mutate(sample = result$nru)

# reorder the position of column 
result_pct7 <- result_pct6[,c(1,64,63,2,3:ncol(result_pct6))]

# rename all column
result_pct8 <- result_pct7 %>% rename(RedeemdAt = result.RedeemedAt_text,
                                      NRU = sample,
                                      pct = nru)
str(result_pct8)
str(result_pct4)

write_xlsx(result_pct8,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/result_pct8.xlsx")
write_xlsx(result_pct4,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/result_pct.xlsx")


#### ------ 30 Aug 2021 

### Total usage time, avg, login 
Aug30_usagetime <- read_excel("/Users/admin/Documents/Linh-R Studio/38 user product launch test/300821_usagetime.xlsx")
data38id <- read_excel("/Users/admin/Documents/Linh-R Studio/38 user product launch test/data38id.xlsx")
data38id <- data38id %>% arrange(desc(data38id$TIME)) %>% mutate(id = rownames(data38id))

usagetime <- Aug30_usagetime %>% group_by(UserID,ActiveDate) %>% summarise(totalTime = sum(`Usage Time (minute)`))
usagetime_w <- usagetime %>% mutate(week = week(ActiveDate)) 

# number of login 
login <- usagetime_w %>% group_by(UserID,week) %>% summarise(count = n())
login_wide <- spread(login, week, count) # 45 user 
login_wide <- merge(data38id,login_wide) # 38 user
write_xlsx(login_wide,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/login_wide.xlsx")

# total time
totaltime <- usagetime_w %>% group_by(UserID,week) %>% summarise(totaltime = sum(totalTime))
totaltime_wide <- spread(totaltime, week, totaltime)
totaltime_wide <- merge(data38id,totaltime_wide)
write_xlsx(totaltime_wide,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/totaltime_wide.xlsx")

# average time
avgtime <- usagetime_w %>% group_by(UserID,week) %>% summarise(avg = mean(totalTime))
avgtime_wide <- spread(avgtime, week, avg)
avgtime_wide <- merge(data38id,avgtime_wide)
write_xlsx(avgtime_wide,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/avgtime_wide.xlsx")


### Usage time by Learn - Entertainment - Discovery 
usagetime1 <- Aug30_usagetime
usagetime2 <- spread(usagetime1, Feature,'Usage Time (minute)')


# total time
nhim <- usagetime2 %>% group_by(UserID,ActiveDate,week) %>% summarise(time=sum(discovery))
nhim_w <- nhim %>% group_by(UserID,week) %>% summarise(timenhim = sum(time,na.rm = T)) #neu khong co na.rm=T thi value + NA = NA, dan den tinh ra ket qua la NA. VD: case 12fd4355-0eb5-4ca5-948a-549c9bdf90b0
nhim_s <- spread(nhim_w, week, timenhim)
nhim_m <- merge(data38id,nhim_s)
write_xlsx(nhim_m,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/nhim_m.xlsx")


khi <- usagetime2 %>% group_by(UserID,ActiveDate,week) %>% summarise(time=sum(entertainment))
khi_w <- khi %>% group_by(UserID,week) %>% summarise(timekhi = sum(time,na.rm = T)) 
khi_s <- spread(khi_w, week, timekhi)
khi_m <- merge(data38id,khi_s)
write_xlsx(khi_m,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/khi_m.xlsx")


cu <- usagetime2 %>% group_by(UserID,ActiveDate,week) %>% summarise(time=sum(learn))
cu_w <- cu %>% group_by(UserID,week) %>% summarise(timecu = sum(time,na.rm = T)) 
cu_s <- spread(cu_w, week, timecu)
cu_m <- merge(data38id,cu_s)
write_xlsx(cu_m,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/cu_m.xlsx")


# number of login
cu$time[is.na(cu$time)] <- 0
cu_n <- cu %>% filter(time > 0) %>% group_by(UserID,week) %>% summarise(n = n())

khi$time[is.na(khi$time)] <- 0
khi_n <- khi %>% filter(time > 0) %>% group_by(UserID,week) %>% summarise(n = n())

nhim$time[is.na(nhim$time)] <- 0
nhim_n <- nhim %>% filter(time > 0) %>% group_by(UserID,week) %>% summarise(n = n())


# avg time
nhim_avg <- nhim %>% group_by(UserID,week) %>% summarise(timenhim = mean(time,na.rm = T)) 
nhim_avgs <- spread(nhim_avg, week, timenhim)
nhim_avgm <- merge(data38id,nhim_avgs)
write_xlsx(nhim_avgm,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/nhim_avg.xlsx")

khi_avg <- khi %>% group_by(UserID,week) %>% summarise(timekhi = mean(time,na.rm = T)) 
khi_avgs <- spread(khi_avg, week, timekhi)
khi_avgm <- merge(data38id,khi_avgs)
write_xlsx(khi_avgm,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/khi_avg.xlsx")

cu_avg <- cu %>% group_by(UserID,week) %>% summarise(timecu = mean(time,na.rm = T)) 
cu_avgs <- spread(cu_avg, week, timecu)
cu_avgm <- merge(data38id,cu_avgs)
write_xlsx(cu_avgm,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/cu_avg.xlsx")




#####------- 26 Aug 2021
dau <- read_excel("/Users/admin/Documents/Linh-R Studio/38 user product launch test/dau.xlsx")
usage <- read_excel("/Users/admin/Documents/Linh-R Studio/38 user product launch test/usagetime.xlsx")
usage1 <- read_excel("/Users/admin/Documents/Linh-R Studio/38 user product launch test/usagetime1.xlsx")

dau_1 <- dau %>% group_by(UserID, ActiveAt_v2) %>% summarise(login = n())
dau_2 <- dau_1 %>% group_by(UserID) %>% summarise(number_of_log = n())

write_xlsx(dau_2,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/dau_@.xlsx")

# Nhom ngay thanh tuan 
usage2 <- usage1
usage2 <- usage2 %>% 
  mutate(week = week(usage2$ActiveDate))
write_xlsx(usage2,"/Users/admin/Documents/Linh-R Studio/38 user product launch test/usage2.xlsx")

---
# Long to wide
data_wide <- spread(data, "Measure Names","Measure Values")

# User ID 
# df <- data_wide %>% group_by(`User ID`) %>% summarise(Usage = sum(`Total Usage Time (minute)`), Learn = sum(`Total Learning Time (minute)`), Entertainment = sum(`Total Entertaining Time (minute)`), Discovery = sum(`Total Discovering Time (minute)`), Child = n()) %>% ungroup()

dfx <- data_wide %>% group_by(`User ID`) %>% summarise(Usage = sum(`Total Usage Time (minute)`,na.rm = T), Learn = sum(`Total Learning Time (minute)`,na.rm = T), Entertainment = sum(`Total Entertaining Time (minute)`,na.rm = T), Discovery = sum(`Total Discovering Time (minute)`,na.rm = T), Child = n()) # na.rm = thay the na = 0 


df3 <- data_wide %>% distinct(`User ID`, .keep_all = TRUE)
df2 <- data_wide[!duplicated(data_wide$`User ID`), ]
df4 <- df2 %>% select(`User ID`,`Day of User Joined Date`,`Number of Active Days`)

df5 <- merge(dfx,df4)
df51 <-  df5[complete.cases(df5),] # remove null / na valaue 

# Cluster 
df52 <- df51 %>% filter(Usage != "0")
df6 <- df52 %>% select(`User ID`, `Usage`, `Number of Active Days`)

df7 <-  df6[complete.cases(df6),]

df7 %>% mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}) %>% select(-`User ID`) -> final_df_scaled

set.seed(29)

wss <- sapply(1:10, 
              function(k){kmeans(final_df_scaled %>% sample_frac(0.2), 
                                 k, nstart = 30)$tot.withinss})

u <- data.frame(k = 1:10, WSS = wss)

u %>% 
  ggplot(aes(k, WSS)) + 
  geom_line() + 
  geom_point() + 
  geom_point(data = u %>% filter(k == 3), color = "red", size = 3) + 
  scale_x_continuous(breaks = seq(1, 10, by = 1)) + 
  labs(title = "Figure 7: The Optimal Number of Clusters, Elbow Method", x = "Number of Clusters (K)") + 
  theme(panel.grid.minor = element_blank())

# Phân cụm với k = 3: 
set.seed(123)
km.res <- kmeans(final_df_scaled, 3, nstart = 30)

# Sử dụng kết quả phân cụm: 
df52 %>% 
  mutate(Group = km.res$cluster) %>% 
  mutate(Group = paste("Group", Group)) -> final_df_clustered

# # reorder column
final_df_clustered <- final_df_clustered[c(1,7,6,8,2,3,4,5,9)] # reorder columns
write_xlsx(final_df_clustered,"/Users/admin/Documents/Linh-R Studio/Irisgo/final_df_clustered.xlsx")

# reorder column
df5 <- df5[c(1,7,6,8,2,3,4,5)]
write_xlsx(df5,"/Users/admin/Documents/Linh-R Studio/Irisgo/df5.xlsx")

