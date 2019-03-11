setwd("~/GitHub/Politics-and-information")
library(dplyr)
library(tm)
data_201801=data.table::fread('data/201801_data.csv',data.table = F,encoding = 'UTF-8')
data_201802=data.table::fread('data/201802_data.csv',data.table = F,encoding = 'UTF-8')
data_201803=data.table::fread('data/201803_data.csv',data.table = F,encoding = 'UTF-8')
data_201804=data.table::fread('data/201804_data.csv',data.table = F,encoding = 'UTF-8')
data_201805=data.table::fread('data/201805_data.csv',data.table = F,encoding = 'UTF-8')
data_201806=data.table::fread('data/201806_data.csv',data.table = F,encoding = 'UTF-8')
data_201807=data.table::fread('data/201807_data.csv',data.table = F,encoding = 'UTF-8')
data_201808=data.table::fread('data/201808_data.csv',data.table = F,encoding = 'UTF-8')
data_201809=data.table::fread('data/201809_data.csv',data.table = F,encoding = 'UTF-8')
data_201810=data.table::fread('data/201810_data.csv',data.table = F,encoding = 'UTF-8')
data_201811=data.table::fread('data/201811_data.csv',data.table = F,encoding = 'UTF-8')
data_201812=data.table::fread('data/201812_data.csv',data.table = F,encoding = 'UTF-8')
alldata=rbind(data_201801,data_201802,data_201803,data_201804,data_201805,data_201806,data_201807,data_201808,data_201809,data_201810,data_201811,data_201812)
page_count=data_201801%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
top10=merge(page_count,data_201801,by="Page_Name")
top10=top10[,-2]

mean(nchar(top10$Message))
filter(top10,grepl("柯文哲",top10$Message)==T)%>%count() #355
filter(top10,grepl("姚文智",top10$Message)==T)%>%count() #31
filter(top10,grepl("丁守中",top10$Message)==T)%>%count() #77

filter(top10,grepl("韓國瑜",top10$Message)==T)%>%count() #27
filter(top10,grepl("陳其邁",top10$Message)==T)%>%count() #40

filter(top10,grepl("#",top10$Message)==T)%>%count()      #575  有TAG的
filter(top10,grepl("https",top10$Message)==T)%>%count()  #291  有連結的

top10$Message=gsub(pattern = "柯文哲",replacement="柯P",x=top10$Message)  #替換
KP_address=gregexpr("柯P",top10$Message)                 #定址
attr(KP_address[[1]],"match.length")
top10$Date=strsplit(top10$Date,"[[:blank:]]|/")          #切割
top10$Date[[1]][1]
top10$Date[[1]][2]

sub_str=data.frame(substr(x=top10$Message,1,regexpr("選舉",top10$Message)+1)) #字串尋找並切割 
top10$id=paste(top10$Page_ID,top10$created_time,sep = "@") # 合併

#########################################################################
#top10$Message=top10$Message%>%removePunctuation()%>%removeNumbers()
    
alldata$Date=as.POSIXct(alldata$Date,format="%Y/%m/%d %H:%M:%S")
before=filter(alldata,Date<"2018-10-01")
after=filter(alldata,Date>"2018-09-30")   
