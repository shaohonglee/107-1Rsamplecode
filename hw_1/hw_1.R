setwd("~/GitHub")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(wordcloud2)
library(ggpubr)
library(topicmodels)
library(tidytext)

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

han=filter(alldata,Page_Name=="韓國瑜")
chen=filter(alldata,Page_Name=="陳其邁 Chen Chi-Mai")
han$Date=as.POSIXct(han$Date,format="%Y/%m/%d %H:%M:%S")
chen$Date=as.POSIXct(chen$Date,format="%Y/%m/%d %H:%M:%S")
han$mes_nchar=nchar(han$Message)
chen$mes_nchar=nchar(chen$Message)
all=rbind(han,chen)

all%>%group_by(Page_Name)%>%count()%>%ggplot(aes(Page_Name,n))+
  geom_bar(stat = "identity")+
  ggtitle("貼文數統計")+
  theme(plot.title = element_text(hjust = 0.5))

F1=all%>%group_by(month=format(Date,"%m"),Page_Name)%>%count()%>%ggplot(aes(x=month,y=n,fill=Page_Name))+
  geom_bar(stat = "identity",position = "dodge")+
  ggtitle("月貼文趨勢")+
  theme(plot.title = element_text(hjust = 0.5))

F2=all%>%group_by(month=format(Date,"%m"),Page_Name)%>%summarize(Reaction_Count=sum(All_Reaction_Count))%>%
  ggplot(aes(x=month,y=Reaction_Count,fill=Page_Name))+
  geom_bar(stat = "identity",position = "dodge")+
  ggtitle("月回覆趨勢")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(F1,F2)

all%>%group_by(Page_Name,Type)%>%summarize(n=n())%>%mutate(freq=n/sum(n))%>%ggplot(aes(Type,freq,fill=Page_Name))+
  geom_bar(stat="identity",position = "dodge")+
  ggtitle("貼文種類")+
  theme(plot.title = element_text(hjust = 0.5))



#############################################################    


## r 相關係數 


ggscatter(all,x="All_Reaction_Count",y="LIKE_COUNT", add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
# method = c("pearson", "kendall", "spearman")
ggqqplot(chen$All_Reaction_Count)  
ggqqplot(chen$LIKE_COUNT)          

cor(chen[c(6:14,19)])%>%corrplot.mixed(lower = "pie",tl.cex=0.6)

###

library(jiebaRD)
library(jiebaR)
all=all%>%filter(Date>="2018-7-1"&Date<"2018-11-24")
all_msg = all%>% group_by(Page_Name) %>% 
  mutate(messageByName = paste0(Message, collapse = ""))
id = which(duplicated(all_msg$Page_Name) == FALSE)
all_msg=all_msg[id,c(2,20)]

han=han%>%filter(Date>="2018-7-1"&Date<"2018-11-24")
han_msg = han%>% group_by(Page_Name) %>% 
  mutate(messageByName = paste0(Message, collapse = ""))
id = which(duplicated(han_msg$Page_Name) == FALSE)
han_msg=han_msg[id,c(2,20)]

chen=chen%>%filter(Date>="2018-7-1"&Date<"2018-11-24")
chen_msg = chen%>% group_by(Page_Name) %>% 
  mutate(messageByName = paste0(Message, collapse = ""))
id = which(duplicated(chen_msg$Page_Name) == FALSE)
chen_msg=chen_msg[id,c(2,20)]

setwd("~/GitHub/Politics-and-information")
cutter <- worker("tag",stop_word ="stopwords-u8.txt",user = "user.txt" ,encoding = "UTF-8",bylines = T)

myFUN<- function(str) {
  str = gsub("[^[:alpha:]]|[A-Za-z0-9]", "", str)
  seg = cutter[str]
  result = seg
}
segment_all = apply(matrix(all_msg$messageByName), MARGIN = 1, myFUN)
segment_han = apply(matrix(han_msg$messageByName), MARGIN = 1, myFUN)
segment_chen= apply(matrix(chen_msg$messageByName), MARGIN = 1, myFUN)

hanfreq=data.frame(table(segment_han[[1]]))
chenfreq=data.frame(table(segment_chen[[1]]))

top_han=hanfreq%>%arrange(desc(Freq))%>%head(30)
top_chen=chenfreq%>%arrange(desc(Freq))%>%head(30)
top_han%>%wordcloud2()
top_chen%>%wordcloud2()

topword=merge(top_chen,top_han,by="Var1",all = TRUE)
colnames(topword) = c("words","chen","han")
rownames(topword) = topword$words
topword= topword[,-1]
topword[is.na(topword)]<-0

CoMatrix = as.matrix(topword) %*% t(as.matrix(topword))
total_occurrences <- rowSums(CoMatrix)
smallid = which(total_occurrences < median(total_occurrences))
co_occurrence_d = CoMatrix / total_occurrences
co_occurrence_s = co_occurrence_d[-as.vector(smallid),-as.vector(smallid)]
require(igraph)
graph <- graph.adjacency(round(co_occurrence_s*10),
                         mode="undirected",
                         diag=FALSE)
plot(graph,
     vertex.label=names(data),
     edge.arrow.mode=0,
     vertex.size=1,
     edge.width=E(graph)$weight,
     layout=layout_with_fr)
##########################

rownames(hanfreq) = hanfreq$Var1
handtm=subset(hanfreq)%>%select(Freq)
dtm_lda <- LDA(t(handtm), k = 4, control = list(seed = 1234))
dtm_topics <- tidy(dtm_lda, matrix = "beta")
top_terms <- dtm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text.y=element_text(colour="black"))
############################

rownames(chenfreq) = chenfreq$Var1
chendtm=subset(chenfreq)%>%select(Freq)
dtm_lda <- LDA(t(chendtm), k = 4, control = list(seed = 1234))
dtm_topics <- tidy(dtm_lda, matrix = "beta")
top_terms <- dtm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text.y=element_text(colour="black"))

##########################

ntuPosEmo=data.table::fread("ntu-positive.txt", header = T,sep="\r",quote = "", stringsAsFactors = F,encoding = "UTF-8")
ntuNegEmo=data.table::fread("ntu-negative.txt", header = T,sep="\r",quote = "", stringsAsFactors = F,encoding = "UTF-8")
han_pos=hanfreq%>%merge(x=.,y=ntuPosEmo,by.x="Var1",by.y="word")%>%summarize(Emo="han_pos",Value=sum(Freq)/232)
han_neg=hanfreq%>%merge(x=.,y=ntuNegEmo,by.x="Var1",by.y="word")%>%summarize(Emo="han_neg",Value=sum(Freq)/232)
chen_pos=chenfreq%>%merge(x=.,y=ntuPosEmo,by.x="Var1",by.y="word")%>%summarize(Emo="chen_pos",Value=sum(Freq)/364)
chen_neg=hanfreq%>%merge(x=.,y=ntuNegEmo,by.x="Var1",by.y="word")%>%summarize(Emo="chen_neg",Value=sum(Freq)/364)

Emotion=rbind(han_pos,han_neg,chen_pos,chen_neg)

ggplot(Emotion,aes(x=Emo,y=Value,fill=Emo))+
  geom_bar(stat = "identity")
