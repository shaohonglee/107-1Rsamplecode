library(readr)
setwd("~/GitHub")
X201801_data <- read_csv("data/201801_data.csv")
X201802_data <- read_csv("data/201802_data.csv")
X201803_data <- read_csv("data/201803_data.csv")
X201804_data <- read_csv("data/201804_data.csv")
X201805_data <- read_csv("data/201805_data.csv")
X201806_data <- read_csv("data/201806_data.csv")
X201807_data <- read_csv("data/201807_data.csv")
X201808_data <- read_csv("data/201808_data.csv")
X201809_data <- read_csv("data/201809_data.csv")
X201810_data <- read_csv("data/201810_data.csv")
X201811_data <- read_csv("data/201811_data.csv")
X201812_data <- read_csv("data/201812_data.csv")
X201901_data <- read_csv("data/201901_data.csv")

library(tidyverse)

VideoMatrix = filter(X201809_data, Type == "video")
Hvideo = filter(VideoMatrix, grepl("韓國瑜", VideoMatrix$Page_Name) == TRUE)
cvideo = filter(VideoMatrix, grepl("陳其邁", VideoMatrix$Page_Name) == TRUE)

Hcomment = Hvideo[,c(2,13,15)]
Ccomment = cvideo[,c(2,13,15)]

HCComment = rbind(Hcomment, Ccomment)

library(ggplot2)

ggplot() + geom_bar( data = HCComment, aes(x=Page_Name) )

ggplot() + geom_bar( data = HCComment, 
                     aes(x=Page_Name, y=Comment_Count, group = Page_Name),
                     stat="identity")

HCmessage = HCComment %>% group_by(Page_Name) %>% 
  mutate(messageByName = paste0(Message, collapse = ""))

id = which(duplicated(HCmessage$Page_Name) == FALSE)

HCmessageID = HCmessage[id,c(1,4)]
