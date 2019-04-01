library(jiebaRD)
library(jiebaR)
cutter <- worker()

dic = c("韓國瑜", "國瑜", "高雄人")

new_user_word(cutter, dic)

string = HCmessageID$messageByName[1]
m1 = table(cutter[string])

string = HCmessageID$messageByName[2]
m2 = table(cutter[string])

string = HCmessageID$messageByName[3]
m3 = table(cutter[string])

string = HCmessageID$messageByName[4]
m4 = table(cutter[string])

tag <- worker("tag")
new_user_word(tag, dic)

string = HCmessageID$messageByName[1]
Stag1 = tag[string]
Stage1Name = names(Stag1)
needTag = c("v", "n", "a")

id = which(Stage1Name == needTag[1])
id = c(id, which(Stage1Name == needTag[2]))
id = c(id, which(Stage1Name == needTag[3]))

temp = cutter[string]
tempTagNeed = temp[id]
m1TagNeed = table(tempTagNeed)
View(m1TagNeed)

string = HCmessageID$messageByName[2]
Stag2 = tag[string]
Stage2Name = names(Stag2)

id = ""
id = which(Stage2Name == needTag[1])
id = c(id, which(Stage2Name == needTag[2]))
id = c(id, which(Stage2Name == needTag[3]))

temp = cutter[string]
tempTagNeed = temp[id]
m2TagNeed = table(tempTagNeed)
View(m2TagNeed)


#Full join
NewTable = merge(x = m1TagNeed, y = m2TagNeed, by = "tempTagNeed", all = TRUE)
NewTable[ which(is.na(NewTable$Freq.x) == TRUE), 2] = 0
NewTable[ which(is.na(NewTable$Freq.y) == TRUE), 3] = 0

allM = as.matrix(NewTable[,2:3])
rownames(allM) = NewTable$tempTagNeed

CoMatrix = allM %*% t(allM)

#total_occurrences <- colSums(CoMatrix)
#smallid = total_occurrences[which(total_occurrences < median(total_occurrences))]
#co_occurrence_d = CoMatrix / total_occurrences
#co_occurrence_s = co_occurrence_d[-as.vector(smallid),-as.vector(smallid)]


require(igraph)
graph <- graph.adjacency(CoMatrix[90:100,90:100],
                         weighted=TRUE,
                         mode="undirected",
                         diag=FALSE)

plot(graph,
     vertex.label=names(data),
     edge.arrow.mode=0,
     vertex.size=1,
     edge.width=E(graph)$weight,
     layout=layout_with_fr)
