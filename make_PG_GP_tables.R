library(erer)


####WORD INITIAL#####
##FOR P-G TABLES
myPs <- rownames(wordinitial_PG_prop)
myTables <- list()

for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(wordinitial_PG_prop[rownames(wordinitial_PG_prop)==myPs[i],][wordinitial_PG_prop[rownames(wordinitial_PG_prop)==myPs[i],]>0])
colnames(myTables[[i]]) <- rownames(wordinitial_PG_prop)[i]
rownames(myTables[[i]]) <- paste0(rownames(wordinitial_PG_prop)[i],"+",rownames(as.data.frame(wordinitial_PG_prop[rownames(wordinitial_PG_prop)==myPs[i],][wordinitial_PG_prop[rownames(wordinitial_PG_prop)==myPs[i],]>0])))
}

write.list(z=myTables,file="wordinitial_PG_table.csv",row.names=T)


##FOR G-P TABLES
myPs <- rownames(wordinitial_GP_prop)
myTables <- list()

for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(wordinitial_GP_prop[rownames(wordinitial_GP_prop)==myPs[i],][wordinitial_GP_prop[rownames(wordinitial_GP_prop)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(wordinitial_GP_prop)[i]
  rownames(myTables[[i]]) <- paste0(rownames(as.data.frame(wordinitial_GP_prop[rownames(wordinitial_GP_prop)==myPs[i],][wordinitial_GP_prop[rownames(wordinitial_GP_prop)==myPs[i],]>0])),"+",rownames(wordinitial_GP_prop)[i])
}

write.list(z=myTables,file="wordinitial_GP_table.csv",row.names=T)


##FOR Freq TABLES
myPs <- rownames(wordinitial_FREQ)
myTables <- list()

for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(wordinitial_FREQ[rownames(wordinitial_FREQ)==myPs[i],][wordinitial_FREQ[rownames(wordinitial_FREQ)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(wordinitial_FREQ)[i]
  rownames(myTables[[i]]) <- paste0(rownames(as.data.frame(wordinitial_FREQ[rownames(wordinitial_FREQ)==myPs[i],][wordinitial_FREQ[rownames(wordinitial_FREQ)==myPs[i],]>0])),"+",rownames(wordinitial_FREQ)[i])
}

write.list(z=myTables,file="wordinitial_FREQ_table.csv",row.names=T)






#####SYLLABLE INITIAL#####

##FOR P-G TABLES
myPs <- rownames(syllableinitial_PG_prop)
myTables <- list()

for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(syllableinitial_PG_prop[rownames(syllableinitial_PG_prop)==myPs[i],][syllableinitial_PG_prop[rownames(syllableinitial_PG_prop)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(syllableinitial_PG_prop)[i]
  rownames(myTables[[i]]) <- paste0(rownames(syllableinitial_PG_prop)[i],"+",rownames(as.data.frame(syllableinitial_PG_prop[rownames(syllableinitial_PG_prop)==myPs[i],][syllableinitial_PG_prop[rownames(syllableinitial_PG_prop)==myPs[i],]>0])))
}

write.list(z=myTables,file="syllableinitial_PG_table.csv",row.names=T)


##FOR G-P TABLES
myPs <- rownames(syllableinitial_GP_prop)
myTables <- list()

for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(syllableinitial_GP_prop[rownames(syllableinitial_GP_prop)==myPs[i],][syllableinitial_GP_prop[rownames(syllableinitial_GP_prop)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(syllableinitial_GP_prop)[i]
  rownames(myTables[[i]]) <- paste0(rownames(as.data.frame(syllableinitial_GP_prop[rownames(syllableinitial_GP_prop)==myPs[i],][syllableinitial_GP_prop[rownames(syllableinitial_GP_prop)==myPs[i],]>0])),"+",rownames(syllableinitial_GP_prop)[i])
}

write.list(z=myTables,file="syllableinitial_GP_table.csv",row.names=T)


##FOR FREQ TABLES
myPs <- rownames(syllableinitial_FREQ)
myTables <- list()

for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(syllableinitial_FREQ[rownames(syllableinitial_FREQ)==myPs[i],][syllableinitial_FREQ[rownames(syllableinitial_FREQ)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(syllableinitial_FREQ)[i]
  rownames(myTables[[i]]) <- paste0(rownames(as.data.frame(syllableinitial_FREQ[rownames(syllableinitial_FREQ)==myPs[i],][syllableinitial_FREQ[rownames(syllableinitial_FREQ)==myPs[i],]>0])),"+",rownames(syllableinitial_FREQ)[i])
}

write.list(z=myTables,file="syllableinitial_FREQ_table.csv",row.names=T)



#####SYLLABLE MEDIAL#####
##FOR P-G TABLES
myPs <- rownames(syllablemedial_PG_prop)
myTables <- list()

for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(syllablemedial_PG_prop[rownames(syllablemedial_PG_prop)==myPs[i],][syllablemedial_PG_prop[rownames(syllablemedial_PG_prop)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(syllablemedial_PG_prop)[i]
  rownames(myTables[[i]]) <- paste0(rownames(syllablemedial_PG_prop)[i],"+",rownames(as.data.frame(syllablemedial_PG_prop[rownames(syllablemedial_PG_prop)==myPs[i],][syllablemedial_PG_prop[rownames(syllablemedial_PG_prop)==myPs[i],]>0])))
}

write.list(z=myTables,file="syllablemedial_PG_table.csv",row.names=T)


##FOR G-P TABLES
myPs <- rownames(syllablemedial_GP_prop)
myTables <- list()

for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(syllablemedial_GP_prop[rownames(syllablemedial_GP_prop)==myPs[i],][syllablemedial_GP_prop[rownames(syllablemedial_GP_prop)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(syllablemedial_GP_prop)[i]
  rownames(myTables[[i]]) <- paste0(rownames(as.data.frame(syllablemedial_GP_prop[rownames(syllablemedial_GP_prop)==myPs[i],][syllablemedial_GP_prop[rownames(syllablemedial_GP_prop)==myPs[i],]>0])),"+",rownames(syllablemedial_GP_prop)[i])
}

write.list(z=myTables,file="syllablemedial_GP_table.csv",row.names=T)



##FOR FREQ TABLES
myPs <- rownames(syllablemedial_FREQ)
myTables <- list()

for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(syllablemedial_FREQ[rownames(syllablemedial_FREQ)==myPs[i],][syllablemedial_FREQ[rownames(syllablemedial_FREQ)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(syllablemedial_FREQ)[i]
  rownames(myTables[[i]]) <- paste0(rownames(as.data.frame(syllablemedial_FREQ[rownames(syllablemedial_FREQ)==myPs[i],][syllablemedial_FREQ[rownames(syllablemedial_FREQ)==myPs[i],]>0])),"+",rownames(syllablemedial_FREQ)[i])
}

write.list(z=myTables,file="syllablemedial_FREQ_table.csv",row.names=T)



####WORD and SYLLABLE FINAL######
##FOR P-G TABLES
myPs <- rownames(syllablefinal_PG_prop)
myTables <- list()
for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(syllablefinal_PG_prop[rownames(syllablefinal_PG_prop)==myPs[i],][syllablefinal_PG_prop[rownames(syllablefinal_PG_prop)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(syllablefinal_PG_prop)[i]
  rownames(myTables[[i]]) <- paste0(rownames(syllablefinal_PG_prop)[i],"+",rownames(as.data.frame(syllablefinal_PG_prop[rownames(syllablefinal_PG_prop)==myPs[i],][syllablefinal_PG_prop[rownames(syllablefinal_PG_prop)==myPs[i],]>0])))
}
write.list(z=myTables,file="syllablefinal_PG_table.csv",row.names=T)
##FOR G-P TABLES
myPs <- rownames(syllablefinal_GP_prop)
myTables <- list()
for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(syllablefinal_GP_prop[rownames(syllablefinal_GP_prop)==myPs[i],][syllablefinal_GP_prop[rownames(syllablefinal_GP_prop)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(syllablefinal_GP_prop)[i]
  rownames(myTables[[i]]) <- paste0(rownames(as.data.frame(syllablefinal_GP_prop[rownames(syllablefinal_GP_prop)==myPs[i],][syllablefinal_GP_prop[rownames(syllablefinal_GP_prop)==myPs[i],]>0])),"+",rownames(syllablefinal_GP_prop)[i])
}

write.list(z=myTables,file="syllablefinal_GP_table.csv",row.names=T)

##FOR FREQ TABLES
myPs <- rownames(syllablefinal_FREQ)
myTables <- list()
for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(syllablefinal_FREQ[rownames(syllablefinal_FREQ)==myPs[i],][syllablefinal_FREQ[rownames(syllablefinal_FREQ)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(syllablefinal_FREQ)[i]
  rownames(myTables[[i]]) <- paste0(rownames(as.data.frame(syllablefinal_FREQ[rownames(syllablefinal_FREQ)==myPs[i],][syllablefinal_FREQ[rownames(syllablefinal_FREQ)==myPs[i],]>0])),"+",rownames(syllablefinal_FREQ)[i])
}

write.list(z=myTables,file="syllablefinal_FREQ_table.csv",row.names=T)

##FOR P-G TABLES
myPs <- rownames(wordfinal_PG_prop)
myTables <- list()
for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(wordfinal_PG_prop[rownames(wordfinal_PG_prop)==myPs[i],][wordfinal_PG_prop[rownames(wordfinal_PG_prop)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(wordfinal_PG_prop)[i]
  rownames(myTables[[i]]) <- paste0(rownames(wordfinal_PG_prop)[i],"+",rownames(as.data.frame(wordfinal_PG_prop[rownames(wordfinal_PG_prop)==myPs[i],][wordfinal_PG_prop[rownames(wordfinal_PG_prop)==myPs[i],]>0])))
}

write.list(z=myTables,file="wordfinal_PG_table.csv",row.names=T)
##FOR G-P TABLES
myPs <- rownames(wordfinal_GP_prop)
myTables <- list()
for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(wordfinal_GP_prop[rownames(wordfinal_GP_prop)==myPs[i],][wordfinal_GP_prop[rownames(wordfinal_GP_prop)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(wordfinal_GP_prop)[i]
  rownames(myTables[[i]]) <- paste0(rownames(as.data.frame(wordfinal_GP_prop[rownames(wordfinal_GP_prop)==myPs[i],][wordfinal_GP_prop[rownames(wordfinal_GP_prop)==myPs[i],]>0])),"+",rownames(wordfinal_GP_prop)[i])
}
write.list(z=myTables,file="wordfinal_GP_table.csv",row.names=T)


##FOR FREQ TABLES
myPs <- rownames(wordfinal_FREQ)
myTables <- list()
for(i in 1:length(myPs)){
  myTables[[i]]<-matrix(wordfinal_FREQ[rownames(wordfinal_FREQ)==myPs[i],][wordfinal_FREQ[rownames(wordfinal_FREQ)==myPs[i],]>0])
  colnames(myTables[[i]]) <- rownames(wordfinal_FREQ)[i]
  rownames(myTables[[i]]) <- paste0(rownames(as.data.frame(wordfinal_FREQ[rownames(wordfinal_FREQ)==myPs[i],][wordfinal_FREQ[rownames(wordfinal_FREQ)==myPs[i],]>0])),"+",rownames(wordfinal_FREQ)[i])
}
write.list(z=myTables,file="wordfinal_FREQ_table.csv",row.names=T)

