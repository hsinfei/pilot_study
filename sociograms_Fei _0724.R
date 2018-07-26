## code updatd by Fei
## last uptaded: 7/24/2018

## clear all
rm(list=ls(all=TRUE))

## call relevant packages
library(network)
library(sna)
#library(gplots)

## import data
load("C:/FAST_Study/Data/Cleaned Raw Data Master Copy/RENN_Cleaned.Rda")

## remame full data set before maniputaing it
data<-renn.cleaned.data

## set working directory
setwd("C:/FAST_Study/Data/Output_datafiles")

#save number of students (i.e. network size)
n<-dim(data)[1]

## list of all variables names/labels as they appear in the full data set
all_labels<-colnames(data[,])

## locate position of the the variable ""Q_sn_f2finteract",  
## the variable where ego reports her interaction partners

for (i in 1:length(data))
{
  if (all_labels[i]=="Q_sn_f2finteract")
  {
    interact_index<-i 
  } 
}

## locate position of the the variable ""Q_id",  
## the variable where ego's ID is reported

for (i in 1:length(data))
{
  if (all_labels[i]=="Q_id")
  {
    i=ego_index<-i 
  } 
}


#Q_id, from factor to character
data$ID<-as.character(data$Q_id)

##get rid of the 'x' before each ID
for (i in 1:n)
{
 data$ID[i]<-ifelse(is.character(data$ID[i]),
                    yes=gsub("x","",data$ID[i]) ,
                    no=NA)
}

## make IDs numeric
data$ID<-as.numeric(data$ID)

##create an empty data frame to store net data as an edgelist
edgelist<-as.data.frame(matrix(NA, ncol = 2, nrow = (n*n)))


## loop through each each ego's interaction alters,
## then make alters' IDs numeric
## then create a tie between ego and alter (i.e. populate  
## the next row in edgelist with ego's -first column- and alter's IDs
## second column-)
index<-0
for (i in 1:n)
{
 alters<-data[i,interact_index]
 alters<-gsub("x","",alters)
 alters<-as.numeric(unlist(strsplit(alters, "\\,")))
 
 for (j in 1:length(alters))
 {
  index<-index + 1 
  edgelist[index,1]<-data$ID[i]
  edgelist[index,2]<-alters[j]
 }
}

## export the resulting edgelist
write.csv(x=edgelist,file="edgelist.csv",row.names=TRUE)

## get ride of non-populated rows (i.e. rows with NAs)
edgelist<-edgelist[complete.cases(edgelist), ]

##add an x to the IDs in edgelist(sna needs labels to be characters, not numbers)
 for (i in 1:nrow(edgelist))
 {
   edgelist$V1[i]<-paste("x",edgelist$V1[i],sep="") 
   edgelist$V2[i]<-paste("x",edgelist$V2[i],sep="") 
 }


for (i in 1:n)
{
  data$ID[i]<-ifelse(is.character(data$ID[i]),
                     yes=gsub("x","",data$ID[i]) ,
                     no=NA)
}


#### comparing egos (variable 'Q_id') and alters (variable 'Q_sn_f2finteract') ####

YYY<-as.vector(unique(edgelist$V1)) ## unique labels in first column of edgelist (i.e. egos)
AAA<-as.vector(unique(edgelist$V2)) ## unique labels in second columns of edgelist (i.e. alters)
TTT<-unique(c(YYY,AAA))             ## unique labels, these are all the labels reported in the edgelist 
ZZZ<-as.character(data$Q_id)        ## retrieve original respondents' labels/IDS based on variable Q_id
OOO<-c(TTT,ZZZ)                     ## combine all labels
CCC<-names(which(table(OOO) == 1))  ## make a list (i.e. CCC) with the labels that are only listed once, 
                                    ## these labels have to be excluded because they (presumably) represent students 
                                    ## for which there are NOT node-level covariates (i.e. these are alters named 
                                    ## by a given ego but who are not respondents themselves)


## subset the edgelist so that the students in CCC are excluded
for(i in 1:length(CCC))   
{
  element<-CCC[i]
  edgelist<-subset(edgelist,V2!=element)
}

## create a network object based on the resulting edgelist
net<-as.network.matrix(edgelist,matrix.type = "edgelist")

## add labels as a node attribute
all_IDs<-as.vector(data$Q_id)
network.vertex.names(net) <-all_IDs

## make gender info numeric
data$gender<-as.numeric(data$Q_gender)

## set a seed for random numbers
set.seed(5)

## save node coordinates based on the fruchtermanreingold algorithm
capturedCoordinates<-gplot(net,gmode="digraph", mode = "fruchtermanreingold")

## calculate out and indegree
odeg<-(degree(net,cmode="outdegree"))
ideg<-(degree(net,cmode="indegree"))

## make in and outdegree node-level attirbutes
set.vertex.attribute(net,"odeg",odeg)
set.vertex.attribute(net,"ideg",ideg)

## change working directory
setwd("C:/FAST_Study/Data/Output_datafiles/plots")

##open a pdf
pdf("renn_sociogram_Fei.pdf",width=15,height = 15) 
colset = c("#9AABB9", "#E2B49A", "#D59BE1")
data$color[data$gender==1] <- "#D59BE1"
data$color[data$gender==2] <- "#9AABB9"
data$color[data$gender==3] <- "#E2B49A"
## draw a sociogram
q = layout_on_sphere(net)


gplot(dat=net,
      gmode="digraph",                               ## directed network
      coord=capturedCoordinates,                     ## use the coordinates computed earlier
      displaylabels = T,                             ## display node-level labels
      pad = 0.005,                                   ## white space around sociogram
      label.pos=5,                                   ## nodes' labels should be inside the nodes
      edge.col="#C5D2DB",                             ## make ties clear graish
      displayisolates = TRUE,                        ## show isolates
      vertex.cex = (log(ideg)),                      ## make nodes' sized a function of their indegree
      vertex.border="white",                         ## make nodes' borders be gray
      label.cex = 0.9,                               ## label size
      vertex.col=data$color,                         ## nodes' color are a function of their gender
      label.col="black",                             ## label color
      layout = q
      )
legend("topleft",legend=paste(c("Boy","Girl","NA")),col=colset,pch=rep(c(16,18),each=4),bty="n",cex=1,pt.cex=0.7)

# updated 07/24
net %v% 'short_ID' <- substring(net %v% 'vertex.names', 3)
data$race = 0
data$race[data$Q_race_eth == "Hispanic or Latino"]<-1
data$color_c = '#e0b2ca'
data$color_c[data$race==1] <- '#9ebac9'

pdf("renn_sociogram_eth_Fei.pdf",width=20,height = 20) 

gplot(dat=net,
      gmode="digraph",                               ## directed network
      coord = capturedCoordinates,                     ## use the coordinates computed earlier
      label = net %v% 'short_ID',
      displaylabels = T,                             ## display node-level labels
      pad = 0.005,                                   ## white space around sociogram
      label.pos=5,                                   ## nodes' labels should be inside the nodes
      edge.col="#C5D2DB",                             ## make ties clear graish
      displayisolates = TRUE,                        ## show isolates
      vertex.cex = (log(ideg)),                      ## make nodes' sized a function of their indegree
      vertex.border="white",                         ## make nodes' borders be gray
      label.cex = 0.9,                               ## label size
      vertex.col=data$color_c,                         ## nodes' color are a function of their gender
      label.col="black",                             ## label color
      layout = q
)
legend("topleft",legend=paste(c("non-Hispanic","Hispanic")),col= c('#e0b2ca','#9ebac9'),pch=rep(c(16,18),each=4),bty="n",cex=1,pt.cex=0.7)


  
## close the pdf
dev.off()



