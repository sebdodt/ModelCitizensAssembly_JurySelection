# optimisation of jury
setwd("~/OneDrive/Beruflich/Peace Child International/Model Citizens Assembly/model for selection")
library(gtools)
library(gridExtra)
library(ggplot2)
library(progress)
library(ggpubr)
library(expss)
library(dplyr)
library(abind)
library(readxl)
library(combinat)

#remember to delete duplicates
responses = read_excel("Jury Application Form (Responses).xlsx")
rawdata_1 = responses[,c(5:15)]
rawdata_1 = data.matrix(rawdata_1)
rawdata = cbind(responses[,c(2:4)],rawdata_1)
dim(rawdata)
sum(is.na(rawdata))

#splits opinion
setrows = 50
set = rawdata[setrows,]
notset = rawdata[-setrows,]
no.set = set[set[,14]==2,]
dontknow.set = set[set[,14]==1,]
yes.set = set[set[,14]==3,]
no = notset[notset[,14]==2,]
dontknow = notset[notset[,14]==1,]
yes = notset[notset[,14]==3,]

#goals
size_jury = 15
no.wanted = 3
dontknow.wanted = 7
yes.wanted = 3
male.wanted = 8

#splits gender
yes.male = yes[yes[,4]==2,]
yes.female = yes[yes[,4]==1,]
dontknow.male = dontknow[dontknow[,4]==2,]
dontknow.female = dontknow[dontknow[,4]==1,]
no.male = no[no[,4]==2,]
no.female = no[no[,4]==1,]

#687225
#combinations
dontknow.comb = combinations(n=nrow(dontknow),r=dontknow.wanted-nrow(dontknow.set))
dontknow.comb = dontknow.comb+nrow(yes)+nrow(no)
yes.comb = combinations(n=nrow(yes),r=yes.wanted-nrow(yes.set))
no.comb = combinations(n=nrow(no),r=no.wanted-nrow(no.set))
no.comb = no.comb+nrow(yes)
dontknow.comb.rep = dontknow.comb[rep(seq_len(nrow(dontknow.comb)), each = nrow(yes.comb)), ]
yes.comb.rep = do.call(rbind, replicate(nrow(dontknow.comb), yes.comb, simplify=FALSE))
dontandyes.rep = cbind(dontknow.comb.rep,yes.comb.rep)
dontandyes.rep = dontandyes.rep[rep(seq_len(nrow(dontandyes.rep)), each = nrow(no.comb)), ]
no.comb.rep = do.call(rbind, replicate(nrow(dontknow.comb.rep), no.comb, simplify=FALSE))
allnotset.rep = cbind(dontandyes.rep,no.comb.rep)
set_rep = matrix(setrows,nrow=1)
allset.rep = set_rep[rep(seq_len(nrow(set_rep)), each = nrow(allnotset.rep)), ]
y = cbind(allnotset.rep,allset.rep)




#____ old _____

#set = rawdata[c(44:48),]
#notset = rawdata[c(1:43),]
#setmale = sum(set[,4]==2)
#setfemale = sum(set[,4]==1)
#notsetmale = sum(notset[,4]==2)
#notsetfemale = sum(notset[,4]==1)

#combinations
#njurymale = size_jury/2-0.5
##njuryfemale = size_jury/2+0.5
#maleleft = njurymale-setmale
#femaleleft = njuryfemale-setfemale
#malecomb = combinations(n=notsetmale,r=maleleft)
#femalecomb = combinations(n=notsetfemale,r=femaleleft)
#femalecomb = femalecomb+notsetmale

#malecomb_rep = malecomb[rep(seq_len(nrow(malecomb)), each = nrow(femalecomb)), ]
#femalecomb_rep = do.call(rbind, replicate(nrow(malecomb), femalecomb, simplify=FALSE))
#set_rep = matrix(c(35:46),nrow=1)
#set_rep = set_rep[rep(seq_len(nrow(set_rep)), each = nrow(malecomb_rep)), ]
#y = cbind(malecomb_rep,femalecomb_rep,set_rep)

goal = read_excel("goals_2.xlsx")
goal = data.frame(goal)
attnames = read_excel("goals_names.xls")
goal[2,1] = 100
numb_appl = nrow(rawdata)
number = seq(1,nrow(rawdata),1)
rawdata = cbind(number,rawdata)
write.csv(rawdata,"all.applicants.csv")
names_and_numbers = rawdata[,c(1:4)]
rawdata = rawdata[,-c(2:4)]


#set up 3D matrix
z = nrow(y)
output_first_dim = array(y, dim=c(z,size_jury,1))
output_extra = array(NA, dim=c(z,size_jury,1))
output = abind(output_first_dim,output_extra,along=3)
dim(output)
output[,,2] = rawdata[output[,,1],2]
nmale = rowSums(output[,,2]==2)
nfemale = rowSums(output[,,2]==1)
parity = ifelse(nmale==male.wanted,TRUE,FALSE)
output = output[!(parity==FALSE),,]
z = nrow(output)
output_extra = array(NA, dim=c(z,size_jury,1))
output = abind(output,output_extra,along=3)
output = abind(output,output_extra,along=3)
output = abind(output,output_extra,along=3)
output = abind(output,output_extra,along=3)
output = abind(output,output_extra,along=3)
output = abind(output,output_extra,along=3)
output = abind(output,output_extra,along=3)
output = abind(output,output_extra,along=3)
output = abind(output,output_extra,along=3)
output = abind(output,output_extra,along=3)

difempty = array(NA, dim=c(nrow(output),1,12))
output = abind(output,difempty,along=2)
nullvector = vector(length=nrow(output))
nullvector[] = 0
output[,ncol(output),2] = nullvector



for (i in 3:12) {
  output[,,i] = rawdata[output[,,1],i]
  dif = (rowSums(output[,1:ncol(output)-1,i]==1)/size_jury-goal[3,i-1])^2 + 
    (rowSums(output[,1:ncol(output)-1,i]==2)/size_jury-goal[4,i-1])^2 + 
    (rowSums(output[,1:ncol(output)-1,i]==3)/size_jury-goal[5,i-1])^2 + 
    (rowSums(output[,1:ncol(output)-1,i]==4)/size_jury-goal[6,i-1])^2 + 
    (rowSums(output[,1:ncol(output)-1,i]==5)/size_jury-goal[7,i-1])^2 + 
    (rowSums(output[,1:ncol(output)-1,i]==6)/size_jury-goal[8,i-1])^2 +
    (rowSums(output[,1:ncol(output)-1,i]==7)/size_jury-goal[9,i-1])^2 + 
    (rowSums(output[,1:ncol(output)-1,i]==8)/size_jury-goal[10,i-1])^2
  coef = goal[2,i-1]
  output[,ncol(output),i] = dif*coef
  print(i)
}
output[,ncol(output),1] = output[,ncol(output),2] + output[,ncol(output),3] + output[,ncol(output),4] + 
  output[,ncol(output),5] + output[,ncol(output),6] + output[,ncol(output),7] + output[,ncol(output),8] + 
  output[,ncol(output),9] + output[,ncol(output),10] + output[,ncol(output),11] + output[,ncol(output),12]


#most representative model
k = min(output[,ncol(output),1])
k
l = match(k,output[,ncol(output),1])
appl1 = output[l,1,1]
appl2 = output[l,2,1]
appl3 = output[l,3,1]
appl4 = output[l,4,1]
appl5 = output[l,5,1]
appl6 = output[l,6,1]
appl7 = output[l,7,1]
appl8 = output[l,8,1]
appl9 = output[l,9,1]
appl10 = output[l,10,1]
appl11 = output[l,11,1]
appl12 = output[l,12,1]
appl13 = output[l,13,1]
appl14 = output[l,14,1]
appl15 = output[l,15,1]
subset = rawdata[c(appl1,appl2,appl3,appl4,appl5,appl6,appl7,appl8,appl9,appl10,
                   appl11,appl12,appl13,appl14,appl15),]
names = names_and_numbers[subset[,1],c(2:4)]
names_attributes = responses[subset[,1],c(5:16)]
final.table = cbind(names,names_attributes)
write.csv(final.table,"model1.csv")
subset = subset[,2:12]

#graph for all
for (j in 1:11) {
  a = sum(subset[,j]==1)/size_jury
  b = sum(subset[,j]==2)/size_jury
  c = sum(subset[,j]==3)/size_jury
  d = sum(subset[,j]==4)/size_jury
  e = sum(subset[,j]==5)/size_jury
  f = sum(subset[,j]==6)/size_jury
  g = sum(subset[,j]==7)/size_jury
  h = sum(subset[,j]==8)/size_jury
  y <- c(a,b,c,d,e,f,g,h)
  aa = ifelse(as.character(attnames[2,j])=="0","z",as.character(attnames[2,j]))
  bb = ifelse(as.character(attnames[3,j])=="0","zz",as.character(attnames[3,j]))
  cc = ifelse(as.character(attnames[4,j])=="0","zzz",as.character(attnames[4,j]))
  dd = ifelse(as.character(attnames[5,j])=="0","zzzzz",as.character(attnames[5,j]))
  ee = ifelse(as.character(attnames[6,j])=="0","zzzz",as.character(attnames[6,j]))
  ff = ifelse(as.character(attnames[7,j])=="0","zzz",as.character(attnames[7,j]))
  gg = ifelse(as.character(attnames[8,j])=="0","zz",as.character(attnames[8,j]))
  hh = ifelse(as.character(attnames[9,j])=="0","z",as.character(attnames[9,j]))
  x <- c(aa,bb,cc,dd,ee,ff,gg,hh)
  x <- factor(x,levels=x)
  p = ggplot() +
    geom_bar(aes(x=x,y=y), stat="identity") +
    geom_segment(aes(x = 0.5, xend = 1.5, y = goal[3,j], yend = goal[3,j], col="red")) +
    geom_segment(aes(x = 1.5, xend = 2.5, y = goal[4,j], yend = goal[4,j], col="red")) +
    geom_segment(aes(x = 2.5, xend = 3.5, y = goal[5,j], yend = goal[5,j], col="red")) +
    geom_segment(aes(x = 3.5, xend = 4.5, y = goal[6,j], yend = goal[6,j], col="red")) +
    geom_segment(aes(x = 4.5, xend = 5.5, y = goal[7,j], yend = goal[7,j], col="red")) +
    geom_segment(aes(x = 5.5, xend = 6.5, y = goal[8,j], yend = goal[8,j], col="red")) +
    geom_segment(aes(x = 6.5, xend = 7.5, y = goal[9,j], yend = goal[9,j], col="red")) +
    geom_segment(aes(x = 7.5, xend = 8.5, y = goal[10,j], yend = goal[10,j], col="red")) +
    theme(legend.position = "none") +
    labs(title = colnames(goal)[j]) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y=element_blank())
  plot(p)
  if (j==1) ggsave("model1.1.jpg")
  if (j==2) ggsave("model1.2.jpg")
  if (j==3) ggsave("model1.3.jpg")
  if (j==4) ggsave("model1.4.jpg")
  if (j==5) ggsave("model1.5.jpg")
  if (j==6) ggsave("model1.6.jpg")
  if (j==7) ggsave("model1.7.jpg")
  if (j==8) ggsave("model1.8.jpg")
  if (j==9) ggsave("model1.9.jpg")
  if (j==10) ggsave("model1.10.jpg")
  if (j==11) ggsave("model1.11.jpg")
}

#second best
dim(output)
output = output[-l,,]
dim(output)

k = min(output[,ncol(output),1])
k
l = match(k,output[,ncol(output),1])
appl1 = output[l,1,1]
appl2 = output[l,2,1]
appl3 = output[l,3,1]
appl4 = output[l,4,1]
appl5 = output[l,5,1]
appl6 = output[l,6,1]
appl7 = output[l,7,1]
appl8 = output[l,8,1]
appl9 = output[l,9,1]
appl10 = output[l,10,1]
appl11 = output[l,11,1]
appl12 = output[l,12,1]
appl13 = output[l,13,1]
appl14 = output[l,14,1]
appl15 = output[l,15,1]
subset = rawdata[c(appl1,appl2,appl3,appl4,appl5,appl6,appl7,appl8,appl9,appl10,
                   appl11,appl12,appl13,appl14,appl15),]
names = names_and_numbers[subset[,1],c(2:4)]
names_attributes = responses[subset[,1],c(5:16)]
final.table = cbind(names,names_attributes)
write.csv(final.table,"model2.csv")
subset = subset[,2:12]

#graph for all
for (j in 1:11) {
  a = sum(subset[,j]==1)/size_jury
  b = sum(subset[,j]==2)/size_jury
  c = sum(subset[,j]==3)/size_jury
  d = sum(subset[,j]==4)/size_jury
  e = sum(subset[,j]==5)/size_jury
  f = sum(subset[,j]==6)/size_jury
  g = sum(subset[,j]==7)/size_jury
  h = sum(subset[,j]==8)/size_jury
  y <- c(a,b,c,d,e,f,g,h)
  aa = ifelse(as.character(attnames[2,j])=="0","z",as.character(attnames[2,j]))
  bb = ifelse(as.character(attnames[3,j])=="0","zz",as.character(attnames[3,j]))
  cc = ifelse(as.character(attnames[4,j])=="0","zzz",as.character(attnames[4,j]))
  dd = ifelse(as.character(attnames[5,j])=="0","zzzzz",as.character(attnames[5,j]))
  ee = ifelse(as.character(attnames[6,j])=="0","zzzz",as.character(attnames[6,j]))
  ff = ifelse(as.character(attnames[7,j])=="0","zzz",as.character(attnames[7,j]))
  gg = ifelse(as.character(attnames[8,j])=="0","zz",as.character(attnames[8,j]))
  hh = ifelse(as.character(attnames[9,j])=="0","z",as.character(attnames[9,j]))
  x <- c(aa,bb,cc,dd,ee,ff,gg,hh)
  x <- factor(x,levels=x)
  p = ggplot() +
    geom_bar(aes(x=x,y=y), stat="identity") +
    geom_segment(aes(x = 0.5, xend = 1.5, y = goal[3,j], yend = goal[3,j], col="red")) +
    geom_segment(aes(x = 1.5, xend = 2.5, y = goal[4,j], yend = goal[4,j], col="red")) +
    geom_segment(aes(x = 2.5, xend = 3.5, y = goal[5,j], yend = goal[5,j], col="red")) +
    geom_segment(aes(x = 3.5, xend = 4.5, y = goal[6,j], yend = goal[6,j], col="red")) +
    geom_segment(aes(x = 4.5, xend = 5.5, y = goal[7,j], yend = goal[7,j], col="red")) +
    geom_segment(aes(x = 5.5, xend = 6.5, y = goal[8,j], yend = goal[8,j], col="red")) +
    geom_segment(aes(x = 6.5, xend = 7.5, y = goal[9,j], yend = goal[9,j], col="red")) +
    geom_segment(aes(x = 7.5, xend = 8.5, y = goal[10,j], yend = goal[10,j], col="red")) +
    theme(legend.position = "none") +
    labs(title = colnames(goal)[j]) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y=element_blank())
  plot(p)
  if (j==1) ggsave("model2.1.jpg")
  if (j==2) ggsave("model2.2.jpg")
  if (j==3) ggsave("model2.3.jpg")
  if (j==4) ggsave("model2.4.jpg")
  if (j==5) ggsave("model2.5.jpg")
  if (j==6) ggsave("model2.6.jpg")
  if (j==7) ggsave("model2.7.jpg")
  if (j==8) ggsave("model2.8.jpg")
  if (j==9) ggsave("model2.9.jpg")
  if (j==10) ggsave("model2.10.jpg")
  if (j==11) ggsave("model2.11.jpg")
}

