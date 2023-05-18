

data3=data[data$origin==48 & data$destination==25,]



edge2node(data3[1,5][[1]])
edge2node(data3[2,5][[1]])


aaaaa=unlist(data3$leave_time)-unlist(data3$enter_time)
bbbbb=numeric(209)
for(i in 1:209){
  if(identical(data3[i,5],data3[1,5])==T){bbbbb[i]=1}
  else{bbbbb[i]=2}
}

sum(bbbbb==1)/209
sum(bbbbb==2)/209


#sample size=121
meana=mean(aaaaa[which(bbbbb==1)])
vara=var(aaaaa[which(bbbbb==1)])

#sample size=88
meanb=mean(aaaaa[which(bbbbb==2)])
varb=var(aaaaa[which(bbbbb==2)])

(vara/2)/(varb/3)

pf((vara/2)/(varb/3),120,87)






