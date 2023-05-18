load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/有路径的训练初始数据/passenger data 300k.RData")
data=exported_passenger_data
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 1 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 2 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 3 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 4 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 5 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 6 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 7 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 8 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 9 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 10 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 11 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 12 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 13 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 14 .RData")
data=rbind(data,exported_passenger_data)
load("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/passenger basic data 15 .RData")
data=rbind(data,exported_passenger_data)
rm(exported_passenger_data)
colnames(data)=c("enter_time", "leave_time","origin","destination","edge_path")
data=as.data.frame(data)
data=data[order(unlist(data$enter_time)), ]
source('C:/Users/10274/Desktop/不均匀分布+效用/1_3 optimal path with highest utility.R')




load(file="C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/node_path.RData")

dir1=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/有向使用量.csv")
dir2=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/有路径的训练初始数据/有向使用量.csv")

AAA=numeric(148)
for(i in 1:148){
  AAA[i]=sum(as.vector(unlist(dir1[i,c(5:19)])))
}
BBB=dir2[,6]
dir_usage=AAA+BBB
rm(AAA,BBB,i,nodes,edges,station_seq,dir1,dir2)



AAA=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/有路径的训练初始数据/时序使用量.csv")
AAA=AAA[,-1]

BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 1 .csv")
BBB=BBB[,-1]
time=BBB

BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 2 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 3 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 4 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 5 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 6 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 7 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 8 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 9 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 10 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 11 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 12 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 13 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 14 .csv")
BBB=BBB[,-1]
time=time+BBB
BBB=read.csv("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/300k0.01/timed 15 .csv")
BBB=BBB[,-1]
time=time+BBB

timed_usage=time+AAA
timed_usage=as.matrix(timed_usage)
timed_usage=timed_usage[,-2281]
rm(AAA,BBB,time)



edge2node=function(vector){
  bbb=line_frame[vector,]
  vec=as.vector(c(unlist(bbb[,1]),unlist(bbb[length(bbb)/3,2])))
  return(vec)
}






