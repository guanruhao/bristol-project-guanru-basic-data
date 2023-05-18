source("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/1 乘客信息读取.R")


#cornered link analysis
total=c(1:71)
stations=c(1,2,3)
#target destinations
data1=data[which(data$origin %in% setdiff(total,stations)
                 &
                   data$destination %in% stations),]
dir_usage[6]
#target origins
data2=data[which(data$origin %in% stations
                 &
                   data$destination %in% setdiff(total,stations)),]
dir_usage[5]


#centered link 


