source("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/1 乘客信息读取.R")

sequence=which(data$enter_time>=1200 & data$enter_time<1320
               &data$origin==58 & data$destination==10)

data1=data[sequence,]
node1=node_path[sequence]

edgename=1
plot(c(601:2880),timed_usage[edgename,],type="l")
line_frame[edgename,]


edge.betweenness(graph,directed = T)

length(which(unlist(data$edge_path)==edgename))/600000


edge.betweenness(graph,directed = T)[edgename]/sum(edge.betweenness(graph,directed = T))














