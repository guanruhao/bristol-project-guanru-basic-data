
source("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/1 乘客信息读取.R")

node_path=rep(list(c(1,1,1)),600000)

for (i in 1:600000){
  aaa=line_frame[data$edge_path[[i]],]
  length=length(aaa)/3
  if(length==1){aaa=t(aaa)}
  node_path[[i]]=as.vector(c(aaa[,1],as.vector(aaa[,2][length])))
}
save(node_path,file="C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/node_path.Rdata")



