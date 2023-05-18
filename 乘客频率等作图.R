
source("C:/Users/10274/Desktop/考虑拥挤程度的效用训练/1_1 passenger sampling principle.R")

vector1 <- seq(from = as.POSIXct("2023-05-15 05:00:00"), 
               to = as.POSIXct("2023-05-15 22:00:00"), 
               by = "60 min")


vector2=c("5~6","6~7","7~8","8~9","9~10","10~11","11~12","12~13","13~14",
          "14~15","15~16","16~17","17~18","18~19","19~20","20~21","21~22")
kkk=barplot(working_day,ylim=c(0,0.14),ylab = "passenger frequency",xlab = "time")
# 在每个柱子下方添加标记
text(x =kkk,y=working_day,labels =vector2,pos =3,cex=0.6)






