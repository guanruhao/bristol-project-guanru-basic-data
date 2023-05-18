



vector1 <- seq(from = as.POSIXct("2023-05-15 05:00:00"), 
               to = as.POSIXct("2023-05-15 22:00:00"), 
               by = "30 min")

plot(vector1,predict,type="p",col="red",pch=20,cex=1,
     xlab="time",ylab="Passengers number",xaxt="n",ylim=c(0,2000))
axis(1, at = vector1, labels = format(vector1, "%H:%M"), cex.axis = 0.8, las = 2)
points(vector1,real,type="p",col="blue",pch=20,cex=1)
legend("topleft",legend = c("predict", "real"), 
       col = c("red", "blue"), pch= 20, cex =1)

grid()



