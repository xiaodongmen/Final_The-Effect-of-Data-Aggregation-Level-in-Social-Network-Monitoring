# Example code for reading the output object from the simulations to generate Figures 9 and 10
library(lattice)

print("p=0.2")
s.tilda.work=output2[2,]
print(mean(s.tilda.work))
detection.timepoint=output2[3,]
print(mean(detection.timepoint))
agg.s.tilda.work=output2[4,]
print(mean(agg.s.tilda.work))
agg.detection.timepoint=output2[5,]
print(mean(agg.detection.timepoint))

print("p=0.5")
s.tilda.work=output2[7,]
print(mean(s.tilda.work))
detection.timepoint=output2[8,]
print(mean(detection.timepoint))
agg.s.tilda.work=output2[9,]
print(mean(agg.s.tilda.work))
agg.detection.timepoint=output2[10,]
print(mean(agg.detection.timepoint))

print("p=1")
s.tilda.work=output2[12,]
print(mean(s.tilda.work))
detection.timepoint=output2[13,]
print(mean(detection.timepoint))
agg.s.tilda.work=output2[14,]
print(mean(agg.s.tilda.work))
agg.detection.timepoint=output2[15,]
print(mean(agg.detection.timepoint))

print("p=1.5")
s.tilda.work=output2[17,]
print(mean(s.tilda.work))
detection.timepoint=output2[18,]
print(mean(detection.timepoint))
agg.s.tilda.work=output2[19,]
print(mean(agg.s.tilda.work))
agg.detection.timepoint=output2[20,]
print(mean(agg.detection.timepoint))

print("p=2")
s.tilda.work=output2[22,]
print(mean(s.tilda.work))
detection.timepoint=output2[23,]
print(mean(detection.timepoint))
agg.s.tilda.work=output2[24,]
print(mean(agg.s.tilda.work))
agg.detection.timepoint=output2[25,]
print(mean(agg.detection.timepoint))

print("p=2.5")
s.tilda.work=output2[27,]
print(mean(s.tilda.work))
detection.timepoint=output2[28,]
print(mean(detection.timepoint))
agg.s.tilda.work=output2[29,]
print(mean(agg.s.tilda.work))
agg.detection.timepoint=output2[30,]
print(mean(agg.detection.timepoint))

print("p=3")
s.tilda.work=output2[32,]
print(mean(s.tilda.work))
detection.timepoint=output2[33,]
print(mean(detection.timepoint))
agg.s.tilda.work=output2[34,]
print(mean(agg.s.tilda.work))
agg.detection.timepoint=output2[35,]
print(mean(agg.detection.timepoint))

print("p=4")
s.tilda.work=output2[37,]
print(mean(s.tilda.work))
detection.timepoint=output2[38,]
print(mean(detection.timepoint))
agg.s.tilda.work=output2[39,]
print(mean(agg.s.tilda.work))
agg.detection.timepoint=output2[40,]
print(mean(agg.detection.timepoint))

print("p=5")
s.tilda.work=output2[42,]
print(mean(s.tilda.work))
detection.timepoint=output2[43,]
print(mean(detection.timepoint))
agg.s.tilda.work=output2[44,]
print(mean(agg.s.tilda.work))
agg.detection.timepoint=output2[45,]
print(mean(agg.detection.timepoint))


agg.detection.time=c(output2[5,], output2[10,], output2[15,], output2[20,], output2[25,], output2[30,], output2[35,], output2[40,], output2[45,])
agg.detection.time=(agg.detection.time+40)*20
shift.time=c(rep(840, 50), rep(841, 50), rep(842, 50), rep(843, 50), rep(844, 50), rep(845, 50), rep(846, 50), rep(847, 50), rep(848, 50),
             rep(849, 50), rep(850, 50), rep(851, 50), rep(852, 50), rep(853, 50), rep(854, 50), rep(855, 50), rep(856, 50), rep(857, 50),
             rep(858, 50), rep(859, 50))
shift.time=rep(shift.time, 9)

agg.detection.time.dist=agg.detection.time-shift.time


select2=agg.detection.time.dist[which(agg.detection.time.dist>0)]
d=barchart(table(select2)/length(select2)*100, origin=0, auto.key=TRUE, main="(d)",
         horizontal = F, col="grey", ylab="Relative Frequency (%)", xlab="Conditional Expected Delay",
         axis = function(side, line.col = "black", ...) {
           # Only draw axes on the left and bottom
           if(side %in% c("left","bottom")) {
             # Call default axis drawing function
             axis.default(side = side, line.col = "black", ...)
           }
         }, scales=list(x=list(at=c(1, 5, 10, 15, 20), labels=c("1", "5", "10", "15", "20"))))

print(a, position=c(0, 0.5, 0.5, 1), more=T)
print(b, position=c(0.5, 0.5, 1, 1), more=T)
print(c, position=c(0, 0, 0.5, 0.5), more=T)
print(d, position=c(0.5, 0, 1, 0.5))


detection.time=c(output2[3,], output2[8,], output2[13,], output2[18,], output2[23,], output2[28,], output2[33,], output2[38,], output2[43,])
agg.detection.time=(detection.time+40)
shift.time=c(rep(840, 50), rep(841, 50), rep(842, 50), rep(843, 50), rep(844, 50), rep(845, 50), rep(846, 50), rep(847, 50), rep(848, 50),
             rep(849, 50), rep(850, 50), rep(851, 50), rep(852, 50), rep(853, 50), rep(854, 50), rep(855, 50), rep(856, 50), rep(857, 50),
             rep(858, 50), rep(859, 50))
shift.time=rep(shift.time, 9)

agg.detection.time.dist=agg.detection.time-shift.time

title2="1x Aggretation +150% shift \nPoisson Time to Signal Distribution"

select=agg.detection.time.dist[which(agg.detection.time.dist>0)]
for(i in 1:length(select)){
  if(select[i]==17){
    select[i]=9
  }
}

barchart(table(select)/length(select)*100, origin=0, auto.key=TRUE, 
         horizontal = F, col="grey", ylab="Relative Frequency (%)", xlab="Conditional Expected Delay",
         axis = function(side, line.col = "black", ...) {
           # Only draw axes on the left and bottom
           if(side %in% c("left","bottom")) {
             # Call default axis drawing function
             axis.default(side = side, line.col = "black", ...)
           }
         })


agg.detection.time.dist=agg.detection.time.dist[which(agg.detection.time.dist>0)]
agg.detection.time.dist=as.data.frame(agg.detection.time.dist)
ggplot(agg.detection.time.dist, aes(x=agg.detection.time.dist)) +
  stat_bin(aes(y=..density..), binwidth = 2, geom="line", colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


ggplot(df, aes(x=weight)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
# Color by groups
ggplot(df, aes(x=weight, color=sex, fill=sex)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2) 
