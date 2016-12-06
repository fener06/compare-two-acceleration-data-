#name and name1 are the acceleration data's. statneu is the exercise number
#param is the number of the movement you want to analyze

compare <- function(name, name1, stateneu, param) {

	abstand1<-0
	abstand2<-0
	div1<-0
state<-stateneu
#name<-ac1$accelerometerAccelerationX
#name1<-ac2$accelerometerAccelerationX
	x <- 1:length(name)/100
	y <- name
	x2 <- 1:length(name1)/100
	y2 <- name1
  peaks <- argmax(x, y, w=50, span=0.05)
  peaks2 <- argmax(x2, y2, w=50, span=0.05)
par(mfrow=c(1,1))  

  plot(x, y, cex=0.75, col="Gray", main=paste(paste(paste(as.character(substitute(name)[2]),as.character(substitute(name)[3]), sep="_"),"", sep="")," w = ", 50, ", span = ", 0.05, sep=""))
  dev.copy(pdf,paste(paste(as.character(substitute(name)[2]),as.character(substitute(name)[3]), sep="_"),"points.pdf", sep="_"), width=10, height=4)
dev.off()
  lines(x, peaks$y.hat,  lwd=2) #$
  dev.copy(pdf,paste(paste(as.character(substitute(name)[2]),as.character(substitute(name)[3]), sep="_"),"points_line.pdf", sep="_"), width=10, height=4)
dev.off()  
    y.min <- min(y)
    sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.min, peaks$y.hat[i]),
         col="Red", lty=2))
           ywerte<-peaks$y.hat[peaks$i]
  xwerte<-peaks$i
  while(length(ywerte) > param){if ( abs(mean(ywerte)-ywerte[1]) < abs(mean(ywerte)-ywerte[length(ywerte)]) ) { ywerte <- ywerte[-length(ywerte)];xwerte<-xwerte[-(length(ywerte)+1)] } else { ywerte <- ywerte[-1];xwerte<-xwerte[-1] }}
  
points(x[xwerte], ywerte, col="Red", pch=19, cex=1.25)
dev.copy(pdf,paste(paste(as.character(substitute(name)[2]),as.character(substitute(name)[3]), sep="_"),"pmaximum_10.pdf", sep="_"), width=10, height=4)
dev.off()
par(mfrow=c(1,1))    
  plot(x2, y2, cex=0.75, col="Gray", main=paste(paste(paste(as.character(substitute(name1)[2]),as.character(substitute(name1)[3]), sep="_"),"", sep="")," w = ", 50, ", span = ", 0.05, sep=""))
  dev.copy(pdf,paste(paste(as.character(substitute(name1)[2]),as.character(substitute(name1)[3]), sep="_"),"points.pdf", sep="_"), width=10, height=4)
dev.off()
  lines(x2, peaks2$y.hat,  lwd=2) #$
  dev.copy(pdf,paste(paste(as.character(substitute(name1)[2]),as.character(substitute(name1)[3]), sep="_"),"points_line.pdf", sep="_"), width=10, height=4)
dev.off() 
  lines(x2, peaks2$y.hat,  lwd=2) #$
  y2.min <- min(y2)
  sapply(peaks2$i, function(i) lines(c(x2[i],x2[i]), c(y2.min, peaks2$y.hat[i]),
         col="Red", lty=2))
           y2werte<-peaks2$y.hat[peaks2$i]
  x2werte<-peaks2$i
    while(length(y2werte) > param){if ( abs(mean(y2werte)-y2werte[1]) < abs(mean(y2werte)-y2werte[length(y2werte)]) ) { y2werte <- y2werte[-length(y2werte)];x2werte<-x2werte[-(length(y2werte)+1)] } else { y2werte <- y2werte[-1];x2werte<-x2werte[-1] }}
points(x2[x2werte], y2werte, col="Red", pch=19, cex=1.25)
dev.copy(pdf,paste(paste(as.character(substitute(name1)[2]),as.character(substitute(name1)[3]), sep="_"),"pmaximum_10.pdf", sep="_"), width=10, height=4)
dev.off()
  #position<-which(peaks$y.hat[peaks$i] == min(peaks$y.hat[peaks$i]))
  #while(length(ywerte) > 10){position<-which(ywerte == min(ywerte));ywerte<-ywerte[-position];xwerte<-xwerte[-position]}
  for(k in 1:length(xwerte)-1){abstand1[k]<-xwerte[k+1]-xwerte[k]}
par(mfrow=c(3,3))
for(k in 1:(length(xwerte)-1)){plot(name[xwerte[k]:xwerte[k+1]],cex=0.75, col="Gray",main=paste(paste(paste(as.character(substitute(name)[2]), paste(substr(as.character(substitute(name)[3]), 1, 3), substr(as.character(substitute(name)[3]), 26, 26), sep="_"),sep="_"),paste(k),sep=" Part "),"|",paste(abstand1[k]/100) ,"Seconds",sep=" "),type = "l")}
dev.copy(pdf,paste(paste(as.character(substitute(name)[2]),as.character(substitute(name)[3]), sep="_"),"Parts.pdf", sep="_"), width=10, height=10)
dev.off()

for(k in 1:(length(xwerte)-1)){plot(name[xwerte[k]:xwerte[k+1]],cex=0.75, col="Gray",main=paste(paste(paste(as.character(substitute(name1)[2]), paste(substr(as.character(substitute(name1)[3]), 1, 3), substr(as.character(substitute(name1)[3]), 26, 26), sep="_"),sep="_"),paste(k),sep=" Part "),"|",paste(abstand1[k]/100) ,"Seconds",sep=" "),type = "l")}

dev.copy(pdf,paste(paste(as.character(substitute(name1)[2]),as.character(substitute(name1)[3]), sep="_"),"Parts.pdf", sep="_"), width=10, height=10)
dev.off()

dist<-0
ndist<-0
par(mfrow=c(1,1))
for(k in 1:(length(xwerte)-1)){
a <- dtw(name[xwerte[k]:xwerte[k+1]],name1[x2werte[k]:x2werte[k+1]],keep=TRUE, type="twoway")
dist[k]<-a$distance
ndist[k]<-a$normalizedDistance}
punktedist<-0
for(k in 1: length(dist)){if ( dist[k] < 10) {
   punktedist[k]<-6
} else if ( dist[k] < 2.5) {
   punktedist[k]<-5
} else if ( dist[k] < 5) {
   punktedist[k]<-4
} else if ( dist[k] < 7.5) {
   punktedist[k]<-3
} else if ( dist[k] < 15) {
   punktedist[k]<-2
} else
   punktedist[k]<-1}
gesamtdist<-0
for(k in 1:length(punktedist)){gesamtdist<-gesamtdist+punktedist[k]}
abstandxwerte<-0
for(k in 1:length(xwerte)-1){abstandxwerte[k]<-xwerte[k+1]-xwerte[k]}
abstandx2werte<-0
for(k in 1:length(x2werte)-1){abstandx2werte[k]<-x2werte[k+1]-x2werte[k]}

prozent<- abstandxwerte/abstandx2werte
punkte<-0	
for(k in 1: length(prozent)){if ( prozent[k] < 1.025 && 0.975 < prozent[k]) {
   punkte[k]<-4.6
} else if ( prozent[k] < 1.05 && 0.95 < prozent[k]) {
   punkte[k]<-3.6
} else if ( prozent[k] < 1.1 && 0.9 < prozent[k]) {
   punkte[k]<-2.6
} else if ( prozent[k] < 1.15 && 0.85 < prozent[k]) {
   punkte[k]<-1.6
} else if ( prozent[k] < 1.2 && 0.8 < prozent[k]) {
   punkte[k]<-0.6
} else
   punkte[k]<-0}
assign("punkte1" , punkte, envir = .GlobalEnv)

gesamt<-0
for(k in 1:length(punkte)){gesamt<-gesamt+punkte[k]}




for(k in 1:(length(xwerte)-1)){
a <- dtw(name[xwerte[k]:xwerte[k+1]],name1[x2werte[k]:x2werte[k+1]],keep=TRUE, type="twoway")
dist[k]<-a$distance
ndist[k]<-a$normalizedDistance
#print(a$distance)
#print(a$normalizedDistance)
plot( dtw(name[xwerte[k]:xwerte[k+1]],name1[x2werte[k]:x2werte[k+1]],keep=TRUE,
      step=rabinerJuangStepPattern(6,"c")),
      type="twoway", main=paste(paste(paste(paste(paste("DTW Optimal Match of ",as.character(substitute(name)[2])),as.character(substitute(name1)[2]) , sep=" and "), k, sep=" | Part: "),paste(round(a$distance,2)),sep=" | dist: "), paste(round(a$normalizedDistance,2)),sep=" | ndist: "),
      xlab=paste(paste("Time axis | Score of DTW: ",paste(punktedist[k]),sep=" ")," | Score of Duration:", paste(punkte[k])))
legend("bottomleft",legend=c(paste(paste(paste(as.character(substitute(name)[2]), paste(substr(as.character(substitute(name)[3]), 1, 3), substr(as.character(substitute(name)[3]), 26, 26), sep="_"),sep="_"))),paste(paste(paste(as.character(substitute(name1)[2]), paste(substr(as.character(substitute(name1)[3]), 1, 3), substr(as.character(substitute(name1)[3]), 26, 26), sep="_"),sep="_")))), text.col=c("red","black"), lty=c(2,1), col=c("red","black"))

dev.copy(pdf,paste(paste(paste(paste(as.character(substitute(name)[2]), paste(substr(as.character(substitute(name)[3]), 1, 3), substr(as.character(substitute(name)[3]), 26, 26), sep="_"),sep="_"),paste(as.character(substitute(name1)[2]), paste(substr(as.character(substitute(name1)[3]), 1, 3), substr(as.character(substitute(name1)[3]), 26, 26), sep="_"),sep="_"),sep="_and_"),"dtw",paste(k),sep="_"),".pdf",sep=""), width=10, height=10)
dev.off()

}



assign("dist1" , dist, envir = .GlobalEnv)
assign("ndist1" , ndist, envir = .GlobalEnv)

print("DTW WERTE dist1 und ndist1")
print(dist)
print(ndist)
print("laenge")
print(length(ywerte)) 
print(length(xwerte))  
print(length(y2werte)) 
print(length(x2werte)) 
print("Supported ywerte und xwerte")
print(ywerte)
print(xwerte)
print("Zu analysierende y2werte und x2werte")
print(y2werte)
print(x2werte)

punktedist<-0
for(k in 1: length(dist)){if ( dist[k] < 5) {
   punktedist[k]<-6
} else if ( dist[k] < 6) {
   punktedist[k]<-5
} else if ( dist[k] < 8) {
   punktedist[k]<-4
} else if ( dist[k] < 9) {
   punktedist[k]<-3
} else if ( dist[k] < 10) {
   punktedist[k]<-2
} else
   punktedist[k]<-1}
gesamtdist<-0
punktedist <- (punktedist/param)*10
for(k in 1:length(punktedist)){gesamtdist<-gesamtdist+punktedist[k]}
print("Score of DTW partially")
print(dist)

print(punktedist)
assign("punktedist1" , punktedist, envir = .GlobalEnv)

print("Sum of Score by DTW")
print(gesamtdist)
assign("gesamtdist1" , gesamtdist, envir = .GlobalEnv)

#for(k in 1:(length(xwerte)-1)){plot( dtw(name[xwerte[k]:xwerte[k+1]],name1[x2werte[k]:x2werte[k+1]],keep=TRUE,
#      step=rabinerJuangStepPattern(6,"c")), type="twoway", main="DTW Optimal Match")}
  #points(x[xwerte], ywerte, col="Red", pch=19, cex=1.25)
#assign("abstandtest" , abstand, envir = .GlobalEnv)
#print(abstand)
return
#prozent<-abstand/mean(abstand)-1
#print(prozent)
#print(ywerte)
#print(xwerte)
assign("dist1" , dist, envir = .GlobalEnv)
assign("dist1" , dist, envir = .GlobalEnv)



abstandxwerte<-0
for(k in 1:length(xwerte)-1){abstandxwerte[k]<-xwerte[k+1]-xwerte[k]}
abstandx2werte<-0
for(k in 1:length(x2werte)-1){abstandx2werte[k]<-x2werte[k+1]-x2werte[k]}

prozent<- abstandxwerte/abstandx2werte
punkte<-0	
for(k in 1: length(prozent)){if ( prozent[k] < 1.025 && 0.975 < prozent[k]) {
   punkte[k]<-4.6
} else if ( prozent[k] < 1.05 && 0.95 < prozent[k]) {
   punkte[k]<-3.6
} else if ( prozent[k] < 1.1 && 0.9 < prozent[k]) {
   punkte[k]<-2.6
} else if ( prozent[k] < 1.15 && 0.85 < prozent[k]) {
   punkte[k]<-1.6
} else if ( prozent[k] < 1.2 && 0.8 < prozent[k]) {
   punkte[k]<-0.6
} else
   punkte[k]<-0}
print("Score of duration time partially")
print(prozent)
punkte <- (punkte/param)*10
print(punkte)
assign("punkte1" , punkte, envir = .GlobalEnv)

gesamt<-0
for(k in 1:length(punkte)){gesamt<-gesamt+punkte[k]}
print("Total Score of duration time")
print(gesamt)
assign("gesamt1" , gesamt, envir = .GlobalEnv)
prozenty <- ywerte/y2werte
punktey<-0	
for(k in 1: length(prozenty)){if ( prozenty[k] < 1.05 && 0.95 < prozenty[k]) {
   punktey[k]<-5
} else if ( prozenty[k] < 1.1 && 0.9 < prozenty[k]) {
   punktey[k]<-4
} else if ( prozenty[k] < 1.15 && 0.85 < prozenty[k]) {
   punktey[k]<-3
} else if ( prozenty[k] < 1.2 && 0.8 < prozenty[k]) {
   punktey[k]<-2
} else if ( prozenty[k] < 1.25 && 0.75 < prozenty[k]) {
   punktey[k]<-1
} else
   punktey[k]<-0}
print("Not relevant peak strenth score partially")
print(punktey)
assign("punktey1" , punktey, envir = .GlobalEnv)

gesamty<-0
for(k in 1:length(punktey)){gesamty<-gesamty+punktey[k]}
print("Not relevant total peak strenth score")
print(gesamty)
assign("gesamty1" , gesamty, envir = .GlobalEnv)
assign("punkte1" , punkte, envir = .GlobalEnv)

par(mfrow=c(2,2))


barplot((punktedist1),col="gray",names.arg = c(1:(length(punktedist1))),border="blue",
main=paste(paste("Score of",paste(as.character(substitute(name1)[2])),sep=" "),paste(gesamtdist),sep=" is "), xlab="Parts of Repetition",
ylab="Score determined by DTW",
ylim=c(0,6),xlim=c(0,length(punktedist1)+6) )

barplot(punkte,names.arg = c(1:(length(punkte))),col="gray",border="blue",
main=paste(paste("Score of",paste(as.character(substitute(name1)[2])),sep=" "),paste(gesamt),sep=" is "), xlab="Parts of Repetition",
ylab="Score determined by Duration",
ylim=c(0,5),xlim=c(0,length(punkte)+6) )
all<-gesamt+gesamtdist
leer<-100-gesamt-gesamtdist
kreis<- c(gesamt, gesamtdist, leer)
labels <- c("Speed", "Movement", "")
pie(kreis,labels,main = "Overall Score",xlab=paste("Score is",all,"of 100"))

plot.new()
text(x = 0.5, y = 0.5, paste("Exercise No.:",state[1]), 
     cex = 1.6, col = "black")
print("Exercise Number:")
print(state[1])
dev.copy(pdf,paste(paste(paste(paste(as.character(substitute(name)[2]), paste(substr(as.character(substitute(name)[3]), 1, 3), substr(as.character(substitute(name)[3]), 26, 26), sep="_"),sep="_"),paste(as.character(substitute(name1)[2]), paste(substr(as.character(substitute(name1)[3]), 1, 3), substr(as.character(substitute(name1)[3]), 26, 26), sep="_"),sep="_"),sep="_and_"),"score"),".pdf",sep=""), width=10, height=10)
dev.off()
#abstandxwerte

absxsum <- round(sum(abstandxwerte)/100, digits = 2)
absx2sum <- round(sum(abstandx2werte)/100, digits = 2)

a<- c(as.character(substitute(name)[2]),as.character(substitute(name1)[2]),paste(substr(as.character(substitute(name)[3]), 1, 3), substr(as.character(substitute(name)[3]), 26, 26), sep="_"),round(sum(dist), digits = 2),round(sum(ndist), digits = 2),absxsum, absx2sum,round(((absxsum/absx2sum)-1)*100, digits = 2),gesamtdist,gesamt,gesamtdist+gesamt ,state[1])


assign("abstand1" , abstandxwerte, envir = .GlobalEnv)
assign("abstand2" , abstandx2werte, envir = .GlobalEnv);print(paste(substr(as.character(substitute(name)[3]), 1, 3), substr(as.character(substitute(name)[3]), 26, 26), sep="_"))
return(a)

}
