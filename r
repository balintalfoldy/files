setwd("c:/Data/egyetem/Southampton_GISRS/SkillsandProjects1/Assignment2")
data<-read.csv("anuran_species_richness.csv")

mean1<-apply(data[,-1],2,mean)
median1<-apply(data[,-1],2,median)

mode_func<-function(x){
	uniq<-unique(x)
	uniq[which.max(tabulate(match(x,uniq)))]
}
mode1<-apply(data[,-1],2,mode_func)
minimum<-apply(data[,-1],2,min)
maximum<-apply(data[,-1],2,max)
variance<-apply(data[,-1],2,var)
sdev<-apply(data[,-1],2,sd)
iqrange<-apply(data[,-1],2,IQR)

sumstat1<-matrix(c(mean1,median1,mode1,minimum,maximum,variance,sdev,iqrange),nrow=8, byrow=TRUE)
rownames(sumstat1)<-c("Arithmetic Mean","Median","Mode","Minimum","Maximum","Variance","Standard Deviation","IQR")
colnames(sumstat1)<-c("Species Richness","Total Edge [m]","Percent of Forest","Road Density","Traffic Density")
sumstat1<-as.table(sumstat1)
write.table(round(sumstat1,digits=2), file="sumstat1.txt",sep="\t")

figure1<-par(mfrow=c(3,3))
boxplot(data$species_richness,main="Species Richness")
boxplot(data$total_edge_m,main="Total Edge")
boxplot(data$percent_forest,main="Percent of Forest")
boxplot(data$road_density,main="Road Density")
boxplot(data$traffic_density,main="Traffic Density")


par(figure1)
