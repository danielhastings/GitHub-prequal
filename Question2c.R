######monotonic data----
#Simulate data 
m<-matrix(ncol = 3, nrow = 1000)
m[,2]<-rep(c(1:20), each=50) #create values for temperature
for (y in 1:1000) m[y,3]<-(.2+(m[y,2]*0.035))# create a vector of probabilities of survival for binomial distribution that are linearly related to temperature data.

for (y in 1:1000) m[y,1]<- rbinom(1,1,prob = m[y,3]) #generate mortality data using binomial distribution.
m<-m[,-3] #remove probability data
m<-as.data.frame(m)#change format to work with glm model
colnames(m)<-(c("survival", "temperature")) #assign column names

#construct logistic regression model

Monotonicsurvivalmodel<-glm(m$survival~m$temperature,data = m,family = binomial)
summary(Monotonicsurvivalmodel)
#for every unit increase in temperature, the log-odds of survival increase by 0.142

#####non-monotonic data----

n<-matrix(ncol = 3, nrow = 1000)
n[,2]<-rep(c(21:40), each=50) 
for (y in 1:1000) n[y,3]<-(.935-(m[y,2]*0.035)) 
for (y in 1:1000) n[y,1]<- rbinom(1,1,prob = n[y,3])
n<-n[,-3]
n<-as.data.frame(n)
colnames(n)<-c("survival", "temperature")
nm<-rbind(m,n)

#construct model

nonmonotonicmodel<-glm(survival~temperature, data = nm, family = binomial)
summary(nonmonotonicmodel) #temperature is not a significant predictor of mortality, largely because of the gaussian distribution of the temperature effect. we need to incorporate the gaussian function into the model. 

library(segmented)
segfit<-segmented(nonmonotonicmodel, seg.Z = ~temperature, npsi = 1)
summary(segfit)
require(ggplot2)
z<-broken.line(segfit)$
ggplot(nm, aes(x = temperature, y = survival)) + 
  geom_point() +
  geom_line(aes(x = temperature, y = z), color = 'blue')
length(z)
