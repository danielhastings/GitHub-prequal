library(mosaicCore)
set.seed(300)
m<-matrix(ncol = 5, nrow = 1000)
#####temp data-----
m[,1]<-rep(c(1:20), each=50) #temperature means
for (y in c(1:1000)) m[y,2]<-rnorm(1, mean = m[y,1],sd=2)# simulated temperature data 
#####survival data-----
for (y in 1:1000) m[y,3]<-70-((m[y,2]-12)^2)#quadratic relationship between temp and survival 
for (y in 1:1000) m[y,4]<-ilogit(m[y,3])#inverse logit transformation to return a probability of survival
for (y in 1:1000) m[y,5]<-rbinom(1,1,m[y,4])#binomial survival data

####check data----
#View(m)
mean(m[,5])
mean(m[,4])
plot(m[,2],m[,3], main = "temperature vs. quadratic survival")
plot(m[,2],m[,4],main = "temperature vs. ilogit survival")
plot(m[,2],m[,5],main = "temperature vs. binomial survival")

####arrange data for model----
m<-as.data.frame(m)#change format to work with glm

colnames(m)<-(c("temp_means", "temperature","quadratic_survival", "ilogit_survival", "survival")) #assign column names
m$temp_sq<-m$temperature^2 # create squared temperature column because of issue below

#####model-----

model<-glm(survival~temp_sq+temperature,data = m,family = binomial)# model with temp and quadratic temp
summary(model)# estimates roughly match the original equation of y=-x^2+24x+74 from simulation
