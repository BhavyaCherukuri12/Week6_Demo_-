
?beavers
View(beaver2)
str(beaver2)
Veiw(beaver2)
? factor
is.factor(beaver2)



Beaver_Data<-beaver2


#H0: Body temperature is not affected by activity 
#H1: Bodyy temperature is affected by activity 

# Convert Activity variable to factor type if needed

beaver2$activ <- factor(beaver2$activ, labels = c("no", "yes"))
str(beaver2)

library()
?hist

install.packages("tidyverse")
install.packages("stats")

library(ggplot2)

# Histogram of Body Temperature

str(Beaver_Data)
numeric(Beaver_Data$temp)

# Histogram of Body Temperature
hist(Beaver_Data$temp, breaks = 20, main = "Histogram of Body Temperature")


windows(16, 10)
ggplot(beaver2,aes(x=temp))+geom_histogram()+theme_bw()

ggplot(beaver2,aes(x=temp))+geom_histogram(breaks=seq(36,38,.2))
+theme_bw()+labs(x="temp",y="Activity")
+scale_y_continuous(breaks=seq(0,60,5))



install.packages("lattice")
library(lattice)

window(20,10)

attach(beaver2)
histogram(~temp | activ,
          data=beaver2,main="distribution of beavers activity data",
          xlab="temparatures(degrees)",ylab="Activity %")
detach(beaver2)
attach(beaver2)
windows(16,10)

qqnorm(temp)
qqline(temp,col="red")


opar<-par(no.readonly = TRUE)
#window(20,10)

par(mfrow=c(1,2))

with(beaver2,{
  qqnorm(temp[activ=="yes"],
         main="Beavers active data")
         qqline(temp[activ=="yes"])
         
})




with(beaver2,{
  qqnorm(temp[activ=="no"],
         main = "Beavers inactive data")
  qqline (temp[activ=="no"]
          )
})


normality_test<- shapiro.test(beaver2$temp)
normality_test


# Independent samples t-test
t.test(temp ~ activ, data = beaver2)


