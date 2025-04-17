#Github帳號名:yiting921107
#B114030015
#王奕婷
rm(list=ls())
data <- read.csv("C:/Users/user/Downloads/Mobiles.csv")

#第一題
apple <- subset(data,Company=="Apple")
samesung <-  subset(data,Company=="Samsung")
mean(apple$PriceUSA)#1028.485
mean(samesung$PriceUSA)#748.4318
sd(apple$PriceUSA)#247.8969
sd(samesung$PriceUSA)#515.3826

#第二題
boxplot(PriceChina~Year,data=data,xlab="年份",ylab="中國售價",main="盒鬚圖")

hist(data$PriceChina,xlab="中國售價",ylab="手機數量",main="長條圖",breaks=20)

#第三題
install.packages("ggplot2")
library(ggplot2)

ggplot(data,aes(x=Weight,y=Battery))+
  geom_point(color = "steelblue", size = 2, alpha = 0.6)+
  labs(title="手機重量-電池容量的點陣圖",x="手機重量",y="電池容量")

#第四題
install.packages("car")
library(car)

leveneTest(PriceUSA~factor(Company),data=data)
#因為p-value為5.218e-14，p-value值小於0.05，因此拒絕虛無假說，不同廠牌中美國售價之變異數不同。
#第五題
run<- aov(Weight~factor(Company),data=data)
summary(run)

#因為p-value為3.55e-05，p-value值小於0.05，因此拒絕虛無假說，各廠牌中手機重量有顯著差異。。


#第六題
run2<- aov(Weight~factor(Company)+factor(Battery),data=data)
summary(run2)


#因為p-value均為<2e-16，p-value值小於0.05，因此拒絕虛無假說，廠牌及電池容量對手機重量有顯著影響。

#第七題

year2024 <- subset(data,Year=="2024")
apple_year_2024 = table(year2024$Company)   
prop.table(apple_year_2024) 
#0.12

#第八題(1)
names(data)[1] <- "company"
names(data)[2] <- "name"
names(data)[3] <- "weight"
names(data)[4] <- "battery"
names(data)[5] <- "price.c"
names(data)[6] <- "price.u"
names(data)[7] <- "year"

#第八題(2)
install.packages("tidyverse")
library(tidyverse)
data_new <- mutate(data,price.t=price.u*32)
data_new$price.t[1:10]
#第八題(3)
write.table(data_new,file="C:/Users/user/Desktop/B114030015 王奕婷 期中/B114030015.csv",sep=",",row.names=F, na = "NA")
