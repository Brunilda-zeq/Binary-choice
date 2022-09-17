rm(list=ls(all=TRUE))

setwd('C:/Users/Bru/Desktop/metaptuxiako/2o/vasilopoulos/2i ergasia')


library("mlogit")
library("dfidx")
library("Formula")

car= read.csv("CarBin.csv", header=TRUE,sep = ';')

car

carbin<-dfidx(car, varying = 5:22,sep = '',
              choice = "choice",
              idnames=c("chid","alt")
)

head(carbin,4)
head(idx(carbin), 8)
table(carbin$size)
carbin$size0=ifelse(carbin$size==0,1,0)
carbin$size1=ifelse(carbin$size==1,1,0)
carbin$size2=ifelse(carbin$size==2,1,0)
carbin$size3=ifelse(carbin$size==3,1,0)

carbin$typeregcar=ifelse(carbin$type=="regcar",1,0)
carbin$typesportuv=ifelse(carbin$type=="sportuv",1,0)
carbin$typestwagon=ifelse(carbin$type=="stwagon",1,0)
carbin$typetruck=ifelse(carbin$type=="truck",1,0)
carbin$typetvan=ifelse(carbin$type=="van",1,0)
carbin$typesportcar=ifelse(carbin$type=="sportcar",1,0)
myformula1=Formula(choice ~ price+range+speed+cost+station+pollute+accel
                   +typetvan+typestwagon+typetruck+typesportuv+size0+size1+typesportcar
                   +size2|hsg2+coml5+college|0))
myformula2=Formula(choice~price+range+cost+station+typesportcar
+typetvan+typestwagon+typetruck+typesportuv+size0+size1
                   +size2|hsg2+coml5+college+speed+pollute+accel)
ml.tm1=mlogit(myformula1,carbin,reflevel ="1")
ml.tm2=mlogit(myformula2,carbin,reflevel ="1")


summary(ml.tm1)
summary(ml.tm2)
library("lmtest")
lrtest(ml.tm2,ml.tm1)
-coef(ml.tm1)[1]/coef(ml.tm1)[2]
-coef(ml.tm1)[3]/coef(ml.tm1)[2]
-coef(ml.tm1)[4]/coef(ml.tm1)[2]
-coef(ml.tm1)[5]/coef(ml.tm1)[2]
-coef(ml.tm1)[6]/coef(ml.tm1)[2]
-coef(ml.tm1)[7]/coef(ml.tm1)[2]
-coef(ml.tm1)[8]/coef(ml.tm1)[2]
-coef(ml.tm1)[9]/coef(ml.tm1)[2]
-coef(ml.tm1)[10]/coef(ml.tm1)[2]
-coef(ml.tm1)[11]/coef(ml.tm1)[2]
-coef(ml.tm1)[12]/coef(ml.tm1)[2]
-coef(ml.tm1)[13]/coef(ml.tm1)[2]
-coef(ml.tm1)[14]/coef(ml.tm1)[2]
-coef(ml.tm1)[15]/coef(ml.tm1)[2]
carbin2=carbin
carbin2$pollute=carbin$pollute*0.8
carbin2$price=carbin2$price*1.2

ivbefore=logsum(ml.tm1)
ivafter=logsum(ml.tm1,data=carbin2)

surplus=-(ivafter-ivbefore)/coef(ml.tm1)["price"]
summary(surplus)

