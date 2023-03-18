library(readxl)
FFinland <- read_excel("C:/Users/LENOVO/Downloads/FFinland.xlsx")
View(FFinland)

age<- 0:100
lx=c(FFinland[,1])
dx=c(FFinland[,2])
px=c(FFinland[,3])
qx=c(FFinland[,4])
sx=c(FFinland[,5])
data1=data.frame(x=age, lx=lx, dx=dx, px=px, qx=qx, sx=sx)


data1

expr<-as.numeric(unlist(sx))
class(sx)

#Asuransi Berjangka
Axn.diskrit<-function(expr,age,i,B,w,n){
  v<-(1+i)^-1
  sx<-eval({x=0:w;expr})
  output<-NULL
  for(k in 1:n){
    output[k]<- B*v^(k)*((sx[age+k-1]-sx[age+k])/sx[age+1])
  }
  sum(output)}

#Asuransi Seumur Hidup
Ax.diskrit<-function(expr,age,i,B,w){
  v<-(1+i)^-1
  sx<-eval({x=0:w;expr})
  output<-NULL
  for(k in 1:(w-age)){
    output[k]<- B*v^(k)*((sx[age+k-1]-sx[age+k])/sx[age+1])
  }
  sum(output)}

expr1=as.numeric(unlist(sx))

#whole life female
Ax.diskrit(expr1, age=20, i=0.0618, B=120000, w=100)


#nterm female
Axn.diskrit(expr1, age=40, i=0.0618, B=120000, w=100, n=20)
