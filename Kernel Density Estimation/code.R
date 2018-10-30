n=30 ##（或100）
p=1/3
MISE=rep(0,1000)
for(j in 1:1000) ##重复模拟1000次
{
  for(i in 1:n) ##生成n个混合正态随机变量
  {
    r<-rbinom(1,1,p)
    if(r==1)
    {  X[i]<-rnorm(1,0,1)}
    if(r==0)
    {  X[i]<-rnorm(1,2.5,1)}
  }
  hist(X,30,prob=T,main='n=100'); lines(density(X)) ##将核密度估计函数与总体概率分布直方图画在一张图中
  z=density(X)
  for(i in 1:511) ##计算MISE
  {
    s=(exp(-(z$x[i])^2/2)+2*exp(-((z$x[i])-2.5)^2/2))/(3*sqrt(2*pi))
    MISE[j]<-MISE[j]+(z$x[i+1]-z$x[i])*(s-z$y[i])^2
  }
}
mean(MISE)