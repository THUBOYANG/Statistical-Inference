1、正态分布总体
a=rep(0,1000)
b=rep(0,1000)
c=rep(0,1000)
d=rep(0,1000)
e=rep(0,1000)
f=rep(0,1000)#为6种估计量建立数据集
bias=rep(0,6)
sd=rep(0,6)
RMSE=rep(0,6)#为三种要求的项目建立数据集
L<-20#按升序对X中间若干项求和的参数L
n<-100#产生的随机样本的个数
for(i in 1:1000)#重复1000次试验
{
  X<-rnorm(n,0,1)#产生n个正态随机样本
  a[i]<-mean(X)
  b[i]<-median(X)
  c[i]<-X[1]
  sort(X)#对X按升序排序
  for(j in 1:n)
  {
    if(j>L & j<=n-L)
    {
      d[i]<-d[i]+X[j]
    }
  }
  d[i]<-d[i]/(n-2*L)
  e[i]<-(max(X)+min(X))/2
  f[i]<-max(X)
}
bias[1]<-mean(a)-0
bias[2]<-mean(b)-0
bias[3]<-mean(c)-0
bias[4]<-mean(d)-0
bias[5]<-mean(e)-0
bias[6]<-mean(f)-0
sd[1]<-sd(a)
sd[2]<-sd(b)
sd[3]<-sd(c)
sd[4]<-sd(d)
sd[5]<-sd(e)
sd[6]<-sd(f)
RMSE[1]<-sqrt(bias[1]*bias[1]+var(a))
RMSE[2]<-sqrt(bias[2]*bias[2]+var(b))
RMSE[3]<-sqrt(bias[3]*bias[3]+var(c))
RMSE[4]<-sqrt(bias[4]*bias[4]+var(d))
RMSE[5]<-sqrt(bias[5]*bias[5]+var(e))
RMSE[6]<-sqrt(bias[6]*bias[6]+var(f))



2、柯西分布总体
a=rep(0,1000)
b=rep(0,1000)
c=rep(0,1000)
d=rep(0,1000)
e=rep(0,1000)
f=rep(0,1000)#为6种估计量建立数据集
bias=rep(0,6)
sd=rep(0,6)
RMSE=rep(0,6)#为三种要求的项目建立数据集
L<-20#按升序对X中间若干项求和的参数L
n<-100#产生的随机样本的个数
for(i in 1:1000)#重复1000次试验
{
  X<-rcauchy(n,0,1)#产生n个柯西随机样本
  a[i]<-mean(X)
  b[i]<-median(X)
  c[i]<-X[1]
  sort(X)#对X按升序排序
  for(j in 1:n)
  {
    if(j>L & j<=n-L)
    {
      d[i]<-d[i]+X[j]
    }
  }
  d[i]<-d[i]/(n-2*L)
  e[i]<-(max(X)+min(X))/2
  f[i]<-max(X)
}
bias[1]<-mean(a)-0
bias[2]<-mean(b)-0
bias[3]<-mean(c)-0
bias[4]<-mean(d)-0
bias[5]<-mean(e)-0
bias[6]<-mean(f)-0
sd[1]<-sd(a)
sd[2]<-sd(b)
sd[3]<-sd(c)
sd[4]<-sd(d)
sd[5]<-sd(e)
sd[6]<-sd(f)
RMSE[1]<-sqrt(bias[1]*bias[1]+var(a))
RMSE[2]<-sqrt(bias[2]*bias[2]+var(b))
RMSE[3]<-sqrt(bias[3]*bias[3]+var(c))
RMSE[4]<-sqrt(bias[4]*bias[4]+var(d))
RMSE[5]<-sqrt(bias[5]*bias[5]+var(e))
RMSE[6]<-sqrt(bias[6]*bias[6]+var(f))

