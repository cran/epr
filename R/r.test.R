r.test <-function(data, digits=6){
names(data)=c("x","treatment","y") 
data$x=as.numeric(data$x)  
data$treatment=as.factor(data$treatment) 
data$y=as.numeric(data$y)  
s=split(data,data[,2])
l <- combn(s,2)
f11=function(x){t=rbind(l[,x][[1]],l[,x][[2]]); return(t)}
i=1:nlevels(data$treatment)
r1=lapply(i,f11)

f22=function(iiii){rr=data.frame(r1[[iiii]][,1],r1[[1]][,2],r1[[iiii]][,3]);names(rr)=c("x","treatment","y");return(rr)}

iiii=1:length(r1)

r2=lapply(iiii, f22)
 
f1=function(data){
# linear
ml1=nls(y~a+b*x, data=data, start=c(a=1,b=1))
# a
ml2=nls(y~a[treatment]+b*x, data=data, start=list(a=c(1,1),b=1))
# b
ml3=nls(y~a+b[treatment]*x, data=data, start=list(a=c(1),b=c(1,1)))
# model
ml4=nls(y~a[treatment]+b[treatment]*x, data=data, start=list(a=c(1,1),b=c(1,1)))
s1l=summary(ml1)[[10]]
s2l=summary(ml2)[[10]]
s3l=summary(ml3)[[10]]
s4l=summary(ml4)[[10]]
a1l=anova(ml1,ml2)
a2l=anova(ml1,ml3)
a3l=anova(ml1,ml4)
l1=list(s1l,s2l,s3l,s4l,a1l,a2l,a3l)
names(l1)=c("y=a+b*x","y=a1+a2+b*x","y=a+b1*x+b2*x","y=a1+a2+b1*x+b2*x","test a parameter", "test b parameter","test model") 
# quadratic
mq1=nls(y~a+b*x+c*x^2, data=data, start=c(a=1,b=1,c=1))
# a
mq2=nls(y~a[treatment]+b*x+c*x^2, data=data, start=list(a=c(1,1),b=c(1), c=c(1)))
# b
mq3=nls(y~a+b[treatment]*x+c*x^2, data=data, start=list(a=c(1),b=c(1,1), c=c(1)))
# c
mq4=nls(y~a+b*x+c[treatment]*x^2, data=data, start=list(a=c(1),b=c(1), c=c(1,1)))
# model
mq5=nls(y~a[treatment]+b[treatment]*x+c[treatment]*x^2, data=data, start=list(a=c(1,1),b=c(1,1), c=c(1,1)))
s1q=summary(mq1)[[10]]
s2q=summary(mq2)[[10]]
s3q=summary(mq3)[[10]]
s4q=summary(mq4)[[10]]
s5q=summary(mq5)[[10]]
a1q=anova(mq1,mq2)
a2q=anova(mq1,mq3)
a3q=anova(mq1,mq4)
a4q=anova(mq1,mq5)
l2=list(s1q,s2q,s3q,s4q,s5q, a1q,a2q,a3q,a4q)
names(l2)=c("y=a+b*x+c*x^2","y=a1+a2+b*x+c*x^2","y=a+b1*x+b2*x+c*x^2","y=a+b*x+c1*x^2+c2*x^2","y=a1+a2+b1*x+b2*x+c1*x^2+c2*x^2","test a parameter", "test b parameter","test c parameter","test model") 
l=list(l1,l2)
names(l)=c("linear model", "quadratic model")
return(l)
}

l=lapply(r2, f1)

name=levels(data$treatment)
c=combn(name,2)

io=1:ncol(c)
fc=function(io){
p=paste(c[,io], collapse=" vs ")
return(p)
}

lc=lapply(io,fc)
comb=unlist(lc)

names(l)=comb
return(l)

}

