bl=function(data, xlab="Explanatory Variable", ylab="Response Variable", position=1){
#dados
data=data
names(data)=c("x","y")
# para obter o knot
knot=seq(from=min(data$x), to=max(data$x), by=0.005)

fk=function(knot){m1=lm(data$y ~ data$x * (data$x < knot) + data$x * (data$x > knot))
c=coef(m1)
c1=c[1];c2=c[2];c3=c[3];c4=c[4];c5=c[5]
f1=function(x){(c1+c3)+((c2+c5)*x)}
f2=function(x){c1+(c2*x)}
r1=f1(knot)
r2=f2(knot)
rf=c(r1,r2)
return(rf)
}

s=sapply(knot, fk)
s=t(s)
s=cbind(knot,s)
s=na.exclude(s); tf=s[,2]-s[,3]; tf=round(tf,2)
vtf=sqrt(tf^2)
s=cbind(s,vtf); s=as.data.frame(s)
s=s[order(s$vtf),]
knot=s[,1][1:3]

fk2=function(knot){m2=lm(data$y ~ data$x * (data$x < knot) + data$x * (data$x > knot))
cc=coef(m2)
c1=cc[1];c2=cc[2];c3=cc[3];c4=cc[4];c5=cc[5]
x1=data$x[data$x<=knot]
x2=data$x[data$x>knot]
f1=function(x1)(c1+c3)+((c2+c5)*x1)
f2=function(x2)c1+(c2*x2)
r1=f1(x1)
r2=f2(x2)
dat1=subset(data, data$x<=knot)
dat2=subset(data, data$x>knot)
rr1=r1-dat1$y;rr2=r2-dat2$y
r=c(rr1,rr2)
r=sum(r^2)
return(r)
}


s=sapply(knot, fk2)
d=data.frame(knot,s)
d=d[order(d$s),]
knot=d[1,1]
# obteve-se o knot e agora a analise
m1=lm(y ~ x * (x < knot) + x * (x > knot), data=data)
c=coef(m1)
cc1=c[1];cc2=c[2];cc3=c[3];cc4=c[4];cc5=c[5]
fo1=function(x){(cc1+cc3)+((cc2+cc5)*x)}
# pode ser f1=function(x){c1+c3+c2*x+c5*x)}
fo2=function(x){cc1+(cc2*x)}
l1=c((cc1+cc3),(cc2+cc5))
l2=c(cc1,cc2)
l=data.frame(l1,l2)
names(l)=c("first line","second line")
rownames(l)=c("a","b")
l=t(l)
s=summary(m1)
r1=s$r.squared
r2=s$adj.r.squared
r3=s$coefficients
cof=r3[,1]
p=r3[,4]
le=length(cof)
cof1=cof
cof2=cof[-4]
cofn=ifelse(le>4,2, 1)
lc=list(cof1,cof2)
coefficientes=lc[[cofn]] 
p1=p
p2=p[-4]
lp=list(p1,p2)
p_values=lp[[cofn]] 
ss=data.frame(coefficientes,p_values)
rownames(ss)=c("a","b","c","d")
m11='a+c+b*x+d*x'
m22='a+b*x'
res=resid(m1)
shap=shapiro.test(resid(m1))
shap=shap$p.value
sres=scale(res)
sres=sres[,1]
resp=list(round(ss,8), m11,m22, round(l,6), knot, round(r1,4),round(r2,4),round(AIC(m1),4),round(BIC(m1),4),res, sres, round(shap,6) )
names(resp)=c("Coefficients", "First model", "Second model", "Lines","Knot", "R-squared", "Adjusted R-squared", "AIC", "BIC", "Residuals", "Standartized residuals", "P value (Shapiro-Wilk test for residuals)")

minx = min(data[, 1], na.rm=TRUE) - sd(data[, 1], na.rm=TRUE)/2
	maxx = max(data[, 1], na.rm=TRUE) + sd(data[, 1], na.rm=TRUE)/2
        miny = min(data[, 2], na.rm=TRUE) - sd(data[, 2], na.rm=TRUE)/2
        maxy = max(data[, 2], na.rm=TRUE) + sd(data[, 2], na.rm=TRUE)/2

c3=l1[1];c4=l1[2];c1=l2[1];c2=l2[2]
	sin1 = ifelse(c1 > 0, "+", "")
        sin2 = ifelse(c2 > 0, "+", "")
        sin3 = ifelse(c3 > 0, "+", "")
        sin4 = ifelse(c4 > 0, "+", "")

r1=round(r1,2)

e5 = substitute(atop(y1==c3*sin4*c4*x,y2==c1*sin2*c2*x)*"    "* R^2 * " = " * r1, list(c1 = c1, c2 = c2, c3 = c3,c4 = c4, r1 = r1, sin4 = sin4, sin2 = sin2))

t = list("top", "bottomright", "bottom", "bottomleft", 
             "left", "topleft", "topright", "right", "center")
    p = t[[position]]

plot(data$y~data$x, ylim=c(miny,maxy), xlim=c(minx,maxx), ylab=ylab, xlab=xlab, bty="n")
curve(fo1,add=T, from=min(data$x, na.rm=TRUE), to=knot, col = "dark red", lty = 2)
curve(fo2, add=T, from=knot, to=max(data$x, na.rm=TRUE), col = "dark blue", lty = 2)
legend(p,legend=e5,bty = "n", cex=0.8)


return(resp)
}


