regplot <-
function(data, xlab="Explanatory Variable", ylab="Response Variable", position=6, mean=TRUE, digits=4){
digits=digits
xlab=xlab
ylab=ylab
dreg=data   
means=function(data){
t=as.factor(data[,1])
d=data.frame(t,data[,-1])
s=split(data.frame(d[,-1]), d$t)
r=lapply(s, colMeans, na.rm=TRUE)
r=lapply(r, round,2)
rr=t(data.frame(r)); rr=data.frame(rr);rownames(rr)=NULL
treat=levels(t)
rr=data.frame(treat, rr); colnames(rr)=colnames(data)
return(rr)
}
     rrr=means(data)
oi=ifelse(mean==TRUE,2,1)
l=list(data, rrr)
data=l[[oi]]

f1=function(dreg){
ml=lm(dreg[,2]~dreg[,1])
rsl=summary(ml)[[8]]
fl=function(qi){coef(ml)[[1]]+coef(ml)[[2]]*qi}
c1=round(coef(ml)[[1]],digits)
c2=round(coef(ml)[[2]],digits)
r2=rsl
r2=round(r2,2)
sin1=ifelse(c2>0,"+","")
e1=substitute(y==c1*sin1*c2*x*"  "* R^2*" = "*r2,  list(c1 = c1, c2 = c2,r2=r2, sin1=sin1))
        t=list("top", "bottomright", "bottom", "bottomleft", "left", "topleft", "topright", "right", "center")
        position=position
        p=t[[position]]
minx=min(dreg[,1])-sd(dreg[,1])/2
maxx=max(dreg[,1])+sd(dreg[,1])/2
miny=min(dreg[,2])-sd(dreg[,2])/2
maxy=max(dreg[,2])+sd(dreg[,2])/2
        plot(as.numeric(as.character(data[,1])),data[,2], xlab=xlab, 
              ylab=ylab, col = 1, xlim=c(minx,maxx), ylim=c(miny,maxy), main="linear", bty="n")
	curve(fl, min(dreg[,1]), max(dreg[,1]), add=TRUE, col = "dark blue", lty = 2)
        legend(p, legend=e1, bty="n")
}

f2=function(dreg){
mq=lm(dreg[,2]~dreg[,1]+I(dreg[,1]^2))
rsq=summary(mq)[[8]]
fq=function(qi){coef(mq)[[1]]+coef(mq)[[2]]*qi+coef(mq)[[3]]*qi^2}
c3=round(coef(mq)[[1]],digits)
c4=round(coef(mq)[[2]],digits)
c5=round(coef(mq)[[3]],digits)
r2=rsq
r2=round(r2,2)
sin1=ifelse(c4>0,"+","")
sin2=ifelse(c5>0,"+","")
e2=substitute(y==c3*sin1*c4*x*sin2*c5*x^2*"  "* R^2*" = "*r2,  list(c3 = c3, c4 = c4,c5=c5,r2=r2, sin1=sin1, sin2=sin2))
        t=list("top", "bottomright", "bottom", "bottomleft", "left", "topleft", "topright", "right", "center")
        position=position
        p=t[[position]]
minx=min(dreg[,1])-sd(dreg[,1])/2
maxx=max(dreg[,1])+sd(dreg[,1])/2
miny=min(dreg[,2])-sd(dreg[,2])/2
maxy=max(dreg[,2])+sd(dreg[,2])/2
        plot(as.numeric(as.character(data[,1])),data[,2], xlab=xlab, 
              ylab=ylab, col = 1, xlim=c(minx,maxx), ylim=c(miny,maxy), main="quadratic", bty="n")
	curve(fq, min(dreg[,1]), max(dreg[,1]), add=TRUE, col = "dark blue", lty = 2)
        legend(p, legend=e2, bty="n")
}

R2 <- function(m) {
        gl <- length(fitted(m)) - 1
        sqt <- var((fitted(m) + resid(m))) * gl
        r1 <- (sqt - deviance(m))/sqt
        return(r1)
    }
dev.new()
f1(dreg)
dev.new()
f2(dreg)
       }
