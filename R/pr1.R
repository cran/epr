pr1 <-function(data, mixed=FALSE, digits=6){
    mixed=mixed

kp <- function(expr) {
            localWarnings <- list()
            value <- withCallingHandlers(expr, warning = function(w) {
                localWarnings[[length(localWarnings) + 1]] <<- w
                invokeRestart("muffleWarning")
            })
            value = value
        }

    opt1=function(data){
    d1 = as.data.frame(data[,1])
    d2 = as.data.frame(data[,-1])
    f = function(h) {
        data.frame(d1, d2[h])}
    h = length(d2)
    h = 1:h
    l = lapply(h, f)
    return(l)
    }

    opt2=function(data){
    d3 = as.data.frame(data[,c(1,2)])
    d4 = as.data.frame(data[,-c(1,2)])
    ff = function(h) {
        data.frame(d3, d4[h])}
    hh = length(d4)
    hh = 1:hh
    ll = lapply(hh, ff)
    return(ll)
    }

    

   

cv <- function(x) {
        sd = (deviance(x)/df.residual(x))^0.5
        mm = mean(fitted(x))
        r = 100 * sd/mm
        return(r)
    }

cvmm <- function(x) {
	dv=data.frame(summary(x)$varcor)[,"sdcor"]
        sd = dv[length(dv)]
        mm = mean(fitted(x))
        r = 100 * sd/mm
        return(r)
    }
        

	fr=function(m){
	mixed=mixed
        r=resid(m)
	t=1:length(r)
	i=ifelse(length(r)>5000, 2,1)
	jr=function(r,aa)r+aa-aa
	jsample=function(r,aa)sample(r,aa)
	rr=list(jr,jsample)
	rr=rr[[i]](r,5000)
        s <- shapiro.test(rr)
	i=ifelse(mixed==FALSE,1,2)
	lf=list(cv,cvmm)
	FUNN=lf[[i]]
        cvf = FUNN(m)
	names(r)=t
        rd=as.data.frame((sort(sqrt(r^2),decreasing=TRUE)))
        rl=as.list(rownames(rd))
        r1=rl[[1]];r2=rl[[2]];r3=rl[[3]]
        d=c(s$"p.value",cvf,as.numeric(r1),as.numeric(r2),as.numeric(r3))
        return(d)}

pres=function(m,name="name"){
	name=name
	r=resid(m)
	r=scale(r)
	t=1:length(r)
	ll=labels=1:length(r);mo=median(ll)
	g1=function(r){plot(r~t, pch="",ylim=c(-4,4), ylab="Standardized residuals", xlab="Sequence data", 		main="Standardized residuals vs Sequence data",axes=FALSE);axis(2,c(-4,-3.5,-3,-2.5,-2,-1,0,1,2,2.5,3,3.5,4));abline(h=2.5, lty=2);abline(h=-2.5,lty=2);abline(h=3.5, lty=2, col=2);abline(h=-3.5,lty=2, col=2); text(2.5,2.7, "2.5 z-score");text(2.5,-2.7, "-2.5 z-score");text(2.5,3.7, "3.5 z-score");text(2.5,-3.7, "-3.5 z-score");text(t,r,labels=1:length(r));text(mo,4,name, cex=2, col=2)}
	g1(r)
	}


R2 <- function(m) {
        gl <- length(fitted(m)) - 1
        sqt <- var((fitted(m) + resid(m))) * gl
        r1 <- (sqt - deviance(m))/sqt
        return(r1)
    }

R3 <- function(m) {
        gl <- length(fitted(m)) - 1
        sqt <- var((fitted(m) + resid(m))) * gl
        r1 <- (sqt - deviance(m))/sqt
        p1 <- (gl/((gl + 1) - (length(coef(m) + 1))) * (1 - 
            r1))
        r2 <- 1 - p1; return(r2)
    }


R2mm <- function(m) {
        gl <- length(fitted(m)) - 1
        sqt <- var((fitted(m) + resid(m))) * gl
	dev=sum(anova(m)[[2]])
        r1 <- (sqt - dev)/sqt
        return(r1)
    }


R3mm <- function(m) {
        gl <- length(fitted(m)) - 1
        sqt <- var((fitted(m) + resid(m))) * gl
	dev=sum(anova(m)[[2]])
        r1 <- (sqt - dev)/sqt
        p1 <- (gl/((gl + 1) - (length(fixef(m) + 1))) * (1 - 
            r1))
        r2 <- 1 - p1; return(r2)
    }

reg=
function(data){
names(data)=c("x","y")
m1=lm(data[,2]~data[,1])
m2=lm(data[,2]~data[,1]+I(data[,1]^2))
c3=coef(m2)[[1]]
c4=coef(m2)[[2]]
c5=coef(m2)[[3]]
s1=summary(m1)
s2=summary(m2)
c11=s1[[4]][,1]
c22=s2[[4]][,1]
p1=s1[[4]][,4]
p2=s2[[4]][,4]
A1=AIC(m1)
A2=AIC(m2)
B1=BIC(m1)
B2=BIC(m2)
r1=R2(m1)
r2=R2(m2)
ar1=R3(m1)
ar2=R3(m2)
pm=c3-(c4^2)/(4*c5)
pc=-0.5*c4/c5
fr1=fr(m1)
fr2=fr(m2)
resi1l=resid(m1);resi1q=resid(m2)
resi2l=scale(resi1l);resi2q=scale(resi1q)
linear=c(c11,NA,p1,NA,r1,ar1,A1,B1,NA,NA, fr1); linear=round(linear,digits)
quadratic=c(c22,p2,r2,ar2,A2,B2, pm, pc, fr2); quadratic=round(quadratic,digits)
resp=data.frame(linear, quadratic)
rownames(resp)=c("coefficient a","coefficient b","coefficient c","p-value t.test for a","p-value t.test for b","p-value t.test for c", "r-squared","adjusted r-squared", "AIC", "BIC", "maximum or minimum value for y","critical point in x", "p.value Shapiro-Wilk test","coefficient of variation (%)", "first value most discrepant","second value most discrepant","third value most discrepant")

l1=list(resp,resi1l,resi1q,resi2l[,1],resi2q[,1])
names(l1)=c("estimates","residuals of linear model", "residuals of quadratic model","residuals standardized of linear model", "residuals standardized of quadratic model") 

return(l1)
        }

funreg=
function(data){
nnn=names(data);nn=nnn[3]
names(data)=c("x","blocks","y")
data$blocks=as.factor(data$blocks)

ml1=lmer(y~x+(1|blocks), data=data)
mq1=lmer(y~x+I(x^2)+(1|blocks), data=data)

fff=function(ml1, mq1){
c1=fixef(ml1)[[1]]
c2=fixef(ml1)[[2]]
c3=fixef(mq1)[[1]]
c4=fixef(mq1)[[2]]
c5=fixef(mq1)[[3]]
s1=summary(ml1)$coefficients
df1=length(data[,3])-1-nlevels(data[,2])
p1=1-pt(sqrt((s1[[5]])^2),df1)
p2=1-pt(sqrt((s1[[6]])^2),df1)
p1=2*p1
p2=2*p2
s2=summary(mq1)$ coefficients
df2=length(data[,3])-2-nlevels(data[,2])
p3=1-pt(sqrt(s2[[7]]^2),df2)
p4=1-pt(sqrt(s2[[8]]^2),df2)
p5=1-pt(sqrt(s2[[9]]^2),df2)
p3=2*p3
p4=2*p4
p5=2*p5
A1=AIC(ml1)
A2=AIC(mq1)
B1=BIC(ml1)
B2=BIC(mq1)
r1=R2mm(ml1)
r2=R2mm(mq1)
ar1=R3mm(ml1)
ar2=R3mm(mq1)
pm=c3-(c4^2)/(4*c5)
pc=-0.5*c4/c5
fr1=fr(ml1)
fr2=fr(mq1)

resi1l=resid(ml1);resi1q=resid(mq1)
resi2l=scale(resi1l);resi2q=scale(resi1q)



linear=c(c1,c2,NA,p1,p2,NA,r1,ar1,A1,B1,NA,NA,fr1); linear=round(linear,digits)
quadratic=c(c3,c4,c5,p3,p4,p5,r2,ar2,A2,B2, pm, pc, fr2); quadratic=round(quadratic,digits)
resp1=data.frame(linear, quadratic)
rownames(resp1)=c("coefficient a","coefficient b","coefficient c","p-value t.test for a","p-value t.test for b","p-value t.test for c", "r-squared","adjusted r-squared", "AIC", "BIC", "maximum or minimum value for y","critical point in x", "p.value Shapiro-Wilk test","coefficient of variation (%)", "first value most discrepant","second value most discrepant","third value most discrepant")
lm1=as.data.frame(coef(ml1)[[1]])
names(lm1)=c("a","b")
lm2=as.data.frame(coef(mq1)[[1]])
names(lm2)=c("a","b","c")



rl=list(resp1,lm1,lm2, resi1l,resi1q,resi2l[,1],resi2q[,1]); names(rl)=c("estimates", "coefficientes of the linear model", "coefficientes of the  quadratic model", "residuals of linear model","residuals of quadratic model","residuals standardized of linear model", "residuals standardized of quadratic model") 


return(rl)
}

res1=fff(ml1,mq1)

ml1=lmer(y~x+(0+x|blocks), data=data)
mq1=lmer(y~x+I(x^2)+(0+x+I(x^2)|blocks), data=data)

res2=fff(ml1,mq1)

ml1=lmer(y~x+(x|blocks), data=data)
mq1=lmer(y~x+I(x^2)+(x+I(x^2)|blocks), data=data)

res3=fff(ml1,mq1)

lf=list(res1,res2,res3);names(lf)=c("intercept (a) random", "regressors random", "all coefficientes random")

return(lf)

        }
i=ifelse(mixed==FALSE,1,2)
lf=list(reg,funreg)
FUN=lf[[i]]
pf=list(opt1,opt2)
pf=pf[[i]]
pf=pf(data)
rep=kp(lapply(pf, FUN))
ln=list(1,c(1,2))
ln=ln[[i]]
names(rep)= names(data)[-ln]
return(rep)
}

