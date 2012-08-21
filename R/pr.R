pr <-
function(data){
    data11=data
    u<-list("",
            
            f1=function(data){ 
                names(data)=c("treatments","response")
                data=data.frame(treatments=as.numeric(data$treatments), response=data$response, linear=data$treatments, quadratic=data$treatments^2, cubic=data$treatments^3, treatment=factor(data$treatments) )
                n1= nlevels(data$treatment)-1
                lack_of_fit=poly(data$treatments, n1)
                m1<-lm(response~linear+lack_of_fit,data=data)
                n2= nlevels(data$treatment)-1
                lack_of_fit=poly(data$treatments, n2)
                m2<-lm(response~linear+quadratic+lack_of_fit,data=data)
                res=list(
                    m3<-lm(response~linear+quadratic+cubic+lack_of_fit,data=data)
                    ,
                    m4<-lm(response~linear+quadratic+cubic,data=data)
                )
                gg=ifelse(n1==3,2,1)
                m3=res[[gg]]
                anova=list(anova(m1),anova(m2),anova(m3))
                names(anova)=c('linear','quadratic','cubic')
                m11=lm(response~linear, data=data)
                m12=lm(response~linear+quadratic, data=data)
                m13=lm(response~linear+quadratic+cubic, data=data)
                models=list(round(m11[1][[1]],4), round(m12[1][[1]],4), round(m13[1][[1]],4))
                names(models)=c('linear','quadratic','cubic')
                c1=round(summary(m1)[[4]],4)
                c2=round(summary(m2)[[4]],4)
                c3=round(summary(m3)[[4]],4)
                c1=as.data.frame(c1);c1=c1[c(1,2),]
                c2=as.data.frame(c2);c2=c2[c(1,2,3),]
                c3=as.data.frame(c3);c3=c3[c(1,2,3,4),]
                
                coefs=list(c1,c2,c3)
                names(coefs)=c('linear','quadratic','cubic')
                st=sum(anova(m1)[[2]][c(1,2)])
                st1=sum(anova(m1)[[2]][1])
                st2=sum(anova(m2)[[2]][c(1,2)])
                st3=sum(anova(m3)[[2]][c(1,2,3)])
                r1=st1/st
                r2=st2/st
                r3=st3/st
                n=length(data$response)
                r12=1- (((n-1)*(1-r1))/(n-(2+1)))
                r22=1- (((n-1)*(1-r2))/(n-(3+1)))
                r23=1- (((n-1)*(1-r3))/(n-(4+1)))
                R_squared=round(c(r1,r2,r3),4)
                Adjusted_R_squared=round(c(r12,r22,r23),4)
                Models=c('linear','quadratic','cubic')
                r=data.frame(Models,R_squared, Adjusted_R_squared)
                rf1=list(anova,models,coefs,r)
                rf2= list(anova[c(1,2)],models[c(1,2)],coefs[c(1,2)],r[c(1,2),])
                rf=list(rf1,rf2)
                j=nlevels(data$treatment)
                j=ifelse(j>3,1,2)
                rf=rf[[j]]
                names(rf)=c("Analysis of variance","Models","t test for coefficients","R-squared")
                return(rf)
            }
            ,
            f2=function(data){ 
                names(data)=c("treatments","blocks","response")
                data=data.frame(treatments=as.numeric(data$treatments), blocks=as.factor(data$blocks), response=data$response, linear=data$treatments, quadratic=data$treatments^2, cubic=data$treatments^3, treatment=factor(data$treatments) )
                n1= nlevels(data$treatment)-1
                lack_of_fit=poly(data$treatments, n1)
                m1<-lm(response~linear+lack_of_fit+blocks,data=data, contrasts=list(blocks=contr.sum))
                n2= nlevels(data$treatment)-1
                lack_of_fit=poly(data$treatments, n2)
                m2<-lm(response~linear+quadratic+lack_of_fit+ blocks,data=data, contrasts=list(blocks=contr.sum))
                res=list(
                    m3<-lm(response~linear+quadratic+cubic+lack_of_fit+ blocks,data=data, contrasts=list(blocks=contr.sum))
                    ,
                    m4<-lm(response~linear+quadratic+cubic+ blocks,data=data, contrasts=list(blocks=contr.sum))
                )
                gg=ifelse(n1==3,2,1)
                m3=res[[gg]]
                anova=list(anova(m1),anova(m2),anova(m3))
                names(anova)=c('linear','quadratic','cubic')
                m11=lm(response~linear, data=data)
                m12=lm(response~linear+quadratic, data=data)
                m13=lm(response~linear+quadratic+cubic, data=data)
                models=list(round(m11[1][[1]],4), round(m12[1][[1]],4), round(m13[1][[1]],4))
                names(models)=c('linear','quadratic','cubic')
                c1=round(summary(m1)[[4]],4)
                c2=round(summary(m2)[[4]],4)
                c3=round(summary(m3)[[4]],4)
                c1=as.data.frame(c1);c1=c1[c(1,2),]
                c2=as.data.frame(c2);c2=c2[c(1,2,3),]
                c3=as.data.frame(c3);c3=c3[c(1,2,3,4),]
                coefs=list(c1,c2,c3)
                names(coefs)=c('linear','quadratic','cubic')
                st=sum(anova(m1)[[2]][c(1,2)])
                st1=sum(anova(m1)[[2]][1])
                st2=sum(anova(m2)[[2]][c(1,2)])
                st3=sum(anova(m3)[[2]][c(1,2,3)])
                r1=st1/st
                r2=st2/st
                r3=st3/st
                n=length(data$response)
                r12=1- (((n-1)*(1-r1))/(n-(2+1)))
                r22=1- (((n-1)*(1-r2))/(n-(3+1)))
                r23=1- (((n-1)*(1-r3))/(n-(4+1)))
                R_squared=round(c(r1,r2,r3),4)
                Adjusted_R_squared=round(c(r12,r22,r23),4)
                Models=c('linear','quadratic','cubic')
                r=data.frame(Models,R_squared, Adjusted_R_squared)
                rf1=list(anova,models,coefs,r)
                rf2= list(anova[c(1,2)],models[c(1,2)],coefs[c(1,2)],r[c(1,2),])
                rf=list(rf1,rf2)
                j=nlevels(data$treatment)
                j=ifelse(j>3,1,2)
                rf=rf[[j]]
                names(rf)=c("Analysis of variance","Models","t test for coefficients","R-squared")
                return(rf)
            }
            , f3=function(data){ 
                names(data)=c("treatments","rows", "cols","response")
                data=data.frame(treatments=as.numeric(data$treatments), rows=as.factor(data$rows), cols=as.factor(data$cols),  response=data$response, linear=data$treatments, quadratic=data$treatments^2, cubic=data$treatments^3, treatment=factor(data$treatments) )
                n1= nlevels(data$treatment)-1
                lack_of_fit=poly(data$treatments, n1)
                m1<-lm(response~linear+lack_of_fit+rows+cols,data=data, contrasts=list(rows=contr.sum, cols=contr.sum))
                n2= nlevels(data$treatment)-1
                lack_of_fit=poly(data$treatments, n2)
                m2<-lm(response~linear+quadratic+lack_of_fit+ rows+cols,data=data, contrasts=list(rows=contr.sum, cols=contr.sum))
                res=list(
                    m3<-lm(response~linear+quadratic+cubic+lack_of_fit+ rows+cols,data=data, contrasts=list(rows=contr.sum, cols=contr.sum))
                    ,
                    m4<-lm(response~linear+quadratic+cubic+ rows+cols,data=data, contrasts=list(rows=contr.sum, cols=contr.sum))
                )
                gg=ifelse(n1==3,2,1)
                m3=res[[gg]]
                anova=list(anova(m1),anova(m2),anova(m3))
                names(anova)=c('linear','quadratic','cubic')
                m11=lm(response~linear, data=data)
                m12=lm(response~linear+quadratic, data=data)
                m13=lm(response~linear+quadratic+cubic, data=data)
                models=list(round(m11[1][[1]],4), round(m12[1][[1]],4), round(m13[1][[1]],4))
                names(models)=c('linear','quadratic','cubic')
                c1=round(summary(m1)[[4]],4)
                c2=round(summary(m2)[[4]],4)
                c3=round(summary(m3)[[4]],4)
                c1=as.data.frame(c1);c1=c1[c(1,2),]
                c2=as.data.frame(c2);c2=c2[c(1,2,3),]
                c3=as.data.frame(c3);c3=c3[c(1,2,3,4),]
                coefs=list(c1,c2,c3)
                names(coefs)=c('linear','quadratic','cubic')
                st=sum(anova(m1)[[2]][c(1,2)])
                st1=sum(anova(m1)[[2]][1])
                st2=sum(anova(m2)[[2]][c(1,2)])
                st3=sum(anova(m3)[[2]][c(1,2,3)])
                r1=st1/st
                r2=st2/st
                r3=st3/st
                n=length(data$response)
                r12=1- (((n-1)*(1-r1))/(n-(2+1)))
                r22=1- (((n-1)*(1-r2))/(n-(3+1)))
                r23=1- (((n-1)*(1-r3))/(n-(4+1)))
                R_squared=round(c(r1,r2,r3),4)
                Adjusted_R_squared=round(c(r12,r22,r23),4)
                Models=c('linear','quadratic','cubic')
                r=data.frame(Models,R_squared, Adjusted_R_squared)
                rf1=list(anova,models,coefs,r)
                rf2= list(anova[c(1,2)],models[c(1,2)],coefs[c(1,2)],r[c(1,2),])
                rf=list(rf1,rf2)
                j=nlevels(data$treatment)
                j=ifelse(j>3,1,2)
                rf=rf[[j]]
                names(rf)=c("Analysis of variance","Models","t test for coefficients","R-squared")
                return(rf)
            }
            ,
            f4=function(data){ 
                names(data)=c("treatments", "squares","rows", "cols","response")
                data=data.frame(treatments=as.numeric(data$treatments), rows=as.factor(data$rows), squares=as.factor(data$squares), cols=as.factor(data$cols),  response=data$response, linear=data$treatments, quadratic=data$treatments^2, cubic=data$treatments^3, treatment=factor(data$treatments) )
                n1= nlevels(data$treatment)-1
                lack_of_fit=poly(data$treatments, n1)
                m1<-lm(response~linear+lack_of_fit+squares+rows+cols,data=data, contrasts=list(squares=contr.sum, rows=contr.sum, cols=contr.sum))
                n2= nlevels(data$treatment)-1
                lack_of_fit=poly(data$treatments, n2)
                m2<-lm(response~linear+quadratic+lack_of_fit+ squares+rows+cols,data=data, contrasts=list(squares=contr.sum,rows=contr.sum, cols=contr.sum))
                res=list(
                    m3<-lm(response~linear+quadratic+cubic+lack_of_fit+ squares+rows+cols,data=data, contrasts=list(squares=contr.sum,rows=contr.sum, cols=contr.sum))
                    ,
                    m4<-lm(response~linear+quadratic+cubic+ squares+rows+cols,data=data, contrasts=list(squares=contr.sum, rows=contr.sum, cols=contr.sum))
                )
                gg=ifelse(n1==3,2,1)
                m3=res[[gg]]
                anova=list(anova(m1),anova(m2),anova(m3))
                names(anova)=c('linear','quadratic','cubic')
                m11=lm(response~linear, data=data)
                m12=lm(response~linear+quadratic, data=data)
                m13=lm(response~linear+quadratic+cubic, data=data)
                models=list(round(m11[1][[1]],4), round(m12[1][[1]],4), round(m13[1][[1]],4))
                names(models)=c('linear','quadratic','cubic')
                c1=round(summary(m1)[[4]],4)
                c2=round(summary(m2)[[4]],4)
                c3=round(summary(m3)[[4]],4)
                c1=as.data.frame(c1);c1=c1[c(1,2),]
                c2=as.data.frame(c2);c2=c2[c(1,2,3),]
                c3=as.data.frame(c3);c3=c3[c(1,2,3,4),]
                coefs=list(c1,c2,c3)
                names(coefs)=c('linear','quadratic','cubic')
                st=sum(anova(m1)[[2]][c(1,2)])
                st1=sum(anova(m1)[[2]][1])
                st2=sum(anova(m2)[[2]][c(1,2)])
                st3=sum(anova(m3)[[2]][c(1,2,3)])
                r1=st1/st
                r2=st2/st
                r3=st3/st
                n=length(data$response)
                r12=1- (((n-1)*(1-r1))/(n-(2+1)))
                r22=1- (((n-1)*(1-r2))/(n-(3+1)))
                r23=1- (((n-1)*(1-r3))/(n-(4+1)))
                R_squared=round(c(r1,r2,r3),4)
                Adjusted_R_squared=round(c(r12,r22,r23),4)
                Models=c('linear','quadratic','cubic')
                r=data.frame(Models,R_squared, Adjusted_R_squared)
                rf1=list(anova,models,coefs,r)
                rf2= list(anova[c(1,2)],models[c(1,2)],coefs[c(1,2)],r[c(1,2),])
                rf=list(rf1,rf2)
                j=nlevels(data$treatment)
                j=ifelse(j>3,1,2)
                rf=rf[[j]]
                names(rf)=c("Analysis of variance","Models","t test for coefficients","R-squared")
                return(rf)
            }
    )
    
    g=u[[length(data11)]]
    g(data)
}
