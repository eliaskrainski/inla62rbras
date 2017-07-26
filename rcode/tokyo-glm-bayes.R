library(INLA)
data(Tokyo)

n <- nrow(Tokyo)
x01 <- function(x)
(x-min(x, na.rm=TRUE)) /
    diff(range(x, na.rm=TRUE))
tt <- matrix(x01(Tokyo$time), nrow=n, ncol=10)
for (j in 2:ncol(tt))
    tt[,j] <- tt[,j-1]*tt[,1]

with(Tokyo, plot(time, y/n, ylim=range(tt)))
for (j in 1:ncol(tt)) 
    lines(tt[,j], col=j, lty=1)

glmtt <- list(m1=glm(cbind(y,n-y)~tt[,1],binomial,Tokyo))
for (j in 2:ncol(tt))
    glmtt[[j]] <- update(glmtt$m1, .~tt)

mtt <- list(m1=inla(y~tt[,1], 'binomial', data=Tokyo, Ntrials=n, 
                    control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE)))
for (j in 2:ncol(tt)) {
    ttm <- tt[,1:j]
    mtt[[j]] <- inla(y~ttm, 'binomial', data=Tokyo, Ntrials=n,
                     control.fixed=list(mean=coef(glmtt[[j]])[-1],
                                        prec=1/coef(summary(glmtt[[j]]))[-1,2]),
                     control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE)) 
}

with(Tokyo, plot(time, y/n))
msel <- c(6:10)
for (j in msel) {
    for (k in c(1, 3, 5)) 
        lines(Tokyo$time, mtt[[j]]$summary.fitted.val[,k], 
              col=pmatch(j,msel), lty=c(1,0,2,0,2)[k])
}
legend('topright', paste('p = ', msel),
       lty=1, col=1:length(msel))

lapply(mtt, function(x) x$summary.fix[,1]/x$summary.fix[,2])
round(mtt[[10]]$summary.fix,4)
round(coef(summary(glm(cbind(y,n-y)~tt,binomial,Tokyo))),4)

z <- 1/(1+exp(-binomial()$linkfun(c(0.01, 1/2, 0.99)[Tokyo$y+1])))
tt2 <- crossprod(tt); ttz <- crossprod(tt, z)
solve(tt2, ttz)

pp <- poly(Tokyo$time, degree=10)
with(Tokyo, plot(time, y/n, ylim=range(pp,0,1)))
for (j in 1:ncol(pp)) 
    lines(pp[,j], col=j, lty=1)


mpp <- list()
msel <- c(5, 6, 8, 10)
for (j in 1:10){
    ppm <- poly(Tokyo$time, j)
    mpp[[j]] <- inla(y~ppm, 'binomial', data=Tokyo, Ntrials=n, 
                    control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE)) 
}
msel <- c(6,8,10)
dres <- data.frame(time=Tokyo$time, p=Tokyo$y/Tokyo$n,
                   grau=factor(paste0('m=',rep(msel,each=nrow(Tokyo)))),
                   m=unlist(lapply(mpp[msel], function(m)
                       m$summary.fitted.val$mean)),
                   q2.5=unlist(lapply(mpp[msel], function(m)
                       m$summary.fitted.val$'0.025q')),
                   q97.5=unlist(lapply(mpp[msel], function(m)
                       m$summary.fitted.val$'0.975q')))
ggplot(dres,aes(x=time,y=p))+geom_point()+
    geom_line(aes(y=m,colour=grau))+
    geom_line(aes(y=q2.5,colour=grau),linetype='dashed')+
    geom_line(aes(y=q97.5,colour=grau),linetype='dashed')

library(splines)
b1 <- bs(Tokyo$time, df=10, degree=1)
b2 <- bs(Tokyo$time, df=10, degree=2)
dbb <- data.frame(time=Tokyo$time,
                  d=factor(rep(1:10,each=nrow(Tokyo))),
                  b1=as.vector(b1), b2=as.vector(b2))
ggplot(dbb,aes(x=time))+ylab('Valor da base')+
    geom_line(aes(y=b1,colour=d))+
    geom_line(aes(y=b2,colour=d))+guides(colour=FALSE)
mb2 <- list()
for (j in 1:8){
    b2m <- bs(Tokyo$time, df=j, degree=2)
    mb2[[j]] <- inla(y~b2m, 'binomial', data=Tokyo, Ntrials=n, 
                    control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE)) 
}
msel <- c(5, 7, 8)

with(Tokyo, plot(time, y/n))
for (j in msel) {
    for (k in c(1, 3, 5)) 
        lines(Tokyo$time, mb2[[j]]$summary.fitted.val[,k], 
              col=pmatch(j,msel), lwd=c(2,0,1,0,1)[k], lty=c(1,0,2,0,2)[k])
}
legend('topright', paste('m = ', msel), lty=1, lwd=2, col=1:length(msel))
lapply(mb2[msel], function(x) round(x$summary.fix,4))

msel <- c(6:8)
dres <- data.frame(time=Tokyo$time, p=Tokyo$y/Tokyo$n,
                   grau=factor(paste0('m=',rep(msel,each=nrow(Tokyo)))),
                   m=unlist(lapply(mb2[msel], function(m)
                       m$summary.fitted.val$mean)),
                   q2.5=unlist(lapply(mb2[msel], function(m)
                       m$summary.fitted.val$'0.025q')),
                   q97.5=unlist(lapply(mb2[msel], function(m)
                       m$summary.fitted.val$'0.975q')))
ggplot(dres,aes(x=time,y=p))+geom_point()+
    geom_line(aes(y=m,colour=grau))+
    geom_line(aes(y=q2.5,colour=grau),linetype='dashed')+
    geom_line(aes(y=q97.5,colour=grau),linetype='dashed')

