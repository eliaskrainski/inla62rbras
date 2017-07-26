options(width=50)
if (FALSE)
    setwd('..')

load('dados/brmi.RData')
names(brmi)

length(jn <- 3:24)
length(jm <- 26:45)

head(brmi@data[,1:5])
ii <- grep('CURITIBA', brmi$NM_MUNICIP)
ii

brmi@data[ii, 1:5]
i <- ii[1]
brmi@data[i, 3:10]
brmi@data[i, jm]

n <- unlist(brmi@data[i, jn])
m <- c(NA, NA, unlist(brmi@data[i, jm]))
round(1000*m/n)

anos <- 1994:2015
plot(anos, m/n)

library(INLA)

lin <- inla(y ~ time, 
            data=list(y=m, n=n, time=1:length(anos)),
            Ntrials=n, family='binomial',
            control.predictor=list(compute=TRUE, link=1), 
            control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))

lin
summary(lin)
names(lin)
lin$summary.fixed

pc.prec <- list(prior='pcprec', param=c(1, 0.01))
pc.rho <- list(prior='pccor1', param=c(0.7, 0.7))
pc.ar1 <- list(prec=pc.prec, rho=pc.rho)

ar1 <- inla(y ~ f(time, model='ar1', hyper=pc.ar1),
            data=list(y=m, n=n, time=1:length(anos)),
            Ntrials=n, family='binomial',
            control.predictor=list(compute=TRUE, link=1), 
            control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))
ar1$summary.fixed
ar1$summary.hyperpar

res <- list(lin=lin, ar1=ar1)
res$rw1 <- inla(y ~ f(time, model='rw1', hyper=list(prec=pc.prec)),
                data=list(y=m, n=n, time=1:length(anos)),
                Ntrials=n, family='binomial',
                control.predictor=list(compute=TRUE, link=1), 
                control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))
res$rw2 <- inla(y ~ f(time, model='rw2', hyper=list(prec=pc.prec)),
                data=list(y=m, n=n, time=1:length(anos)),
                Ntrials=n, family='binomial',
                control.predictor=list(compute=TRUE, link=1), 
                control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))
res$sig <- inla(y ~ f(time, model='sigm'), 
                data=list(y=m, n=n, time=1:length(anos)),
                Ntrials=n, family='binomial',
                control.predictor=list(compute=TRUE, link=1), 
                control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))

stats <- rbind(dic=sapply(res, function(x) x$dic$dic), 
               waic=sapply(res, function(x) x$waic$waic), 
               cpo=sapply(res, function(x)
                   -mean(log(x$cpo$cpo[3:length(anos)]))))
round(stats, 4)
stats-apply(stats, 1, min)

res$sig$summary.fix
res$sig$summary.hy
inla.doc('sigm')

sigmf <- function(x, b, a, k) b*x^k/(x^k + a^k)

plot(m/n, ylim=c(0.0075, 0.020))
plot(function(x) plogis(res$sig$summary.fix$mean +
                        sigmf(x, res$sig$summary.hy$mean[1],
                              res$sig$summary.hy$mean[2],
                              res$sig$summary.hy$mean[3])),
     1, length(anos), add=TRUE)
for (k in c(3,4,5))
    lines(res$sig$summary.fitted.val[, k], lty=2, col=4)


