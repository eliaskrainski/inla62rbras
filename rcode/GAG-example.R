
library(MASS)
data(GAGurine)
head(GAGurine)

par(mar=c(3,3,1,1), mgp=2:0, las=1)
plot(GAGurine)

library(INLA)
precprior <- list(theta=list(prior='pcprec', param=c(1, 0.01)))
ires <- inla(GAG ~ f(inla.group(Age,50), model='rw2', 
                     scale.model=TRUE, hyper=precprior),
             family='gamma', data=GAGurine,
             control.predictor=list(compute=TRUE),
             control.compute=list(config=TRUE)) ## use in inla.posterior.sample() 

### sample from the posterior distribution
model.samples <- inla.posterior.sample(
    n=1e4, ### number of posterior samples 
    result=ires, ### result from inla()
    add.names=FALSE) ## reduce the object size

### sample observations from the posterior samples
obs.samples <- sapply(model.samples, function(x)
    rgamma(nrow(GAGurine),
           x$hyperpar[1],
           x$hyperpar[1]/exp(x$latent[1:nrow(GAGurine),1])))

y.res <- apply(obs.samples, 1, function(x)
    c(mean(x), quantile(x, c(0.025, 0.975))))

### visualize the observation band
par(mar=c(3,3,1,1), mgp=2:0, las=1)
plot(GAGurine)
lines(GAGurine$Age, y.res[1,], col=2)
lines(GAGurine$Age, y.res[2,], col=2, lty=2)
lines(GAGurine$Age, y.res[3,], col=2, lty=2)

### treating the Age effect as continuous
knots <- inla.mesh.1d(0:36/2)
dim((proj <- inla.mesh.projector(knots, GAGurine$Age))$proj$A)
colSums(proj$proj$A)
sdat <- inla.stack(
    tag='estimation',
    data=list(y=GAGurine$GAG, link=1),
    A=list(proj$proj$A, 1),
    effects=list(i=1:ncol(proj$proj$A), b0=rep(1,nrow(GAGurine))))
res.cont <- inla(y ~ 0 + b0 +
                     f(i, model='rw2', scale.model=TRUE, hyper=precprior),
                 family='gamma', data=inla.stack.data(sdat), 
                 control.predictor=list(compute=TRUE, link=1, 
                                        A=inla.stack.A(sdat)),
                 control.compute=list(config=TRUE)) ## use in inla.posterior.sample() 

str(idx.r <- inla.stack.index(sdat, 'estimation')$data)

par(mar=c(3,3,1,1), mgp=2:0, las=1)
plot(GAGurine)
lines(GAGurine$Age, ires$summary.fitted.val$mean, col=2)
lines(GAGurine$Age, res.cont$summary.fitted.val$mean[idx.r], col=4)

### sample from the posterior distribution
model2.samples <- inla.posterior.sample(
    n=1e4, ### number of posterior samples 
    result=res.cont, ### result from inla()
    add.names=FALSE) ## reduce the object size

### sample observations from the posterior samples
obs2.samples <- sapply(model2.samples, function(x)
    rgamma(nrow(GAGurine),
           x$hyperpar[1],
           x$hyperpar[1]/exp(x$latent[idx.r]))) 

y2.res <- apply(obs2.samples, 1, function(x)
    c(mean(x), quantile(x, c(0.025, 0.975))))

### visualize the resulting observation band
par(mar=c(3,3,1,1), mgp=2:0, las=1)
plot(GAGurine)
lines(GAGurine$Age, y2.res[1,], col=4)
lines(GAGurine$Age, y2.res[2,], col=4, lty=2)
lines(GAGurine$Age, y2.res[3,], col=4, lty=2)
