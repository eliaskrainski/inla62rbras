## ----loading-------------------------------------------------------------
#-----------------------------------------------------------------------
# load INLA and the the dataset

library(INLA)
data(Tokyo)

# inspect it
head(Tokyo)
(n <- nrow(Tokyo))

table(Tokyo$n)
table(Tokyo$y)

# visualize
library(ggplot2)
ggplot(Tokyo, aes(x=time, y=y/n)) + geom_point()

## ----pcprior-------------------------------------------------------------
# define the pc prior: P(sigma > 2) = 0.05
pcprec <- list(theta=list(
                   prior='pc.prec', param=c(2, 0.05)))

## ----model1--------------------------------------------------------------
formula1 <- y ~ 0 +
    f(time, model='rw1',
      cyclic=TRUE, constr=FALSE,
      scale.model=TRUE, hyper=pcprec)

## ----fit1----------------------------------------------------------------
tokyo1 <- inla(formula1, data=Tokyo, Ntrials=n,
               family='binomial', control.predictor=list(compute=TRUE),
               control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))

## ----fit2----------------------------------------------------------------
formula2 = y ~ 0 +
    f(time, model='rw2', cyclic=TRUE, constr=FALSE,
      scale.model=TRUE, hyper=pcprec)
tokyo2 <- inla(formula2, data=Tokyo, Ntrials=n,
               family='binomial', control.predictor=list(compute=TRUE),
               control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))


## ----results1------------------------------------------------------------

tokyo1
names(tokyo1)
tokyo1$cpu


# Hyperparameter summary 
rbind(tokyo1$summary.hyperpar,
      tokyo2$summary.hyperpar)

## ----plot----------------------------------------------------------------
par(mar=c(3,3,1,3), mgp=c(2,1,0))
with(Tokyo, plot(y/n, main=''))
for (j in c(1, 3, 5)) {
    lines(tokyo1$summary.fitted.val[,j], lwd=(j==1)+1, col=gray(.3))
    lines(tokyo2$summary.fitted.val[,j], lwd=(j==1)+1, col=2)
} 

### Medidas de ajuste
### DIC
tokyo1$dic$dic
tokyo2$dic$dic

### WAIC
tokyo1$waic$waic
tokyo2$waic$waic

### log-CPO
-sum(log(tokyo1$cpo$cpo))
-sum(log(tokyo2$cpo$cpo))

### visualiza
plot(tokyo1, plot.prior=TRUE)
