### options(width=50)
if (FALSE) ### se no diretorio de codigo, recue um nivel
    setwd('..')

### Carrega os dados
load('dados/prmi.RData')
names(prmi)

### indice das colunas de nascimentos
length(jn <- 3:24)
### indice das colunas de obitos 
length(jm <- 26:45)

### qual linha está Curitiba
head(prmi@data[,1:5])
ii <- grep('CURITIBA', prmi$NM_MUNICIP)
ii

### olha dados de Curitiba
prmi@data[ii, 1:5]
i <- ii[1]
prmi@data[i, 3:10]
prmi@data[i, jm]

### organiza os dados de Curitiba em vetores
n <- unlist(prmi@data[i, jn])
o <- c(NA, NA, unlist(prmi@data[i, jm]))
round(1000*o/n)

### Visualiza a taxa anual de mortalidade em Curitiba
anos <- 1994:2015
plot(anos, o/n)

### Carrega pacote inla
library(INLA)
### modelo de regressão linear para dados Binomial (logística)
lin <- inla(y ~ time, 
            data=list(y=o, n=n, time=1:length(anos)),
            Ntrials=n, family='binomial',
            control.predictor=list(compute=TRUE, link=1), 
            control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))

lin
summary(lin)
names(lin)
lin$summary.fixed

### Define distribuições à priori para efeito aleatório AR(1)
pc.prec <- list(prior='pcprec', param=c(1, 0.01))
pc.rho <- list(prior='pccor1', param=c(0.7, 0.7))
pc.ar1 <- list(prec=pc.prec, rho=pc.rho)

### Estima modelo com efeito aleatório AR(1)
ar1 <- inla(y ~ f(time, model='ar1', hyper=pc.ar1),
            data=list(y=o, n=n, time=1:length(anos)),
            Ntrials=n, family='binomial',
            control.predictor=list(compute=TRUE, link=1), 
            control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))
ar1$summary.fixed
round(ar1$summary.hyperpar, 4)

res <- list(lin=lin, ar1=ar1) ## junta dois resultados anteriores em lista

### Estima modelo com efeito aleatório RW(1) 
res$rw1 <- inla(y ~ f(time, model='rw1',
                      scale.model=TRUE, hyper=list(prec=pc.prec)),
                data=list(y=o, n=n, time=1:length(anos)),
                Ntrials=n, family='binomial',
                control.predictor=list(compute=TRUE, link=1), 
                control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))

### Estima modelo com efeito aleatório RW(2)
res$rw2 <- inla(y ~ f(time, model='rw2',
                      scale.model=TRUE, hyper=list(prec=pc.prec)),
                data=list(y=o, n=n, time=1:length(anos)),
                Ntrials=n, family='binomial',
                control.predictor=list(compute=TRUE, link=1), 
                control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))

### Estima modelo com efeito fixo não linear (sigmoidal)
### Ver inla.doc('sigm')
res$sig <- inla(y ~ f(time, model='sigm'), 
                data=list(y=o, n=n, time=1:length(anos)),
                Ntrials=n, family='binomial',
                control.predictor=list(compute=TRUE, link=1), 
                control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE))

### medidas de ajuste de cada modelo
stats <- rbind(dic=sapply(res, function(x) x$dic$dic), 
               waic=sapply(res, function(x) x$waic$waic), 
               cpo=sapply(res, function(x)
                   -mean(log(x$cpo$cpo[3:length(anos)]))))
round(stats, 4)
stats-apply(stats, 1, min)

res$sig$summary.fix
res$sig$summary.hy
inla.doc('sigm')

### Visualiza curva do modelo sigmoidal
### considerando média a posteriori dos parametros
sigmf <- function(x, b, a, k) b*x^k/(x^k + a^k)
plot(o/n, ylim=c(0.0075, 0.020))
plot(function(x) plogis(res$sig$summary.fix$mean +
                        sigmf(x, res$sig$summary.hy$mean[1],
                              res$sig$summary.hy$mean[2],
                              res$sig$summary.hy$mean[3])),
     1, length(anos), add=TRUE)
for (k in c(1, 3, 5)) ### visualiza media e quantis a posteriori da taxa
    lines(res$sig$summary.fitted.val[, k], lty=(k>1)+1, col=2)

### Visualiza todos os resultados
plot(o/n, ylim=c(0.0075, 0.020), pch=19)
for (m in 1:length(res)) 
    for (k in c(1, 3, 5))
        lines(res[[m]]$summary.fitted.val[, k],
              lwd=(k==1)+1, lty=(k>1)*2+1, col=m)
legend('topright', names(res), lty=1, col=1:m)
