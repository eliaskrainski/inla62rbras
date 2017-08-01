if (FALSE) ### se no diretorio de codigo, recue um nivel
    setwd('..')

### consideramos o script de exploracao
source('rcode/tmi-explore.R')

ls()

### consideramos os ultimos cinco anos agregados

### calcula taxa no Brasil no periodo 
1e3*(tx5 <- sum(obt5, na.rm=TRUE)/sum(nasc5, na.rm=TRUE))

### numero esperado de obitos se cada municipio tivesse essa taxa
summary(esp5 <- tx5*nasc5)

### propriedade: soma observados == soma esperados 
c(sum(obt5, na.rm=TRUE), sum(esp5, na.rm=TRUE))

### O_i ~ Poisson(E_i * r_i)
###  log(r_i) = a + s_i
### a: intercepto
### s: efeito espacial (http://journals.sagepub.com/doi/abs/10.1177/0962280216660421)
inla.doc('bym2')

idat <- list(area=1:n, o=obt5, e=ifelse(is.na(esp5),1,esp5))

summary(smr.o <- idat$o/idat$e)
### visualiza smr observado
(q.smr <- c(0, .67, .8, .94, 1.06, 1.25, 1.5, Inf))### quantile(smr.o, rev(k1)/(k+1), na.rm=TRUE))
1/q.smr
table(i.smr <- findInterval(smr.o, q.smr, TRUE))
plot(brmi, col=cores[smr.i], border='transparent')
smr.l <- leglabs(format(q.smr, dig=2), '<', '>', '-')
legend('bottomleft', smr.l, fill=cores, title='obitos/mil')

### o efeito espacial precisa de matriz de adjacencia
### a lista de vizinhanca pode ser obtida por
library(spdep)
brnb <- poly2nb(brmi)
brnb ### duas areas nao tem vizinhos (cuidado com os indices! labels!!!)
brmi@data[c(1525, 3499)+1,] ### Fernando de Noronha e Ilhabela

### matriz de adjacencia
table(nviz <- card(brnb))
brnb[which(nviz==0)] ### cuidado: lista nao vazia!!! 
(n <- length(nviz))
jj <- unlist(brnb)
adj <- sparseMatrix(i=rep(1:n, nviz), j=jj[jj!=0], x=1, dims=c(n,n))

pcprec <- list(theta=list(prior='pc.prec', param=c(1, 0.01)))

library(INLA)
formula0 <- o ~ 1 
formula1 <- o ~ 1 + f(area, model='iid', graph=adj, hyper=pcprec)
formula2 <- o ~ 1 + f(area, model='besag', graph=adj,
                      scale.model=TRUE, hyper=pcprec)
formula3 <- o ~ 1 + f(area, model='bym2', graph=adj)

result0 <- inla(formula0, 'poisson', data=idat, E=e)
result1 <- inla(formula1, 'poisson', data=idat, E=e)
result2 <- inla(formula2, 'poisson', data=idat, E=e)
result3 <- inla(formula3, 'poisson', data=idat, E=e)

rbind(result0$cpu, result1$cpu, result2$cpu, result3$cpu)

### porque diferenca nos tempos?
### depende de duas coisas:
###   numero de hyperparametros
###   numero de elementos nao zero em L, L'L=Q
### tamanho de theta: 0, 1, 1 e 2, respectivamente
result0$summary.hyperpar
result1$summary.hyperpar
result2$summary.hyperpar
result3$summary.hyperpar

### tamanho do problema em cada iteracao: 
result0$logfile[grep('nnz', result0$logfile)]
result1$logfile[grep('nnz', result1$logfile)]
result2$logfile[grep('nnz', result2$logfile)]
result3$logfile[grep('nnz', result3$logfile)]

### predito pelo modelo 1: E_i * exp(intercepto)
round(result0$summary.fix, 4) ###  a=0
plot(exp(result0$summary.fix$mean)*idat$e, idat$o)
abline(0:1)

### inclui demais modelos
par(mfrow=c(2,2), mar=c(3,3,1,1), mgp=2:0)
plot(exp(result0$summary.fix$mean)*idat$e, idat$o, xlab='pred', ylab='obs')
abline(0:1)

plot(exp(result1$summary.fix$mean +
         result1$summary.ran$area$mean)*idat$e,
     idat$o, xlab='pred', ylab='obs')
abline(0:1)
plot(exp(result1$summary.fix$mean +
         result2$summary.ran$area$mean[1:n])*idat$e,
     idat$o, xlab='pred', ylab='obs')
abline(0:1)
plot(exp(result3$summary.fix$mean +
         result3$summary.ran$area$mean[1:n])*idat$e,
     idat$o, xlab='pred', ylab='obs')
abline(0:1)

### visualiza: smr observado e s_i dos tres ultimos modelos
s123 <- cbind(result1$summary.ran$area$mean[1:n],
              result2$summary.ran$area$mean[1:n],
              result3$summary.ran$area$mean[1:n])
i123 <- apply(exp(s123), 2, findInterval, q.smr, TRUE)
n.i123 <- sapply(1:3, function(m) table(i123[,m]))

par(mfrow=c(2,2), mar=c(0,0,0,0))
### ver rcode/tmi-explore.R
plot(brmi, col=cores[smr.i], border='transparent')
legend('bottomleft', paste0(smr.l, ', n=', table(smr.i)),
       fill=cores, title='SMR')
for (m in 1:3) {
    plot(brmi, col=cores[i123[,m]], border='transparent')
    legend('bottomleft', paste0(smr.l, ', n=', n.i123[,m]),
           fill=cores, title='SMR')
}

### qual modelo e' melhor??? depende do objetivo!
### erro medio
colMeans(exp(s123)-idat$o/idat$e)
### quadratico erro medio
colMeans((exp(s123)-idat$o/idat$e)^2)

### erro de validacao cruzada...
