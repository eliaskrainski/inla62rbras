if (FALSE) ### se no diretorio de codigo, recue um nivel
    setwd('..')

### consideramos o script de exploracao
source('rcode/tmi-explore.R')
ls()

### consideremos dados de alguns municipios para que o tempo
### de execucao seja adequado para o minicurso
head(prmi@data[,1:5])
dim(sel.mi <- prmi[coordinates(prmi)[,1]>(-51), ])
##which(substr(brmi$CD_GEOCMU,1,2)==41),])
plot(sel.mi)

### taxa no periodo 
tx0 <- sum(sel.mi@data[,jm], na.rm=TRUE) /
    sum(sel.mi@data[,tail(jn,length(jm))],na.rm=TRUE)
tx0*1e3

### a parte espacial do efeito espaco temporal
### precisa da matriz de adjacencia (vizinhanca) 
### a lista de vizinhanca pode ser obtida por
library(spdep)
sel.nb <- poly2nb(sel.mi)
sel.nb

### visualiza 
plot(sel.mi)
plot(sel.nb, coordinates(sel.mi), add=TRUE)

### matriz de adjacencia
table(nviz <- card(sel.nb))

(n <- length(nviz))
adj <- sparseMatrix(i=rep(1:n, nviz), j=unlist(sel.nb), x=1, dims=c(n,n))

(nt <- length(jm))
itdat <- list(i=rep(1:n, nt), t=rep(1:nt, each=n))
itdat$o <- unlist(sel.mi@data[, jm])
itdat$e <- unlist(sel.mi@data[, tail(jn,nt)])*tx0
itdat$e[which(is.na(itdat$e))] <- 0
c(obs=sum(itdat$o), esp=sum(itdat$e))

### define distribuicao a priori da correlacao temporal
pcrho1 <- list(prior='pccor1', param=c(0.7, 0.7))

### define o modelo
formula1 <- o ~ 1 + f(i, model='bym2', graph=adj, group=t,
                      control.group=list(model='ar1', hyper=list(theta=pcrho1)))
library(INLA)
result1 <- inla(formula1, 'poisson', data=itdat, E=e,
                control.inla=list(strategy='gaussian', ## rapida e grosseira
                                  int.strategy='eb')) ## nao integra nos hyperpar.
result1$cpu

### resultados
result1$summary.fix
round(result1$summary.hyper, 4)

### visualiza
rep(0:3, each=5)*2*5+1:5 
plot(result1$summary.ran$i$mean[2*n*rep(1:nt-1,each=n)+1:n])
st.mean <- result1$summary.ran$i$mean[2*n*rep(1:nt-1,each=n)+1:n]
cores <- gray(rank(st.mean)/(nt*n))
par(mfrow=c(4,5), mar=c(0,0,0,0))
for (j in 1:20)
    plot(sel.mi, col=cores[(j-1)*n+1:n], border='transparent')

### 
