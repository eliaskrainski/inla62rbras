### se no diretorio de codigo, recue um nivel
if (FALSE) ### nao executa em source()
    setwd('..')

## pacote e dados
library(maptools)
load('dados/prmi.RData')

### inspeciona o objeto
class(prmi)
dim(prmi)
head(prmi@data[,1:5])
names(prmi)

### indice das colunas de nascimentos
length(jn <- 3:24)

### indice das colunas de obitos
length(jm <- 26:45)

### algumas linhas de cada parte
head(prmi@data[, jn], 3) 
head(prmi@data[, jm], 3)

### NA nos nascidos e' desconhecido 
### NA nos obitos e' zero
for (j in jm) prmi@data[is.na(prmi@data[,j]), j] <- 0

### sumario 
sapply(prmi@data[,jn], summary)
sapply(prmi@data[,jm], summary)

### tmi em cada ano (1996-2015) para cada municipio
tmi <- prmi@data[, jm]/prmi@data[, jn[-c(1,2)]]
colnames(tmi) <- gsub('m', 't', colnames(tmi))
### (anos iniciais parecem ter problemas)
apply(1e4*tmi, 2, mean, na.rm=TRUE)

### define cores
k <- 6; k0 <- k:0
length(cores <- rgb(rev(k0/k), sqrt(1-2*abs(k0-k/2)/k), k0/k))

### define limites para definir categorias de taxas
q <- c(0, quantile(tmi[tmi[,20]>0,20], rev(k0)/k, na.rm=TRUE))
round(1e4*q,2)
table(id <- findInterval(tmi[,20], q, TRUE))

### visualiza
if (FALSE) { ### nao executa em source()
    par(mar=c(0,0,0,0))
    plot(prmi, col=cores[id], border='transparent')
    labs <- leglabs(format(1e3*q, dig=2), '<', '>', '-')
    legend('bottomleft', labs, fill=cores, title='obitos/mil')
}

### taxa observada nos ultimos cinco anos
nasc5 <- rowSums(prmi@data[, tail(jn,5)]) 
obt5 <- rowSums(prmi@data[, tail(jm,5)])
summary(tmi5 <- obt5/nasc5) 

k1 <- (k+1):0
1e3*(q5 <- quantile(tmi5, rev(k1)/(k+1), na.rm=TRUE))
table(i5 <- findInterval(tmi5, q5, TRUE))

if (FALSE) { ### nao executa em source()
    par(mar=c(0,0,0,0))
    plot(prmi, col=cores[i5], border='transparent')
    labs <- leglabs(format(1e3*q5, dig=2), '<', '>', '-')
    legend('bottomleft', labs, fill=cores, title='obitos/mil')
}
