
setwd("D:/OneDrive - ICMBio/Anexos/")

# read dataset
q <- read.table('queixadas.csv', header=T, sep=",")
q$media.prop <- q$tagged_n/q$total_n
hist(q$tagged_n)


#-------------- somando total das colunas (Y = M*(soma de n)/(soma de m))
M<- 858-117 #Marcados exceto os mantidos em bretes fixos
pop <- rep(NA, 10000)    # criando objeto para receber valores de simulaÃ§Ã£o
q$iboot <- 1:nrow(q) # criando coluna indice
for(i in 1:10000)    # criando contador
{
  iboot <- sample(1:nrow(q), replace=TRUE) # gerando numeros aleatorios dentro da range da coluna indice 
  bootdata <- q[q$iboot %in% iboot,] # reamostrando q com base nos numeros gerados acima 
  pop[i] <- M*(sum(bootdata$total_n)/sum(bootdata$tagged_n)) # preenchendo pop com valores simulados
  assign("pop", pop, .GlobalEnv) # salvando no global env
}
mean(pop) # media
round(quantile(pop, probs=c(0.025, 0.975), na.rm=T), 4)    # criando objeto com quantis referentes ao 95% CI
sd(pop) # sd

ker95km<- 378.9897 #área de estudo em kilometros quadrados
dens<- mean(pop)/ker95km
#-----------------calculando a proporÃ§Ã£o por linha, e usando a mÃ©dia das proporÃ§Ãµes ---------------------------------------

pop2 <- rep(NA, 10000)    # criando objeto para receber valores de simulaÃ§Ã£o

for(i in 1:10000)    # criando contador
{
  iboot <- sample(1:nrow(q), replace=TRUE)
  bootdata <- q[q$iboot %in% iboot,]
  pop2[i] <- M*(1/mean(bootdata$media.prop))
  assign("pop2", pop2, .GlobalEnv)
}
mean(pop2)
round(quantile(pop2, probs=c(0.025, 0.975), na.rm=T), 4)    # criando objeto com quantis referentes ao 95% CI
sd(pop2)
hist(pop2)
dens2<- mean(pop2)/ker95km

