###______________criar uma população hipotética com 5000 individuos e X marcados
pop.simul.teorica<- NULL #criar um vetor vazio para receber os valores
for (i in 1:(858-117)) #marcados exceto os mantidos nos bretes fixos
pop.simul.teorica[i]<-1 # criar valor 1 no vetor para o nuemro de indviduos marcados
for(i in (858-117+1):4800)#criar a população de numero conhecido (4800, no caso)
  pop.simul.teorica[i]<-0 #preencher com valor 0 os demais "individuos" da população
pop.simul.teorica


####______________________criar uma tabela de recaptura aleatória

setwd("D:/OneDrive - ICMBio/Anexos")

# read dataset
q <- read.table('data/queixadas.csv', header=T, sep=",")
matriz.teorica<-data.frame(total_n=q$total_n, 
                           tagged_n= 0)

for(i in 1:300)
  matriz.teorica$tagged_n[i]<-sum(sample(pop.simul.teorica, matriz.teorica$total_n[i], replace= TRUE))
hist(matriz.teorica$tagged_n)



#____________________TESTAR O MODELO TEORICO

matriz.teorica$media.prop <- matriz.teorica$tagged_n/matriz.teorica$total_n
boxplot(matriz.teorica$media.prop)
hist(matriz.teorica$media.prop)


#-------------- somando total das colunas (Y = M*(soma de n)/(soma de m))
matriz.teorica$media.prop <- matriz.teorica$tagged_n/matriz.teorica$total_n

M<- 858-117 #Marcados exceto os mantidos em bretes fixos
pop.simul <- rep(NA, 10000)    # criando objeto para receber valores de simulaÃ§Ã£o
matriz.teorica$iboot <- 1:nrow(matriz.teorica) # criando coluna indice
for(i in 1:10000)    # criando contador
{
  iboot <- sample(1:nrow(matriz.teorica), replace=TRUE) # gerando numeros aleatorios dentro da range da coluna indice 
  bootdata <- matriz.teorica[matriz.teorica$iboot %in% iboot,] # reamostrando matriz.teoricacom base nos numeros gerados acima 
  pop.simul[i] <- M*(sum(bootdata$total_n)/sum(bootdata$tagged_n)) # preenchendo pop.simul com valores simulados
  assign("pop.simul", pop.simul, .GlobalEnv) # salvando no global env
}
mean(pop.simul) # media
round(quantile(pop.simul, probs=c(0.025, 0.975), na.rm=T), 4)    # criando objeto com quantis referentes ao 95% CI

sd(pop.simul) # sd
hist(pop.simul,main="simulado1")

ker95km<- 378.9897 #área de estudo em kilometros quadrados
dens<- mean(pop.simul)/ker95km
#-----------------calculando a proporÃ§Ã£o por linha, e usando a mÃ©dia das proporÃ§Ãµes ---------------------------------------

pop.simul2 <- rep(NA, 10000)    # criando objeto para receber valores de simulaÃ§Ã£o

for(i in 1:10000)    # criando contador
{
  iboot <- sample(1:nrow(matriz.teorica), replace=TRUE)
  bootdata <- matriz.teorica[matriz.teorica$iboot %in% iboot,]
  pop.simul2[i] <- M*(1/mean(bootdata$media.prop))
  assign("pop.simul2", pop.simul2, .GlobalEnv)
}
mean(pop.simul2)
round(quantile(pop.simul2, probs=c(0.025, 0.975), na.rm=T), 4)    # criando objeto com quantis referentes ao 95% CI
sd(pop.simul2)
hist(pop.simul2)
dens2<- mean(pop.simul2)/ker95km
bootdata$media.prop

mean(matriz.teorica$total_n)
sd(matriz.teorica$total_n)

