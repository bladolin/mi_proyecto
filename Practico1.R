# Practico 1 Diseño 1 2026

## Creo Vectores
c(1,2,2,1,3,3)
v1<-c(1,2,2,1,3,3)

?c

v1[5]
v1[5:6]
v1

mode(v1)
length(v1)

v1+1

sum(v1)

sum(v1)/length(v1)
mean(v1)

v2<-c('M','F','M','M','F','F')
mode(v2)
v2[2:3]

sum(v2)

v2<-c('M','F','M','M','F','F')

matrix(c(1,2,2,1,3,3),ncol=2)
matrix(v1,ncol=2)
matrix(v1,ncol=3)
matrix(v1,nrow=2)

m1<-matrix(v1,nrow=2)
m1[,3]
dim(m1)

m1<-matrix(v1,nrow=2)

list(v1,v2)
lis1<-list(v1,v2)

length(lis1)

#Largodelprimerelementocontenidoenlalista
length(lis1[[1]])

str(lis1)

dim(lis1)

lis1[6,1]
lis1[4]
lis1[2]
lis1[[2]][6]

#Es posible hacer listas de listas
lis2<-"hoyllueve"
lis<-list(lis1,lis2)
str(lis)


lis1<-list(v1,v2)
lis2<-"hoyllueve"
lis<-list(lis1,lis2)


data.frame(v1,v2)
dat1<-data.frame(v1,v2)

dat1

dat1$v1
dat1$v2


colnames(dat1)<-c('var1','var2')
rownames(dat1)<-seq(1:nrow(dat1))#nrow_es_una_funcion_que_indica_el_numero_de_filas_del_objeto
dat1


attach(dat1)

var1
var2

#Lectura_de_datos_en_R
ep<-read.table('epoca.txt',header=T)


str(ep)
head(ep)


summary(ep)

----fig.height=4,fig.width=4,echo=TRUE,eval=FALSE-----------------------
ep$Peso
plot(ep$Peso)


hist(ep$Peso,main="Hisograma de frecuencias absolutas")


sum(ep$Peso<=25&ep$Peso>=20)

----fig.height=4,fig.width=4--------------------------------------------
hist(ep$Peso,main="Hisograma de frecuencias relativas",freq=FALSE)


nrow(ep)
4/nrow(ep)
freq=4/nrow(ep)
#Laalturadelhistogramaparapesosentre0y25es:
freq/5
#5eslaamplituddelabarraquereunelospesosentre20y25

----fig.height=4,fig.width=4--------------------------------------------
boxplot(Peso~Epoca,ep,col=c("red","gray"))
#Seusauncolorparacadaniveldelfactorépoca


ep$Peso[ep$Epoca=='Abril']
summary((ep$Peso[ep$Epoca=='Abril']))

ep$Peso[ep$Epoca=='Agosto']
summary(ep$Peso[ep$Epoca=='Agosto'])

----fig.height=4,fig.width=4--------------------------------------------
dotchart(ep$Peso,groups=ep$Epoca)
dotchart(ep$Peso,labels=row.names(ep),groups=ep$Epoca)


ep.t<-t.test(Peso~Epoca,var.equal=TRUE,data=ep)
ep.t
names(ep.t)
str(ep.t)
ep.t$conf.int

#anexos
with(ep,shapiro.test(Peso[Epoca=="Abril"]))
with(ep,shapiro.test(Peso[Epoca=="Agosto"]))
hist(with(ep,Peso[Epoca=="Abril"]))


with(ep,var(Peso[Epoca=="Abril"]))
with(ep,var(Peso[Epoca=="Agosto"]))


with(ep,var.test(Peso~Epoca))
with(ep,t.test(Peso[Epoca=="Abril"],Peso[Epoca=="Agosto"]))
subset(ep,Epoca=="Abril")


tabla1<-matrix(c(7,14,6,11,11,5,15,8),ncol=4)
tabla1
dimnames(tabla1)[[1]]<-c('Sano','Enfermo')
dimnames(tabla1)[[2]]<-c('4','10','20','>20')

prop.table(tabla1)
margin.table(tabla1)
margin.table(tabla1,1)
margin.table(tabla1,2)

barplot(tabla1)
barplot(prop.table(tabla1))
barplot(prop.table(tabla1)*100)

barplot(prop.table(tabla1)*100,beside=T)

tabla1<-matrix(c(7,14,6,11,11,5,15,8),ncol=4)


dotchart(tabla1)


x<-rnorm(100,78)
mean(x)
sd(x)

plot(x)
plot(sort(x))
plot(sort(x),type='s')

hist(x)
hist(x,freq=F);curve(dnorm(x,mean=mean(x),sd=sd(x)),add=T)


x<-rnorm(100,78)

