a<-read.table(file="proiect.csv",header=TRUE,sep=';')
a
attach(a)
fix(a)
a[complete.cases(a), ] #eliminarea randurilor ce contin NA


#Verificarea existentei outlierilor
par(mfrow=c(3,3))
boxplot(Price,main="Price",col="red")
boxplot(Change,main="Change",col="green")
boxplot(VolumeMil,main="VolumeMil",col="blue")
hist(Price,main="Price",col="red")
hist(Change,main="Change",col="green")
hist(VolumeMil,main="VolumeMil",col="blue")

par(mfrow=c(2,2))
boxplot(MkCapBil,main="MkCapBil",col="brown")
boxplot(Ratio,main="Ratio",col="yellow")
hist(MkCapBil,main="MkCapBil",col="brown")
hist(Ratio,main="Ratio",col="yellow")

par(mfrow=c(3,3))
boxplot(ProfitMg,main="ProfitMg",col="black")
boxplot(OperatingCF,main="OperatingCF",col="grey")
boxplot(RevenueBil,main="RevenueBil",col="pink")
hist(ProfitMg,main="ProfitMg",col="black")
hist(OperatingCF,main="OperatingCF",col="grey")
hist(RevenueBil,main="RevenueBil",col="pink")

par(mfrow=c(3,3))
boxplot(GssProfitBil,main="GssProfitBil",col="orange")
boxplot(ROA,main="ROA",col="cyan")
boxplot(ROE,main="ROE",col="chocolate")
hist(GssProfitBil,main="GssProfitBil",col="orange")
hist(ROA,main="ROA",col="cyan")
hist(ROE,main="ROE",col="chocolate")

# identificarea outlierilor
#Pentru coloana Price
outlierPrice<-boxplot(Price)
outlierPrice$out
c<-outlierPrice$out
c
d<-which(Price %in% c)

#Pentru coloana Change
outlierChange<-boxplot(Change)
outlierChange$out
c<-outlierChange$out
c
d<-which(Change %in% c)

#Pentru coloana VolumeMil
outlierVolumeMil<-boxplot(VolumeMil)
outlierVolumeMil$out
c<-outlierVolumeMil$out
c
d<-which(VolumeMil %in% c)

#Pentru coloana MkCapBil
outlierMkCapBil<-boxplot(MkCapBil)
outlierMkCapBil$out
c<-outlierMkCapBil$out
c
d<-which(MkCapBil %in% c)

#Pentru coloana Ratio
outlierRatio<-boxplot(Ratio)
outlierRatio$out
c<-outlierRatio$out
c
d<-which(Ratio %in% c)

#Pentru coloana ProfitMg
outlierProfitMg<-boxplot(ProfitMg)
outlierProfitMg$out
c<-outlierProfitMg$out
c
d<-which(ProfitMg %in% c)

#Pentru coloana OperatingCF
outlierOperatingCF<-boxplot(OperatingCF)
outlierOperatingCF$out
c<-outlierOperatingCF$out
c
d<-which(OperatingCF %in% c)

#Pentru coloana RevenueBil
outlierRevenueBil<-boxplot(RevenueBil)
outlierRevenueBil$out
c<-outlierRevenueBil$out
c
d<-which(RevenueBil %in% c)

#Pentru coloana GssProfitBil
outlierGssProfitBil<-boxplot(GssProfitBil)
outlierGssProfitBil$out
c<-outlierGssProfitBil$out
c
d<-which(GssProfitBil %in% c)

#Pentru coloana ROA
outlierROA<-boxplot(ROA)
outlierROA$out
c<-outlierROA$out
c
d<-which(ROA %in% c)

#Pentru coloana ROE
outlierROE<-boxplot(ROE)
outlierROE$out
c<-outlierROE$out
c
d<-which(ROE %in% c)

#Eliminarea randurilor cu outlieri
a[-c(1,2,3,4,5,6,7,8,12,22,23,35,37,39), ]
a<-a[-c(1,2,3,4,5,6,7,8,12,22,23,35,37,39), ]
a[complete.cases(a), ]


#### I.Statistici descriptive
a[complete.cases(a), ]
summary(a[complete.cases(a), ])

library(mnormt)
library(psych)
describe(a)

library(moments)
skewness(a2[complete.cases(a2), ])
kurtosis(a2[complete.cases(a2), ])
cov(a2[complete.cases(a2), ])
cor(a2[complete.cases(a2), ])

library(sp)
library(raster)
sd(a2[complete.cases(a2), ]$Price)
var(a2[complete.cases(a2), ]$Price)
cv(a2[complete.cases(a2), ]$Price)
with(a2[complete.cases(a2), ],plot(ProfitMg,RevenueBil,col=c("red","blue")))
hist(a2[complete.cases(a2), ]$Price,breaks=12,col="red",main="Price")
b=a2[complete.cases(a2), ]$Price
hist(b,freq=F,xlab="Price",main="Price")
lines(density(b),col="red",lwd=2)
d=density(a2[complete.cases(a2), ]$ROA)
plot(d,col="blue",main="Densitate ROA")

library(corrplot)
a2<-a[2:12]
a2[complete.cases(a2), ]
corrplot(cor(a2[complete.cases(a2), ]),method="circle")


#### II. ANALIZA COMPONENTELOR PRINCIPALE
# Standardizarea datelor: mean=0, sd=1
date_std=scale(a[2:12],scale=TRUE)
date_std[complete.cases(date_std), ]

boxplot(date_std[complete.cases(date_std), ],col=c("red","green","blue","brown","yellow","black","grey","pink","orange","cyan","chocolate"),main="Boxplot cu toti indicatorii")

apply(date_std[complete.cases(date_std), ], 2, sd)
apply(date_std[complete.cases(date_std), ], 2, mean)

corelatie<-cor(date_std[complete.cases(date_std), ])
corelatie
covarianta<-cov(date_std[complete.cases(date_std), ])
covarianta

#Componente principale
pc=princomp(date_std[complete.cases(date_std), ],cor=TRUE) #principals components
sd=pc$sd #standard deviation = abaterea standard
valpr=sd*sd
procentA= valpr*100/11  #n=11
procentC=cumsum(procentA)  #cumuleaza, face suma de procentA
v=zapsmall(data.frame(valpr,procentA,procentC))
v
scree_plot=prcomp(date_std[complete.cases(date_std), ])
plot(scree_plot,type="l",main="Scree plot")

#Vectori si valori proprii
ev<-eigen(covarianta)
ev
val<-ev$values
val
vect<-ev$vectors
vect

#Vectori proprii
vectpr=zapsmall(pc$loadings)
vectpr

#Scorurile principale
nume_obs=a[complete.cases(a), ][1]
nume_obs
date_std=scale(a[2:12],scale=TRUE)
date_std[complete.cases(date_std), ]

scoruriprinc=zapsmall(pc$scores) #scoruri principale
d=cbind(scoruriprinc,nume_obs)
d


#Matricea factor
c=zapsmall(pc$scores)
corFact=zapsmall(cor(date_std[complete.cases(date_std), ],c))
corFact
#W1 are corelatii puternice cu OperatingCF si cu GssProfitBil => W1 componenta Cash Flow-ului si Profitului
#W2 are corelatii puternice cu ROA si cu ROE => W2 componenta rentabilitatilor
#W3 are corelatii puternice cu Price => W3 componenta preturilor
#W4 are corelatii puternice cu VolumeMil => W4 componenta numarului de actiuni

#Cercul corelatiilor
dev.new()
cerc=seq(0,2*pi,length=100)
plot(cos(cerc),sin(cerc),type="l",col="blue",xlab="W1",ylab="W2")
text(corFact[ , 1],corFact[ ,2],rownames(corFact),col="red",cex=0.7)

cerc=seq(0,2*pi,length=100)
plot(cos(cerc),sin(cerc),type="l",col="blue",xlab="W3",ylab="W4")
text(corFact[ , 3],corFact[ ,4],rownames(corFact),col="red",cex=0.7)

#Plot companii:
dev.new()
plot(d[ ,1],d[ ,2],main="Plot componente-W1 si W2",xlab="W1",ylab="W2")
text(d[,1],d[,2],labels=d[,12],col="red",pos=3,cex=0.7)

plot(d[ ,3],d[ ,4],main="Plot componente-W3 si W4",xlab="W3",ylab="W4")
text(d[,3],d[,4],labels=d[,12],col="red",pos=3,cex=0.7)


~~~~~~~~~~~~~~~~~~Partea II~~~~~~~~~~~~~~~~~~

#Importul si prezentarea datelor
a<-read.table(file="proiect.csv",header=TRUE,sep=';')
a[complete.cases(a), ]
attach(a[complete.cases(a), ])

simbol_obs=a[1]
simbol_obs

#Eliminarea randurilor cu outlieri
a[-c(1,2,3,4,5,6,7,8,12,22,23,35,37,39), ]
a<-a[-c(1,2,3,4,5,6,7,8,12,22,23,35,37,39), ]
a[complete.cases(a), ]

# Standardizarea datelor
date_std=scale(a[2:12],scale=TRUE)
date_std[complete.cases(date_std), ]


#Coeficienti de corelatie partiala(KMO)
####Metoda 1
x<-rnorm(100, 2, 0.3)
y<-rnorm(100, 3, 0.5)
z<-rnorm(100, 3.4, 0.7)
cxz<-lm(x~z)
rxz<-resid(cxz)
cyz<-lm(y~z)
ryz<-resid(cyz)
cor(x,y)
cpxy<-cor(rxz, ryz)
cpxy

####Metoda 2
(cor(x,y)-(cor(x,z)*cor(y,z)))/sqrt((1-cor(x,z)^2)*(1-cor(y,z)^2))

####Metoda 3
m<-cbind(x,y,z)
r<-cor(m)
p<-solve(r)
cpxy<- -1*p[1,2]/sqrt(p[1,1]*p[2,2])
cpxy

#Indicele KMO
#Inversa matricii de corelatie
R=cor(date_std[complete.cases(date_std), ])
R
invR<-solve(R)
invR

#Extragerea coeficientului de corelatie partiala
A<-matrix(1, nrow(invR), ncol(invR))
for(i in 1:nrow(invR))
{ 
  for(j in (i+1):ncol(invR))
{
  A[i,j]<-invR[i,j]/sqrt(invR[i,i]*invR[j,j])
  A[j,i]<-A[i,j]
}
}
A

colnames(A)<-colnames(date_std[complete.cases(date_std), ])
rownames(A)<-colnames(date_std[complete.cases(date_std), ])
print(A)

kmo.num<-sum(R^2)-sum(diag(R^2))
kmo.denom<-kmo.num+(sum(A^2)-sum(diag(A^2)))
kmo<-kmo.num/kmo.denom
print(kmo)

#Testul barlett de sfericitate - Scrierea functiei
bart<-function(date)
{
R<-cor(date)
p<-ncol(date)
n<-nrow(date)
chi2<- -((n-1)-((2*p)+5)/6)*log(det(R))
df<-(p*(p-1)/2)
df
crit<-qchisq(.95,df) #valoarea critica
p<-pchisq(chi2, df, lower.tail=F) #p-value 
cat("Barlette's test of sphericity: X2(",df,")=",chi2,", p=", round(p,6), sep="")
}
bart(date_std[complete.cases(date_std), ])
qchisq(0.05,78,lower.tail=F)


fit.4<-factanal(date_std[complete.cases(date_std), ], factors=4, rotation="varimax")
print(fit.4, digits=2, cutoff=.2, sort=TRUE)

fit.5<-factanal(date_std[complete.cases(date_std), ], factors=5, rotation="varimax")
print(fit.5, digits=2, cutoff=.2, sort=TRUE)

fit.3<-factanal(date_std[complete.cases(date_std), ], factors=3, rotation="varimax")
print(fit.3, digits=2, cutoff=.2, sort=TRUE)


colnames(fit.4$loadings)<-c("Factorul Profitului si al Cash-Flow-ului", "Factorul Rentabilitatilor", "Factorul Venitului", "Factorul Pretului Actiunilor")
print(loadings(fit.4), digits=2, cutoff=0.2, sort=TRUE)

library(corrplot)
corrplot(fit.4$loadings, method="circle")

fit.4_coef<-solve(fit.4$correlation) %*% fit.4$loadings
fit.4_coef

fit.4_scores<-date_std[complete.cases(date_std), ] %*% solve(cor(date_std[complete.cases(date_std), ])) %*% fit.4$loadings
fit.4_scores

df=data.frame(fit.4_scores)
df
biplot(df[,1:2], fit.4$loadings[,1:2], cex=c(0.7, 0.8))






