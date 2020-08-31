#Analiza cluster
a <- read.csv(file="proiect.csv", header=TRUE, sep=";")
a
View(a)

install.packages("cluster")
require (cluster)

install.packages("factoextra")
require(factoextra)

#reprezentarea grafica a tarilor in functie de doi dintre indicatori
plot (Price~ROA, a)
with(a, text(Price~ROA, labels=Symbol, pos=4, cex=.3))

#Creare dateframe cu variabile numerice
b <- a[,2:12]
rownames(b, do.NULL = TRUE, prefix = "row")
rownames(b)<- a$Symbol #etichetarea randurilor cu numele tarilor
View(b)

#standardizarea observatiilor in vederea aplicarii analizei cluster
standardizare <- function(x) {(x - mean(x))/sd(x)} #standardizarea observatiilor
b_std <-apply(b[complete.cases(b),],2,standardizare)
b_std

#calcularea distantelor dintre obiecte
distance <- dist(as.matrix(b_std))
fviz_dist(distance, gradient = list(low = "white", mid = "turquoise", high = "blue"))

dist_mat <- as.matrix(distance)
write.csv(dist_mat, file="Distance.csv")

#reprezentarea grafica sub forma de heatmap
heatmap(b_std) #realizeaza analiza cluster ierarhic de tipul agregarii complete (Complete linkage)

#aplicarea functiei hclust pt realizarea analizei cluster

# analiza cluster metoda agregarii complete
hc.c <- hclust(distance)
plot(hc.c,  hang=-1)
rect.hclust(hc.c, k = 3, border = 2:5)
member.c <- cutree(hc.c,k=4)
member.c

#Asociaza fiecarui obiect clasa caruia ii apartine

# analiza cluster metoda distantelor medii
hc.a <- hclust(distance, method="average")
plot(hc.a, labels = b$Symbol, hang=-1)
rect.hclust(hc.a,k=3,border=2:5)
member.a <- cutree(hc.a,k=3)
member.a

# analiza cluster metoda Ward
hc.w <- hclust(distance, method="ward.D2")
plot(hc.w, labels = b$Symbol, hang=-1)
rect.hclust(hc.w, k = 3, border = 2:5)
member.w <- cutree(hc.w,k=3)
member.w

table(member.w, member.c)
table(member.w, member.a)
table(member.a, member.c)

aggregate(b_std, list(member.w), mean)
#variabilele a caror medie difera cel mai mult intre clase au cea mai mare putere deseparare a grupelor

install.packages("cluster")
require (cluster)

#verificarea acuratetii clasificarii
plot(silhouette(cutree(hc.a, 3), distance))
plot(silhouette(cutree(hc.c, 3), distance))
plot(silhouette(cutree(hc.w, 3), distance))

#metoda lui Ward asigura o clasificare ai buna, indicatorul avand valori mai ridicate pentru toate obiectele clasificate 
#nefiind negativ pentru niciuna dintre variabile
#totusi, valorile sunt foarte mici

#reprezentarea grafica a variantei in interiorul grupelor
wss <- (nrow(b_std)-1)*sum(apply(b_std,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(b_std, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Nr. de clustere", ylab="varianta din interiorul grupelor")
#PANA AICI METODE IERARHICE

k.c <- kmeans(b_std, 3)  
k.c

fviz_cluster(k.c, data = b_std)

#dimensiunea clusterelor
k.c$size

#alocarea obiectelor in clase
k.c$cluster

#centroizii
k.c$centers

# plot clusters
plot(b, col = k.c$cluster)

#variante totala
k.c$totss
#in interiorul grupelor
k.c$withinss
k.c$tot.withinss
# intre grupe
k.c$betweenss

table(member.w, k.c$cluster)


# Analiza discriminata liniara (LDA)
#### presupunem ca apartenenta reala la grupe este cea oferita de analiza cluster ierarhica, metoda lui Ward
c <- data.frame( member.w,a[complete.cases(a),])
#am stocat apartenenta la grupe in prima coloana din data frame-ul c
View(c)
require(MASS)
###aplicarea LDA
c.lda<-lda(member.w~ Price+Change+VolumeMil+MkCapBil+Ratio+ProfitMg+OperatingCF+RevenueBil+GssProfitBil+ROA+ROE,data=c)
c.lda
Price.aov<-aov(Price ~ member.w, data=c)
summary(Price.aov)

Change.aov<-aov(Change ~ member.w, data=c)
summary(Change.aov)

VolumeMil.aov<-aov(VolumeMil ~ member.w, data=c)
summary(VolumeMil.aov)

MkCapBil.aov<-aov(MkCapBil ~ member.w, data=c)
summary(MkCapBil.aov)

Ratio.aov<-aov(Ratio ~ member.w, data=c)
summary(Ratio.aov)

ProfitMg.aov<-aov(ProfitMg ~ member.w, data=c)
summary(ProfitMg.aov)

OperatingCF.aov<-aov(OperatingCF ~ member.w, data=c)
summary(OperatingCF.aov)

RevenueBil.aov<-aov(RevenueBil ~ member.w, data=c)
summary(RevenueBil.aov)

GssProfitBil.aov<-aov(GssProfitBil ~ member.w, data=c)
summary(GssProfitBil.aov)

ROA.aov<-aov(ROA ~ member.w, data=c)
summary(ROA.aov)

ROE.aov<-aov(ROE ~ member.w, data=c)
summary(ROE.aov)

c[,c(3:12)]
# predictia apartenentei la clase
c.lda.p <- predict(c.lda,newdata=c[,c(3:13)])$class
c.lda.p 

table(c.lda.p, c[,1])
# pe baza tabelului se poate calcula abilitatea predictiva a clasificatorului (suma elementelor de pe diagona/nr total de elemente)


# pentru a vedea cum se comporta atunci cand apare un obiect nou, putem realiza predictia cu optiunea leave one out (CV=TRUE)
c.lda2<-lda(member.w~ Price+Change+VolumeMil+MkCapBil+Ratio+ProfitMg+OperatingCF+RevenueBil+GssProfitBil+ROA+ROE,
                data=c,
                CV=TRUE)
c.lda2
#si apoi calculata abilitatea predictiva
table(c.lda2$class, c[,1])
