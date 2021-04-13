
#*****************************************************
#*****************************************************
#**                       TP                        **
#**     Classification Ascendante Hiérarchique CAH  **
#**         avec hclust() de package "stats"        **
#**                                                 **
#*****************************************************
#*****************************************************


#library(stats)
#library("factoextra")

#*****************************************************


#Matrcie des données

#On considère l'exemple simple du cours:

ligne=c("I1", "I2","I3","I4","I5")
ligne

colonne=c("X1","X2")
colonne

X=matrix(c(2, 2,7.5, 4,3, 3,0.5, 5,6, 4),5 ,2,byrow=F,
dimnames=list(ligne,colonne))

X

#Matrice des distances euclidienne entre les individus

distance = dist(X, method = "euclidean")

distance


#CAH avec la méthode d'agrégation de "Ward"

CAH <- hclust(distance, method="ward.D2")

CAH

summary(CAH)

names(CAH)

#Affichage de dendrogramme

plot(CAH)

# En se basant sur le dendrogramme, on peut sugérer un 
# découpage en 2 groupes.

#Découpage de dendrogramme en 2 groupes

rect.hclust(CAH, k=2)

classes=cutree(CAH,k=2)
classes

#Liste des classes

print(sort(classes))

# Détails sur CAH

summary(CAH)

CAH$merge
CAH$order
CAH$labels
CAH$method
CAH$call
CAH$dist.method


# Distances de Ward à chaque agrégation

dist.ward=(CAH$height^2)/2
dist.ward

# Calcul de l'inertie intra classe à chaque agrégation


n=5

Intra.P5=0
Intra.P5


Intra.P4=dist.ward[1]/n
Intra.P4

Intra.P3= Intra.P4 + dist.ward[2]/n
Intra.P3

Intra.P2= Intra.P3 + dist.ward[3]/n
Intra.P2

Intra.P1= Intra.P2 + dist.ward[4]/n
Intra.P1

#Vérification: On sait que Inertie Totale=(1/n)*sum(d^2(Ii, g))

#Les cordonnées de g sont données par:

g=c(sum(X[,1])/n, sum(X[,2])/n)
g

Y=rbind(X,g)
Y

d=dist(Y)
d

d=as.matrix(d)
d

# Donc l'inertie totale est:

I.total=(1/n)*sum(d[6,]^2)
I.total

# Caractériser les classes

# Première méthode
C1=X[-c(2,5),]
C1

C2=X[-c(1,3,4),]
C2


par(mfrow=c(1,2))
boxplot(C1,ylim=c(1,8))
boxplot(C2,ylim=c(1,8))

# Deuxième méthode

par(mfrow=c(1,2))
boxplot(X[,1]~classes,ylim=c(1,8))
boxplot(X[,2]~classes,ylim=c(1,8))


# Qualité de la typologie en k classes

#  Q.T=[I_totale - I_intra(Pk)]/I_totale
 
n=5

Q.T5=100*(I.total-Intra.P5)/I.total     #k=5 classes 
Q.T5

Q.T4=100*(I.total-Intra.P4)/I.total     #k=4 classes 
Q.T4

Q.T3=100*(I.total-Intra.P3)/I.total     #k=3 classes 
Q.T3

Q.T2=100*(I.total-Intra.P2)/I.total     #k=2 classes 
Q.T2

Q.T1=100*(I.total-Intra.P1)/I.total     #k=1 classe  
Q.T1



#****************************************************
#****************************************************
#**               Classification K-means           **
#****************************************************
#****************************************************




# On reprend l'exemple précéent, c-à-d la matrice X

X

#Classification K-means en K=2 classes avec la fonction "kmeans"

#Création des centres initiaux g10 et g20

g10=c(2,2)
g10

g20=c(6,4)
g20

#Réaliser la classification

?kmeans

km=kmeans (X,centers=rbind(g10, g20))
km

km$cluster       # Les classes obtenues
km$centers       # Les centrres de gravité de chaque classe
km$totss         # Somme des carrés totale
km$withinss      # Somme des carrés intra classes pour chaque groupe
km$betweenss     # Somme des carrés inter classes
km$tot.withinss  # Somme des carrés intra classes pour les duex groupes

#Remarque:***********************************************
#                                                       *
#Inertie Totale=Somme des carrés totale/n               *
#                                                       *
#De même pour Inertie Intra classe et Inter classe....  *
#                                                       *
#********************************************************

# Visualiser les classes avec les centres de gravité

library(factoextra)

fviz_cluster(km,data=X, main = "Les classes obtenues", stand=FALSE)




#******************************************************
#*                     FIN                            *
#******************************************************



