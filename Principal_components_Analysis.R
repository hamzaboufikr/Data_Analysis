getwd()
Voitures=read.csv("Voitures.csv")
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(corrplot)
#Tracer les boites à moustache
B= Voitures[,2:8]
B
as.matrix(B)
summary(B)
boxplot(B[,2:7])
#la fonction PCA() normalise (c-à-d centre et réduit)*
  #la matrice des données automatiquement pendant l'ACP.           
  #donc ç'est pas la peine de faire cette transformation     
  #avant la réalisation de l'ACP.                                
n=PCA(B,scale.unit=TRUE,quali.sup=1,ncp=5,graph=TRUE)
n
#Les valeurs propres de la matrice R sont données par:                                        
eig.val
fviz_eig(n)
fviz_pca_var(n)
#Visualiser les contributions des variables sur les deux premières composantes principales
fviz_contrib(n, choice = "var", axes = 1, top = 5)
#Colorer les variables en fonction de la qualité de représentation
fviz_pca_var(n, col.var = "cos2", 
             gradient.cols = c("green", "red"),
             repel = TRUE)
#Colorer les variables en fonction des contributions
fviz_pca_var(n, col.var = "contrib",
             gradient.cols = c("green", "red"),
             repel = TRUE)
#Critère du coude (éboulis des valeurs propres):
# On sélectionne les axes avant le décrochement:
coude=fviz_eig(n, choice = c("eigenvalue"),
               barfill = "red",  barcolor = "green",
               linecolor = "blue",
               ncp = 5,
               addlabels = TRUE,
               main = "Eboulis des valeurs propres")
coude
#====> Le nombre de composantes à retenir est deux.

#*****************Etude des variables*********************#
#********************************************************#
variables=get_pca_var(n)
variables$coord #les coordonnées des variables sur les composantes principales
variables$cor#corrélations entre les variables et les composantes
variables$cos2  #qualité de représentation des variables sur chaque composante
variables$contrib #contributions des variables aux composantes principales (en %)
eig.val=get_eigenvalue(n) #Pour obtenir les valeurs propres (de la matrice R)
par(mfrow=c(1,3))
corrplot(variables$cor, is.corr=TRUE)
corrplot(variables$cos2, is.corr=FALSE)
corrplot(variables$contrib, is.corr=FALSE)
#Calcul de la qualité de représentation des variables sur 
#les deux premières composantes qui est égale
Q.cylindre=sum(variables$cos2[1,1:2])
Q.cylindre
Q.puissance=sum(variables$cos2[2,1:2])
Q.puissance
Q.vitesse=sum(variables$cos2[3,1:2])
Q.vitesse
Q.poid=sum(variables$cos2[4,1:2])
Q.poid
Q.largeur=sum(variables$cos2[5,1:2])
Q.largeur
#On peut visualiser ces qualités de représentation sur 
#les deux premières composantes principales avec la commande:

Qu_2_axes=fviz_cos2(n, choice = "var", axes =1:2,
                    fill = "green")
Qu_2_axes
#Visualiser les contributions des variables à la première 
#composante principale
fviz_contrib(n, choice = "var", axes = 1, top = 5)
#Visualiser les contributions des variables à la deuxième 
#composante principale
fviz_contrib(n, choice = "var", axes = 2, top = 5)
#Visualiser les contributions des variables sur les deux
#premières composantes principales
fviz_contrib(n, choice = "var", axes = 1:2, top = 5)
#Colorer les variables en fonction de la qualité
#de représentation
fviz_pca_var(n, col.var = "cos2", 
             gradient.cols = c("green", "red"),
             repel = TRUE)

#Colorer les variables en fonction des contributions
fviz_pca_var(n, col.var = "contrib",
             gradient.cols = c("green", "red"),
             repel = TRUE)

#*************Etude des voitures**********************                 *
#********************************************************

#Carte des voitures:
fviz_pca_ind(n, col.var = "black")
#Les résultats de l'ACP concernant les voiture sont 
#obtenus à l’aide de la fonction "get_pca_ind()"
voitures=get_pca_ind(n)
voitures
#Coordonnées des voitures sur les composantes principales

voitures$coord       #round(voitures$coord,2)

#Qualité de représentation des voitures

voitures$cos2   

#Contribution des voitures aux composantes principales (en %)

voitures$contrib     #round(voitures$contrib,3) 
#colorer la carte des voitures en fonction de la qualité
#de représentation

fviz_pca_ind (n, col.ind = "cos2",
              gradient.cols = c("green", "blue", "red"),
              repel = TRUE )
#Représentation simultanée des variables et des voitures

fviz_pca_biplot(n, repel = TRUE,
                col.var = "red",  
                col.ind = "blue"  
)
