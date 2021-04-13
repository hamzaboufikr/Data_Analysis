library(FactoMineR)
library("datasets")
CO2=datasets::CO2
CO2
as.data.frame(CO2)
res.mca <- MCA(CO2, 
               quanti.sup = 4:5, # Variables quantitatives supplémentaire
               quali.sup = 2:3, # Variables quantitatives supplémentaire
               graph=FALSE)
res.mca
eig.val <- res.mca$eig
eig.val
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")
plot(res.mca, autoLab = "yes")
plot(res.mca,
     invisible = c("var", "quali.sup", "quanti.sup"),
     cex = 0.8,                                    
     autoLab = "yes")
plot(res.mca, 
     invisible = c("ind", "quali.sup", "quanti.sup"),
     cex = 0.8,
     autoLab = "yes")
plotellipses(res.mca, keepvar = c("Type", "Treatment"))
# Valeurs propres
res.mca$eig
# Résultats des variables actives
res.var <- res.mca$var
res.var$coord          # Coordonnées
res.var$contrib        # Contributions 
res.var$cos2           # Qualité de représentation 
# Résultats des variables quantitatives supplémentaires
res.mca$quali.sup
# Résultats des individus actifs
res.ind <- res.mca$var
res.ind$coord          # Coordonnées
res.ind$contrib        # Contributions 
res.ind$cos2           # Qualités de représentation
