# Génération de données hypothéthiques sur les fumeurs
# Le jeu de données final aura 300 sujets, 50%-50% Hommes/Femmes,
# 35% de fumeurs, un IMC généré aléatoirement, et un indicateur
# "maladie" dont la probabilité dépend de l'imc, de l'âge et du
# statut tabagique

options(stringsAsFactors = FALSE)
set.seed(783)

# ---------------------------------------------------------------- #
# --- 1. Création des variables et stats univariées --------------
# ---------------------------------------------------------------- #

# --- 1.1 facteur sexe --------------------------------------------

sexe <- factor(rep(c("Femme","Homme"), each=150))
table(sexe)

# --- 1.2 Vecteur age, facteur age.gr  ----------------------------

age <- round(runif(n=300, min=18, max=75))
mean(age)
sd(age)
plot(age) # dans ce cas-ci, plot() n'est pas d'un grand secours
hist(age) # on utilise plutôt hist()
hist(age, breaks = c(18,25,35,45,55,65,75))
hist(age, breaks = 25) # breaks ne respecte pas nécessairement
                       # le nombre de barres!


# Diviser les âges en groupes (facteur age.gr)
age.gr <- cut(age,
              breaks = c(18,35,51,71,Inf),
              labels = c("18-34 ans",
                         "35-50 ans",
                         "51-70 ans",
                         "71 ans +"),
              include.lowest = TRUE,
              right = FALSE)

# vérification (trier par age dans le Viewer)
View(data.frame(a=age, f=age.gr))
data.frame(a=age, f=age.gr)[order(age),]


# --- 1.3 Facteur fumeur (binaire) ----------------------------------

fumeur <- rbinom(n = 300, size = 1, prob = .35)

# table de fréquences
table(fumeur)

# Proportion de fumeurs
sum(fumeur)/length(fumeur)
prop.table(table(fumeur))

# Conversion en facteur
fumeur <- factor(fumeur, labels = c("Non fumeur", "Fumeur"))
prop.table(table(fumeur))

# Graphiques
plot(fumeur)
barplot(table(fumeur))  # équivalent
barplot(table(fumeur), ylim=c(0,250)) # Ajustement de l'axe des y

# Pour obtenir les proportions au lieu des fréquences
barplot(prop.table(table(fumeur)),
        ylim = c(0,0.8),
        main="Proportion de fumeurs / non fumeurs",
        ylab = "Proportion")


# --- 1.4 Vecteur IMC  ----------------------------------------------

# Générer un indice de masse corporelle qui tienne compte de l'âge
IMC <- numeric(300)
IMC[1:150] <- rnorm(n = 150, mean = 19, sd = 4.8) + sqrt(age[1:150]) # Femmes
IMC[151:300] <- rnorm(n = 150, mean = 18.5, sd = 3.7) + sqrt(age[151:300]) # Hommes
hist(IMC, breaks = 20)


# --- 1.5 Vecteur de "probabilités" de maladie ----------------------

prob.malade <- (age^3 +  IMC^3 + (fumeur=="Fumeur")*100000) # Formule improvisée!
range(prob.malade)

# On ramène le tout sur une échelle de probabilités raisonnable (~0 à 40%)
prob.malade <- prob.malade/(max(prob.malade)*2.5)
range(prob.malade)

# Graphique
hist(prob.malade, breaks=15)


# --- 1.6 Vecteur malade selon probabilités calculées ---------------

malade <- factor(rbinom(n = 300, size=1, prob = prob.malade),
                 labels = c("En santé", "Malade"))
table(malade)
prop.table(table(malade))



# ---------------------------------------------------------------- #
# ---- 2. Examen des relations entre les variables ---------------
# ---------------------------------------------------------------- #

# --- 2.1 Création d'un dataframe ---------------------------------

tabagisme <- data.frame(sexe, age, age.gr, IMC, fumeur, malade)
rm(age, age.gr, fumeur, IMC, malade, sexe)

# ---- 2.2 Vue d'ensemble - graphique de toutes les variables -----

plot(tabagisme)

# ---- 2.3 Deux variables continues: IMC et age -------------------

# Graphique - Nuage de points
plot(IMC ~ age, data=tabagisme, main="Âge et IMC", xlab="Âge")

# Corrélation
cor(x=tabagisme$age, y=tabagisme$IMC)
cor.test(x = tabagisme$age, y=tabagisme$IMC)

# Régression linéaire simple
reg.imc.age <- lm(IMC ~ age, data=tabagisme)
reg.imc.age # a l'apparence d'un objet anodin... mais en y regardant de plus près:
typeof(reg.imc.age)
attributes(reg.imc.age)

# summary affiche bcp plus de détails (extraits de la liste reg.imc.age)
summary(reg.imc.age)

# calculer les i.c.
confint(reg.imc.age)

# plot() permet de faire des vérifications "diagnostiques"
plot(reg.imc.age)

# Vérifier si les valeurs résiduelles sont +/- normalement distribuées
hist(reg.imc.age$residuals)

# Ajout d'une droite de régression au nuage de points
plot(IMC ~ age, data=tabagisme, main="Âge et IMC", xlab="Âge")
abline(a = reg.imc.age$coefficients[1],
       b = reg.imc.age$coefficients[2],
       col="red")



# Une variable numérique et une variable catégorielle
# ---------------------------------------------------
plot(IMC ~ age.gr, data=tabagisme, xlab="Groupe d'âge", main="IMC par groupe d'âge")

aggregate(IMC ~ age.gr, data=tabagisme, FUN = mean)
aggregate(IMC ~ age.gr, data=tabagisme, FUN = function(x) {c(moy=mean(x), é.t.=sd(x))})

by(data = tabagisme$IMC, FUN = psych::describe, INDICES = tabagisme$age.gr)
by(data = tabagisme$IMC, FUN = Hmisc::describe, INDICES = tabagisme$age.gr)
by(data = tabagisme$IMC, FUN = fivenum, INDICES = tabagisme$age.gr)
by(data = tabagisme$IMC, FUN = summarytools::descr, INDICES = tabagisme$age.gr)


reg <- lm(IMC ~ age.gr, data=tabagisme)
summary(reg)
anova(reg)
plot(reg)

# Deux variables catégorielles
# ----------------------------

# Tableau croisé entre fumeur et malade
(tab <- table(tabagisme$fumeur,tabagisme$malade))

# Avec pourcentages, par rangée (margin=1)
prop.table(tab, margin = 1)

# CrossTable, calqué sur Proc Freq
gmodels::CrossTable(tabagisme$fumeur, tabagisme$malade, format = "SAS")

# Epitable et epitab (pour odds ratios)
epitools::epitable(x=tabagisme$fumeur, y=tabagisme$malade)

epitools::epitab(x=tabagisme$fumeur, y=tabagisme$malade)
epitools::oddsratio(x=tabagisme$fumeur, y=tabagisme$malade)

# Graphiques
plot(tabagisme$malade ~ tabagisme$fumeur, xlab="Statut tabagique",
     ylab="État de santé", main="Tabagisme et état de santé")

plot(tabagisme$malade ~ tabagisme$age.gr,
     ylab="État de santé", main="Age et état de santé")

# Inversion de l'ordre des termes de l'équation
plot(tabagisme$age.gr ~ tabagisme$malade,
     ylab="État de santé", main="Age et état de santé")

# inverser l'ordre des catégories pour avoir le % de malades

plot(factor(2 - as.numeric(tabagisme$malade), labels = c("Malade", "En santé")) ~ tabagisme$fumeur,
     xlab="Statut tabagique",
     ylab="État de santé", main="Tabagisme et état de santé")


plot(tabagisme$malade ~ tabagisme$age.gr,
     xlab="Âge",
     ylab="État de santé",
     main="État de santé par groupes d'âge")


# ---------------------
# Régression logistique
# ---------------------
rlog <- glm(malade ~ age + sexe + IMC + fumeur, data=tabagisme, family = binomial(logit))
resume <- summary(rlog)
c.i <- confint(rlog)

# Ajouter intervales de confiance aux résultats fournis par summary()
cbind(resume$coefficients, c.i)

# graphiques diagnostiques
plot(rlog)

