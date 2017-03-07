options(stringsAsFactors = FALSE)

# tabagisme
set.seed(783235)

Sexe <- factor(rep(c("F","M"), each=150))
Age <- round(runif(n=300, min=18, max=75))
Age.gr <- cut(Age,
              breaks = c(18,35,51,71,Inf),
              labels = c("18-34",
                         "35-50",
                         "51-70",
                         "71 +"),
              include.lowest = TRUE,
              right = FALSE)
fumeur.tmp <- sample(c(1,2), size = 300, prob = c(.35,.65), replace = TRUE)
Fumeur <- factor(fumeur.tmp, labels = c("Oui", "Non"))
Smoker <- factor(fumeur.tmp, labels = c("Yes", "No"))
IMC <- numeric(300)
IMC[1:150] <- rnorm(n = 150, mean = 19, sd = 4.8) + sqrt(Age[1:150]) # Femmes
IMC[151:300] <- rnorm(n = 150, mean = 18.5, sd = 3.7) + sqrt(Age[151:300]) # Hommes
prob.malade <- (Age^3 +  IMC^3 + (Fumeur=="Oui")*100000) # Formule improvisée!
# On ramène le tout sur une échelle de probabilités raisonnable (~0 à 40%)
prob.malade <- prob.malade/(max(prob.malade)*2.5)
malade.tmp <- 2 - rbinom(n = 300, size=1, prob = prob.malade)
Malade <- factor(malade.tmp,
                 labels = c("Oui", "Non"))
Diseased <- factor(malade.tmp,
                   labels = c("Yes", "No"))
tabagisme <- data.frame(Sexe, Age, Age.gr, IMC, Fumeur, Malade)
tobacco <- data.frame(Gender=Sexe, Age, Age.gr, BMI=IMC, Smoker, Diseased)
rm(Age, Age.gr, Fumeur, Smoker, IMC, Malade, Diseased, Sexe, fumeur.tmp, malade.tmp, prob.malade)

# View(tabagisme)
is.na(tabagisme) <- matrix(data = sample(c(TRUE,FALSE), size = 1800, replace = TRUE, prob = c(0.05, 0.95)), nrow = 300)
is.na(tobacco) <- is.na(tabagisme)
freq(is.na(tobacco))
ctable(tobacco$Gender, tobacco$Smoker, prop = "r")
write.table(tabagisme, "data/tabagisme.txt", sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
write.table(tobacco, "data/tobacco.txt", sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)


# Examens
set.seed(83459)

moyennes <- round(rnorm(n = 30, mean = 78, sd = 6.5), 1)
range(moyennes)

ecart.types <- round(rnorm(n = 30, mean = 4, sd = 1.25), 1)
range(ecart.types)

moyennes.langues <- round(rnorm(n = 30, mean = moyennes, sd = ecart.types), 1)
range(moyennes.langues)

moyennes.geo.hist <- round(rnorm(n = 30, mean = moyennes, sd = ecart.types), 1)
range(moyennes.geo.hist)

francais <- round(rnorm(n = 30, mean = moyennes.langues, sd = ecart.types), 1)
range(francais)

anglais <- round(rnorm(n = 30, mean = moyennes.langues, sd = ecart.types), 1)
range(anglais)

geographie <- round(rnorm(n = 30, mean = moyennes.geo.hist, sd = ecart.types) - 2, 1)
range(geographie)

histoire <- round(rnorm(n = 30, mean = moyennes.geo.hist, sd = ecart.types), 1)
range(histoire)

maths <- round(rnorm(n = 30, mean = moyennes, sd = ecart.types), 1)
range(maths)

economie <- round(rnorm(n = 30, mean = moyennes, sd = ecart.types), 1)
range(economie)

examens <- data.frame(francais, maths, geographie, histoire, economie, anglais)
exams <- data.frame(english=anglais, math=maths, geography=geographie,
                    history=histoire, economics=economie, french=francais)
#View(examens)

write.table(examens, file = "data/examens.txt", sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
write.table(exams, file = "data/exams.txt", sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)


