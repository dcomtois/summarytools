options(stringsAsFactors = FALSE)

# tabagisme
set.seed(783235)

sexe <- factor(rep(c("F","M"), each=150))
age <- round(runif(n=300, min=18, max=75))
age.gr <- cut(age,
              breaks = c(18,35,51,71,Inf),
              labels = c("18-34",
                         "35-50",
                         "51-70",
                         "71 +"),
              include.lowest = TRUE,
              right = FALSE)
fumeur.tmp <- rbinom(n = 300, size = 1, prob = .35)
fumeur <- factor(fumeur.tmp, labels = c("Non fumeur", "Fumeur"))
smoker <- factor(fumeur.tmp, labels = c("Non-smoker", "Smoker"))
IMC <- numeric(300)
IMC[1:150] <- rnorm(n = 150, mean = 19, sd = 4.8) + sqrt(age[1:150]) # Femmes
IMC[151:300] <- rnorm(n = 150, mean = 18.5, sd = 3.7) + sqrt(age[151:300]) # Hommes
prob.malade <- (age^3 +  IMC^3 + (fumeur=="Fumeur")*100000) # Formule improvisée!
# On ramène le tout sur une échelle de probabilités raisonnable (~0 à 40%)
prob.malade <- prob.malade/(max(prob.malade)*2.5)
malade.tmp <- rbinom(n = 300, size=1, prob = prob.malade)
malade <- factor(malade.tmp,
                 labels = c("En sante", "Malade"))
diseased <- factor(malade.tmp,
                   labels = c("Healthy", "Diseased"))
tabagisme <- data.frame(sexe, age, age.gr, IMC, fumeur, malade)
tobacco <- data.frame(gender=sexe, age, age.gr, BMI=IMC, smoker=smoker, diseased=diseased)
rm(age, age.gr, fumeur, IMC, malade, sexe)

# View(tabagisme)

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


