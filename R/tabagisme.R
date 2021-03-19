#' Usage du tabac et etat de sante (Donnees simulees)
#'
#' Jeu de donnees simulees de 1000 sujets, avec les
#' colonnes suivantes:
#' \itemize{
#'   \item sexe Variable categorielle (facteur), 2 niveaux:
#'     \dQuote{F} et \dQuote{M}. Environ 500 chacun.
#'   \item age Numerique.
#'   \item age.gr Groupe d'age - variable categorielle, 4 niveaux.
#'   \item IMC Indice de masse corporelle (numerique).
#'   \item fumeur Variable categorielle, 2 niveaux
#'     (\dQuote{Oui} / \dQuote{Non}).
#'   \item cigs.par.jour Nombre de cigarettes fumees par jour
#'     (numerique).
#'   \item malade Variable categorielle, 2 niveaux
#'     (\dQuote{Oui} / \dQuote{Non}).
#'   \item maladie Champs texte.
#'   \item ponderation Poids echantillonal (numerique).
#' }
#'
#' Note sur la simulation des donnees: la probabilite pour
#' un sujet de tomber dans la categorie \dQuote{malade} est
#' basee sur une fonction arbitraire faisant intervenir l'age,
#' l'IMC et le nombre de cigarettes fumees par jour.
#'
#' A copy of this dataset is \strong{available in English} under the name
#' \dQuote{tobacco}.
#'
#' @docType data
#' @keywords datasets
#' @name tabagisme
#' @usage data(tabagisme)
#' @format Un data frame de 1000 rangees et 9 colonnes
NULL
