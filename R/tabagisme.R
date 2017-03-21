#' Usage du Tabac et &Eacute;tat de Sant&eacte; (Donn&eacute;es simul&eacute;es)
#'
#' Jeu de données simulées de 1000 sujets, avec les
#' colonnes suivantes:
#' \itemize{
#'   \item sexe Variable catégorielle (facteur), 2 niveaux:
#'     \dQuote{F} et \dQuote{M}. Environ 500 chacun.
#'   \item age Numérique.
#'   \item age.gr Age regroupé en facteur (4 niveaux).
#'   \item IMC Indice de masse corporelle (numérique).
#'   \item fumeur Variable catégorielle à 2 niveaux
#'     (\dQuote{Oui} / \dQuote{Non}).
#'   \item cigs.par.jour Nombre de cigarettes fumées par jour
#'     (numérique).
#'   \item malade Variable catégorielle à 2 niveaux
#'     (\dQuote{Oui} / \dQuote{Non}).
#'   \item maladie Champs texte.
#'   \item ponderation Poids échantillonal (numérique).
#' }
#'
#' Note sur la simulation des données: la probabilité pour
#' un sujet de tomber dans la catégorie \dQuote{malade} est
#' basée sur une fonction arbitraire faisant intervenir l'âge,
#' l'IMC et le nombre de cigarettes fumées par jour.
#'
#' @docType data
#' @keywords datasets
#' @name tabagisme
#' @usage data(tabagisme)
#' @format Un data frame de 1000 rangées et 9 colonnes
"tabagisme"
