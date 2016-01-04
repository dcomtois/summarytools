by.st <- function(data, INDICES, FUN, ..., simplify = TRUE) {
  dd <- as.data.frame(data)
  #if (length(dim(data)))
  #  by(dd, INDICES, FUN, ..., simplify = simplify)
  #else {
  if (!is.list(INDICES)) {
    IND <- vector("list", 1L)
    IND[[1L]] <- INDICES
    names(IND) <- deparse(substitute(INDICES))[1L]
  }
  else
    IND <- INDICES
  by.call = as.character(match.call()[2])
  # TODO: créer une colonne qui indique le groupe, qui peut être formé d'une combinaison de facteurs
  #FUNx <- function(x) FUN(dd, by.call, ...)
  #nd <- nrow(dd)
  structure(eval(substitute(tapply(seq_len(nd), IND, FUNx, by.call=by.call,
                                   simplify = simplify)), dd),
            call = match.call(),
            class = c("summarytools", "by"))
  #}
}
