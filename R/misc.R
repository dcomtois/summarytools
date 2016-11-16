# .parse.arg ####################################################################
#
# This function takes a string referring to existing data and parses it
# to get information on the data structure.
#
# info returned: df.name, var.names, rows.subset, var.indexes, data.struct
#
# Example:
#
# > .parse.arg("iris[1:200,c(1,4)]")
# $arg.str
# [1] "iris[1:200,c(1,4)]"
#
# $rows.subset
# [1] "1:200"
#
# $var.indexes
# [1] "c(1,4)"
#
# $df.name
# [1] "iris"
#
# $var.names
# [1] "Sepal.Length" "Petal.Width"

.parse.arg <- function(arg.str) {

  # Check if arg.str is a string
  if(!is.character(arg.str))
    stop("arg.str must be a string")

  # Initialise output list
  output <- list()
  output$arg.str <- arg.str

  # Recuperate the object designated by arg.str
  x <- try(eval(parse(text=arg.str)),silent = TRUE)
  if(inherits(x, "try-error")) {
    return(output)
  }

  if(!is.data.frame(x) && !is.atomic(x)) {
    return(output)
  }

  # Trim the string removing leading/trailing blanks
  arg.str <- gsub("^\\s+|\\s+$", "", arg.str)

  # Get rid of spaces next to brackets and next to comma in indexing brackets.
  # Note: that way assures us not to remove any spaces in quoted structures
  # such as ['var name']
  arg.str <- gsub("\\s*\\[\\s*","[", arg.str, perl=TRUE) # remove blanks near [
  arg.str <- gsub("\\s*\\]\\s*","]", arg.str, perl=TRUE) # remove blanks near ]

  # remove blanks around comma
  arg.str <- gsub("^(.*)(\\[\\d+:\\d+)?\\s?,\\s?(.+)$", "\\1\\2,\\3", arg.str, perl=TRUE)

  # Change [[]] to [] for the last pair of brackets; this simplifies the work
  arg.str <- sub("\\[{2}(.*)\\]{2}$", "[\\1]", arg.str, perl=TRUE)

  # Change references to data with ['name'] or [['name']] into $name, also to simplify matters
  re.brack <- '\\[{1,2}[\'\"]'
  if(grepl(re.brack, arg.str)) {
    arg.str <- gsub('\\[{1,2}[\'\"]', "$", arg.str, perl=TRUE)
    arg.str <- gsub('[\'\"]\\]{1,2}', "", arg.str, perl=TRUE)
  }

  # Isolate indexing in the last brackets
  re.index <- "(.*?)\\[(.*?)\\]$"

  if(grepl(re.index, arg.str)) {
    indexes <- sub(re.index, "\\2", arg.str, perl=TRUE)

    # Further decompose the indexes
    # indexing having 2 elements (rows, columns), will be identified by this regex
    # [1:10,] or [,"Species] will also match
    re.split.index <- "^(.+)?,+(c\\(.*\\)|\\d+|\\d+:\\d+|'.*'|\".+\")$"
    if(grepl(re.split.index, indexes, perl = TRUE)) {
      output$rows.subset <- sub(re.split.index, "\\1", indexes, perl=TRUE)
      output$var.indexes <- sub(re.split.index, "\\2", indexes, perl=TRUE)

      # Remove any empty string
      if(nchar(output$rows.subset) == 0)
        output$rows.subset <- NULL
      if(nchar(output$var.indexes) == 0)
        output$var.indexes <- NULL
    }

    # When previous regex does not match, it means the index has only 1 element,
    # either row or column.
    # When indexing starts with a comma:
    else if(substring(indexes, 1, 1) == ",")
      output$var.indexes <- sub("^,", "", indexes, perl = TRUE)
    # When indexing ends with a comma:
    else if(substring(indexes, nchar(indexes), nchar(indexes)) == ",")
      output$rows.subset <- sub(",$", "", indexes, perl = TRUE)

    # When there is no comma, we'll check if x is a dataframe or not.
    # If it is, the index refers to columns, and otherwise, to rows
    else {
      # first we need to reevaluate the arg.str
      x.tmp <- eval(parse(text = arg.str))
      if(is.data.frame(x.tmp))
        output$var.indexes <- indexes
      else
        output$rows.subset <- indexes
    }

    # Update the string to remove what's already accounted for
    arg.str <- sub(re.index, "\\1", arg.str, perl=TRUE)
  }

  # Split arg.str by "$" to identify structures
  output$data.struct <- strsplit(arg.str, "$", fixed = TRUE)[[1]]

  if(is.data.frame(x)) {
    # If x is a dataframe, we can set the var.names
    output$var.names <- colnames(x)

    # normally the last element in the data structures
    # should be the df name; unless it's nested in a list and referred to by [[n]]
    output$df.name <- tail(output$data.struct,1)
  }

  # Otherwise, depending on the situation, we'll try to get at the df name and its colnames
  else {
    # If vector is referred to via column indexing, recup the column's name
    # by an evaluation of the form df[var.indexes]
    if("var.indexes" %in% names(output)) {
      output$var.names <- eval(parse(text=paste("colnames(",arg.str,"[",output$var.indexes,"])")))
      output$df.name <- tail(output$data.struct,1)
    }

    # If there is no column indexing, it means the vector's name is in the
    # data.struc list, along with the df name one level higher, unless the vector
    # was "standalone"
    else {
      output$var.names <- tail(output$data.struct,1)
      if(length(output$data.struct)>1)
        output$df.name <- output$data.struct[length(output$data.struct)-1]
    }
  }

  # remove last item from data.struct when it's the same as var.names to avoid redundancy
  output$data.struct <- setdiff(output$data.struct, output$var.names)

  # same with df.name and data.struct
  output$data.struct <- setdiff(output$data.struct, output$df.name)

  # cleanup
  if(length(output$data.struct)==0)
    output$data.struct <- NULL

  # Further validate the items to return;
  if(isTRUE(grepl('[\\(\\[]', output$df.name)))
    output$df.name <- NULL

  if(isTRUE(grepl('[\\(\\[]', output$var.names)))
    output$var.names <- NULL

  return(output)
}

# view is a wrapper function for print(x, "view"). Allows alternate "browser" or "pander" methods as well.
view <- function(x, method="viewer", ...) {
  print.summarytools(x, method=method, ...)
}


# by.st <- function(data, INDICES, FUN, ..., simplify = TRUE) {
#   dd <- as.data.frame(data)
#   #if (length(dim(data)))
#   #  by(dd, INDICES, FUN, ..., simplify = simplify)
#   #else {
#   if (!is.list(INDICES)) {
#     IND <- vector("list", 1L)
#     IND[[1L]] <- INDICES
#     names(IND) <- deparse(substitute(INDICES))[1L]
#   }
#   else
#     IND <- INDICES
#   by.call = as.character(match.call()[2])
#   # TODO: creer une colonne qui indique le groupe, qui peut etre forme d'une combinaison de facteurs
#   #FUNx <- function(x) FUN(dd, by.call, ...)
#   #nd <- nrow(dd)
#   structure(eval(substitute(tapply(seq_len(nd), IND, FUNx, by.call=by.call,
#                                    simplify = simplify)), dd),
#             call = match.call(),
#             class = c("summarytools", "by"))
#   #}
# }
