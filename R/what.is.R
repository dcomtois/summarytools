what.is <- function(x, show.all.tests=FALSE) {

  # set the warn option to -1 to temporarily ignore warnings
  op <- options("warn")
  options(warn = -1)
  on.exit(options(op))

  # Part 1. Data Properties - class, typeof, mode, storage.mode
  obj.properties <- data.frame(property=c("class", "typeof", "mode", "storage.mode", "dim", "length", "object.size"),
                               value = c(paste(class(x),collapse=" "), typeof(x), mode(x), storage.mode(x),
                                         paste(dim(x), collapse = " x "), length(x), paste(object.size(x), "Bytes")))


  # Part 2. Test object against all "is.[...]" functions
  # Find all 'is.' functions (excluding the assignment ones)
  list.identifier.functions <- grep(methods(is), pattern = "<-", invert = TRUE, value = TRUE)
  identifiers <- c()

  # loop over all "is.(...)" functions and store the results
  for(fun in list.identifier.functions) {
    res <- try(eval(call(fun,x)),silent=TRUE)
    if(isTRUE(res))
      identifiers <- append(identifiers, fun)
  }

  # Part 3. Make a list of all the object's attribute and their length
  obj.attributes <- sapply(attributes(x),length)


  if(show.all.tests) {

    # Generate table of all identifier tests
    identifiers.full <- data.frame(test=character(), value=character(),
                                   warnings=character(), stringsAsFactors = FALSE)

    # loop over all "is.(...)" functions and store the results
    for(fun in list.identifier.functions) {
      res <- try(eval(call(fun,x)),silent=TRUE)
      if(class(res)=="try-error") {
        next() # ignore tests that yield an error
      } else if (length(res)>1) {
        warn <- "*Applies only to the first element of the provided object"
        value <- paste(res,sep="")
        #value <- paste(res,"*",sep="")
      } else {
        warn <- ""
        value <- res
      }
      identifiers.full[nrow(identifiers.full)+1,] <- list(fun, value, warn)
    }

    # sort the results
    identifiers.full <- identifiers.full[order(identifiers.full$value, identifiers.full$warnings=="", decreasing = TRUE),]
  }

  output <- list()
  output$obj.properties <- obj.properties
  output$identifiers <- identifiers
  if(show.all.tests)
    output$identifiers.full <- identifiers.full
  output$attributes <- obj.attributes
  return(output)

}

