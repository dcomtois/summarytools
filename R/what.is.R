what.is <- function(x, show.all=FALSE, ignore.size.warn=FALSE) {

  if(object.size(x) > 10000 && ignore.size.warn == FALSE) {
    stop("object.size(x) is greater than 10K; computing time might be long. Set argument ignore.size.warn to TRUE to force execution anyway")
  }
  # set the warn option to -1 to temporarily ignore warnings
  op <- options("warn")
  options(warn = -1)
  on.exit(options(op))

  # Part 1. Data Properties - class, typeof, mode, storage.mode
  obj.properties <- data.frame(property=c("class", "typeof", "mode", "storage.mode", "dim", "length", "object.size"),
                               value = c(paste(class(x),collapse=" "), typeof(x), mode(x), storage.mode(x),
                                         paste(dim(x), collapse = " x "), length(x), paste(object.size(x), "Bytes")))


  # Part 2. Test object against all "is.[...]" functions
  # Look for all relevant functions
  list.id.fun <- grep(methods(is), pattern = "<-", invert = TRUE, value = TRUE)

  # remove is.R which is not relevant and can take a lot of time
  list.id.fun <- setdiff(list.id.fun, "is.R")

  # loop over all functions with x as argument, and store the results
  if(!show.all.tests) {
    identifiers <- c()
    for(fun in list.id.fun) {
      res <- try(eval(call(fun,x)),silent=TRUE)
      if(isTRUE(res))
        identifiers <- append(identifiers, fun)
    }
  } else {

    # Generate table of all identifier tests
    identifiers <- data.frame(test=character(), value=character(),
                                   warnings=character(), stringsAsFactors = FALSE)

    # loop over all functions with x as argument, and store the results
    for(fun in list.id.fun) {
      value <- try(eval(call(fun,x)),silent=TRUE)
      if(class(value)=="try-error") {
        next() # ignore tests that yield an error
      } else if (length(value)>1) {
        warn <- "!!! Logical value applies only to the first element of the provided object !!!"
        value <- paste(value,sep="")
      } else {
        warn <- ""
      }
      identifiers[nrow(identifiers)+1,] <- list(fun, value, warn)
    }

    # sort the results according to the results and warnings if any
    identifiers <- identifiers[order(identifiers$value, identifiers$warnings=="", decreasing = TRUE),]
  }

  # Part 3. Make a list of all x's attribute and their length
  obj.attributes <- sapply(attributes(x),length)


  output <- list()
  output$obj.properties <- obj.properties
  output$identifiers <- identifiers
  output$attributes <- obj.attributes
  return(output)

}
