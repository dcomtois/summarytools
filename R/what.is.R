what.is <- function(x, show.all=FALSE, ignore.size.warn=FALSE) {

  if(!is.function(x) && object.size(x) > 20000 && ignore.size.warn == FALSE) {
    stop("object.size(x) is greater than 10K; computing time might be long. Set argument ignore.size.warn to TRUE to force execution anyway")
  }

  # set the warn option to -1 to temporarily ignore warnings
  op <- options("warn")
  options(warn = -1)
  on.exit(options(op))

  # Part 1. Data Properties - class, typeof, mode, storage.mode
  properties <- data.frame(property=c("class", "typeof", "mode", "storage.mode", "dim", "length",
                                      "is.object","object.type","object.size"),
                           value = c(paste(class(x),collapse=" "), typeof(x), mode(x), storage.mode(x),
                                     paste(dim(x), collapse = " x "), length(x),
                                     base::is.object(x), pryr::otype(x), paste(object.size(x), "Bytes")))


  # Part 2. Make a list of all x's attribute and their length
  attributes.lengths <- sapply(attributes(x),length)
  if(length(attributes.lengths)==0)
    attributes.lengths <- NULL


  # Part 3. Test object against all "is.[...]" functions
  # Look for all relevant functions
  list.id.fun <- grep(methods(is), pattern = "<-", invert = TRUE, value = TRUE)

  # remove is.R which is not relevant and can take a lot of time
  list.id.fun <- setdiff(list.id.fun, "is.R")

  # loop over all "is" functions with x as argument, and store the results
  if(!show.all) {
    extensive.is <- c()
    for(fun in list.id.fun) {
      res <- try(eval(call(fun,x)),silent=TRUE)
      if(isTRUE(res))
        extensive.is <- append(extensive.is, fun)
    }
  } else {

    # Generate table of all identifier tests
    extensive.is <- data.frame(test=character(), value=character(),
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
      extensive.is[nrow(extensive.is)+1,] <- list(fun, value, warn)
    }

    # sort the results according to the results and warnings if any
    extensive.is <- extensive.is[order(extensive.is$value, extensive.is$warnings=="", decreasing = TRUE),]
  }

  # Part 4. Get info on the type of object - S3, S4, attributes / slots

  function.type <- NULL
	generic.function.methods <- NULL
	obj.class.methods <- NULL
  #obj.is.object <- base::is.object(x)
  #obj.otype <- pryr::otype(x)

  if(is.function(x)) {

  	function.type <- pryr::ftype(x)
  	# if("generic" %in% function.type)
    #   generic.function.methods <- methods(deparse(substitute(x)))

  }

  # 	else {
  #
  #   	obj.class.methods <- lapply(class(x), function(cl) methods(class=cl))
  #   	names(obj.class.methods) <- class(x)
  #   }

  output <- list()
  output$properties <- properties
  output$attributes.lengths <- attributes.lengths
  output$extensive.is <- extensive.is
  output$function.type <- function.type
  #output$generic.function.methods <- generic.function.methods
  #output$is.object <- obj.is.object
  #output$object.type <- obj.otype
  #output$obj.class.methods <- obj.class.methods
  return(output)

}
