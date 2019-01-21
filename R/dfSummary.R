#' Data frame Summary
#'
#' Summary of a data frame consisting of: variable names, labels if any, factor
#' levels, frequencies and/or numerical summary statistics, and valid/missing
#' observation counts.
#'
#' @param x A data frame.
#' @param round.digits Number of significant digits to display. Defaults to
#'   \code{2} and can be set globally; see \code{\link{st_options}}.
#' @param varnumbers Logical. Should the first column contain variable number?
#'   Defaults to \code{TRUE}. Can be set globally; see \code{\link{st_options}},
#'   option \dQuote{dfSummary.varnumbers}.
#' @param labels.col Logical. If \code{TRUE}, variable labels (as defined with
#'   \pkg{rapportools}, \pkg{Hmisc} or \pkg{summarytools}' \code{label}
#'   functions) will be displayed. \code{TRUE} by default, but the \emph{labels}
#'   column is only shown if at least one column has a defined label. This
#'   option can also be set globally; see \code{\link{st_options}}, option 
#'   \dQuote{dfSummary.labels.col}.
#' @param valid.col Logical. Include column indicating count and proportion of
#'   valid (non-missing) values. \code{TRUE} by default, but can be set
#'   globally; see \code{\link{st_options}}, option
#'   \dQuote{dfSummary.valid.col}.
#' @param na.col Logical. Include column indicating count and proportion of
#'   missing (NA) values. \code{TRUE} by default, but can be set globally; see
#'   \code{\link{st_options}}, option \dQuote{dfSummary.na.col}.
#' @param graph.col Logical. Display barplots / histograms column in \emph{html}
#'   reports. \code{TRUE} by default, but can be set globally; see
#'   \code{\link{st_options}}, option \dQuote{dfSummary.graph.col}.
#' @param graph.magnif Numeric. Magnification factor, useful if the graphs show
#'   up too large (then use a value < 1) or too small (use a value > 1). Must be
#'   positive. Default to \code{1}. Can be set globally; see
#'   \code{\link{st_options}}, option \dQuote{dfSummary.graph.magnif}.
#' @param style Style to be used by \code{\link[pander]{pander}} when rendering
#'   output table. Defaults to \dQuote{multiline}. The only other valid option
#'   is \dQuote{grid}. Style \dQuote{simple} is not supported for this
#'   particular function, and \dQuote{rmarkdown} will fallback to
#'   \dQuote{multiline}.
#' @param plain.ascii Logical. \code{\link[pander]{pander}} argument; when
#'   \code{TRUE}, no markup characters will be used (useful when printing to
#'   console). Defaults to \code{TRUE}. Set to \code{FALSE} when in context of
#'   markdown rendering. To change the default value globally, see
#'   \code{\link{st_options}}.
#' @param justify String indicating alignment of columns; one of \dQuote{l}
#'   (left) \dQuote{c} (center), or \dQuote{r} (right). Defaults to \dQuote{l}.
#' @param col.widths Numeric or character. Vector of column widths. If numeric,
#'   values are assumed to be numbers of pixels. Otherwise, any CSS-supported
#'   units can be used. \code{NA} by default, meaning widths are calculated 
#'   automatically.
#' @param headings Logical. Set to \code{FALSE} to omit headings. To change this
#'   default value globally, see \code{\link{st_options}}.
#' @param display.labels Logical. Should data frame label be displayed in the
#'   title section?  Default is \code{TRUE}. To change this default value
#'   globally, see \code{\link{st_options}}.
#' @param max.distinct.values The maximum number of values to display
#'   frequencies for. If variable has more distinct values than this number, the
#'   remaining frequencies will be reported as a whole, along with the number of
#'   additional distinct values. Defaults to 10.
#' @param trim.strings Logical; for character variables, should leading and
#'   trailing white space be removed? Defaults to \code{FALSE}. See
#'   \emph{details} section.
#' @param max.string.width Limits the number of characters to display in the
#'   frequency tables. Defaults to \code{25}.
#' @param split.cells A numeric argument passed to \code{\link[pander]{pander}}.
#'   It is the number of characters allowed on a line before splitting the cell.
#'   Defaults to \code{40}.
#' @param split.tables \pkg{pander} argument which determines the maximum width
#'   of a table. Keeping the default value (\code{Inf}) is recommended.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}}.
#'
#' @return A data frame with additional class \code{summarytools} containing as
#'   many rows as there are columns in \code{x}, with attributes to inform
#'   \code{print} method. Columns in the output data frame are:
#'   \describe{
#'     \item{No}{Number indicating the order in which column appears in the data
#'      frame.}
#'     \item{Variable}{Name of the variable, along with its class(es).}
#'     \item{Label}{Label of the variable (if applicable).}
#'     \item{Stats / Values}{For factors, a list of their values, limited by the
#'       \code{max.distinct.values} parameter. For character variables, the most
#'        common values (in descending frequency order), also limited by
#'       \code{max.distinct.values}. For numerical variables, common univariate
#'       statistics (mean, std. deviation, min, med, max, IQR and CV).}
#'     \item{Freqs (\% of Valid)}{For factors and character variables, the
#'       frequencies and proportions of the values listed in the previous
#'       column. For numerical vectors, number of distinct values, or frequency
#'       of distinct values if their number is not greater than
#'       \code{max.distinct.values}.}
#'     \item{Text Graph}{An ascii histogram for numerical variables, and ascii
#'       barplot for factors and character variables.} \item{Valid}{Number and
#'       proportion of valid values.}
#'     \item{Missing}{Number and proportion of missing (NA and NAN) values.} }
#'
#' @details The default \code{plain.ascii = TRUE} option is there to make
#'   results appear cleaner in the console. When used in a context of
#'   \emph{rmarkdown} rendering, set this option to \code{FALSE}.
#'
#'   When the \code{trim.strings} is set to \code{TRUE}, trimming is done
#'   \emph{before} calculating frequencies, so those will be impacted
#'   accordingly.
#'
#'   The package vignette \dQuote{Recommendations for Rmarkdown} provides
#'   valuable information for creating optimal \emph{Rmarkdown} documents with
#'   summarytools.
#'
#' @examples
#' data(tobacco)
#' dfSummary(tobacco)
#' \dontrun{view(dfSummary(iris))}
#'
#' @keywords univar attribute classes category
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @importFrom dplyr n_distinct
#' @importFrom stats start end
#' @export
dfSummary <- function(x, round.digits = st_options("round.digits"),
                      varnumbers = st_options("dfSummary.varnumbers"),
                      labels.col = st_options("dfSummary.labels.col"),
                      valid.col = st_options("dfSummary.valid.col"),
                      na.col = st_options("dfSummary.na.col"),
                      graph.col = st_options("dfSummary.graph.col"),
                      graph.magnif = st_options("dfSummary.graph.magnif"),
                      style = "multiline",
                      plain.ascii = st_options("plain.ascii"),
                      justify = "left", col.widths = NA, 
                      headings = st_options("headings"),
                      display.labels = st_options("display.labels"),
                      max.distinct.values = 10, trim.strings = FALSE,
                      max.string.width = 25, split.cells = 40,
                      split.tables = Inf, ...) {


  # Validate arguments ---------------------------------------------------------
  errmsg <- character()  # problems with arguments will be stored here

  # Flag to replace colname when x is not a data frame
  converted_to_df <- FALSE
  if (!is.data.frame(x)) {
    xnames <- substitute(x)
    x <- try(as.data.frame(x))

    if (inherits(x, "try-error")) {
      errmsg %+=% paste(deparse(xnames), " is not coercible to a data frame")
    } else {
      converted_to_df <- TRUE
      df_name <- setdiff(all.names(xnames), c("[", "[[", ":", "$"))[1]
      msg(paste(deparse(xnames), "was converted to a data frame"))
    }
  }

  errmsg <- c(errmsg, check_arguments(match.call(), list(...)))

  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "))
  }

  # End of arguments validation ------------------------------------------------

  if (isTRUE(labels.col) && length(label(x, all = TRUE)) == 0) {
    labels.col <- FALSE
  }
  
  # Get info on x from parsing function ----------------------------------------
  #max.varnames <- as.numeric(converted_to_df)
  parse_info <- try(parse_args(sys.calls(), sys.frames(), match.call(),
                               #max.varnames = max.varnames,
                               var_name = converted_to_df, 
                               var_label = converted_to_df,
                               caller = "dfSummary"),
                    silent = TRUE)

  if (inherits(parse_info, "try-error")) {
    parse_info <- list()
  }

  if (!("df_name" %in% names(parse_info)) && exists("df_name")) {
    parse_info$df_name <- df_name
  }

  if (isTRUE(converted_to_df) && identical(colnames(x), "x")) {
    if ("var_name" %in% names(parse_info)) {
      colnames(x) <- parse_info$var_name
    } else {
      colnames(x) <- parse_info$df_name
    }
  }

  if (!isTRUE(plain.ascii) && style == "grid" && isTRUE(graph.col)) {
    store_imgs <- paste0("img/", parse_info$df_name)
    message("Images used in dfSummary() will be written to ", 
            normalizePath(store_imgs, mustWork = FALSE))
    dir.create(normalizePath(store_imgs, mustWork = FALSE),
               showWarnings = FALSE, recursive = TRUE)
  } else {
    store_imgs <- NA
  }

  # Initialize the output data frame -------------------------------------------

  output <- data.frame(no               = numeric(),
                       variable         = character(),
                       label            = character(),
                       stats.values     = character(),
                       freqs.pct.valid  = character(),
                       graph            = character(),
                       text.graph       = character(),
                       valid            = character(),
                       missing          = character(),
                       stringsAsFactors = FALSE,
                       check.names      = FALSE)

  n_tot <- nrow(x)

  
  # iterate over columns of x --------------------------------------------------

  for(i in seq_len(ncol(x))) {

    # extract column data
    column_data <- x[[i]]

    # Add column number
    output[i,1] <- i

    # Add column name and class
    output[i,2] <- paste0(names(x)[i], "\\\n[",
                          paste(class(column_data), collapse = ", "),
                          "]")
    
    # Add UPC/EAN info if applicable
    if(is.factor(column_data)) {
      barcode_type <- detect_barcode(levels(column_data))
    } else {
      barcode_type <- detect_barcode(column_data)
    }
    
    if (is.character(barcode_type)) {
      output[i,2] <- paste(output[i,2], 
                           paste(barcode_type, trs("codes")),
                           sep = "\\\n")
      if(is.numeric(column_data)) {
        column_data <- as.character(column_data)
      }
    }
    
    # Add column label (if applicable)
    if (isTRUE(labels.col)) {
      output[i,3] <- label(x[[i]])
      if (is.na(output[i,3]))
        output[i,3] <- ""
    }

    # Calculate valid vs missing data info
    n_miss <- sum(is.na(column_data))
    n_valid <- n_tot - n_miss

    # Factors: display a column of levels and a column of frequencies ----------
    if (is.factor(column_data)) {
      output[i,4:7] <- crunch_factor(column_data)
    }

    # Character data: display frequencies whenever possible --------------------
    else if (is.character(column_data)) {
      output[i,4:7] <- crunch_character(column_data)
    }

    # Numeric data, display a column of descriptive stats + column of freqs ----
    else if (is.numeric(column_data)) {
      output[i,4:7] <- crunch_numeric(column_data, is.character(barcode_type))
    }

    # Time/date data -----------------------------------------------------------
    else if (inherits(column_data, c("Date", "POSIXct"))) {
      output[i,4:7] <- crunch_time_date(column_data)
    }

    # Data does not fit in previous categories ---------------------------------
    else {
      output[i,4:7] <- crunch_other(column_data)
    }

    output[i,8] <-
      paste0(n_valid, "\\\n(", round(n_valid / n_tot * 100, round.digits), "%)")
    output[i,9] <-
      paste0(n_miss,  "\\\n(", round(n_miss  / n_tot * 100, round.digits), "%)")
  }

  # Prepare output object ------------------------------------------------------
  if (!isTRUE(varnumbers)) {
    output$no <- NULL
  }

  if (!isTRUE(labels.col)) {
    output$label <- NULL
  }

  if (!isTRUE(graph.col)) {
    output$graph <- NULL
    output$text.graph <- NULL
  }

  if (!isTRUE(valid.col)) {
    output$valid <- NULL
  }

  if (!isTRUE(na.col)) {
    output$missing <- NULL
  }

  # apply translations to colnames
  for (i in seq_along(output)) {
    colnames(output)[i] <- trs(colnames(output)[i])
  }

  # Set output attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "dfSummary"
  attr(output, "date") <- Sys.Date()
  attr(output, "fn_call") <- match.call()

  data_info <-
    list(Data.frame       = parse_info$df_name,
         Dataf.rame.label = ifelse("df_label" %in% names(parse_info),
                                   parse_info$df_label, NA),
         Dimensions       = paste(n_tot, "x", ncol(x)),
         Duplicates       = n_tot - n_distinct(x),
         Group            = ifelse("by_group" %in% names(parse_info),
                                   parse_info$by_group, NA),
         by_first         = ifelse("by_group" %in% names(parse_info), 
                                   parse_info$by_first, NA),
         by_last          = ifelse("by_group" %in% names(parse_info), 
                                   parse_info$by_last , NA))

  attr(output, "data_info") <- data_info[!is.na(data_info)]

  format_info <- list(style          = style,
                      round.digits   = round.digits,
                      plain.ascii    = plain.ascii,
                      justify        = justify,
                      headings       = headings,
                      display.labels = display.labels,
                      labels.col     = labels.col,
                      split.cells    = split.cells,
                      split.tables   = split.tables,
                      col.widths     = col.widths)
  
  attr(output, "format_info") <- format_info[!is.na(format_info)]
  
  attr(output, "user_fmt") <- list(... = ...)

  attr(output, "lang") <- st_options("lang")
  
  return(output)
}

#' @keywords internal
crunch_factor <- function(column_data) {
  
  outlist <- list()
  outlist[[1]] <- ""
  outlist[[2]] <- ""
  outlist[[3]] <- ""
  outlist[[4]] <- ""
  
  max.string.width    <- parent.frame()$max.string.width
  max.distinct.values <- parent.frame()$max.distinct.values
  graph.magnif        <- parent.frame()$graph.magnif
  round.digits        <- parent.frame()$round.digits
  
  n_levels <- nlevels(column_data)
  counts   <- table(column_data, useNA = "no")
  props    <- prop.table(counts)
  
  if (n_levels == 0 && parent.frame()$n_valid == 0) {
    outlist[[1]] <- "No levels defined"
    outlist[[2]] <- "All NA's"
    outlist[[3]] <- ""
    outlist[[4]] <- ""
    
  } else if (parent.frame()$n_valid == 0) {
    outlist[[1]] <- paste0(1:n_levels,"\\. ", levels(column_data),
                           collapse = "\\\n")
    outlist[[2]] <- "All NA's"
    outlist[[3]] <- ""
    outlist[[4]] <- ""
    
  } else if (n_levels <= max.distinct.values + 1) {
    outlist[[1]] <- paste0(seq_along(counts),"\\. ",
                           substr(levels(column_data), 1, max.string.width),
                           collapse = "\\\n")
    counts_props <- align_numbers_dfs(counts, round(props, round.digits + 2))
    outlist[[2]] <- paste0("\\", counts_props, collapse = "\\\n")
    if (isTRUE(parent.frame()$graph.col) &&
        any(!is.na(column_data))) {
      outlist[[3]] <- encode_graph(counts, "barplot", graph.magnif)
      if (!is.na(parent.frame()$store_imgs)) {
        tmp_png <- encode_graph(counts, "barplot", graph.magnif, TRUE)
        destfile <- paste0(parent.frame()$store_imgs, "/", 
                           parent.frame()$i, ".png")
        file.copy(tmp_png, destfile, overwrite = TRUE)
        outlist[[4]] <- paste0("![](", destfile, ")")
      } else {
        outlist[[4]] <- txtbarplot(prop.table(counts))
      }
    }
  } else {
    
    # more levels than allowed by max.distinct.values
    n_extra_levels <- n_levels - max.distinct.values
    
    outlist[[1]] <-
      paste0(1:max.distinct.values,"\\. ",
             substr(levels(column_data), 1,
                    max.string.width)[1:max.distinct.values],
             collapse="\\\n")

    outlist[[1]] <- paste(outlist[[1]],
                          paste("[", n_extra_levels, trs("others"), "]"),
                          sep="\\\n")

    counts_props <- align_numbers_dfs(
      c(counts[1:max.distinct.values],
        sum(counts[(max.distinct.values + 1):length(counts)])),
      c(props[1:max.distinct.values],
        round(sum(props[(max.distinct.values + 1):length(props)]),
              round.digits + 2))
    )
    
    outlist[[2]] <- paste0("\\", counts_props, collapse = "\\\n")
    
    if (isTRUE(parent.frame()$graph.col) &&
        any(!is.na(column_data))) {
      # Prepare data for bar plot
      tmp_data <- column_data
      levels(tmp_data)[max.distinct.values + 1] <-
        paste("[", n_extra_levels, trs("others"), "]")
      tmp_data[which(as.numeric(tmp_data) > max.distinct.values)] <-
        paste("[", n_extra_levels, trs("others"), "]")
      levels(tmp_data)[(max.distinct.values + 2):n_levels] <- NA
      outlist[[3]] <- encode_graph(table(tmp_data), "barplot", graph.magnif)
      if (!is.na(parent.frame()$store_imgs)) {
        tmp_png <- encode_graph(table(tmp_data), "barplot", graph.magnif, TRUE)
        destfile <- paste0(parent.frame()$store_imgs, "/", 
                           parent.frame()$i, ".png")
        file.copy(tmp_png, destfile, overwrite = TRUE)
        outlist[[4]] <- paste0("![](", destfile, ")")
      } else {
        outlist[[4]] <- txtbarplot(prop.table(table(tmp_data)))
      }
    }
  }
  
  Encoding(outlist[[1]]) <- "UTF-8"
  Encoding(outlist[[2]]) <- "UTF-8"
  Encoding(outlist[[3]]) <- "UTF-8"
  return(outlist)
}

#' @keywords internal
crunch_character <- function(column_data) {
  
  outlist <- list()
  outlist[[1]] <- ""
  outlist[[2]] <- ""
  outlist[[3]] <- ""
  outlist[[4]] <- ""
  
  max.string.width    <- parent.frame()$max.string.width
  max.distinct.values <- parent.frame()$max.distinct.values
  graph.magnif        <- parent.frame()$graph.magnif
  round.digits        <- parent.frame()$round.digits
  
  if (isTRUE(parent.frame()$trim.strings)) {
    column_data <- trimws(column_data)
  }
  
  n_empty <- sum(column_data == "", na.rm = TRUE)
  
  if (n_empty == parent.frame()$n_tot) {
    outlist[[1]] <- paste0(trs("all.empty.str"), "\n")
  } else if (parent.frame()$n_miss == parent.frame()$n_tot) {
    outlist[[1]] <- paste0(trs("all.nas"), "\n") # \n to circumvent pander bug
  } else if (n_empty + parent.frame()$n_miss == parent.frame()$n_tot) {
    outlist[[1]] <- paste0(trs("all.empty.str.nas"), "\n")
  } else {
    
    counts <- table(column_data, useNA = "no")
    props <- prop.table(counts)
    
    if (length(counts) <= max.distinct.values + 1) {
      # Report all frequencies when allowed by max.distinct.values
      outlist[[1]] <- paste0(seq_along(counts), "\\. ",
                             substr(names(counts), 1, max.string.width),
                             collapse = "\\\n")
      counts_props <- align_numbers_dfs(counts, round(props, round.digits + 2))
      outlist[[2]] <- paste0("\\", counts_props, collapse = "\\\n")
      if (isTRUE(parent.frame()$graph.col) &&
          any(!is.na(column_data))) {
        outlist[[3]] <- encode_graph(counts, "barplot", graph.magnif)
        if (!is.na(parent.frame()$store_imgs)) {
          tmp_png <- encode_graph(counts, "barplot", graph.magnif, TRUE)
          destfile <- paste0(parent.frame()$store_imgs, "/", 
                             parent.frame()$i, ".png")
          file.copy(tmp_png, destfile, overwrite = TRUE)
          outlist[[4]] <- paste0("![](", destfile, ")")
        } else {
          outlist[[4]] <- txtbarplot(prop.table(counts))
        }
      }
    } else {
      # more values than allowed by max.distinct.values
      counts <- sort(counts, decreasing = TRUE)
      props <- sort(props, decreasing = TRUE)
      n_extra_values <- length(counts) - max.distinct.values
      outlist[[1]] <- paste0(
        paste0(1:max.distinct.values,"\\. ",
               substr(names(counts), 1,
                      max.string.width)[1:max.distinct.values],
               collapse="\\\n"),
        paste("\\\n[", n_extra_values, trs("others"), "]")
      )
      counts_props <- align_numbers_dfs(
        c(counts[1:max.distinct.values],
          sum(counts[(max.distinct.values + 1):length(counts)])),
        c(props[1:max.distinct.values],
          round(sum(props[(max.distinct.values + 1):length(props)]),
                round.digits + 2))
      )
      
      outlist[[2]] <- paste0("\\", counts_props, collapse = "\\\n")
      
      if (isTRUE(parent.frame()$graph.col) &&
          any(!is.na(column_data))) {
        # Prepare data for bar plot
        counts[max.distinct.values + 1] <-
          sum(counts[(max.distinct.values + 1):length(counts)])
        names(counts)[max.distinct.values + 1] <-
          paste("[", n_extra_values, trs("others"),"]")
        counts <- counts[1:(max.distinct.values + 1)]
        outlist[[3]] <- encode_graph(counts, "barplot", graph.magnif)
        if (!is.na(parent.frame()$store_imgs)) {
          tmp_png <- encode_graph(counts, "barplot", graph.magnif, TRUE)
          destfile <- paste0(parent.frame()$store_imgs, "/", 
                             parent.frame()$i, ".png")
          file.copy(tmp_png, destfile, overwrite = TRUE)
          outlist[[4]] <- paste0("![](", destfile, ")")
        } else {
          outlist[[4]] <- txtbarplot(prop.table(counts))
        }
      } 
    }
  }
  
  Encoding(outlist[[1]]) <- "UTF-8"
  Encoding(outlist[[2]]) <- "UTF-8"
  Encoding(outlist[[3]]) <- "UTF-8"
  return(outlist)
}
#' @importFrom stats IQR median ftable sd
#' @keywords internal
crunch_numeric <- function(column_data, is_barcode) {

  outlist <- list()
  outlist[[1]] <- ""
  outlist[[2]] <- ""
  outlist[[3]] <- ""
  outlist[[4]] <- ""

  max.distinct.values <- parent.frame()$max.distinct.values
  graph.magnif        <- parent.frame()$graph.magnif
  round.digits        <- parent.frame()$round.digits
  
  if (parent.frame()$n_miss == parent.frame()$n_tot) {
    outlist[[1]] <- paste0(trs("all.nas"), "\n")
  } else {
    counts <- table(column_data, useNA = "no")

    if (length(counts) == 1) {
      outlist[[1]] <- paste(1, trs("distinct.value"))
    } else {
      if (isTRUE(is_barcode)) {
        maxchars <- max(nchar(c(trs("min"), trs("max"), trs("mode"))))
        outlist[[1]] <- paste0(
          trs("min"), strrep(" ", maxchars - nchar(trs("min"))), " : ",
          min(column_data, na.rm = TRUE), "\\\n",
          trs("mode"), strrep(" ", maxchars - nchar(trs("mode"))), " : ",
          names(counts)[which.max(counts)][1], "\\\n",
          trs("max"), strrep(" ", maxchars - nchar(trs("max"))), " : ",
          max(column_data, na.rm = TRUE)
        )
      } else if (length(counts) == 2) {
        maxchars <- max(nchar(c(trs("min"), trs("max"), trs("mean"))))
        outlist[[1]] <- paste0(
          trs("min"), strrep(" ", maxchars - nchar(trs("min"))), " : ",
          round(min(column_data, na.rm = TRUE), round.digits - 1), "\\\n",
          trs("mean"), strrep(" ", maxchars - nchar(trs("mean"))), " : ",
          round(mean(column_data, na.rm = TRUE), round.digits - 1), "\\\n",
          trs("max"), strrep(" ", maxchars - nchar(trs("max"))), " : ",
          round(max(column_data, na.rm = TRUE), round.digits - 1)
        )
      } else {
        outlist[[1]] <- paste(
          trs("mean"), paste0(" (", trs("sd"), ") : "),
          round(mean(column_data, na.rm = TRUE), round.digits - 1),
          " (", round(sd(column_data, na.rm = TRUE), round.digits - 1), ")\\\n",
          tolower(paste(trs("min"), "<", trs("med.short"), "<", trs("max"))),
          ":\\\n", round(min(column_data, na.rm = TRUE), round.digits - 1),
          " < ", round(median(column_data, na.rm = TRUE), round.digits - 1),
          " < ", round(max(column_data, na.rm = TRUE), round.digits - 1), "\\\n",
          paste0(trs("iqr"), " (", trs("cv"), ") : "),
          round(IQR(column_data, na.rm = TRUE), round.digits - 1),
          " (", round(sd(column_data, na.rm = TRUE) /
                        mean(column_data, na.rm = TRUE),
                      round.digits - 1), ")", collapse="", sep="")
      }
    }
    
    extra_space <- FALSE

    # Values columns
    # for "ts" objects, display n distinct & start / end
    if (inherits(column_data, "ts")) {
      maxchars <- max(nchar(c(trs("start"), trs("end"))))
      outlist[[2]] <-
        paste(length(counts), trs("distinct.values"),
              paste0("\\\n", trs("start"),
                     strrep(" ", maxchars - nchar(trs("start"))), ":"),
              paste(sprintf("%02d", start(column_data)),
                    collapse = "-"),
              paste0("\\\n", trs("end"),
                     strrep(" ", maxchars - nchar(trs("end"))), ":"),
              paste(sprintf("%02d", end(column_data)),
                    collapse = "-"))
    }

    # In specific circumstances, display most common values
    else if (
      length(counts) <= max.distinct.values &&
      (all(column_data %% 1 == 0, na.rm = TRUE) ||
       identical(names(column_data), "0") ||
       all(abs(as.numeric(names(counts[-which(names(counts) == "0")]))) >=
                  10^-round.digits))) {

      props <- round(prop.table(counts), round.digits + 2)
      counts_props <- align_numbers_dfs(counts, props)

      rounded_names <- 
        trimws(format(
          round(as.numeric(names(counts)), round.digits),
          nsmall = round.digits * !all(column_data %% 1 == 0, na.rm = TRUE)
        ))
      
      maxchars <- max(nchar(rounded_names))

      outlist[[2]]  <-
        paste(
          paste0(rounded_names, strrep(" ", maxchars - nchar(rounded_names)),
                 ifelse(as.numeric(names(counts)) != as.numeric(rounded_names),
                        "!", " ")),
          counts_props, sep = ": ", collapse = "\\\n"
        )

      if (any(as.numeric(names(counts)) != as.numeric(rounded_names))) {
        extra_space <- TRUE
        outlist[[2]] <- paste(outlist[[2]], paste("!", trs("rounded")),
                              sep = "\\\n")
      }

    } else {
      # Do not display specific values - only the number of distinct values
      outlist[[2]] <- paste(length(counts), trs("distinct.values"))
      if (parent.frame()$n_miss == 0 &&
          (isTRUE(all.equal(column_data, min(column_data):max(column_data))) ||
           isTRUE(all.equal(column_data, max(column_data):min(column_data))))) {
        outlist[[2]] <- paste(outlist[[2]], 
                              paste0("(", trs("int.sequence"), ")"),
                              sep = "\\\n")
      }
    }

    if (isTRUE(parent.frame()$graph.col)) {
      if (length(counts) <= max.distinct.values) {
        outlist[[3]] <- encode_graph(counts, "barplot", graph.magnif)
        if (!is.na(parent.frame()$store_imgs)) {
          tmp_png <- encode_graph(counts, "barplot", graph.magnif, TRUE)
          destfile <- paste0(parent.frame()$store_imgs, "/", 
                             parent.frame()$i, ".png")
          file.copy(tmp_png, destfile, overwrite = TRUE)
          outlist[[4]] <- paste0("![](", destfile, ")")
        } else {
          outlist[[4]] <- txtbarplot(prop.table(counts))
        }
        
        if (isTRUE(extra_space)) {
          outlist[[3]] <- paste0(outlist[[3]], "\n\n")
          outlist[[4]] <- paste0(outlist[[4]], " \\ \n \\")
        }
      } else {
        outlist[[3]] <- encode_graph(column_data, "histogram", graph.magnif)
        if (!is.na(parent.frame()$store_imgs)) {
          tmp_png <- encode_graph(column_data, "histogram", graph.magnif, TRUE)
          destfile <- paste0(parent.frame()$store_imgs, "/", 
                             parent.frame()$i, ".png")
          file.copy(tmp_png, destfile, overwrite = TRUE)
          outlist[[4]] <- paste0("![](", destfile, ")")
        } else {
          outlist[[4]] <- txthist(column_data)
        }
      }
    }
  }
  Encoding(outlist[[1]]) <- "UTF-8"
  Encoding(outlist[[2]]) <- "UTF-8"
  Encoding(outlist[[3]]) <- "UTF-8"
  return(outlist)
}

#' @importFrom lubridate as.period interval
#' @keywords internal
crunch_time_date <- function(column_data) {

  outlist <- list()
  outlist[[1]] <- ""
  outlist[[2]] <- ""
  outlist[[3]] <- ""
  outlist[[4]] <- ""

  #max.string.width    <- parent.frame()$max.string.width
  max.distinct.values <- parent.frame()$max.distinct.values
  graph.magnif        <- parent.frame()$graph.magnif
  round.digits        <- parent.frame()$round.digits
  
  if (parent.frame()$n_miss == parent.frame()$n_tot) {
    outlist[[1]] <- paste0(trs("all.nas"), "\n")
  } else {

    counts <- table(column_data, useNA = "no")

    # Report all frequencies when allowed by max.distinct.values
    if (length(counts) <= max.distinct.values) {
      outlist[[1]] <- paste0(seq_along(counts),". ", names(counts),
                             collapse="\\\n")
      props <- round(prop.table(counts), round.digits + 2)
      counts_props <- align_numbers_dfs(counts, props)
      outlist[[2]] <- paste(counts_props, collapse = "\\\n")
      outlist[[3]] <- encode_graph(counts, "barplot", graph.magnif)
      if (!is.na(parent.frame()$store_imgs)) {
        tmp_png <- encode_graph(counts, "barplot", graph.magnif, TRUE)
        destfile <- paste0(parent.frame()$store_imgs, "/", 
                           parent.frame()$i, ".png")
        file.copy(tmp_png, destfile, overwrite = TRUE)
        outlist[[4]] <- paste0("![](", destfile, ")")
      } else {
        outlist[[4]] <- txtbarplot(prop.table(counts))
      }
    } else {

      outlist[[1]] <- paste0(
        "min : ", tmin <- min(column_data, na.rm = TRUE), "\\\n",
        "med : ", median(column_data, na.rm = TRUE), "\\\n",
        "max : ", tmax <- max(column_data, na.rm = TRUE), "\\\n",
        "range : ", sub(pattern = " 0H 0M 0S", replacement = "",
                        x = round(as.period(interval(tmin, tmax)),round.digits))
      )

      outlist[[2]] <- paste(length(counts), trs("distinct.values"))

      if (isTRUE(parent.frame()$graph.col)) {
        tmp <- as.numeric(column_data)[!is.na(column_data)]
        outlist[[3]] <- encode_graph(tmp - mean(tmp), "histogram", graph.magnif)
        if (!is.na(parent.frame()$store_imgs)) {
          tmp_png <- encode_graph(tmp - mean(tmp), "histogram", 
                                  graph.magnif, TRUE)
          destfile <- paste0(parent.frame()$store_imgs, "/", 
                             parent.frame()$i, ".png")
          file.copy(tmp_png, destfile, overwrite = TRUE)
          outlist[[4]] <- paste0("![](", destfile, ")")
        } else {
          outlist[[4]] <- txthist(tmp - mean(tmp))
        }
      }
    }
  }
  outlist
}

#' @keywords internal
crunch_other <- function(column_data) {

  outlist <- list()
  outlist[[1]] <- ""
  outlist[[2]] <- ""
  outlist[[3]] <- ""
  outlist[[4]] <- ""

  max.distinct.values <- parent.frame()$max.distinct.values
  round.digits        <- parent.frame()$round.digits
  #graph.magnif        <- parent.frame()$graph.magnif
  #max.string.width    <- parent.frame()$max.string.width

  counts <- table(column_data, useNA = "no")

  if (parent.frame()$n_miss == parent.frame()$n_tot) {
    outlist[[1]] <- paste0(trs("all.nas"), "\n")

  } else if (length(counts) <= max.distinct.values) {
    props <- round(prop.table(counts), round.digits + 2)
    counts_props <- align_numbers_dfs(counts, props)
    outlist[[2]] <- paste0(counts_props, collapse = "\\\n")

  } else {
    outlist[[2]] <- paste(as.character(length(unique(column_data))),
                          trs("distinct.values"))
  }

  return(outlist)
}

# Utility functions ------------------------------------------------------------
#' @keywords internal
align_numbers_dfs <- function(counts, props) {
  maxchar_cnt <- nchar(as.character(max(counts)))
  maxchar_pct <- nchar(sprintf(paste0("%.",parent.frame()$round.digits - 1, "f"), 
                               max(props*100)))
  paste(sprintf(paste0("%", maxchar_cnt, "i"), counts),
        sprintf(paste0("(%", maxchar_pct, ".", parent.frame()$round.digits - 1,
                       "f%%)"), props*100))
}

#' @importFrom RCurl base64Encode
#' @importFrom graphics barplot hist par text plot.new
#' @importFrom grDevices dev.off nclass.Sturges png
#' @importFrom magick image_read image_trim image_border image_write 
#'             image_transparent
#' @keywords internal
encode_graph <- function(data, graph_type, graph.magnif = 1, pandoc = FALSE) {
  bg <- par('bg'=NA)
  on.exit(par(bg))
  if (graph_type == "histogram") {
    png(img_png <- tempfile(fileext = ".png"), width = 150 * graph.magnif,
        height = 110 * graph.magnif, units = "px", bg = "transparent")
    mar <- par("mar" = c(0.01, 0.00, 0.01, 0.01)) # bottom, left, top, right
    on.exit(par(mar), add = TRUE)
    data <- data[!is.na(data)]
    breaks_x <- pretty(range(data), n = min(nclass.Sturges(data), 250),
                       min.n = 1)
    cl <- try(suppressWarnings(hist(data, freq = FALSE, breaks = breaks_x,
                                    axes = FALSE, xlab = NULL, ylab = NULL,
                                    main = NULL, col = "grey95",
                                    border = "grey65")),
              silent = TRUE)
    if (inherits(cl, "try-error")) {
      plot.new()
      text("Graph Not Available", x = 0.5, y = 0.5, cex = 1)
    }
    
  } else if (graph_type == "barplot") {
    
    png(img_png <- tempfile(fileext = ".png"), width = 150 * graph.magnif,
        height = 26 * length(data) * graph.magnif, units = "px",
        bg = "transparent")
    mar <- par("mar" = c(0.05, 0.30, 0.05, 0.05)) # bottom, left, top, right
    on.exit(par(mar), add = TRUE)
    data <- rev(data)
    barplot(data, names.arg = "", axes = FALSE, space = 0.22, #0.21,
            col = "grey97", border = "grey65", horiz = TRUE,
            xlim = c(0, sum(data)))
  }
  
  dev.off()
  if (isTRUE(pandoc)) {
    if (graph_type == "barplot") {
      ii <- image_read(img_png)
      ii <- image_border(image_trim(ii), color = "white", geometry = "4x2")
      image_write(image_transparent(ii, 'white'), img_png)
    }
    return(img_png)
  } else {
    img_txt <- base64Encode(txt = readBin(con = img_png, what = "raw",
                                          n = file.info(img_png)[["size"]]),
                            mode = "character")
    return(sprintf('<img style="border:none;background:none;padding:0" 
                 src="data:image/png;base64, %s">', img_txt))
  }
}

#' @keywords internal
txtbarplot <- function(props, maxwidth = 20) {
  #widths <- props / max(props) * maxwidth
  widths <- props * maxwidth
  outstr <- character(0)
  for (i in seq_along(widths)) {
    outstr <- paste(outstr, paste0(rep(x = "I", times = widths[i]),
                                   collapse = ""),
                    sep = " \\ \n")
  }
  outstr <- sub("^ \\\\ \\n", "", outstr)
  return(outstr)
}

#' @importFrom grDevices nclass.Sturges
#' @keywords internal
txthist <- function(data) {
  data <- data[!is.na(data)]
  breaks_x <- pretty(range(data), n = nclass.Sturges(data), min.n = 1)
  if (length(breaks_x) <= 10) {
    counts <- hist(data, breaks = breaks_x, plot = FALSE)$counts
  } else {
    counts <- as.vector(table(cut(data, breaks = 10)))
  }
  
  # make counts top at 10
  counts <- matrix(round(counts / max(counts) * 10), nrow = 1, byrow = TRUE)
  graph <- matrix(data = "", nrow = 6, ncol = length(counts))
  for (ro in 6:1) {
    for (co in seq_along(counts)) {
      if (counts[co] > 1) {
        graph[ro,co] <- ": "
      } else if (counts[co] > 0) {
        graph[ro,co] <- ". "
      } else {
        if (sum(counts[1, co:length(counts)] > 0)) {
          graph[ro,co] <- "\\ \\ "
        }
      }
    }
    counts <- matrix(apply(X = counts - 2, MARGIN = 2, FUN = max, 0),
                     nrow = 1, byrow = TRUE)
  }
  
  graphlines <- character()
  for (ro in seq_len(nrow(graph))) {
    graphlines[ro] <-  trimws(paste(graph[ro,], collapse = ""), "right")
  }
  return(paste(graphlines, collapse = "\\\n"))
}

#' @importFrom utils head
#' @importFrom stats na.omit
#' @keywords internal
detect_barcode <- function(x) {
  
  op <- options(warn=2)
  on.exit(options(op))
  
  x <- try(as.numeric(x), silent = TRUE)
  if (inherits(x, "try-error")) {
    return(FALSE)
  }
  
  x <- na.omit(x)[1:100]
  if (length(x) < 10 || (len <- min(nchar(x))) != max(nchar(x)) ||
      !len %in% c(8,12,13,14)) {
    return(FALSE)
  }
  
  x <- head(x, 20)
  
  type <- switch(as.character(len),
                 "8"  = "EAN-8",
                 "12" = "UPC",
                 "13" = "EAN-13",
                 "14" = "ITF-14")
  
  x_pad      <- paste0(strrep("0", 14-len), x)
  vect_code  <- lapply(strsplit(x_pad,""), as.numeric)
  weighted   <- lapply(vect_code, FUN = function(x) x * c(3,1))
  sums       <- mapply(weighted, FUN = sum)
  
  if (any(sums %% 10 != 0, na.rm = TRUE)) {
    return(FALSE)
  }
  
  return(type)
}
