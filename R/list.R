#' a structure variable with class attribute of result
#'
#' \code{list} is a structure variable with class attribute of \code{result}.
#' @export
list <- structure(NA, class = "result")
#' Extract variables from named elements of a list object
#'
#' Operator acting on a list with named elements to extract list elements of
#' interest to named variables.
#'
#' @usage
#' S3 method for class 'result'
#' list[] <- value
#' list[x, ] <- value
#' list[, y] <- value
#' list[x, y, ..., all = FALSE] <- value
#' @param list an exported structure variable with class attribute of \code{result}.
#' @param x,y variable names. List elements at corresponding index are extracted and then
#' assigned to those variables.
#' @param ... empty strings for positions of list elements without interest. By default
#' these elements are not extracted to corresponding variables with same names.
#' Setting \code{all} to TRUE can also extract those elements without interest.
#' @param all whether extract all named elements to variables. The default is FALSE
#' # except for the case without specifying any arguments, in which all elements
#' are extracted to corresponding variables.
#' @param value a list object with named elements.
#' @return named variables such as \code{x}, \code{y} and variables with element names.
#'
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), Ph.D. student from
#' Center for Earth System Science, Tsinghua University
#' @export
#' @examples
#' # unpack results from function returning multiple values
#' # define a function returning two values
#' fun <- function() list(a = 1, b = 2)
#'
#' # implicitly extract all variabes without renaming
#' list[] <- fun()  # return a = 1, b = 2
#'
#' # explicitly extract all variables without renaming
#' list[, , all = TRUE] <- fun()  # return a = 1, b = 2
#'
#' # no variables extracted
#' list[, , all = FALSE] <- fun()
#'
#' # only extract the first variable and assign it to x. note that by default only
#' # element of interest is extracted and renamed.
#' list[x, ] <- fun()  # return x = 1
#'
#' # set all to TRUE to extract all elements.
#' list[x, , all = TRUE] <- fun()  # return x = 1, b = 2
#'
#' # only extract the second variable and assign it to y
#' list[, y] <- fun()  # return y = 2
#'
#' # explicitly extract all variables with renaming
#' list[x, y] <- fun()  # return x = 1, y = 2
#'
#' # parameter all ignored
#' list[x, y, all = TRUE] <- fun()  # return x = 1, y = 2
"[<-.result" <- function(x, ..., value) {
  nv <- names(value)
  args <- as.list(match.call())
  args <- args[-c(1:2, length(args))]
  if (!"all" %in% names(args)) {
    all <- FALSE
  } else {
    all <- args[["all"]]
    stopifnot(is.logical(all))
    args["all"] <- NULL
  }

  if (length(args) == 1) args <- sapply(nv, as.name)
  for (i in seq(along = args)) {
    a <- args[[i]]
    if (missing(a)) {
      if (all) a <- as.name(nv[i]) else next
    }
    eval.parent(substitute(a <- v, list(a = a, v = value[[i]])))
  }
  x
}
