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
#' list[x, y, ..., drop = TRUE] <- value
#' @param list an exported structure variable with class attribute of \code{result}.
#' @param x,y variable names. List elements at corresponding index are extracted and then
#' assigned to those variables.
#' @param ... spaces for positions of list elements without interest. By default
#' these elements are not extracted to corresponding variables with same names.
#' Setting \code{all} to TRUE can also extract those elements without interest.
#' @param all whether extract all named elements to variables. The default is FALSE
#' except for two cases:  \emph{no} or \emph{all} variables specified, in which all elements
#' are extracted to corresponding variables.
#' @param drop delete the dimensions of extracted variables which have only one level.
#' The default is TRUE.
#' @param value a list object with named elements.
#' @return named variables such as \code{x}, \code{y} and variables with element names.
#'
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), Ph.D. student from
#' Center for Earth System Science, Tsinghua University
#' @references \url{http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value}
#' @export
#' @examples
#' # unpack results from function returning multiple values
#' # define a function returning a matrix and a numeric, and the matrix can be
#' # dropped into a numeric
#' fun <- function() list(a = matrix(1, nr = 1, nc = 1), b = 2)
#'
#' # implicitly extract all variabes without renaming. note that by default
#' # dimensions are dropped.
#' list[] <- fun()  # return a = 1, b = 2
#'
#' # explicitly extract all variables without renaming and dimension dropping
#' list[, , all = TRUE, drop = FALSE] <- fun()  # return matrix a, b = 2
#'
#' # only extract the first variable and assign it to x. note that by default only
#' # element of interest is extracted and renamed.
#' list[x, ] <- fun()  # return x = 1
#'
#' # set drop to FALSE without dimension dropping
#' list[x, , drop = FALSE] <- fun()  # return matrix x
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
#' list[, all = FALSE] <- fun()  # return a = 1, b = 2
#' list[x, y, all = FALSE] <- fun()  # return x = 1, y = 2
"[<-.result" <- function(x, ..., value) {
  nv <- names(value)
  args <- as.list(match.call())
  args <- args[-c(1:2, length(args))]
  all <- FALSE
  if ("all" %in% names(args)) {
    all <- args[["all"]]
    stopifnot(is.logical(all))
    args["all"] <- NULL
  }
  drop = TRUE
  if ("drop" %in% names(args)) {
    drop <- args[["drop"]]
    stopifnot(is.logical(drop))
    args["drop"] <- NULL
  }

  if (length(args) == 1) args <- sapply(nv, as.name)
  for (i in seq(along = args)) {
    a <- args[[i]]
    if (missing(a)) {
      if (all) a <- as.name(nv[i]) else next
    }
    if (drop) v <- drop(value[[i]]) else v <- value[[i]]
    eval.parent(substitute(a <- v, list(a = a, v = v)))
  }
  x
}
