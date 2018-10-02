


#' OrderedVarNames
#'
#' @param x input 
#' @param sep 
#'
#' @return output
#' @keywords internal
#' @export
#'
#' @examples
#' z <- data.frame(A = factor(c("a", "b", "c")), B = factor(1:2), C = 1:6)
#' x <- colnames(model.matrix(~B * C * A, z))
#' OrderedVarNames(x)
OrderedVarNames <- function(x, sep = ":") {
  unlist(lapply(strsplit(x, sep), function(x) paste(sort(x), collapse = sep)))
}


#' MatchVarNames
#'
#' @param x x
#' @param y y
#' @param sep sep 
#' @param makeWarning Warning when matching by reordering
#'
#' @return An integer vector giving the position in y of the first match if there is a match, otherwise NA.
#' @keywords internal
#' @export
#'
#' @examples
#' z <- data.frame(A = factor(c("a", "b", "c")), B = factor(1:2), C = 1:6)
#' x <- colnames(model.matrix(~B * C * A, z))
#' y <- colnames(model.matrix(~A * B + A:B:C, z))
#' MatchVarNames(x, y)
MatchVarNames <- function(x, y, sep = ":", makeWarning = FALSE) {
  matchNames <- match(x, y)
  noMatch <- is.na(matchNames)
  if (!any(noMatch)) 
    return(matchNames)
  matchNames[noMatch] <- match(OrderedVarNames(x[noMatch]), OrderedVarNames(y))
  if (makeWarning) 
    if (any(!is.na(matchNames[noMatch]))) 
      warning("Matching by reordering")
  matchNames
}

setTotal <- function(total, lmObject) {
  x <- model.matrix(lmObject)[1, ]
  x[] <- NA
  matchNames <- MatchVarNames(names(x), names(total), makeWarning = FALSE)
  if (any(duplicated(matchNames[!is.na(matchNames)]))) 
    stop("Duplicates when matching")
  x[] <- total[matchNames]
  x
}

getTotal <- function(data, lmObject, w = NULL) {
  mfCal <- model.frame(lmObject)
  mfCalNA <- mfCal[c(NA, 1), ][1, , drop = FALSE]
  rownames(mfCalNA) <- NULL
  mfCalNames <- colnames(mfCal)
  okNames <- mfCalNames %in% colnames(data)
  if (any(!okNames)) 
    data <- cbind(data[, mfCalNames[okNames], drop = FALSE], mfCalNA[, mfCalNames[!okNames], drop = FALSE])
  m <- model.matrix(lmObject, data = data, na.action = NULL)
  
  x <- NULL
  if (is.null(w)) {
    x$colSum <- colSums(m, na.rm = TRUE)
    x$N <- dim(m)[1]
    if (anyNA(m)) {
      x$colN <- colSums(!is.na(m))
    } else {
      x$colN <- rep(x$N, length(x$colSum))
      names(x$colN) <- names(x$colSum)
    }
  } else {
    x$colSum <- colSums(w * m, na.rm = TRUE)
    x$N <- sum(w)
    if (anyNA(m)) {
      # will not happen
      x$colN <- colSums(w * (!is.na(m)))
    } else {
      x$colN <- rep(x$N, length(x$colSum))
      names(x$colN) <- names(x$colSum)
    }
  }

  x
}












