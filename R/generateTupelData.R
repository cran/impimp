# Copyright (C) 2018  Paul Fink, Eva Endres
#
# This file is part of impimp.
#
# imptree is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# imptree is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with imptree.  If not, see <https://www.gnu.org/licenses/>.

#' @title Tuple representation
#'
#' @description Generating a tuple representation of a
#' data.frame with imprecise observations
#'
#' @param data a data.frame object, with potentially
#' imprecise entries; see 'Note'.
#' @param constraints a list of so-called logical constraints or
#' fixed zeros. Each element must be an object of class
#' \code{"impimp_event"}. See 'Details'.
#'
#' @return A list of length \code{NROW(data)} of data.frames
#' for the observation within the original data.frame.
#'
#' Each such data.frame contains the precise observations which
#' are compatible with its imprecise representation.
#'
#' @details
#' By specifying \code{constraints} one can exlude combinations of
#' imputed values which are deemed impossible, so called
#' \sQuote{logical constraints} or \sQuote{fixed zeros}.
#'
#' @note
#' No sanity check is performed on whether \code{data} actually
#' contains imprecise observations or is in the form for denoting
#' imprecision throughoutly used in the impimp-package. A warning is
#' triggered if it is not of class \code{"impimp"}.
#'
#' @keywords datagen
#'
#' @seealso \code{\link{impimp}}, \code{\link{impimp_event}} for
#' sepcifying the constraints
#'
#' @examples
#' A <- data.frame(x1 = c(1,0), x2 = c(0,0),
#'                 y1 = c(1,0), y2 = c(2,2))
#' B <- data.frame(x1 = c(1,1,0), x2 = c(0,0,0),
#'                 z1 = c(0,1,1), z2 = c(0,1,2))
#' AimpB <- impimp(A, B, method = "domain")
#'
#' ## no constraints
#' generateTupelData(AimpB)
#'
#' ## (y1,z1) = (0,0) as constraint
#' generateTupelData(AimpB, list(impimp_event(y1 = 0, z1 = 0)))
#'
#' \donttest{
#' data(iris)
#' generateTupelData(iris) # emits a warning
#' }
#' @export
generateTupelData <- function(data, constraints = NULL) {

  if(!is.impimp(data)) {
    warning(gettextf("%s is not of class %s: the result may be inaccurate",
                     sQuote("data"), dQuote("impimp"),
                     domain = "R-impimp"))
  }
  hasConstraints <- !is.null(constraints)
  if (hasConstraints) {
    if(!is.list(constraints) || is.impimp_event(constraints) ) {
      stop(gettextf("if specified, %s must be a list of objects of class %s",
                    sQuote("constraints"), dQuote("impimp_event"),
                    domain = "R-impimp"))
    }
    if(any(vapply(constraints,
                  FUN = function(x) {!is.impimp_event(x)},
                  FUN.VALUE = logical(1L), USE.NAMES = FALSE))) {
      stop(gettextf("the elements of %s must all be of class %s",
                    sQuote("constraints"), dQuote("impimp_event"),
                    domain = "R-impimp"))
    }
    constraints <- unlist(constraints, recursive = FALSE)
  }

  nrowData <- NROW(data)
  # initialising result list
  tupels <- vector("list", nrowData)
  # converting all to character for convenience
  mat <- sapply(data, as.character, USE.NAMES = TRUE)

  # get the different separators from the options
  varsep <- getOption("impimp.varsep", ",")
  obssep <- getOption("impimp.obssep", "|")

  # original colnames
  colNames <- unlist(strsplit(names(data), varsep, fixed = TRUE),
                     use.names = FALSE, recursive = FALSE)
  ncolData <- length(colNames)
  icols <- seq_len(ncolData)

  # iteration over all observations
  for (i in seq_len(nrowData)) {

    # burst the imprecise observations
    varlist <- strsplit(mat[i, ], obssep, fixed = TRUE)
    # get all possible combinations; especially relevenat,
    # if there are >1 variables with imprecise observations
    rowexpand <- expandGridLocal(varlist)
    # combine the rows into a single tuple each ...
    rowtupels <- apply(rowexpand, 1, collapse_values)
    # ... and then combine them to a data.frame
    rowobs <- do.call(rbind, strsplit(rowtupels, varsep, fixed = TRUE))

    # put them into a list
    precObs <- vector("list", ncolData)
    for (j in icols) {precObs[[j]] <- rowobs[, j]}
    # assign the repective row and column names
    attr(precObs, "row.names") <-
      paste(i, seq_len(NROW(rowobs)), sep = ".")
    names(precObs) <- colNames
    # assign class to be a data.frame
    class(precObs) <- "data.frame"

    if(hasConstraints) {
      logiconstr <- evalConditions(constraints, precObs)
      precObs <- precObs[!logiconstr, , drop = FALSE]
    }
    # store in the result list
    tupels[[i]] <- precObs
  }
  tupels
}
