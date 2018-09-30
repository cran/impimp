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

#' @title Imprecise Events
#'
#' @description Helper function to allow the generation of a set of
#' events as cartesian product.
#'
#' @param ... these arguments are of the form \code{varname = value}.
#' For each component the varname should be a variable name from the
#' underlying data.frame and value a vector of possible outcomes;
#' may also be of length one.
#'
#' @return A object of class \code{"impimp_event"} as a list of lists,
#' where each sublist contains one point in the cartesian product,
#' spanned by the input values and variables.
#'
#' @note
#' There is no plausibility check on whether the supplied varnames
#' are actually contained in the data.frame for which the
#' resulting impimp_event object is later used for.
#'
#' @keywords robust
#'
#' @seealso \code{\link{impest}}, \code{\link{impestcond}}
#'
#' @examples
#' ## underlying data set: x1: 1:6, x2: 1:10
#'
#' ## subspace, requiring: x1 == 1 & ((z2 == 1 ) | (z2 == 2))
#' impimp_event(x1 = 1, z2 = c(1,2))
#'
#' ## subsapce containing all points whitin the Cartesian
#' ## product of (x1 =) {1,2,3,6} x {5,8} (= z2)
#' impimp_event(x1 = c(1:3,6), z2 = c(5,8))
#'
#' @importFrom stats setNames
#' @export
impimp_event <- function(...) {

  y <- list(...)
  if(!all(nzchar(names(y)))) {
    stop("all supplied entries must have names", domain = "R-impimp")
  }
  if(any(lengths(y, use.names = FALSE) > 1)) {
    x <- apply(expandGridLocal(y), MARGIN = 1, FUN = as.list)
  } else {
    x <- list(as.list(stats::setNames(as.character(y), names(y))))
  }
  if(any(sapply(x, function(z) {lengths(z, use.names = FALSE) > 1}))) {
    stop("illegal structure after parsing; possibly nested objects",
         domain = "R-impimp")
  }
  class(x) <- c("list", "impimp_event")
  x
}

#' @rdname impimp_event
#' @param x object to test for class \code{"impimp_event"}
#' @export
is.impimp_event <- function(x) {
  inherits(x = x, what = "impimp_event")
}
