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

#' @title Imprecise Imputation
#'
#' @description Check whether the variables of a data frame
#' contain imprecise observations
#'
#' @param data data.frame to test to apply the check onto.
#'
#' @return A named logical vector of length \code{ncol(data)},
#' where \code{TRUE} indicates that \code{"|"} is present in the
#' values, which is used to indicate an imprecise observations.
#'
#' @note This check is only reliabe for \code{data}, inheriting
#' class \code{"impimp"}. If \code{data} does not inherit class
#' \code{"impimp"}, the check is tried, but additionaly the
#' user is notified with a warning.
#'
#' @seealso \code{\link{impimp}}
#'
#' @examples
#' A <- data.frame(x1 = c(1,0), x2 = c(0,0),
#'                 y1 = c(1,0), y2 = c(2,2))
#' B <- data.frame(x1 = c(1,1,0), x2 = c(0,0,0),
#'                 z1 = c(0,1,1), z2 = c(0,1,2))
#' AimpB <- impimp(A, B, method = "variable_wise")
#' BimpA <- impimp(B, A, method = "variable_wise")
#' AB <- rbindimpimp(AimpB, BimpA)
#' checkImprecision(AB)
#'
#' \donttest{
#' data(iris)
#' checkImprecision(iris) # emits a warning
#' }
#' @export
checkImprecision <- function(data) {

  if(!is.impimp(data)) {
    warning(sprintf(gettext("%s is not of class %s: the result may be inaccurate",
                            domain = "R-impimp"),
                    sQuote("data"), dQuote("impimp")))
  }
  # returns TRUE if a variable includes imprecise observations
  sep <- getOption("impimp.obssep", "|")
  vapply(data, FUN = function(x) {any(grepl(sep, x, fixed = TRUE))},
         FUN.VALUE = logical(1L))
}
