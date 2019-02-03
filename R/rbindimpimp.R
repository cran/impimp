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

#' @title Combine impimp Objects
#'
#' @description Combine two object of class \code{"impimp"}
#' like \code{rbind} would do with data frames.
#'
#' @param x,y objects of class \code{"impimp"}. As such may contain
#' variables in form of tuples, they are not required to have the
#' same number of variables as returned from \code{ncol}.
#' However, they are required to have the same underlying variables.
#' If that condition is not satisfied an error is raised.
#'
#' @return An object of class \code{"impimp"}, inheriting the
#' attributes, specific to imimp objects, of \code{x} and \code{y}.
#'
#' @details
#' The resulting object is constructed in such a way that minimizes
#' the creation of 'tupled' variables. Only those variables are joined
#' as tuples which are actually necessary to keep the data frame like
#' consise representation of impimp objects.
#'
#' The attributes \code{"impmethod"} and \code{"varlevels"} contain
#' the set union of those of \code{x} and \code{y} on a global and
#' per underlying variable basis, respectively.
#'
#' @keywords datagen
#'
#' @seealso \code{\link{impimp}}
#'
#' @examples
#' A <- data.frame(x1 = c(1,0), x2 = c(0,0),
#'                 y1 = c(1,0), y2 = c(2,2))
#' B <- data.frame(x1 = c(1,1,0), x2 = c(0,0,0),
#'                 z1 = c(0,1,1), z2 = c(0,1,2))
#' impA <- impimp(A, B, method = "case_wise")
#' impB <- impimp(B, A, method = "case_wise")
#' rbindimpimp(impA, impB)
#'
#' @importFrom stats setNames
#' @export
rbindimpimp <- function(x, y){

  if(!is.impimp(x) || !is.impimp(y)) {
    stop(gettextf("%s and %s must be both of class %s",
                  sQuote("x"), sQuote("y"),
                  dQuote("impimp"), domain = "R-impimp"))
  }

  # get the speical separator chars
  varsep <- getOption("impimp.varsep", ",")
  obssep <- getOption("impimp.obssep", "|")

  # variable names of the inputs
  namesX <- names(x)
  namesY <- names(y)

  # names of imputed variables
  impvars <- unique(c(attr(x, "imputedvarnames"),
                      attr(y, "imputedvarnames")))

  # burst the combined variable names -> original variable names
  namesSplitX <- strsplit(namesX, varsep, fixed = TRUE)
  namesSplitY <- strsplit(namesY, varsep, fixed = TRUE)

  # if original variable names differ, raise error
  if(!setequal(unlist(namesSplitX), unlist(namesSplitY))) {
    stop(gettextf("The underlying variable names of %s and %s must be the same",
                  sQuote("x"), sQuote("y"),
                  domain = "R-impimp"))
  }

  # calculate the variables names of the result as maximal set
  # of disjoint names, such that the supplied variable names are
  # subsets of the final variable names.
  # 'tuple variables' are lists of original variable names
  newnames <- c(namesSplitX, namesSplitY)
  while(isNotDisjoint(newnames)) {
    newnames <- makeDisjoint(newnames)
  }

  # generate the final variables names by collapsing the list-tuples
  newColNames <- unlist(lapply(newnames, collapse_values),
                        recursive = FALSE, use.names = FALSE)

  # construct the sequence on how the variable names in the inputs
  # are joined to the resulting variable names
  resX <- resY <- vector(mode = "list", length(newnames))
  for(i in seq_along(newnames)) {
    newname <- newnames[[i]]
    if(length(newname) == 1) {
      resX[[i]] <- resY[[i]] <- newname
    } else {
      membersX <- sapply(namesSplitX, function(z){
        any(z %in% newname)}, USE.NAMES = FALSE)
      membersY <- sapply(namesSplitY, function(z){
        any(z %in% newname)}, USE.NAMES = FALSE)
      resX[[i]] <- namesX[membersX]
      resY[[i]] <- namesY[membersY]
    }
  }

  # construct the data.frame object from input and according sequence
  dataFromGroups <- function(impData, vargroups) {

    # init the result as list and assign names
    finalRes <- vector(mode = "list", length(newnames))
    names(finalRes) <- newColNames

    # Which groups consists of just one element?
    copyDirectly <- (lengths(vargroups) == 1)

    # names in the final list (directly assignable variables)
    tocopynames <- unlist(vargroups[copyDirectly],
                          recursive = FALSE, use.names = FALSE)

    # names in the final list (processed variables)
    # lapply is needed to keep in order with copyDirectly
    tonotcopynames <- unlist(lapply(vargroups[!copyDirectly],
                                    collapse_values),
                             recursive = FALSE, use.names = FALSE)

    # assiging the directly assignable variables
    finalRes[tocopynames] <- impData[tocopynames]

    # processing the remainders
    finalRes[tonotcopynames] <-
      lapply(vargroups[!copyDirectly], function(varnames) {
        newVal <- sapply(seq_len(NROW(impData)), function(i) {
          burstvalues <- lapply(impData[i, varnames], function(z) {
            strsplit(as.character(z), obssep, fixed = TRUE)[[1]]})
          tupelData <- expandGridLocal(burstvalues)
          collapse_obs(apply(tupelData, MARGIN = 1, collapse_values))
        })
      })
    # returning the interesting result as a data.frame
    # and the vector of copied variable names
    list(data = as.data.frame.list(finalRes,
                                   col.names = newColNames,
                                   optional = TRUE,
                                   stringsAsFactors = FALSE),
         copiednames = tocopynames)
  }

  # generate the subdata
  impX <- dataFromGroups(impData = x, vargroups = resX)
  impY <- dataFromGroups(impData = y, vargroups = resY)

  # which variables were copied
  copied_vars <- unique(c(impX$copiednames, impY$copiednames))
  # which variables are copied and amongst the imputed variables
  impvars_single <- intersect(copied_vars, impvars)

  # transform variables which contain imputed values into character
  impX$data[, impvars_single] <-
    sapply(impX$data[, impvars_single], as.character)
  impY$data[, impvars_single] <-
    sapply(impY$data[, impvars_single], as.character)

  # combine the two data sets
  impXY <- rbind(impX$data, impY$data, stringsAsFactors = FALSE)

  # adjust the class of the result
  class(impXY) <- c("impimp", class(impXY))
  # set the 'impmethod' attribute as union of the ones of the inputs
  attr(impXY, "impmethod") <- union(attr(x, "impmethod"),
                                       attr(y, "impmethod"))

  # set the 'varlevels' attribute as variable wise union
  # of those of the inputs
  vlX <- attr(x, "varlevels")
  vlY <- attr(y, "varlevels")
  vnXY <- unique(c(names(vlX), names(vlY)))
  attr(impXY, "varlevels") <-
    stats::setNames(mapply(FUN = union, vlX[vnXY], vlY[vnXY],
                    USE.NAMES = FALSE), vnXY)
  attr(impXY, "imputedvarnames") <-
    setdiff(newColNames, setdiff(copied_vars, impvars))

  # return the generate impimp object
  impXY
}
