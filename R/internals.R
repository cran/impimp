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

##### helper functions needed in ImpreciseImputation.R ###############

# @title Helper function
# @description Impute each value by collection of
# the supplied levels
# @param zlvls list of levels of the variables to impute
# @param znames vector of variable names to impute
# @return list object where each named entry contains the set
# of all supplied levels to be imputed for the variable.
# @importFrom stats setNames
# @keywords internal
imputation_values <- function(zlvls, znames) {
  lapply(stats::setNames(nm = znames), function(zname) {
    collapse_obs(zlvls[[zname]])
  })
}

# @title Helper function
# @description Collect the differnt values in \code{x}
# in a lexicographically sorted unique character vector
# @param x input (atomic) vector
# @return character vector containing all unique values
# of \code{x}, lexicographically sorted.
# @keywords internal
gather_levels <- function(x) {
  y <- unique(as.character(x))
  y[sort.list(y)]
}

# @title Helper function
# @description Collapse variables into tuple notation
# @param x object containing the variables to collapse
# @param znames variable names
# @return List with single vector entry, containing the
# observations of the variables in \code{znames} of
# \code{B} in tuple notation; includes the name of the variables.
# @keywords internal
collapse_variables <- function(x, znames) {
  y <- list(apply(x[,znames], MARGIN = 1, collapse_values))
  names(y) <- collapse_values(znames)
  y
}

# @title Helper function
# @description Collapse vector into single value
# (generation of a tuple)
# @param z vector to collapse
# @return single element where all elements in \code{x}
# are glued together by \code{getOption("impimp.varsep"}
# @keywords internal
collapse_values <- function (z) {
  paste(z, collapse = getOption("impimp.varsep", ","))
}

# @title Helper function
# @description Collapse vector into single value
# (generation of tuples for observations)
# @param z vector to collapse
# @return single element where all elements in \code{x}
# are glued together by \code{getOption("impimp.obssep"}
# @keywords internal
collapse_obs <- function (z) {
  paste(z, collapse = getOption("impimp.obssep", "|"))
}

##### helper functions needed in ImpreciseEstimation.R ###############
# @title Helper function
# @description Checking the format of arguments supplied to
# \code{event}, \code{condition}, \code{constraints}
# @param events event to check for consistency
# @return the calling argument invisibly
# @note This functions is only invoked for testing the input
# @keywords internal
eventcheck <- function(events) {

  if(!is.list(events) || is.impimp_event(events)) {
    stop(gettextf("%s must be a list of %s objects",
                  sQuote(deparse(substitute(events))),
                  dQuote("impimp_event"),
                  domain = "R-impimp"))
  }

  if(any(vapply(events, function(x) {!is.impimp_event(x)},
                FUN.VALUE = logical(1L)))) {
    stop(gettextf("only objects of class %s are permitted in %s",
                  dQuote("impimp_event"),
                  sQuote(deparse(substitute(events))),
                  domain = "R-impimp"))
  }
  invisible(events)
}

# @title Helper function
# @description Helper function mimicking expand.grid
# @param args list of objects to expand into all combintaions
# @return data.frame. For further explanations see
# \code{\link[base]{expand.grid}}.
# @note No consistency checks are performed on the input
# @seealso \code{\link[base]{expand.grid}}
# @keywords internal
expandGridLocal <- function(args) {
  nargs <- length(args)
  iArgs <- seq_len(nargs)
  rep.fac <- 1L
  d <- lengths(args)
  orep <- prod(d)
  cargs <- matrix("", ncol = nargs, nrow = orep,
                  dimnames = list(NULL, names(args)))
  for (i in iArgs) {
    nx <- d[i]
    orep <- orep/nx
    cargs[,i] <- args[[i]][rep.int(
      rep.int(seq_len(nx), rep.int(rep.fac, nx)), orep)]
    rep.fac <- rep.fac * nx
  }
  cargs
}

# @title Helper function
# @description Generating logical vector indicating which
# observations of the data.frame \code{data} fulfill \code{events}
# @param events probability events to (eventually) evaluate.
# List of objects of class \code{"impimp_event"}.
# @param data underlying data frame
# @return logical vector of length \code{NROW(data)}
# @note No consistency checks are performed on the inputs
# @keywords internal
evalConditions <- function(events, data) {

  evalSingleEvent <- function(event, data) {
    # For each element of event, see if the observations match it
    x <- sapply(names(event), function(nam) {
      data[, nam] == event[[nam]]
    })
    # make sure the result is a matrix for 'apply' to work
    if(!is.matrix(x)) {x <- matrix(x, nrow = 1)}
    # join the event-wise conditions per row by logical AND
    apply(x, MARGIN = 1, FUN = all)
  }

  y <- sapply(events, function(event) {
    evalSingleEvent(event, data)
  })
  res <- if(is.matrix(y)) {
    apply(y, MARGIN = 1, FUN = any)
  } else {
    any(y)
  }
  res
}

##### helper functions needed in union.R #############################
# @title Helper function
# @description Construct disjoint entries
# @param x list of vectors, each entry corresponding to a set
# @return a list of smaller lengths where entries have been joined
# from the front. The result does not necessary contain a list
# of disjoint entries.
# @keywords internal
makeDisjoint <- function(x) {
  if(length(x) == 1) {
    return(x)
  }
  for(i in seq(1, length(x) - 1)) {
    for(j in seq(i + 1, length(x))) {
      if(is.na(x[j])) {
        next
      }
      if(length(intersect(x[[i]], x[[j]]))) {
        x[[i]] <- union(x[[i]], x[[j]])
        x[[j]] <- NA
      }
    }
  }
  x[!is.na(x)]
}

# @title Helper function
# @description Check if list contains entries which are not disjoint.
# @param x list of vectors, each entry corresponding to a set
# @return \code{TRUE} is returned if there are at least 2 elements
# of \code{x} which have a non-empty intersection, otherwise
# \code{FALSE} is returned.
# @keywords internal
isNotDisjoint <- function(x) {
  if(length(x) > 1) {
    for(i in seq(1, length(x) - 1)) {
      for(j in seq(i + 1, length(x))) {
        if(length(intersect(x[[i]], x[[j]]))) {
          return(TRUE)
        }
      }
    }
  }
  FALSE
}
