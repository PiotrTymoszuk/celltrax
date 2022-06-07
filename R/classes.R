# trax class definitions

# trax class -------

#' Create a trax object.
#'
#' @description Generates a trax object form a 'tracks' class object.
#' @details The 'trax' class inherits most of its methods from the 'tracks'
#' class but provides enhanced plotting and filtering features.
#' @param x a 'tracks' object generated i.e. with
#' \code{\link[celltrackR]{tracks}}.
#' @param ... extra arguments, currently none.
#' @return an object of 'trax' class.
#' @export

  trax <- function(x, ...) {

    if(!is.tracks(x)) {

      stop("The 'x' argument needs to be an instance of 'tracks' class.",
           call. = FALSE)

    }

    structure(x,
              class = c('trax', 'tracks'))

  }

# Class testing ------

#' Tests for the 'trax' class.
#'
#' @description Tests if the object is an instance of the 'trax' class.
#' @param x an object.
#' @param .... extra arguments, currently none.
#' @return a logical value.
#' @export

  is_trax <- function(x, ...) {

    inherits(x, 'trax')

  }

# Coercion tools ---------

#' Create a trax object.
#'
#' @description Generates a trax object form a 'tracks' class object.
#' @details The 'trax' class inherits most of its methods from the 'tracks'
#' class but provides enhanced plotting and filtering features.
#' @param x a 'tracks' object generated i.e. with
#' \code{\link[celltrackR]{tracks}}.
#' @param ... extra arguments, passed to methods.
#' @return an object of 'trax' class.
#' @export

  as_trax <- function(x, ...) {

    UseMethod('as_trax')

  }

#' Create a trax object.
#'
#' @description Generates a trax object form a 'tracks' class object.
#' @details The 'trax' class inherits most of its methods from the 'tracks'
#' class but provides enhanced plotting and filtering features.
#' @param x an object to be coerced.
#' @param ... extra arguments, passed to \code{\link[celltrackR]{tracks}}.
#' @return an object of 'trax' class.
#' @export

  as_trax.tracks <- function(x, ...) {

    trax(x, ...)

  }

#' Create a trax object from a list.
#' @description Converts a list to a trax object. See:
#' \code{\link[celltrackR]{tracks}} for details.
#' @param x an object to be coerced.
#' @param ... extra arguments, passed to \code{\link[celltrackR]{tracks}}.
#' @return an object of 'trax' class.
#' @export

  as_trax.list <- function(x, ...) {

    trax(celltrackR:::as.tracks.list(x, ...))

  }

#' Create a trax object from a data frame.
#' @description Converts a list to a trax object. See:
#' \code{\link[celltrackR]{tracks}} for details.
#' @param x an object to be coerced.
#' @param ... extra arguments, passed to \code{\link[celltrackR]{tracks}}.
#' @return an object of 'trax' class.
#' @export

  as_trax.data.frame <- function(x, ...) {

    trax(celltrackR:::as.tracks.data.frame(x, ...))

  }

# The coercion method preserving the 'trax' class ------

#' Tracks Objects
#'
#' @description The default method for the 'as.tracks' generic.
#' @param x  an object to be coerced or tested.
#' @param tracks_only logical, should the output be a tracks class instance?
#' @param ... extra arguments, passed to \code{\link[celltrackR]{tracks}}.
#' @return an object of 'trax' or 'tracks' class.
#' @export

  as.tracks.trax <- function(x, tracks_only = FALSE, ...) {

    stopifnot(is_trax(x))
    stopifnot(is.logical(tracks_only))

    if(tracks_only) {

      structure(x, class = 'tracks')

    } else  {

      x

    }

  }

# END ----
