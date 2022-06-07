# Wrappers around tracks object manipulation functions


# Filtering with a function -----

#' Filter Tracks
#'
#' @inherit celltrackR::filterTracks
#' @details Preserves 'trax' class.
#' @export

  filterTracks <- function(f, x, ...) {

    output <- celltrackR::filterTracks(f = f, x = x, ...)

    if(is_trax(x)) {

      return(trax(output))

    } else {

      return(output)

    }

  }

# Normalization -------

#' Normalize Tracks
#'
#' @inherit celltrackR::normalizeTracks
#' @details Preserves 'trax' class.
#' @export

  normalizeTracks <- function(x) {

    output <- celltrackR::normalizeTracks(x = x)

    if(is_trax(x)) {

      return(trax(output))

    } else {

      return(output)

    }

  }

# Subsampling and subtracking ------

#' Subsample Track by Constant Factor
#'
#' @inherit celltrackR::subsample
#' @details Preserves 'trax' class.
#' @export

  subsample <- function(x, k = 2) {

    output <- celltrackR::subsample(x = x, k = k)

    if(is_trax(x)) {

      return(trax(output))

    } else {

      return(output)

    }

  }

#' Decompose Track(s) into Subtracks
#'
#' @inherit celltrackR::subtracks
#' @details Preserves 'trax' class.
#' @export

  subtracks <- function(x, i, overlap = i - 1) {

    output <- celltrackR::subtracks(x = x, i = i, overlap = overlap)

    if(is_trax(x)) {

      return(trax(output))

    } else {

      return(output)

    }

  }

#' Get Track Prefixes
#'
#' @inherit celltrackR::prefixes
#' @details Preserves 'trax' class.
#' @export

  prefixes <- function(x, i) {

    output <- celltrackR::prefixes(x = x, i = i)

    if(is_trax(x)) {

      return(as_trax(output))

    } else {

      return(output)

    }

  }

#' Extract Subtracks Starting at a Specific Time
#'
#' @inherit celltrackR::subtracksByTime
#' @details Preserves 'trax' class.
#' @export

  subtracksByTime <- function(X,
                              t,
                              i = 1,
                              epsilon = 1e-04,
                              tlo = t,
                              thi = t) {

    output <- celltrackR::subtracksByTime(X = X, t = t,
                                          i = i, epsilon = epsilon,
                                          tlo = tlo, thi = thi)

    if(is_trax(X)) {

      return(trax(output))

    } else {

      return(output)

    }

  }

# Projection ------

#' Extract Spatial Dimensions
#'
#' @inherit celltrackR::projectDimensions
#' @details Preserves 'trax' class.
#' @export

  projectDimensions <- function(x, dims = c('x', 'y')) {

    output <- celltrackR::projectDimensions(x = x, dims = dims)

    if(is_trax(x)) {

      return(trax(output))

    } else {

      return(output)

    }

  }

# Gap repair ------

#' Process Tracks Containing Gaps
#'
#' @inherit celltrackR::repairGaps
#' @details Preserves 'trax' class.
#' @export

  repairGaps <- function(x,
                         how = 'split',
                         tol = 0.05,
                         split.min.length = 2) {

    output <- celltrackR::repairGaps(x = x,
                                     how = how,
                                     tol = tol,
                                     split.min.length = split.min.length)

    if(is_trax(x)) {

      return(trax(output))

    } else {

      return(output)

    }

  }

# Pair analysis, tibble output -------

#' Find Distances and Angles for all Pairs of Tracks
#'
#' @inherit celltrackR::analyzeCellPairs
#' @export

  analyzeCellPairs <- function(X, searchRadius = Inf, quietly = FALSE, ...) {

    tibble::as_tibble(celltrackR::analyzeCellPairs(X,
                                                   searchRadius = Inf,
                                                   quietly = FALSE, ...))

  }

#' Find Distances and Angles for all Pairs of Steps
#'
#' @inherit celltrackR::analyzeStepPairs
#' @export

  analyzeStepPairs  <- function(X,
                                filter.steps = NULL,
                                searchRadius = Inf,
                                quietly = FALSE, ...) {

    tibble::as_tibble(celltrackR::analyzeStepPairs(X,
                                                   filter.steps = NULL,
                                                   searchRadius = Inf,
                                                   quietly = FALSE, ...))


  }



# END -----
