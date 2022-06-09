# Pre-processing functions for track step number filtering, gap correction,
# drift compensation and filtering of non-motile cells.
# All functions may, as an option, return the complete filtering results,
# i.e. trax objects with the cells passing and missing the filter procedure.

# Track step number filtering ------

#' Filter tracks by step number range.
#'
#' @description Filters a trax object with the user-provided step number range.
#' @return a trax object or, if return_both is TRUE, a list of trax object with
#' the 'outcome' (cells passing the procedure) and 'miss' elements, each of them
#' is a trax object.
#' @param x a trax object.
#' @param min_steps minimal step number of the track to be kept.
#' Ignored, if NULL (default).
#' @param max_steps maximal step number of the track to be kept.
#' Ignored, if NULL (default).
#' @param return_both logical: if TRUE, both the cells passing and missing
#' the filtering procedure are returned. FALSE by default.
#' @export

  filter_steps <- function(x,
                           min_steps = NULL,
                           max_steps = NULL,
                           return_both = FALSE) {

    ## entry control

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    stat_tbl <- calculate(x, type = 'steps')

    stat_range <- range(stat_tbl$value)

    if(!is.null(min_steps)) {

      stopifnot(is.numeric(min_steps))

      min_steps <- as.integer(min_steps)

      if(!min_steps %in% 0:stat_range[2]) {

        stop('The min_steps argument beyond the step number in the track set.',
             call. = FALSE)

      }

    } else {

      min_steps <- stat_range[1]

    }

    if(!is.null(max_steps)) {

      stopifnot(is.numeric(max_steps))

      max_steps <- as.integer(max_steps)

      if(!max_steps %in% stat_range[1]:stat_range[2]) {

        stop('The max_steps argument beyond the step number in the track set.',
             call. = FALSE)

      }

    } else {

      max_steps <- stat_range[2]

    }

    stopifnot(is.logical(return_both))

    ## filtering

    records_keep <- dplyr::filter(stat_tbl, value %in% min_steps:max_steps)$id

    if(!return_both) return(x[records_keep])

    if(length(records_keep) < nrow(stat_tbl)) {

      miss <- x[!names(x) %in% records_keep]

    } else {

      miss <- NULL

    }

    list(outcome = x[records_keep],
         miss = miss)

  }

# Gap detection and repair ------

#' Detect step time abnormalities.
#'
#' @description Detects steps in a trax object with differing time differences.
#' @return A tibble with frequency of time intervals.
#' @param x a trax object.
#' @export

  time_intervals <- function(x) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    dts <- dplyr::count(calculate(x, type = 'time'), value)

    dplyr::mutate(dts,
                  percent = n/sum(n) * 100)

  }

#' Process Tracks Containing Gaps
#'
#' @inherit celltrackR::repairGaps
#' @return a trax object or, if return_both is TRUE, a list of trax object with
#' the 'outcome' (cells passing the procedure) and 'before' elements, each of
#' them is a trax object
#' @param return_both logical: if TRUE, both the cells before and after the
#' transformation. FALSE by default.
#' @export

  repair_gaps <- function(x,
                          how = 'split',
                          tol = 0.05,
                          split.min.length = 2,
                          return_both = FALSE) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    how <- match.arg(how, c('split', 'drop', 'interpolate'))

    stopifnot(is.numeric(tol))
    stopifnot(is.numeric(split.min.length))
    stopifnot(is.logical(return_both))

    rep_trax <- repairGaps(x,
                           how = how,
                           tol = tol,
                           split.min.length = split.min.length)

    if(!return_both) {

      return(rep_trax)

    } else {

      return(list(outcome = rep_trax,
                  before = x))

    }

  }

# Drift correction ------

#' Correct for non-specific drift.
#'
#' @description Corrects for unspecific drift in the trax object by subtracting
#' the user-provided mean speed vector.
#' @return a trax object or, if return_both is TRUE, a list of trax object with
#' the 'outcome' (cells passing the procedure) and 'before' elements, each of
#' them is a trax object.
#' @param x a trax object.
#' @param drift_vector a drift vector. Its dimensions need to correspond to
#' the track dimension.
#' @param return_both logical: if TRUE, both the cells before and after the
#' transformation. FALSE by default.
#' @export

  correct_drift <- function(x,
                            drift_vector,
                            return_both = FALSE) {

    ## entry control

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    available_dims <- ncol(x[[1]]) - 1

    if(length(drift_vector) < available_dims) {

      stop('The length of the drift_vector needs to correspond with the x dimentions.',
           call. = FALSE)

    }

    stopifnot(is.logical(return_both))

    ## computation

    corrx <- as_trax(purrr::map(x,
                                adjust_drift,
                                drift_vector = drift_vector[1:available_dims]))

    if(!return_both) {

      return(corrx)

    } else {

      return(list(outcome = corrx,
                  before = x))

    }

  }

# Motility cutoff ------

#' Filter out tracks for non-motile cells.
#'
#' @description Filters out non-motile cells based on the results of
#' Gaussian modeling.
#' @return a trax object or, if return_both is TRUE, a list of trax object with
#' the 'outcome' (cells passing the procedure) and 'miss' elements, each of them
#' is a trax object.
#' @details See: \code{\link{bic_position}}, \code{\link{detlaBIC}} and
#' https://cran.rstudio.com/web/packages/celltrackR/vignettes/QC.html.
#' @param x a trax object.
#' @param bic_cutoff BIC (Bayesian Information Criterion) cutoff for
#' identification of motile cells (cells with >= cutoff are kept).
#' The value of 6 is a good starting point.
#' @param sigma SD of the Gaussian distribution. The expected cell diameter
#' is usually a good approximation.
#' @param dims dimensions used for computation of the statistic.
#' @param return_both logical: if TRUE, both the cells passing and missing
#' the filtering procedure are returned. FALSE by default.
#' @export

  filter_motility <- function(x,
                              bic_cutoff = 6,
                              sigma = 10,
                              dims = c('x', 'y'),
                              return_both = FALSE) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    stat_tbl <- calculate(x, type = 'delta_BIC', dims = dims)

    ## filtering

    records_keep <- dplyr::filter(stat_tbl, value >= bic_cutoff)$id

    if(!return_both) return(x[records_keep])

    if(length(records_keep) < nrow(stat_tbl)) {

      miss <- x[!names(x) %in% records_keep]

    } else {

      miss <- NULL

    }

    list(outcome = x[records_keep],
         miss = miss)


  }

# Duplicate removal ------

#' Remove multiplets.
#'
#' @description Removes candidate multiplets from the trax objects based on
#' results of the cell pair analysis. Potential multiplets are characterized
#' by small distance and low angle.
#' @return a trax object or, if return_both is TRUE, a list of trax object with
#' the 'outcome' (cells passing the procedure) and 'miss' elements, each of them
#' is a trax object.
#' @details The pair statistics is calculated with
#' \code{\link[celltrackR]{analyzeCellPairs}} and
#' \code{\link[celltrackR]{analyzeStepPairs}}.
#' @param x a trax object.
#' @param method analysis method, 'cells' (default) or 'steps'.
#' @param angle_cutoff angle cutoff. By default 10 degrees
#' (pairs >= cutoff are kept).
#' @param dist_cutoff distance cutoff (pairs >= cutoff are kept).
#' An approximate cell diameter is a good starting value.
#' @param return_both logical: if TRUE, both the cells passing and missing
#' the filtering procedure are returned. FALSE by default.
#' @param ... extra arguments passed to
#' \code{\link[celltrackR]{analyzeCellPairs}} or
#' \code{\link[celltrackR]{analyzeStepPairs}}.
#' @export

  filter_multiplets <- function(x,
                                method = c('cells', 'steps'),
                                angle_cutoff = 10,
                                dist_cutoff = 10,
                                return_both = FALSE, ...) {

    ## entry control

    if(!is_trax(x)) stop("'x' needs to be a 'trax' object.", call. = FALSE)

    stopifnot(is.numeric(angle_cutoff))
    stopifnot(is.numeric(dist_cutoff))
    stopifnot(is.logical(return_both))

    method <- match.arg(method[1], c('cells', 'steps'))

    ## filtering data

    analysis_fun <- switch(method,
                           cells = analyzeCellPairs,
                           steps = analyzeStepPairs)

    stat_tbl <- analysis_fun(x, ...)

    stat_tbl <- dplyr::filter(stat_tbl,
                              complete.cases(stat_tbl),
                              angle < angle_cutoff,
                              dist < dist_cutoff)

    records_rm <- unique(c(stat_tbl[[1]], stat_tbl[[2]]))

    pass <- x[!names(x) %in% records_rm]

    if(!return_both) return(pass)

    if(length(records_rm) > 0) {

      miss <- x[records_rm]

    } else {

      miss <- NULL

    }

    list(outcome = pass,
         miss = miss)

  }

# Track splitting ----

#' Split tracks by a displacement cutoff.
#'
#' @description Given a trax object, the function calculates displacements for
#' each step and splits of the signle track into multiple ones
#' if the cutoff is met.
#' @param x a trax object.
#' @param disp_cutoff the scalar displacement cutoff.
#' @param return_both logical: if TRUE, both the cells before and after the
#' transformation. FALSE by default.
#' @return a trax object.
#' @export

  split_tracks <- function(x, disp_cutoff, return_both = FALSE) {

    if(!is_trax(x)) stop("'x' needs to be a 'trax' object.", call. = FALSE)

    stopifnot(is.numeric(disp_cutoff))

    new_trax <- purrr::map2(x, names(x),
                            ~split_track(.x,
                                         disp_cutoff = disp_cutoff,
                                         prefix = .y))

    new_trax <- purrr::reduce(new_trax, c.trax)

    if(!return_both) return(new_trax)

    list(outcome = new_trax,
         before = x)

  }

# END -----
