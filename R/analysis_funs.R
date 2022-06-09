# Functions for higher non-vector analyses

# Autocovariance --------

#' Autocovariance of displacement vectors.
#'
#' @description Calculates autocovariance of between the displacement vectors
#' of for subsequent steps. The results may be aggregated with mean (default),
#' median or other user-supplied function to
#' \code{\link[celltrackR]{aggregate.tracks}}.
#' @details The autocovariance is calculated as a dot product of the vectors
#' returned by \code{\link[celltrackR]{overallDot}} or overall angle computed
#' by \code{\link[celltrackR]{overallAngle}}.
#' @param x a trax object.
#' @param method the method of autocovariance calculation: 'dot_product'
#' (default) or 'angle', as described in details.
#' @param aggregate logical: should the results be aggregated in a step-wise
#' manner? Done with \code{\link[celltrackR]{aggregate.tracks}}.
#' @param ... extra arguments passed to
#' \code{\link[celltrackR]{aggregate.tracks}}.
#' @return a tibble with the results. If aggregate is FALSE, contains sets of
#' autocovariance stats for each track, otherwise the step-wise
#' statistic values.

  autocov <- function(x,
                      method = c('dot_product', 'angle'),
                      aggregate = TRUE,
                      ...) {

    ## entry control

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    stopifnot(is.logical(aggregate))

    method <- match.arg(method[1], c('dot_product', 'angle'))

    stat_fun <- switch(method,
                       dot_product = overallDot,
                       angle = overallAngle)

    ## calculation

    if(aggregate) {

      tibble::as_tibble(aggregate(x, stat_fun, ...))

    } else {

      subs <- purrr::map(x, subtracks, i = 1)

      covs <- purrr::map(subs, ~purrr::map_dbl(.x, stat_fun))

      covs <- purrr::map(covs, ~tibble::tibble(i = 1:length(.x),
                                               value = .x))

      purrr::map2_dfr(covs, names(covs),
                      ~dplyr::mutate(.x, id = .y))[c('id', 'i', 'value')]

    }

  }

# Hotellings test -------

#' Hotellings test.
#'
#' @description A pipeline-friendly version of the hotellings test implemented
#' by \code{\link[celltrackR]{hotellingsTest}}.
#' @return a tibble with statistic value, degrees of freedom and p value.
#' @param x a trax object.
#' @param dims dimensions to be analyzed.
#' @param step_spacing how many positions are to be left out between the steps
#' that are considered for the test,
#' see: \code{\link[celltrackR]{hotellingsTest}} for details.
#' @export

  hotellings_test <- function(x, dims = c('x', 'y'), step_spacing = 0) {

    ## entry control

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    if(any(!dims %in% c('x', 'y', 'z'))) {

      stop('dims must be a pair from the x, y, z set.', call. = FALSE)

    }

    stopifnot(is.numeric(step_spacing))

    ## testing

    tst_res <- hotellingsTest(x, dim = dims, step.spacing = step_spacing)

    tibble::tibble(statistic = tst_res$statistic,
                   df1 = tst_res$parameter[1],
                   df2 = if(length(dims) > 1) tst_res$parameter[2] else NA_integer_,
                   df3 = if(length(dims) > 2) tst_res$parameter[2] else NA_integer_,
                   p_value = tst_res$p.value[1])

  }

# Track step number, length and time interval -------

#' Calculate track step numbers.
#'
#' @description Returns track step numbers.
#' @return a tibble with step numbers for each track.
#' @param x a trax object.

  track_steps <- function(x) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    lens <- purrr::map_dbl(x, nrow)

    tibble::tibble(id = names(lens),
                   value = lens)

  }

#' Calculate track lengths.
#'
#' @description Returns track lengths.
#' @return a tibble with lengths for each track.
#' @param x a trax object.

  track_lengths <- function(x) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    lens <- purrr::map_dbl(x, trackLength)

    tibble::tibble(id = names(lens),
                   value = lens)

  }

#' Calculate track duration.
#'
#' @description Returns track duration.
#' @return a tibble with track duration for each track.
#' @param x a trax object.

  track_duration <- function(x) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    lens <- purrr::map_dbl(x, duration)

    tibble::tibble(id = names(lens),
                   value = lens)

  }

#' Calculate track time steps.
#'
#' @description Returns time intervals for every track step.
#' @return a tibble with track duration for each track step.
#' @param x a trax object.

  track_times <- function(x) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    lens <- purrr::map(x, subtracks, i = 1)

    lens <- purrr::map(lens, ~purrr::map_dbl(.x, duration))

    lens <- purrr::map(lens, ~tibble::tibble(i = 1:length(.x),
                                             value = .x))

    purrr::map2_dfr(lens, names(lens),
                    ~dplyr::mutate(.x, id = .y))[c('id', 'i', 'value')]

  }

# Displacement and speed --------

#' Calculate scalar displacements.
#'
#' @description Calculates displacements (lengths of displacement vectors),
#' step-wise, track-wise, mean or median.
#' @return a tibble with the requested values.
#' @param x a trax object.
#' @param aggregate 'none' (default): displacements for each step are returned,
#' 'total': total displacements are calculated, 'mean' or 'median': mean or
#' median displacements per track are returned.

  track_displacements <- function(x,
                                  aggregate = c('none', 'total', 'mean')) {

    ## entry control

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    aggregate <- match.arg(aggregate[1],
                           c('none', 'total', 'mean', 'median'))

    ## calculation

    if(aggregate == 'total') {

      disps <- purrr::map_dbl(x, displacement)

      return(tibble::tibble(id = names(disps),
                            value = disps))

    }

    disps <- purrr::map(x, subtracks, i = 1)

    disps <- purrr::map(disps, ~purrr::map_dbl(.x, displacement))

    disps <- purrr::map(disps, ~tibble::tibble(i = 1:length(.x),
                                               value = .x))

    disps <- purrr::map2_dfr(disps, names(disps),
                             ~dplyr::mutate(.x, id = .y))[c('id', 'i', 'value')]

    if(aggregate == 'none') return(disps)

    disps <- dplyr::group_by(disps, id)

    sum_fun <- switch(aggregate,
                      mean = function(x) mean(x, na.rm = TRUE),
                      median = function(x) median(x, na.rm = TRUE))

    dplyr::summarise(disps, value = sum_fun(value))


  }

#' Calculate scalar speeds.
#'
#' @description Calculates speeds, step-wise or track-wise.
#' @return a tibble with the requested values.
#' @param x a trax object.
#' @param aggregate 'none' (default): speeds for each step are returned,
#' 'mean' or 'median': mean or median speeds per track are returned, 'total':
#' total displacement by time.

  track_speeds <- function(x,
                           aggregate = c('none', 'total', 'mean', 'median')) {

    ## entry control

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    aggregate <- match.arg(aggregate[1],
                           c('none', 'mean', 'median', 'total'))

    ## calculation

    if(aggregate == 'total') {

      speeds <- purrr::map_dbl(x, speed)

      return(tibble::tibble(id = names(speeds),
                            value = speeds))

    }

    speeds <- purrr::map(x, subtracks, i = 1)

    speeds <- purrr::map(speeds, ~purrr::map_dbl(.x, speed))

    speeds <- purrr::map(speeds, ~tibble::tibble(i = 1:length(.x),
                                                 value = .x))

    speeds <- purrr::map2_dfr(speeds, names(speeds),
                              ~dplyr::mutate(.x, id = .y))[c('id', 'i', 'value')]

    if(aggregate == 'none') return(speeds)

    speeds <- dplyr::group_by(speeds, id)

    sum_fun <- switch(aggregate,
                      mean = function(x) mean(x, na.rm = TRUE),
                      median = function(x) median(x, na.rm = TRUE))

    dplyr::summarise(speeds, value = sum_fun(value))


  }

# Straightness ------

#' Calculate track straightness.
#'
#' @description Calculates track straightness as described in
#' \code{\link[celltrackR]{TrackMeasures}}
#' @param x a trax object.

  track_straightness <- function(x) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    stats <- purrr::map_dbl(x, straightness)

    tibble::tibble(id = names(stats),
                   value = stats)

  }

#' Calculate track asphericity.
#'
#' @description Calculates track asphericity as described in
#' \code{\link[celltrackR]{TrackMeasures}}
#' @param x a trax object.

  track_asphericity <- function(x) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    stats <- purrr::map_dbl(x, asphericity)

    tibble::tibble(id = names(stats),
                   value = stats)

  }

# Angles --------

#' Calculate track overall angle.
#'
#' @description Calculates track overall angle as described in
#' \code{\link[celltrackR]{TrackMeasures}}
#' @param x a trax object.
#' @param ... other arguments passed to \code{\link[celltrackR]{overallAngle}}

  track_overall_angles <- function(x, ...) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    stats <- purrr::map_dbl(x, overallAngle, ...)

    tibble::tibble(id = names(stats),
                   value = stats)

  }

#' Calculate track mean angle.
#'
#' @description Calculates track mean turning angle as described in
#' \code{\link[celltrackR]{TrackMeasures}}
#' @param x a trax object.
#' @param ... other arguments passed to
#' \code{\link[celltrackR]{meanTurningAngle}}

  track_mean_angles <- function(x, ...) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    stats <- purrr::map_dbl(x, meanTurningAngle, ...)

    tibble::tibble(id = names(stats),
                   value = stats)

  }

# Dot products --------

#' Calculate overall dot product of displacement vectors.
#'
#' @description Calculates track overall dot product of displacement vectors
#' as described in \code{\link[celltrackR]{TrackMeasures}}
#' @param x a trax object.
#' @param ... other arguments passed to \code{\link[celltrackR]{overallDot}}

  track_overall_dots <- function(x, ...) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    stats <- purrr::map_dbl(x, overallDot, ...)

    tibble::tibble(id = names(stats),
                   value = stats)

  }

#' Calculate normal overall dot product of displacement vectors.
#'
#' @description Calculates track normal overall dot product of displacement
#' vectors as described in \code{\link[celltrackR]{TrackMeasures}}
#' @param x a trax object.
#' @param ... other arguments passed to \code{\link[celltrackR]{overallNormDot}}

  track_normal_dots <- function(x, ...) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    stats <- purrr::map_dbl(x, overallNormDot, ...)

    tibble::tibble(id = names(stats),
                   value = stats)

  }

# Motility testing ------

#' Test for active cell mobility.
#'
#' @description The movement of cells without active motility is expected to be
#' well described by a simple Gaussian model. If the motility is active,
#' at least two Gaussian distributions are required to accomplish that.
#' The function compares the one - versus two - Gaussian models applied to
#' each track by calculating difference in BIC (Bayesian Information Criterion).
#' @details See: \code{\link{bic_position}}, \code{\link{detlaBIC}} and
#' https://cran.rstudio.com/web/packages/celltrackR/vignettes/QC.html. The
#' function can calculate the delta BIC statistic for tracks with 4 steps or more,
#' others are ignored and a warning raised.
#' @param x a trax object.
#' @param dims dimensions used for computation of the statistic.
#' @param sigma SD of the Gaussian distribution. The approximate cell diameter
#' is a good starting point here.
#' @return A tibble with the delta BIC values (one- vs two-Gaussian model).

  track_bic_motility <- function(x,
                                 sigma = 10,
                                 dims = c('x', 'y')) {

    ## entry control

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    stopifnot(is.numeric(sigma))

    if(any(!dims %in% c('x', 'y', 'z'))) {

      stop('dims must be a pair from the x, y, z set.', call. = FALSE)

    }

    if(any(purrr::map_dbl(x, nrow) < 4)) {

      warning('There are tracks with < 4 steps in the trax object.
              They will be removed from the analysis.',
              call. = FALSE)

      x <- filter_steps(x, min_steps = 4)

    }

    ## computation

    tst_res <- purrr::map_dbl(x,
                              ~celltrax:::deltaBIC(track = .x,
                                                   sigma = sigma,
                                                   dims = dims))

    tibble::tibble(id = names(tst_res),
                   value = tst_res)

  }


# END ----
