# S3 methods for the trax class. the majority of them is simply inherited
# from the 'tracks' class


# Concatenation ------

#' Concatenate two or more trax objects.
#'
#' @description Concatenates trax object into a larger trax object.
#' @param ... one or more trax objects.
#' @return a trax object.
#' @export

  c.trax <- function(...) {

    longer_track <- celltrackR:::c.tracks(...)

    trax(longer_track)

  }

# Subsetting -----

#' Subset a trax object.
#'
#' @description Subsets a trax object in a list-like manner.
#' @param x a trax object.
#' @param y an index or index vector.
#' @return a trax object.
#' @export

  `[.trax` <- function(x, y) {

    as_trax.list(as.list(x)[y])

  }

# Filtering generic and method -------

#' Filter a trax object content using the tidyverse API.
#'
#' @description Filters a trax object by logical criteria applied to 'id', 'x',
#' 'y', 'z' or 't' parameters.
#' @param .data a trax object.
#' @param ... expressions that return a logical value used for the record
#' selection.
#' @param .preserve logical, ignored.
#' @param rm_empty logical, should empty tracks be removed?
#' @return a trax object.
#' @export filter.trax
#' @export

  filter.trax <- function(.data, ..., .preserve = FALSE, rm_empty = TRUE) {

    stopifnot(is_trax(.data))
    stopifnot(is.logical(.preserve))
    stopifnot(is.logical(rm_empty))

    tr_df <- as.data.frame(.data)

    tr_df <- dplyr::filter(tr_df, ..., .preserve = .preserve)

    tr_obj <- celltrackR:::as.tracks.data.frame(x = tr_df)

    if(rm_empty) {

      tr_obj <- filterTracks(function(x) nrow(x) > 0, tr_obj)

    }

    trax(tr_obj)

  }

# Printing ---------

#' Print a trax object heading.
#'
#' @description Prints first names of the trax object tracks, time step and
#' the total number of tracks.
#' @param x a trax object.
#' @param ... extra arguments passed to methods.
#' @export

  print.trax <- function(x, ...) {

    stopifnot(is_trax(x))

    n_tracks <- length(x)

    head_tracks <- head(names(x))

    dt <- timeStep(x)

    cat(paste0('trax object with n = ',
               n_tracks,
               ' tracks: ',
               paste(head_tracks, collapse = ', '),
               ', time step = ', dt))



  }

# Summary: key track statistics -------

#' Summary of the trax object track measures.
#'
#' @description Calculates key track-wise statistics, see Details.
#' @return A tibble with the statistic values for each track.
#' @details stat names are as follows:
#' 'steps': number of steps per track (\code{\link{track_steps}}),
#' 'length': track length (\code{\link{track_lengths}}),
#' 'duration': track duration (\code{\link{track_duration}}),
#' 'delta_BIC': Gaussian motility test (\code{\link{track_bic_motility}}),
#' 'total_displacement': total track displacements (\code{\link{track_displacements}}),
#' 'mean_displacement': mean track displacements (\code{\link{track_displacements}}),
#' 'median_displacement': median track displacements (\code{\link{track_displacements}}),
#' 'mean_speed': track mean speeds (\code{\link{track_speeds}}),
#' 'median_speed': track median speeds (\code{\link{track_speeds}}),
#' 'overall_angle': track overall angles (\code{\link{track_overall_angles}}),
#' 'mean_angle': track mean turning angles (\code{\link{track_mean_angles}}),
#' 'overall_dot': overall dot products (\code{\link{track_overall_dots}})
#' 'normal_dot': normal dot products (\code{\link{track_normal_dots}}),
#' 'straightness': track straightness (\code{\link{track_straightness}}),
#' 'asphericity: track asphericity (\code{\link{track_asphericity}})),
#' 'x_displ', 'y_displ', 'z_displ': total displacements for each track
#' (\code{\link{get_total_vectors}}).
#' @param object a trax object.
#' @param ... additional arguments passed to \code{\link{track_bic_motility}}
#' @export summary.trax
#' @export

  summary.trax <- function(object, ...) {

    stopifnot(is_trax(object))

    res <- list(steps = track_steps(object),
                length = track_lengths(object),
                duration = track_duration(object),
                delta_BIC = track_bic_motility(object, ...),
                total_displacement = track_displacements(object, aggregate = 'total'),
                mean_displacement = track_displacements(object, aggregate = 'mean'),
                median_displacement = track_displacements(object, aggregate = 'median'),
                mean_speed = track_speeds(object, aggregate = 'mean'),
                median_speed = track_speeds(object, aggregate = 'median'),
                overall_angle = track_overall_angles(object),
                mean_angle = track_mean_angles(object),
                overall_dot = track_overall_dots(object),
                normal_dot = track_normal_dots(object),
                straightness = track_straightness(object),
                asphericity = track_asphericity(object))

    stats <- purrr::reduce(res, dplyr::full_join, by = 'id')

    stats <- rlang::set_names(stats, c('id', names(res)))

    disps <- get_total_vectors(object, simplify = TRUE)

    names(disps)[2:ncol(disps)] <- paste0(names(disps)[2:ncol(disps)],
                                          '_displ')

    dplyr::full_join(stats, disps, by = 'id')

  }

# Plotting -------

#' Plot the trax object tracks, hotellings vectors or statistics.
#'
#' @description Baustelle

# END -----
