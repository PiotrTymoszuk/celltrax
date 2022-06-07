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
#' @description Generates a requested plot type for the trax object.
#' @details Types and corresponding functions:
#' 'tracks' (default): plots single tracks as specified
#' in \code{\link{plot_tracks}},
#' 'vectors': plots total displacement vectors with \code{\link{plot_vectors}},
#' 'hotellings': generates a hotellings plot with \code{\link{plot_hotellings}},
#' 'autocov': autocovariance plot with \code{\link{plot_autocov}},
#' 'statistic': tracks' statistic distribution with \code{\link{plot_statistic}},
#' 'stat_pair': tracks' statistic pair as a point plot
#' with \code{\link{plot_stat_pair}},
#' 'pair_analysis': cell or step pair distances and angles
#' as specified in \code{\link{plot_pair_analysis}}.
#' @param x a trax object.
#' @param type plot type, see: Details. 'tracks' by default.
#' @param normalize logica: should the tracks be normalized prior to plotting?
#' @param coverage fraction of the tracks to be presented in the plot. If 1 all
#' tracks are presented. If < 1, a random sample of tracks of the given size
#' is presented.
#' @param plot_title plot title.
#' @param plot_subtitle plot_subtitle.
#' @param cust_theme custom ggplot2 theme.
#' @param ... extra arguments passed to downstream functions, see: Details.
#' @export plot.trax
#' @export

  plot.trax <- function(x,
                        type = c('tracks', 'vectors',
                                 'hotellings', 'autocov',
                                 'statistic', 'stat_pair',
                                 'pair_analysis'),
                        normalize = FALSE,
                        coverage = 1,
                        plot_title = NULL,
                        plot_subtitle = NULL,
                        cust_theme = theme_trax(), ...) {

    stopifnot(is_trax(x))
    stopifnot(is.logical(normalize))

    type <- match.arg(type[1],
                      c('tracks', 'vectors',
                        'hotellings', 'autocov',
                        'statistic', 'stat_pair',
                        'pair_analysis'))

    ## plotting

    end_plot <- switch(type,
                       tracks = plot_tracks(x,
                                            normalize = normalize,
                                            coverage = coverage, ...),
                       vectors = plot_vectors(x, normalize = normalize,
                                              coverage = coverage, ...),
                       hotellings = plot_hotellings(x, ...),
                       autocov = plot_autocov(x,
                                              coverage = coverage, ...),
                       statistic = plot_statistic(x, ...),
                       stat_pair = plot_stat_pair(x,
                                                  coverage = coverage, ...),
                       pair_analysis = plot_pair_analysis(x,
                                                          coverage = coverage, ...))

    end_plot +
      labs(title = plot_title,
           subtitle = plot_subtitle) +
      cust_theme

  }

# Statistic calculation -------

#' Calculate track measures.
#'
#' @description Calculates the requested track statistic for a trax object.
#' @details Types of returned statistics:
#' 'steps' (default): number of steps per track (\code{\link{track_steps}}),
#' 'lengths': track length (\code{\link{track_lengths}}),
#' 'times': track's step time intervals (\code{\link{track_times}}),
#' 'duration': track duration (\code{\link{track_duration}}),
#' 'displacements': track displacements (\code{\link{track_displacements}}),
#' 'speeds': track speeds (\code{\link{track_speeds}}),
#' 'overall_angles': track overall angles (\code{\link{track_overall_angles}}),
#' 'mean_angles': track mean turning angles (\code{\link{track_mean_angles}}),
#' 'overall_dots': overall dot products (\code{\link{track_overall_dots}})
#' 'normal_dots': normal dot products (\code{\link{track_normal_dots}}),
#' 'straightness': track straightness (\code{\link{track_straightness}}),
#' 'asphericity: track asphericity (\code{\link{track_asphericity}}),
#' 'delta_BIC': track's elements Gaussian modeling outcome
#' (\code{\link{track_bic_motility}}),
#' 'autocov': autocovariance (\code{\link{autocov}})
#' 'hotellings': results of hotellings test (\code{\link{hotellings_test}}),
#' 'cell_pairs': cell painr distance and angles (\code{\link{analyzeCellPairs}}),
#' 'step_pairs': step pair distance and angles (\code{\link{analyzeStepPairs}}).
#' @param x a trax object.
#' @param type statistic type, see: Details. 'steps' by default.
#' @param ... additional arguments to statiscit computing function, see: Details.
#' @return a tibble with the requested statistic, track id
#' and optionally step id.
#' @export calculate.trax
#' @export

  calculate.trax <- function(x,
                             type = c('steps', 'lengths',
                                      'times', 'duration',
                                      'displacements', 'speeds',
                                      'overall_angles', 'mean_angles',
                                      'overall_dots', 'normal_dots',
                                      'straightness', 'asphericity',
                                      'delta_BIC', 'autocov',
                                      'hotellings', 'cell_pairs',
                                      'step_pairs'),
                             ...) {

    stopifnot(is_trax(x))

    type = match.arg(type[1],
                     c('steps', 'lengths',
                       'times', 'duration',
                       'displacements', 'speeds',
                       'overall_angles', 'mean_angles',
                       'overall_dots', 'normal_dots',
                       'straightness', 'asphericity',
                       'delta_BIC', 'autocov',
                       'hotellings', 'cell_pairs',
                       'step_pairs'))

    switch(type,
           steps = track_steps(x),
           lengths = track_lengths(x),
           times = track_times(x),
           duration = track_duration(x),
           displacements = track_displacements(x, ...),
           speeds = track_speeds(x, ...),
           overall_angles = track_overall_angles(x, ...),
           normal_dots = track_normal_dots(x, ...),
           straightness = track_straightness(x),
           asphericity = track_asphericity(x),
           delta_BIC = track_bic_motility(x, ...),
           autocov = autocov(x, ...),
           hotellings = hotellings_test(x, ...),
           cell_pairs = analyzeCellPairs(x, ...),
           step_pairs = analyzeStepPairs(x, ...))

  }

# END -----
