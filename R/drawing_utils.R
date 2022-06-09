# Non-exported drawing utils, accessible via plot.trax.

# Basic track ggplot -----

#' Draw single tracks.
#'
#' @description Draws single tracks given a trax object.
#' @param x a trax object.
#' @param dims a pair of dimensions to be plotted.
#' @param normalize logical, should the tracks be normalized?
#' @param coverage fraction of the tracks to be presented in the plot. If 1 all
#' tracks are presented. If < 1, a random sample of tracks of the given size
#' is presented.
#' @param color color name or value. If NULL (default), each path is colored
#' in a default ggplot2 style and can be modified by adjusting manual color
#' scale, see: \code{\link[ggplot2]{scale_color_manual}} for details.
#' @param show_zero logical, should the X = 0 and Y = 0 lines be plotted?
#' @param point_size point size.
#' @param point_alpha point alpha.
#' @param line_size line width.
#' @param line_alpha line alpha.
#' @param end_size endpoint size.
#' @param legend form of the color legend, as specified in
#' \code{\link[ggplot2]{guides}}. Defaults to 'none'.
#' @param seed seed value for random number generation. Ignored if coverage = 1.
#' @return a ggplot2 object.

  plot_tracks <- function(x,
                          dims = c('x', 'y'),
                          normalize = FALSE,
                          coverage = 1,
                          color = NULL,
                          show_zero = FALSE,
                          point_size = 1,
                          point_alpha = 1,
                          line_size = 0.5,
                          line_alpha = 1,
                          end_size = 4,
                          legend = 'none',
                          seed = NULL) {

    ## entry control

    if(!is_trax(x)) stop("'x' needs to be a 'trax' object.", call. = FALSE)

    if(any(!dims %in% c('x', 'y', 'z'))) {

      stop('dims must be a pair from the x, y, z set.', call. = FALSE)

    }

    if(length(dims) != 2) {

      stop('dims must be a pair from the x, y, z set.', call. = FALSE)

    }

    stopifnot(is.logical(show_zero))
    stopifnot(is.numeric(point_size))
    stopifnot(is.numeric(point_alpha))
    stopifnot(is.numeric(line_size))
    stopifnot(is.numeric(line_alpha))
    stopifnot(is.numeric(end_size))
    stopifnot(is.numeric(coverage))

    if(coverage <= 0 | coverage > 1) {

      stop('coverage needs to be (0, 1]', call. = FALSE)

    }

    plot_tag <- paste('\nn =', length(x))

    ## plotting table

    if(normalize) x <- normalizeTracks(x)

    if(coverage < 1) {

      size <- floor(coverage * length(x))

      x <- sample_trax(x, size = size, replace = FALSE, seed = seed)

    }

    plot_tbl <- as.data.frame(x)

    end_table <- plyr::ddply(plot_tbl, 'id', function(x) x[nrow(x), ])

    ## plotting

    if(is.null(color)) {

      base_plot <- ggplot(plot_tbl,
                          aes(x = .data[[dims[[1]]]],
                              y = .data[[dims[[2]]]],
                              color = id))

      if(show_zero) {

        base_plot <- base_plot +
          geom_hline(yintercept = 0, linetype = 'dashed') +
          geom_vline(xintercept = 0, linetype = 'dashed')

      }

      base_plot <- base_plot +
        geom_point(data = end_table,
                   size = end_size,
                   shape = 1) +
        geom_path(aes(group = id),
                  size = line_size,
                  alpha = line_alpha) +
        geom_point(shape = 16,
                   size = point_size,
                   alpha = point_alpha) +
        guides(color = legend)

    } else {

      base_plot <- ggplot(plot_tbl,
                          aes(x = .data[[dims[[1]]]],
                              y = .data[[dims[[2]]]]))

      if(show_zero) {

        base_plot <- base_plot +
          geom_hline(yintercept = 0, linetype = 'dashed') +
          geom_vline(xintercept = 0, linetype = 'dashed')

      }

      base_plot <- base_plot +
        geom_point(data = end_table,
                   size = end_size,
                   shape = 1,
                   color = color) +
        geom_path(aes(group = id),
                  size = line_size,
                  alpha = line_alpha,
                  color = color) +
        geom_point(shape = 16,
                   size = point_size,
                   alpha = point_alpha,
                   color = color)

    }

    base_plot +
      labs(tag = plot_tag)

  }

# Vector field track plot ------

#' Draw total displacement vectors.
#'
#' @description Draws total displacement vectors for each track.
#' @param x a trax object.
#' @param dims a pair of dimensions to be plotted.
#' @param normalize logical, should the tracks be normalized?
#' @param coverage fraction of the tracks to be presented in the plot. If 1 all
#' tracks are presented. If < 1, a random sample of tracks of the given size
#' is presented.
#' @param color color name or value. If NULL (default), each path is colored
#' in a default ggplot2 style and can be modified by adjusting manual color
#' scale, see: \code{\link[ggplot2]{scale_color_manual}} for details.
#' @param mean_color color of the mean vector. If NULL, no mean vector is
#' displayed. Ignored if normalize = FALSE.
#' @param mean_size width of the mean vector line.
#' @param show_zero logical, should the X = 0 and Y = 0 lines be plotted?
#' @param line_size line width.
#' @param line_alpha line alpha.
#' @param arrow arrow specification for each displacement vector,
#' as created by \code{\link[grid]{arrow}}.
#' @param arrow arrow specification for the mean vector,
#' as created by \code{\link[grid]{arrow}}.
#' @param legend form of the color legend, as specified in
#' \code{\link[ggplot2]{guides}}. Defaults to 'none'.
#' @param seed seed value for random number generation. Ignored if coverage = 1.
#' @return a ggplot2 object.

  plot_vectors <- function(x,
                           dims = c('x', 'y'),
                           normalize = FALSE,
                           coverage = 1,
                           color = NULL,
                           mean_color = 'orangered3',
                           mean_size = 0.75,
                           show_zero = TRUE,
                           line_size = 0.5,
                           line_alpha = 1,
                           arrow = grid::arrow(angle = 30,
                                               length = grid::unit(3, 'points'),
                                               ends = 'last',
                                               type = 'open'),
                           mean_arrow = grid::arrow(angle = 45,
                                                    length = grid::unit(12, 'points'),
                                                    ends = 'last',
                                                    type = 'open'),
                           legend = 'none',
                           seed = NULL) {

    ## entry control

    if(!is_trax(x)) stop("'x' needs to be a 'trax' object.", call. = FALSE)

    if(any(!dims %in% c('x', 'y', 'z'))) {

      stop('dims must be a pair from the x, y, z set.', call. = FALSE)

    }

    if(length(dims) != 2) {

      stop('dims must be a pair from the x, y, z set.', call. = FALSE)

    }

    stopifnot(is.logical(show_zero))
    stopifnot(is.numeric(line_alpha))
    stopifnot(is.numeric(line_size))
    stopifnot(is.numeric(coverage))
    stopifnot(is.numeric(mean_size))

    if(coverage <= 0 | coverage > 1) {

      stop('coverage needs to be (0, 1]', call. = FALSE)

    }

    plot_tag <- paste('\nn =', length(x))

    ## plotting table

    if(normalize) x <- normalizeTracks(x)

    if(!is.null(mean_color)) mean_vec <- get_mean_vector(x = x)

    if(coverage < 1) {

      size <- floor(coverage * length(x))

      x <- sample_trax(x, size = size, replace = FALSE, seed = seed)

    }

    plot_tbl <- as.data.frame(get_total_vectors(x = x))

    ## plotting

    if(is.null(color)) {

      base_plot <- ggplot(plot_tbl,
                          aes(x = .data[[dims[1]]],
                              y = .data[[dims[2]]],
                              color = id))

      if(show_zero) {

        base_plot <- base_plot +
          geom_hline(yintercept = 0, linetype = 'dashed') +
          geom_vline(xintercept = 0, linetype = 'dashed')

      }

      base_plot <- base_plot +
        geom_path(aes(group = id),
                  size = line_size,
                  alpha = line_alpha,
                  arrow = arrow) +
        guides(color = legend)

    } else {

      base_plot <- ggplot(plot_tbl,
                          aes(x = .data[[dims[1]]],
                              y = .data[[dims[2]]]))

      if(show_zero) {

        base_plot <- base_plot +
          geom_hline(yintercept = 0, linetype = 'dashed') +
          geom_vline(xintercept = 0, linetype = 'dashed')

      }

      base_plot <- base_plot +
        geom_path(aes(group = id),
                  size = line_size,
                  alpha = line_alpha,
                  arrow = arrow,
                  color = color)

    }

    if(!normalize | is.null(mean_color)) return(base_plot)

    base_plot +
      geom_path(data = as.data.frame(mean_vec),
                size = mean_size,
                color = mean_color,
                arrow = arrow) +
      labs(tag = plot_tag)

  }

# Hotellings plot -------

#' Draw a 2D hotellings plot.
#'
#' @description Draws a two-dimensional hotellings plot with the confidence
#' interval ellipsis and mean displacement.
#' @details a wrapper around \code{\link[celltrackR]{hotellingsTest}}.
#' @param x a trax object.
#' @param dims a pair of dimensions to be plotted.
#' @param step_spacing how many positions are to be left out between the steps
#' that are considered for the test,
#' see: \code{\link[celltrackR]{hotellingsTest}} for details.
#' @param conf_level confidence level.
#' @param point_alpha alpha of points.
#' @param color point color.
#' @param ellipse_border ellipse border color.
#' @param ellipse_color ellipse fill color.
#' @param ellipse_alpha ellipse alpha
#' @param center_color ellipse center color.
#' @param center_shape shape of the ellipse center point.
#' @param center_size size of the ellipse center point.

  plot_hotellings <- function(x,
                              dims = c('x', 'y'),
                              step_spacing = 0,
                              conf_level = 0.95,
                              color = 'steelblue',
                              point_alpha = 0.25,
                              ellipse_border = 'coral4',
                              ellipse_color = 'coral3',
                              ellipse_alpha = 0.25,
                              center_color = 'coral4',
                              center_shape = 16,
                              center_size = 3) {

    ## entry control

    if(!is_trax(x)) stop("'x' needs to be a 'trax' object.", call. = FALSE)

    if(any(!dims %in% c('x', 'y', 'z'))) {

      stop('dims must be a pair from the x, y, z set.', call. = FALSE)

    }

    if(length(dims) != 2) {

      stop('dims must be a pair from the x, y, z set.', call. = FALSE)

    }

    stopifnot(is.numeric(step_spacing))
    step_spacing <- as.integer(step_spacing)

    stopifnot(is.numeric(conf_level))
    stopifnot(is.numeric(point_alpha))
    stopifnot(is.numeric(ellipse_alpha))
    stopifnot(is.numeric(center_size))

    plot_tag <- paste('\nn =', length(x))

    ## plotting data: I'm leaving the author's code
    ## to generate the plotting data untouched

    Tx <- projectDimensions(x, dims)

    sx <- sapply(subtracks(Tx, 1, overlap = -step_spacing),
                 celltrackR::displacementVector)

    sx <- t(sx)

    n <- dim(sx)[1]
    p <- dim(sx)[2]

    df.1 <- p
    df.2 <- n - p

    S <- stats::cov(sx)
    mX <- colMeans(sx)

    t <- sqrt(((n - 1) * df.1/(n * df.2)) * stats::qf(1 - conf_level,
                                                      df.1,
                                                      df.2,
                                                      lower.tail = FALSE))

    ## plotting

    plot_tbl <- as.data.frame(sx)

    plot_tbl <- rlang::set_names(plot_tbl, dims)

    data_ellip <- ellipse::ellipse(S,
                                   centre = mX,
                                   t = t)

    data_ellip <- as.data.frame(data_ellip)

    ggplot(plot_tbl,
           aes(x = .data[[dims[1]]],
               y = .data[[dims[2]]])) +
      geom_point(shape = 16,
                 size = 2,
                 alpha = point_alpha,
                 color = color) +
      geom_hline(yintercept = 0,
                 linetype = 'dashed') +
      geom_vline(xintercept = 0,
                 linetype = 'dashed') +
      geom_polygon(data = data_ellip,
                   color = ellipse_border,
                   alpha = ellipse_alpha,
                   fill = ellipse_color) +
      geom_point(data = tibble::tibble(x0 = mX[1],
                                       y0 = mX[2]),
                 aes(x = x0,
                     y = y0),
                 shape = center_shape,
                 size = center_size,
                 color = center_color) +
      labs(tag = plot_tag)

  }

# Autocovariance plot ------

#' Draw autocovariance of displacement vectors.
#'
#' @description Plots step-wise values of autocovariance of displacement
#' vectors.
#' @details The autocovariance is calculated as a dot product of the vectors
#' returned by \code{\link[celltrackR]{overallDot}} or overall angle computed
#' by \code{\link[celltrackR]{overallAngle}}
#' @param x a trax object.
#' @param method the method of autocovariance calculation: 'dot_product'
#' (default) or 'angle', as descibed in details.
#' @param coverage fraction of the tracks to be presented in the plot. If 1 all
#' tracks are presented. If < 1, a random sample of tracks of the given size
#' is presented.
#' @param color color name or value for the single tracks' covariances.
#' If NULL (default), single covariance paths are not shown.
#' @param mean_color color of the mean vector autocovariance line.
#' @param mean_size width of the mean vector autocovariance line.
#' @param line_size line width for single track autocovariances.
#' @param line_alpha line alpha for single track autocovariances.
#' @param point_size size of the points representing the mean autocovariances.
#' @param seed seed value for random number generation. Ignored if coverage = 1.
#' @return a ggplot2 object.

  plot_autocov <- function(x,
                           method = c('dot_product', 'angle'),
                           coverage = 1,
                           color = NULL,
                           mean_color = 'orangered3',
                           mean_size = 0.75,
                           line_size = 0.5,
                           line_alpha = 0.5,
                           point_size = 2,
                           seed = NULL) {

    ## entry control

    if(!is_trax(x)) stop("'x' needs to be a 'trax' object.", call. = FALSE)

    stopifnot(is.numeric(line_alpha))
    stopifnot(is.numeric(line_size))
    stopifnot(is.numeric(coverage))
    stopifnot(is.numeric(mean_size))

    if(coverage <= 0 | coverage > 1) {

      stop('coverage needs to be (0, 1]', call. = FALSE)

    }

    method <- match.arg(method, c('dot_product', 'angle'))

    plot_tag <- paste('\nn =', length(x))

    ## plotting data

    mean_tbl <- autocov(x, method = method, aggregate = TRUE)

    if(coverage < 1) {

      size <- floor(coverage * length(x))

      x <- sample_trax(x, size = size, replace = FALSE, seed = seed)

    }

    if(!is.null(color)) {

      plot_tbl <- autocov(x, method = method, aggregate = FALSE)

    }

    ## plotting

    if(is.null(color)) {

      base_plot <- ggplot(mean_tbl,
                          aes(x = i,
                              y = value)) +
        geom_path(color = mean_color,
                  size = mean_size) +
        geom_point(shape = 16,
                   color = mean_color,
                   size = point_size)

    } else {

      base_plot <- ggplot(plot_tbl,
                          aes(x = i,
                              y = value)) +
        geom_path(aes(group = id),
                  color = color,
                  size = line_size,
                  alpha = line_alpha) +
        geom_path(data = mean_tbl,
                  color = mean_color,
                  size = mean_size) +
        geom_point(data = mean_tbl,
                   shape = 16,
                   color = mean_color,
                   size = point_size)

    }

    base_plot +
      labs(y = 'Auctocovariance coefficient',
           x = 'Step, i',
           tag = plot_tag)

  }

# Track statistic distribution plots -------

#' Plot track measure.
#'
#' @description Plots distribution of various track statistics as histograms,
#' violin or box plots.
#' @details 'steps' (default): number of steps per track (\code{\link{track_steps}}),
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
#' (\code{\link{track_bic_motility}}).
#' @param x a trax object.
#' @param stat statistic name as specified in Details,
#' @param geom plot form: 'histogram' (default), 'violin' or 'boxplot'.
#' @param color shape color.
#' @param point_color point color.
#' @param point_size point size.
#' @param point_alpha point alpha.
#' @param point_jitter point jitter width.
#' @param shape_alpha shape alpha.
#' @param seed seed value for random number generation. Ignored if coverage = 1.
#' @param bins number of histogram bins.
#' @param scale violin plot area/width scaling (\code{\link[ggplot2]{geom_violin}}).
#' @param draw_quantiles violin plot quantiles (\code{\link[ggplot2]{geom_violin}}).
#' @param ... extra arguments to the statistic calculation functions as
#' specified in Details.
#' @return a ggplot2 object.

  plot_statistic <- function(x,
                             stat = c('steps', 'lengths',
                                      'times', 'duration',
                                      'displacements', 'speeds',
                                      'overall_angles', 'mean_angles',
                                      'overall_dots', 'normal_dots',
                                      'straightness', 'asphericity',
                                      'delta_BIC'),
                             geom = c('histogram', 'violin', 'boxplot'),
                             color = 'steelblue',
                             point_color = color,
                             point_size = 2,
                             point_alpha = 0.5,
                             point_jitter = 0.1,
                             shape_alpha = 0.25,
                             bins = NULL,
                             scale = 'width',
                             draw_quantiles = c(0.25, 0.5, 0.75), ...) {

    ## entry control

    if(!is_trax(x)) stop("'x' needs to be a 'trax' object.", call. = FALSE)

    geom <- match.arg(geom[1],
                      c('histogram', 'violin', 'boxplot'))

    stat <- match.arg(stat[1],
                      c('steps', 'lengths',
                        'times', 'duration',
                        'displacements', 'speeds',
                        'overall_angles', 'mean_angles',
                        'overall_dots', 'normal_dots',
                        'straightness', 'asphericity',
                        'delta_BIC'))

    stopifnot(is.numeric(point_size))
    stopifnot(is.numeric(point_alpha))
    stopifnot(is.numeric(point_jitter))
    stopifnot(is.numeric(shape_alpha))

    plot_tag <- paste('\nn =', length(x))

    ## plot axes

    ax_titles <- list(steps = '# steps',
                      lengths = 'Track length',
                      times = 'Step duration',
                      duration = 'Track duration',
                      displacements = 'Displacement',
                      speeds = 'Speed',
                      overall_angles = 'Overall angle',
                      mean_angles = 'Mean turning angle',
                      overall_dots = 'Overall displacement dot product',
                      normal_dots = 'Overall normal displacement dot product',
                      straightness = 'Straightness',
                      asphericity = 'Asphericity',
                      delta_BIC = 'Motility, \u0394 BIC')

    ## data table

    plot_tbl <- switch(stat,
                       steps = track_steps(x),
                       lengths = track_lengths(x),
                       times = track_times(x),
                       duration = track_duration(x),
                       displacements = track_displacements(x, ...),
                       speeds = track_speeds(x, ...),
                       overall_angles = track_overall_angles(x, ...),
                       mean_angles = track_mean_angles(x, ...),
                       overall_dots = track_overall_dots(x, ...),
                       normal_dots = track_normal_dots(x, ...),
                       straightness = track_straightness(x),
                       asphericity = track_asphericity(x),
                       delta_BIC = track_bic_motility(x, ...))

    ## plotting

    if(geom == 'histogram') {

      return(ggplot(plot_tbl,
                    aes(x = value)) +
               geom_histogram(color = 'black',
                              fill = color,
                              alpha = shape_alpha,
                              bins = bins) +
               labs(x = ax_titles[stat]))

    } else if(geom == 'violin') {

      base_plot <- ggplot(plot_tbl,
                          aes(x = 'Track',
                              y = value)) +
        geom_violin(color = 'black',
                    fill = color,
                    alpha = shape_alpha,
                    scale = scale,
                    draw_quantiles = draw_quantiles)

    } else {

      base_plot <- ggplot(plot_tbl,
                          aes(x = 'Track',
                              y = value)) +
        geom_boxplot(color = 'black',
                     fill = color,
                     alpha = shape_alpha,
                     outlier.color = NA)


    }

    base_plot +
      geom_point(shape = 16,
                 color = point_color,
                 size = point_size,
                 alpha = point_alpha,
                 position = position_jitter(width = point_jitter)) +
      labs(y = ax_titles[stat],
           x = '',
           tag = plot_tag)

  }

# Two statistic plot ------

#' Plot a pair of track measures.
#'
#' @description Generates a point plot with a pair of track statistics,
#' see: Details.
#' @details 'steps' (default): number of steps per track (\code{\link{track_steps}}),
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
#' (\code{\link{track_bic_motility}}).
#' @param x a trax object.
#' @param x_stat statistic to be presented on the X axis, see Details.
#' @param y_stat statistic to be presented on the Y axis, see Details.
#' @param coverage fraction of the tracks to be presented in the plot. If 1 all
#' tracks are presented. If < 1, a random sample of tracks of the given size
#' is presented.
#' @param color point color.
#' @param point_size point size.
#' @param point_alpha point alpha.
#' @param point_jitter point jitter width.
#' @param seed seed value for random number generation. Ignored if coverage = 1.
#' @param x_args extra arguments to the X statistic calculation functions as
#' specified in Details. Provided as a named list.
#' @param y_args extra arguments to the Y statistic calculation functions as
#' specified in Details. Provided as a named list.
#' @return a ggplot2 object.

  plot_stat_pair <- function(x,
                             x_stat = 'mean_angles',
                             y_stat = 'normal_dots',
                             coverage = 1,
                             color = 'steelblue',
                             point_size = 2,
                             point_alpha = 0.5,
                             point_jitter = 0,
                             seed = NULL,
                             x_args = rlang::list2(),
                             y_args = rlang::list2()) {

    ## entry control

    if(!is_trax(x)) stop("'x' needs to be a 'trax' object.", call. = FALSE)

    stopifnot(is.numeric(point_size))
    stopifnot(is.numeric(point_alpha))
    stopifnot(is.numeric(coverage))
    stopifnot(is.numeric(point_jitter))

    if(coverage <= 0 | coverage > 1) {

      stop('coverage needs to be (0, 1]', call. = FALSE)

    }

    x_stat <- match.arg(x_stat,
                        c('steps', 'lengths',
                          'times', 'duration',
                          'displacements', 'speeds',
                          'overall_angles', 'mean_angles',
                          'overall_dots', 'normal_dots',
                          'straightness', 'asphericity',
                          'delta_BIC'))

    y_stat <- match.arg(y_stat,
                        c('steps', 'lengths',
                          'times', 'duration',
                          'displacements', 'speeds',
                          'overall_angles', 'mean_angles',
                          'overall_dots', 'normal_dots',
                          'straightness', 'asphericity',
                          'delta_BIC'))

    plot_tag <- paste('\nn =', length(x))

    ## data functions and axis titles

    data_funs <- list(steps = track_steps,
                      lengths = track_lengths,
                      times = track_times,
                      duration = track_duration,
                      displacements = track_displacements,
                      speeds = track_speeds,
                      overall_angles = track_overall_angles,
                      mean_angles = track_mean_angles,
                      overall_dots = track_overall_dots,
                      normal_dots = track_normal_dots,
                      straightness = track_straightness,
                      asphericity = track_asphericity,
                      delta_BIC = track_bic_motility)

    ax_titles <- list(steps = '# steps',
                      lengths = 'Track length',
                      times = 'Step duration',
                      duration = 'Track duration',
                      displacements = 'Displacement',
                      speeds = 'Speed',
                      overall_angles = 'Overall angle',
                      mean_angles = 'Mean turning angle',
                      overall_dots = 'Overall displacement dot product',
                      normal_dots = 'Overall normal displacement dot product',
                      straightness = 'Straightness',
                      asphericity = 'Asphericity',
                      delta_BIC = 'Motility, \u0394 BIC')

    ## plotting data

    if(coverage < 1) {

      size <- floor(coverage * length(x))

      x <- sample_trax(x, size = size, replace = FALSE, seed = seed)

    }

    tblx <- eval(rlang::call2(data_funs[[x_stat]], x, !!!x_args))

    tbly <- eval(rlang::call2(data_funs[[y_stat]], x, !!!y_args))

    plot_tbl <- full_join(tblx, tbly, by = 'id')

    ## plotting

    ggplot(plot_tbl,
           aes(x = value.x,
               y = value.y)) +
      geom_point(size = point_size,
                 shape = 16,
                 alpha = point_alpha,
                 color = color,
                 position = position_jitter(width = point_jitter)) +
      labs(x = ax_titles[[x_stat]],
           y = ax_titles[[y_stat]],
           tag = plot_tag)

  }

# Pair analysis plots ------

#' Plot distance and/or angle between cell or step pairs.
#'
#' @description Plots distribution of angle or distance or values of
#' angle versus distance between the cell or step pairs.
#' @return a ggplot object.
#' @param x a cell trax object.
#' @param method analysis method, 'cells' (default) or 'steps'.
#' @param stat statistic to be shown in the plot: 'both' displays the angles
#' and distances, 'angle' or 'dist' produce distribution plots for
#' single statistics.
#' @param geom distribution plot form: 'histogram' (default), 'violin'
#' or 'boxplot'. Ignored if stat argument is 'both'.
#' @param coverage fraction of the tracks to be presented in the plot. If 1 all
#' tracks are presented. If < 1, a random sample of tracks of the given size
#' is presented.
#' @param trend_method method/function for the trend line fitting, as described
#' for the 'method' argument of \code{\link[ggplot2]{geom_smooth}}.
#' If 'none' (default), no trend line is plotted.
#' @param color point color.
#' @param point_alpha point alpha.
#' @param point_size poin size.
#' @param point_jitter jitter width for data points in the distribution plots,
#' ignored if stat is 'both'.
#' @param shape_color color of the shape in the distribution plot.
#' @param shape_alpha alpha of the shape in the distribution plot.
#' @param line_color trend line color.
#' @param line_alpha alpha for the trend confidence interval.
#' @param seed seed value for random number generation. Ignored if coverage = 1.
#' @param analysis_args additional arguments passed to analysis functions
#' \code{\link[celltrackR]{analyzeCellPairs}} or
#' \code{\link[celltrackR]{analyzeStepPairs}}
#' @param ... additional arguments passed to \code{\link[ggplot2]{geom_smooth}},
#' such as sd, step and so on in case, when both stats are plotted. For
#' distribution plots, extra arguments passed to
#' \code{\link[ggplot2]{geom_histogram}}, \code{\link[ggplot2]{geom_violin}} or
#' \code{\link[ggplot2]{geom_boxplot}}.

  plot_pair_analysis <- function(x,
                                 method = c('cells', 'steps'),
                                 stat = c('both', 'dist', 'angle'),
                                 geom = c('histogram', 'violin', 'boxpplot'),
                                 coverage = 1,
                                 trend_method = 'none',
                                 color = 'steelblue',
                                 point_alpha = 0.25,
                                 point_size = 2,
                                 point_jitter = 0.1,
                                 shape_color = color,
                                 shape_alpha = 0.25,
                                 line_size = 0.75,
                                 line_color = 'orangered3',
                                 line_alpha = 0.5,
                                 analysis_args = rlang::list2(),
                                 seed = NULL, ...) {

    ## entry control

    if(!is_trax(x)) stop("'x' needs to be a 'trax' object.", call. = FALSE)

    stopifnot(is.numeric(point_size))
    stopifnot(is.numeric(point_alpha))
    stopifnot(is.numeric(coverage))
    stopifnot(is.numeric(line_size))
    stopifnot(is.numeric(line_alpha))

    if(coverage <= 0 | coverage > 1) {

      stop('coverage needs to be (0, 1]', call. = FALSE)

    }

    method <- match.arg(method[1], c('cells', 'steps'))

    stat <- match.arg(stat[1], c('both', 'dist', 'angle'))

    geom = match.arg(geom[1], c('histogram', 'violin', 'boxpplot'))

    ax_title <- c(angle = 'angle', dist = 'distance')

    plot_tag <- paste('\nn =', length(x))

    ## plotting data

    if(coverage < 1) {

      size <- floor(coverage * length(x))

      x <- sample_trax(x, size = size, replace = FALSE, seed = seed)

    }

    analysis_fun <- switch(method,
                           cells = analyzeCellPairs,
                           steps = analyzeStepPairs)

    plot_tbl <- eval(rlang::call2(analysis_fun, x, !!!analysis_args))

    plot_tbl <- dplyr::filter(plot_tbl, complete.cases(plot_tbl))

    ## plotting

    if(stat == 'both') {

      base_plot <- ggplot(plot_tbl,
                          aes(x = dist,
                              y = angle)) +
        geom_point(shape = 16,
                   size = point_size,
                   color = color,
                   alpha = point_alpha) +
        labs(x = 'distance',
             y = 'angle')

      if(trend_method != 'none') {

        base_plot <- base_plot +
          geom_smooth(color = line_color,
                      alpha = line_alpha,
                      method = trend_method, ...)

      }

    } else if(geom == 'histogram') {

      base_plot <- ggplot(plot_tbl,
                          aes(x = .data[[stat]])) +
        geom_histogram(fill = color,
                       color = 'black', ...) +
        labs(x = ax_title[stat])

    } else if(geom %in% c('violin', 'boxplot')) {

      geom_fun <- switch(geom,
                         violin = geom_violin,
                         boxplot = geom_boxplot)


      base_plot <- ggplot(plot_tbl,
                          aes(x = 'Track',
                              y = .data[[stat]])) +
        geom_fun(fill = shape_color,
                 color = 'black',
                 alpha = shape_alpha, ...) +
        geom_point(shape = 16,
                   size = point_size,
                   alpha = point_alpha,
                   color = color,
                   position = position_jitter(width = point_jitter))
        labs(y = ax_title[stat],
             x = '')

    }

    return(base_plot +
             labs(tag = plot_tag))

  }

# END ----
