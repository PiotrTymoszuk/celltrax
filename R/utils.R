# Non-exported utilities

# Colors -----

#' Choose colors randomly from the grDevice palette.
#'
#' @description Chooses n colors at random from the palette provided by
#' \code{\link[grDevices]{colors}}.
#' @param n number of colors to choose.
#' @param seed seed for the random number generator.
#' @return a character vector with color names.

  set_colors_ <- function(n, seed = NULL) {

    if(!is.numeric(n)) stop('n argument needs to be numeric.', call. = FALSE)

    n <- as.integer(n)

    if(!is.null(seed)) set.seed(seed)

    cols <- grDevices::colors()[grep('gr(a|e)y',
                                     grDevices::colors(),
                                     invert = TRUE)]

    sample(cols, size = color_no)

  }

#' Emulate the default ggplot2 color palette.
#'
#' @description Choose n subsequent colors from the defult ggplot2 palette,
#' @details The idea was presented at
#' https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#' @param n number of subsequent colors.
#' @return a character vector with color names.

  gg_color_hue_ <- function(n) {

    hues = seq(15, 375, length = n + 1)

    hcl(h = hues, l = 65, c = 100)[1:n]

  }

# Motility modeling ------

#' Check if cell motion can be described by multiple Gaussian distributions.
#'
#' @description A simple motility test. Positions of a cell without any active
#' motility (e.g. subjected to Brownian motion) are expected to follow a
#' multi-variate Gaussian distribution around some central point with a
#' sigma standard deviation. If the cell moves actively, the displacements are
#' expected to be a sum of the Gaussian noise and displacement vectors. Hence,
#' more than just one Gaussian is needed to model it's movement. The function
#' tests, based on the Bayesian Information Criterion (BIC), the goodness of fit
#' of the one or two Gaussian model.
#' @details The idea behind it comes from the celltracR package vignette:
#' https://cran.rstudio.com/web/packages/celltrackR/vignettes/QC.html.
#' @param track a single track, e.g. an element of a tracks or trax object.
#' @param dims dimensions used for computation of the statistic.
#' @param sigma SD of the Gaussian distribution.
#' @param m the point, expressed as a step number used for splitting the tracks.
#' The Gaussian mode is fitted to each track subset.
#' @param log_lik logical: if TRUE, log likelihood is returned, otherwise
#' the BIC value.
#' @return a numeric value.

  bic_position <- function(track,
                           sigma,
                           dims = c('x', 'y'),
                           m = NULL,
                           log_lik = FALSE){

    if(!is.null(m)) {

      m <- as.integer(m)

      if(!m %in% 2:(nrow(track) - 2)) {

        stop('The m paramater must be within the (2, nrow(track) - 2) range.',
             call. = FALSE)

      }

    }

    stopifnot(is.logical(log_lik))

    ## calculation

    allPoints <- track[ ,dims]

    if(!is.null(m)) {

      firstCoords <- allPoints[1:m, , drop = FALSE]
      lastCoords <- allPoints[(m + 1):nrow(allPoints), , drop = FALSE]

      logL <- bic_position(firstCoords, sigma = sigma, m = NULL, log_lik = TRUE) +
        bic_position(lastCoords, sigma = sigma, m = NULL, log_lik = TRUE)

      return(6*log(nrow(allPoints)) - 2*logL)

    }

    # Compute the log likelihood under a multivariate gaussian.
    # For each point, we get the density under the Gaussian distribution
    # (using dmvnorm from the mvtnorm package).
    # The product of these densities is then the likelihood;
    # but since we need the log likelihood,
    # we can also first log-transform and then sum:

    Lpoints <- mvtnorm::dmvnorm(allPoints,
                                mean = colMeans(allPoints),
                                sigma = sigma * diag(2),
                                log = TRUE)
    logL <- sum(Lpoints)

    if(log_lik) return(logL)

    # BIC = k log n - 2 logL; here k = 3 (mean x, mean y, sigma)

    return(3*log(nrow(allPoints)) - 2*logL)

  }

#' Check if cell motion can be described by multiple Gaussian distributions.
#'
#' @description Checks if the cell motility may be described by one or two
#' Gaussian models by computing the difference in Bayesian Information
#' Criterion (BIC).
#' @details See: \code{\link{bic_position}}.
#' @param track a single track, e.g. an element of a tracks or trax object.
#' @param dims dimensions used for computation of the statistic.
#' @param sigma SD of the Gaussian distribution.
#' @return a numeric value being the difference in BIC between the one- and
#' two - Gaussian model.

  deltaBIC <- function(track,
                       sigma,
                       dims = c('x', 'y')) {

    ## BIC for the one-Gaussian model

    b1 <- celltrax:::bic_position(track,
                                  sigma,
                                  dims = dims,
                                  m = NULL,
                                  log_lik = FALSE)

    ## Two Gaussian model. The optimal point for splitting the track is
    ## determined simply as one with the smallest BIC

    cutoffOptions <- 2:(nrow(track) - 2)

    b2 <- purrr::map_dbl(cutoffOptions,
                         ~celltrax:::bic_position(track,
                                                  sigma,
                                                  dims = dims,
                                                  m = .x,
                                                  log_lik = FALSE))

    b1 - min(b2, na.rm = TRUE)

  }

# Drift correction ------

#' Adjust a single track with a mean speed vector.
#'
#' @description Adjusts a single track for non-specific drift by subtracting a
#' user-provided mean speed vector.
#' @param track a single track, i.e. element of tracks or trax object.
#' @param drift_vector a drift vector. Its dimensions need to correspond to
#' the track dimension.
#' @return a matrix.

  adjust_drift <- function(track, drift_vector) {

    # separate timepoints and coordinates

    tvec <- track[,1]
    coords <- track[,-1]

    # compute movement due to drift.

    drift.matrix <- matrix(rep(drift_vector,
                               nrow(coords)),
                           ncol = ncol(coords),
                           byrow = TRUE )

    drift.matrix <- drift.matrix * tvec

    # Add drift to coordinates

    track[,-1] <- coords - drift.matrix

    return(track)

  }

# Graphics theme ------

#' Standard ggplot theme.
#'
#' @description Defines a standard ggplot theme for the package's plots.
#' @return a ggplot theme object
#' @export

  theme_trax <- function() {

    common_text <- element_text(size = 8, face = 'plain', color = 'black')

    common_margin <- ggplot2::margin(t = 4, l = 3, r = 2, unit = 'mm')

    theme_classic() + theme(axis.text = common_text,
                            axis.title = common_text,
                            plot.title = element_text(size = 8,
                                                      face = 'bold',
                                                      color = 'black',
                                                      hjust = 0),
                            plot.subtitle = common_text,
                            plot.tag = element_text(size = 8,
                                                    face = 'plain',
                                                    color = 'black',
                                                    hjust = 0),
                            plot.tag.position = 'bottom',
                            legend.text = common_text,
                            legend.title = common_text,
                            strip.text = common_text,
                            strip.background = element_rect(fill = 'gray95',
                                                            color = 'gray80'),
                            plot.margin = common_margin,
                            panel.grid.major = element_line(color = 'gray90'))

  }

# Splitting long tracks by displacement cutoffs -----

#' Split a single track by a displacement cutoff.
#'
#' @description Given a single track, the function calculated displacements for
#' each step and splits the track into multiple ones if the cutoff is met.
#' @param track a single track, i.e. element of tracks or trax object.
#' @param disp_cutoff the scalar displacement cutoff.
#' @param prefix a prefix of the track split names.
#' @return a trax object.

  split_track <- function(track,
                          disp_cutoff,
                          prefix = NULL) {

    track <- as_trax(wrapTrack(track))

    if(!is.null(prefix)) track <- rlang::set_names(track, prefix)

    if(nrow(track[[1]]) < 3) return(track)

    cutpoints <- dplyr::filter(track_displacements(track),
                               value > disp_cutoff)$i

    if(length(cutpoints) == 0) return(track)

    splits <- splitTrack(track[[1]], positions = cutpoints)

    if(!is.null(prefix)) {

      splits <- rlang::set_names(splits,
                                 paste(prefix, 1:length(splits), sep = '_'))

    }

    as_trax(splits)

  }


# END -----
