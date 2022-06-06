# Non.-exported utilities

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

# END -----
