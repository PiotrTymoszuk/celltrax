# Exported vector and other utils

# Total displacement vector per track ------

#' Calculate total displacement vectors
#'
#' @description Calculates the total displacement vectors for each track.
#' @return a trax object or if simplify is TRUE, a tibble with with
#' the vector dimensions.
#' @param x a trax object.
#' @param simplify logical, should the vector be displayed as a unit vector?
#' (x, y, z)?
#' @export

  get_total_vectors <- function(x, simplify = FALSE) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    vecs <- purrr::map(x, function(t) t[c(1, nrow(t)), ])

    if(!simplify) return(as_trax(vecs))

    vecs <- purrr::map(vecs, ~as.vector(.x[2, -1] - .x[1, -1]))

    res_tbl <- as.data.frame(purrr::reduce(vecs, rbind))

    res_tbl <- tibble::as_tibble(cbind(id = names(vecs), res_tbl))

    rlang::set_names(res_tbl, c('id', colnames(x[[1]])[-1]))

  }

# Total population vector ------

#' Calculate mean displacement vector for cell population.
#'
#' @description Calculates the mean displacement vector for all tracks.
#' The tracks are normalized prior to calculation.
#' @return a trax object or a vector if simplify is TRUE.
#' @param x a trax object with a single track representing
#' the sum displacement vector.
#' @export

  get_mean_vector <- function(x, simplify = FALSE) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    vecs <- purrr::map(normalizeTracks(x),
                       function(t) t[nrow(t), ])

    vecs <- purrr::reduce(vecs, rbind)

    mean_vec <- colMeans(vecs)[-1]

    if(simplify) return(mean_vec)

    zero_vec <- rlang::set_names(rep(0, length(mean_vec)),
                                 names(mean_vec))

    rbind(zero_vec,
          mean_vec)

  }

# Random sampling of tracks ------

#' Sample tracks at random.
#'
#' @description Gets a random sample of tracks.
#' @return a trax object.
#' @param x a tracks object.
#' @param size sample size.
#' @param replace logical: should the sampling be done with replacement?
#' @param seed seed value for random number generation.
#' @export

  sample_trax <- function(x, size, replace = FALSE, seed = NULL) {

    ## entry control

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    if(!is.null(seed)) set.seed(seed)

    ## sampling

    sample_idx <- sample(1:length(x), size = size, replace = replace)

    x[sample_idx]

  }

# Calculate mean speed vector -----

#' Calculate mean speed vector.
#'
#' @description Calculates mean speed vector by dividing the mean total
#' displacement vector by median time interval.
#' @return a vector x, y and z dimensions.
#' @param x a trax object with a single track representing
#' the sum displacement vector.
#' @export

  get_mean_speed <- function(x) {

    if(!is_trax(x)) stop("Argument 'x' needs to be a 'trax' object.", call. = FALSE)

    mean_vec <- get_mean_vector(x, simplify = TRUE)

    av_time <- mean(track_steps(x)$value) * timeStep(x, na.rm = TRUE)

    mean_vec/av_time

  }

# END -----
