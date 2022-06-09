# Import function for the tracks in different formats

# Import from separate files -----

#' Import of track data from separate x, y, z position text files.
#'
#' @description The function enables for import of track data from separate
#' tab-separated text files containing the X, Y and Z positions, respectively.
#' @details The cells are identified by consistent heading of each file.
#' The obligatory file format requires time in the first column and track names
#' in the remaining headings and coordinate values distributed in rows. Note:
#' tracks with less than 2 steps are automatically removed.
#' @return a trax class object.
#' @param x_file a path to the file containing X coordinates.
#' @param y_file a path to the file containing Y coordinates.
#' @param z_file a path to the file containing Z coordinates. Optional, if not
#' provided, the tracks will be constrained to the X and Y coordinates.
#' @param ... extra arguments passed to \code{\link[readr]{read_tsv}}.
#' @export

  read_trax_parts <- function(x_file,
                              y_file,
                              z_file = NULL, ...) {

    ## reading and entry control

    read_list <- purrr::compact(list(x = x_file,
                                     y = y_file,
                                     z = z_file))

    coord_tbl <- purrr::map(read_list,
                            readr::read_tsv,
                            show_col_types = FALSE, ...)

    track_ids <- names(coord_tbl[[1]])[2:ncol(coord_tbl[[1]])]

    if(any(!track_ids %in% names(coord_tbl[[2]]))) {

      stop('Inconsistent track IDs found. Sure you have exported right data?',
           call. = FALSE)

    }

    if(!is.null(z_file)) {

      coord_names <- c('x', 'y', 'z')

      if(any(!track_ids %in% names(coord_tbl[[3]]))) {

        stop('Inconsistent track IDs found. Sure you have exported right data?',
             call. = FALSE)

      }

    } else {

      coord_names <- c('x', 'y')

    }

    ## clearing

    coord_tbl <- purrr::map2(coord_tbl,
                             coord_names,
                             ~tidyr::pivot_longer(.x,
                                                  all_of(track_ids),
                                                  names_to = 'id',
                                                  values_to = .y))

    coord_tbl <- purrr::map2(coord_tbl,
                             coord_names,
                             ~rlang::set_names(.x, c('t', 'id', .y)))

    coord_tbl<- purrr::reduce(coord_tbl, dplyr::left_join, by = c('t', 'id'))

    coord_tbl <- dplyr::filter(coord_tbl, complete.cases(coord_tbl))

    coord_tbl <- as_trax(coord_tbl[c('id', 't', coord_names)])

    filter_steps(coord_tbl, min_steps = 2, return_both = FALSE)

  }

# Import from a single file ------

#' Imports tracks from a single text file.
#'
#' @description Reads track information from a tab-separated text file of the
#' following columns named: id, t, x, y and optionally z, containing track id,
#' time, x, y and optionally z coordinates, respectively.
#' @details Note: tracks with less than 2 steps are automatically removed.
#' @param file a path to the file.
#' @param ... extra arguments passed to \code{\link[readr]{read_tsv}}.
#' @export

  read_trax <- function(file, ...) {

    coord_tbl <- readr::read_tsv(file, ...)

    coord_tbl <- dplyr::filter(coord_tbl, complete.cases(coord_tbl))

    filter_steps(as_trax(coord_tbl), min_steps = 2, return_both = FALSE)

  }

# Write a trax object locally ----

#' Write a trax object locally.
#'
#' @description Writes a trax object locally as a tab-separated text file with
#' the following columns named: id, t, x, y and optionally z, containing
#' track id, time, x, y and optionally z coordinates, respectively.
#' @param x a trax object.
#' @param file a path to the file.
#' @param ... extra arguments passed to \code{\link[readr]{write_tsv}}.
#' @export

  write_trax <- function(x, file, ...) {

    if(!is_trax(x)) stop("'x' needs to be a 'trax' object.", call. = FALSE)

    readr::write_tsv(as.data.frame(x), file = file, ...)

  }

# END -----
