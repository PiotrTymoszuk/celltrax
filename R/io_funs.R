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

#' Import tracks from a single formatted text file.
#'
#' @description Reads track information from a tab-separated text file of the
#' following columns named: id, t, x, y and optionally z, containing track id,
#' time, x, y and optionally z coordinates, respectively.
#' @details Note: tracks with less than 2 steps are automatically removed.
#' @return a trax class object.
#' @param file a path to the file.
#' @param ... extra arguments passed to \code{\link[readr]{read_tsv}}.
#' @export

  read_trax <- function(file, ...) {

    coord_tbl <- readr::read_tsv(file, ...)

    coord_tbl <- dplyr::filter(coord_tbl, complete.cases(coord_tbl))

    filter_steps(as_trax(coord_tbl), min_steps = 2, return_both = FALSE)

  }

# Import from an extended output of the live imager hardware ------

#' Import tracks from a single unformatted text file.
#'
#' @description This the most flexible way of importing tracks from a text file.
#' It handles most of tabular text formats with named columns. th user is asked
#' to specify names of columns containing track ID, time and coordinates. It may
#' work considerably slower than \code{\link{read_trax}} or
#' \code{\link{read_trax_parts}}.
#' @details Note: tracks with less than 2 steps are automatically removed. Uses
#' \code{\link[readr]{read_delim}} under the hood.
#' @return a trax class object or, if multiple samples defined (sample_id),
#' a list of trax objects named after the provided sample IDs.
#' @param file a path to the text file.
#' @param id_name name of the column storing track IDs.
#' @param t_name name of the column storing times.
#' @param x_name name of the column storing x coordinates.
#' @param y_name name of the column storing y coordinates.
#' @param z_name name of the column storing z coordinates, optional.
#' @param sample_name name of the column storing sample IDs, optional.
#' @param ... extra arguments passed to \code{\link[readr]{read_delim}}.
#' @export

  read_trax_text <- function(file,
                             id_name,
                             t_name,
                             x_name,
                             y_name,
                             z_name = NULL,
                             sample_name = NULL,
                             delim = '\t', ...) {

    ## data entry and checks

    text_entry <- readr::read_delim(file = file,
                                    delim = delim,
                                    col_names = TRUE,
                                    show_col_types = FALSE, ...)

    if(any(!c(id_name, t_name, x_name, y_name) %in% names(text_entry))) {

      stop('At least one of id_name, t_name, x_name, y_name
           does not match column names of the text file.',
           call. = FALSE)

    }

    if(!is.null(z_name)) {

      if(!z_name %in% names(text_entry)) {

        stop('z_name does not match column names of the text file.')

      }

    }

    if(!is.null(sample_name)) {

      if(!sample_name %in% names(text_entry)) {

        stop('sample_name does not match column names of the text file.')

      }

    }

    ## trax object

    text_entry <- text_entry[c(id_name, t_name, x_name, y_name, z_name, sample_name)]

    text_entry <- dplyr::filter(text_entry, complete.cases(text_entry))

    if(!is.null(sample_name)) {

      if(length(unique(text_entry[[sample_name]])) == 1) {

        return(filter_steps(as_trax(text_entry),
                            min_steps = 2,
                            return_both = FALSE))

      }

      sample_name <- paste0('`', sample_name, '`')

      trax_list <- plyr::dlply(text_entry, sample_name, as_trax)

      purrr::map(trax_list,
                 filter_steps,
                 min_steps = 2,
                 return_both = FALSE)

    } else {

      filter_steps(as_trax(text_entry), min_steps = 2, return_both = FALSE)

    }

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
