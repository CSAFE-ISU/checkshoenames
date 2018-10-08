## check_files is the main function in this script. It's at the
## bottom.

##' Print tally of files
##'
##' @param file_ext Vector of characters with all the extensions founf
##'     in a directory.
print_files_table <- function (file_ext) {
    cat("\nFile count:")
    data.frame(extension = file_ext) %>%
        dplyr::group_by_("extension") %>%
            dplyr::summarize(count = dplyr::n()) %>%
            dplyr::filter(!is.na(extension)) %>%
            knitr::kable() %>%
            print()
    cat("\n\n")
}


##' Extract File Extension
##'
##' Get the everything after the last dot in a filename
##' @param filename File name
##' @return character
##' @author Guillermo Basulto-Elias
get_file_extension <- function(filename) {
    filename %>%
        str_extract("\\.[:alpha:]+[:punct:]?$") %>%
        str_replace(".", "")
}

get_method_extension <- function(method, mode){

    ## Check mode is not null for case 1 (matscan)
    if (method == 1 & !(mode %in% 1:10)) {
        stop("Mode is required for method 1")
    }

    ## Obtain method
    out <-
        dplyr::case_when(
                   method == 1 & mode %in% c(1, 6) ~ "avi",
                   method == 1 & mode %in% c(2, 7) ~ "csv",
                   method == 1 & mode %in% c(3, 8) ~ "csv",
                   method == 1 & mode %in% c(4, 9) ~ "jpg",
                   method == 1 & mode %in% c(5, 10) ~ "fsx",
                   TRUE ~ "na"
               )

    ## Check is method is valid.
    if (out == "na") stop("Invalid method")

    out
}


## -------------------------------------------------------------------

##' Check File Names
##'
##' Check that every file in a direrctory matched the naming criterion
##' for a particular shoeprint method.
##' @param method Integer specifying the method.
##' @param mode NULL by default. It specifies the mode to
##'     check. Mandatory for method 1, optional for the rest.
##' @return NULL
##' @importFrom magrittr %>%
##' @author Guillermo Basulto Elias
##' @export
check_files <- function(method, mode = NULL) {
    ## Save current directory
    current_dir <- getwd()

    ## Choose directory to read files from
    chosen_dir <- tcltk::tclvalue(tcltk::tkchooseDirectory())

    ## Set temporary directory
    setwd(chosen_dir)

    ## Print list of files and remove NAs.
    files <- list.files()

    ## Extract extension of files
    extensions <- get_file_extension(files)

    ## Print table
    print_files_table(extensions)

    ## Get extension based on method and mode
    mode_ext <- get_method_extension(method, mode)

    out <-
      data_frame(filename = files) %>%
      mutate(
        id = filename %>% str_extract("^[0-9]{3}"),
        checksum = filename %>%
          str_extract("^[0-9]{6}[LR]"),
        side = checksum %>%
          str_extract("[LR]"),
        date = ymd(str_extract(filename, "[0-9]{8}")),
        aux = str_extract(filename, "_[1-9]_[:digit:]{1,2}_[:digit:]{1,2}"),
        aux = str_remove(aux, "^_"),
        method = str_extract(aux, "^[1-9]"),
        mode = str_extract(aux, "_[:digit:]{1,2}_"),
        mode = str_remove_all(mode, "_"),
        rep = str_extract(aux, "[:digit:]{1,2}$"),
        ext = filename %>%
          str_extract("\\.[:alpha:]+[:punct:]?$") %>%
          str_replace(".", ""),
        checksum_error = !(checksum %in% all_id_checksum_side)) %>%
      select(-aux, -checksum) %>%
      filter(ext %in% mode_ext)


    ## Set the directory to the way it was.
    setwd(current_dir)

    return (out)
}



extract_id <- function(x) {
    id <- x %>% str_extract("^[0-9]{3}")

    if (is.na(id)) {
        cat("The file ", x, " does not have a valid ID.\n\n")
        is_correct <- FALSE
    }
}





##' Check Shoeprint Name
##'
##' Check if the name of a particular file is correct.
##' @param filename Filename
##' @param checksum_table Table with checksums for ID and side.
##' @return Logical value indicating if name is correct
##' @author Guillermo Basulto-Elias
##' @import stringr
##' @importFrom magrittr %>%
##' @export
check_individual_name <- function (filename, checksum_table) {
    ## Default output value
    is_correct <- TRUE

    ## Extract characteristics
    id <- filename %>% str_extract("^[0-9]{3}")
    side <- filename %>%
        str_extract("^[0-9]{6}[LR]") %>%
        str_extract("[LR]")




    if (is.na(id)) {
        cat("The file ", filename, " does not have a valid ID.\n\n")
        is_correct <- FALSE
    }

    is_correct
}


## c(TRUE, FALSE)[c(1, 1, 2, 1, 1, 2)]

## c("id", "side", "id-checksum-side", "method", "mode", "repetition")[c(TRUE, FALSE)[c(1, 1, 2, 1, 1, 2)]]


## xx <- 2

## ff <- function(x)
## {
##     print(x)
##     deparse(substitute(x))
## }

## ff(xx)
