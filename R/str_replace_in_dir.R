##' Rename Files
##'
##' Look for a pattern in the files of a directoty and replace them
##' with an specified string.
##' @param pattern Pattern to be replaced. You may use regular
##'     expressions.
##' @param replacement Replacement
##' @return NULL
##' @author Guillermo Basulto-Elias
##' @export
str_replace_in_dir <- function(pattern, replacement) {
    ## Save current directory
    current_dir <- getwd()

    ## Choose directory to read files from
    chosen_dir <- tcltk::tclvalue(tcltk::tkchooseDirectory())

    ## Set temporary directory
    setwd(chosen_dir)

    ## Get list of files
    input_files <- list.files()

    ## Replace strings
    output_files <- str_replace(input_files, pattern, replacement)

    ## Files to be renamed
    idx <- input_files != output_files
    modified <- output_files[idx]

    ## Rename files
    map2(input_files[idx], modified, file.rename)

    ## Set the directory to the way it was.
    setwd(current_dir)

    cat("Renamed files: ", sum(idx), "\n")
}

