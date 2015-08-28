#' log_eaf Function
#'
#' This function finds the newest ELAN files in a given directory.
#' @param days This defines how old files are returned. Default is 3 days.
#' @param path This is a path. Defaults to current working directory.
#' @param pattern This is a pattern in regex. Defaults has ELAN files ending with ".eaf".
#' @keywords ELAN
#' @export
#' @examples
#' log_eaf(5)

log_eaf <- function(days = 3, path = ".", pattern = ".eaf$"){
        eaf_paths <- file.info(list.files(path, pattern, recursive = TRUE, full.names = TRUE))
        dt <- difftime(Sys.time(), eaf_paths$mtime, units = "days") <= days
        eaf_files <- setNames(eaf_paths$mtime[dt], row.names(eaf_paths)[dt])
        eaf_files <- data.frame(eaf_files)
        eaf_files$Filename <- row.names(eaf_files)
        eaf_files$Session_name <- gsub("(.+)(/.+\\.eaf$)", "\\2", eaf_files$Filename)
        tbl_df(eaf_files) %>% rename(Modified = eaf_files) %>% select(Modified, Session_name, Filename)
}
