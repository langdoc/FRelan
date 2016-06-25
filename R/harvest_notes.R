#' harvest_notes Function
#'
#' This function launches a Shiny app. Please see the documentation in the address [github.com/langdoc/FRorpus-demo](http://github.com/langdoc/FRorpus-demo).
#' @param corpus The corpus file
#' @keywords ELAN
#' @export

library(readr)
library(plyr)
suppressPackageStartupMessages(library(dplyr))
library(yaml)

harvest_notes <- function(path = "."){

notes <- list.files(path = path, pattern = "ldnotes\\....", recursive = T, full.names = T)

get_header_attr <- function(note_file){

        # http://stackoverflow.com/questions/30153194/access-name-of-rmd-file-and-use-in-r
        lines <- readLines(note_file)
        # Find the header portion contained between the --- lines.
        header_line_nums <- which(lines == "---") + c(1, -1)
        # Create a string of just that header portion
        header <- paste(lines[seq(header_line_nums[1],
                                  header_line_nums[2])],
                        collapse = "\n")
        # parse it as yaml, which returns a list of property values
        header <- yaml.load(header)
        dplyr::data_frame(session_number = header$session_number,
                          location = header$session_location,
                          file = note_file)
}

note_headers <- ldply(notes, get_header_attr)


note_text <- llply(notes, function(x) {
        read_tsv(x, skip = 10, col_names = F) %>% rename(time_stamp = X1, annotation = X2) %>%
                mutate(file = x)
})

note_content <- ldply(note_text, function(x){
        left_join(x, note_headers, by = "file") %>% tbl_df
})

note_content %>% tbl_df %>% filter(! is.na(annotation)) %>%
        mutate(tag = stringr::str_replace(annotation, "(^[A-Z]+) (.+)", "\\1")) -> note_content

note_content[stringi::stri_detect_regex(note_content$annotation, "([A-Z]+) .+") == FALSE,]$tag <- NA

note_content %>% mutate(annotation = stringr::str_replace(annotation, "(^[A-Z]+) (.+)", "\\2")) -> note_content

note_content

}
