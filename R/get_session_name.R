#' get_session_name Function extracts session name from the path. Super simple, but keeps that regex in one place
#'
#' This function just applies a regex
#' @param filename
#' @keywords ELAN
#' @export
#' @examples
#' get_session_name("/Volumes/data/kom/subfolder/another/good_session.eaf")

get_session_name <- function(file_path){

        gsub("([^/]+).eaf$", "\\1", file_path)

        }
