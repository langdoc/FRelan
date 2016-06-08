#' open_eaf Function
#'
#' This function opens an ELAN file from R. Intended use covers mainly situations where another function has, as an example, listed all files which have incorrectly linked media files which user wants to relocate file by file. In principle one could also develop it to open file in a specific location, but as far as I see this involves manipulating .psfx file which defines the selection on the timeline when opened.
#' @param corpus To which data frame you want to apply the function
#' @param row The number of dataframe row you want to open
#' @param program The program with which you want to open ELAN file
#' @keywords ELAN
#' @export
#' @examples
#' open_eaf(5)

open_eaf <- function(corpus = corpus_kpv, row = 30, program = FALSE){

        `%>%` <- dplyr::`%>%`

        corpus %>% dplyr::slice(row) -> corpus

        file <- gsub("\\.eaf$", ".pfsx", corpus$filename)

                # read in to a tree:
                x = XML::xmlParse(file)

                # this returns a *list* of text nodes under sequence
                # and NOT the text nodes under taxon
                sel_begin <- XML::xpathApply(x,"//pref[@key='SelectionBeginTime']/Long/text()")
                sel_end <- XML::xpathApply(x,"//pref[@key='SelectionEndTime']/Long/text()")

                scale_begin <- XML::xpathApply(x,"//pref[@key='TimeScaleBeginTime']/Long/text()")

                media_time <- XML::xpathApply(x,"//pref[@key='MediaTime']/Long/text()")

                # now we loop over the list returned, and get and modify the node value:

                if("time_start" %in% colnames(corpus)) {

                sapply(sel_begin, function(G){
                        text = corpus$time_start
                        XML::xmlValue(G) = text
                })

                sapply(sel_end, function(G){
                        text = corpus$time_end
                        XML::xmlValue(G) = text
                })

                sapply(scale_begin, function(G){
                        text = corpus$time_start
                        XML::xmlValue(G) = text
                })

                sapply(media_time, function(G){
                        text = corpus$time_start
                        XML::xmlValue(G) = text
                })

                XML::saveXML(x, file)
                }


                if (is.character(program) == TRUE){
                system(paste("open", " -a ", program, gsub("\\.pfsx$", ".eaf", corpus$filename)))
                } else {
                system(paste("open", gsub("\\.pfsx$", ".eaf", corpus$filename)))
                }
        }
