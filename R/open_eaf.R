#' open_eaf Function
#'
#' This function opens an ELAN file from R. If no row number is specified it is opened from the beginning. However, one can specify row number, and then the file is opened from that utterance. The row number means here the index number in the current local frame. Please note that operations like slice() impact the row numbering. Also the function demands that there is a row called `filename`, which contains complete path. The place where file is opened is determined by editing .psfx file, so that also has to be present (it is automatically generated after one opening).
#' @param corpus To which data frame you want to apply the function
#' @param row The number of dataframe row you want to open
#' @param program The program with which you want to open ELAN file
#' @keywords ELAN
#' @export
#' @examples
#' open_eaf(5)

open_eaf <- function(corpus = corpus_kpv, row = 1, program = FALSE){

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
