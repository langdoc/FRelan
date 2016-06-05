#' change_stereotype Function
#'
#' This function adds a new tier into an ELAN file. It is prototypical at best.
#' @param file ELAN file
#' @param linguistic_type Name of the linguistic type
#' @param stereotype Name of the stereotype. Accepted values are, among some others, "Included_in", "Symbolic_Association" or "Symbolic_Subdivision".
#' @param time_alignable Here the accepted values are "true" or "false". Please note that these must be character vectors typed exactly like this.
#' @param add Should the type be added, or is it intended to modify an existing type with the same name?
#' @keywords ELAN
#' @export

change_stereotype <- function(file, linguistic_type, stereotype, time_alignable, add = FALSE){

        `%>%` <- dplyr::`%>%`

        doc <- XML::xmlParse(file)

        if (add == FALSE){
        XML::removeNodes(doc[paste0("//LINGUISTIC_TYPE[@LINGUISTIC_TYPE_ID='", linguistic_type, "']")])
        }

        tier <- XML::newXMLNode("LINGUISTIC_TYPE", attrs = c(CONSTRAINTS = stereotype,
                                                                       GRAPHIC_REFERENCES = "false",
                                                                       LINGUISTIC_TYPE_ID = linguistic_type,
                                                                       TIME_ALIGNABLE = time_alignable))

        doc <- XML::xmlParse(file)
        eaf <- XML::getNodeSet(doc, "//ANNOTATION_DOCUMENT")
        XML::xmlChildren(eaf[[1]]) <- XML::addChildren(eaf[[1]], tier)

        XML::xmlChildren(eaf[[1]]) <- c(XML::xmlChildren(eaf[[1]]))[c(order(factor(names(eaf[[1]]), levels = c("HEADER","TIME_ORDER", "TIER", "LINGUISTIC_TYPE", "LOCALE", "LANGUAGE", "CONSTRAINT", "CONTROLLED_VOCABULARY", "EXTERNAL_REF"))))]

        XML::saveXML(eaf[[1]], file)
}
