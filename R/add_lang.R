#' add_lang Function
#'
#' This function adds a language reference into ELAN tiers which are specified by type and speaker
#' @param eaf_file Filename
#' @param participant The id of the participant
#' @param linguistic type The linguistic type of the tier
#' @param lang ISO-code for the language which has to be added
#' @keywords ELAN
#' @export

add_lang <- function(eaf_file = "testfile.eaf", participant = "AXK-F-193X", linguistic_type = "wordT", lang = "kpv"){

        doc <- XML::xmlParse(file)

        nodeSet = XML::xpathApply(doc, paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='", participant,"']"))

        new_tier <- XML::addAttributes(nodeSet[[1]], LANG_REF = "kpv")

        eaf <- XML::getNodeSet(doc, "//ANNOTATION_DOCUMENT")
        suppressWarnings(XML::removeNodes(eaf[paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='", participant,"']")]))
        eaf <- XML::addChildren(eaf[[1]], new_tier)

        XML::saveXML(eaf, file)
}
