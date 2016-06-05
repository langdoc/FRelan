#' remove_tier Function
#'
#' This function entirely removes a tier from an ELAN file. There is no testing about the tier content, so be careful!
#' @param file ELAN file to be parsed
#' @param linguistic_type The linguistic type which is to be removed. If tier_id is used, this should be empty.
#' @param tier_id The beginning of the tier name which should be removed. The idea is that one doesn't need to define the whole tier name, which may contain speaker id's and other information. This is especially suitable strategy when dealing with files which don't have well differentiated linguistic types. If linguistic_type is defined, this should be left empty.
#' @keywords ELAN
#' @export

remove_tier <- function(file, linguistic_type = FALSE, tier_id = FALSE){

        eaf <- XML::xmlParse(file)

        if (linguistic_type != FALSE){

        XML::removeNodes(eaf[paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "']")])
        } else {
        XML::removeNodes(eaf[paste0("//TIER[starts-with(@TIER_ID, '", tier_id,"')]")])
        }

        XML::saveXML(eaf, file)

}
