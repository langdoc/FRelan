#' add_kwic Function
#'
#' This function creates columns 'word' for originally punctuated tokens, plus 'before' and 'after' for the surrounding context
#' @param corpus A data frame that contains a column for tokens
#' @keywords ELAN
#' @export
#' @examples
#' tier_exists("path/to/elan/file.eaf", "orth@NTP-M-1986")

add_kwic <- function(corpus){

corpus$word <- corpus$token
corpus$token <- tolower(corpus$token)

corpus_split <- split(corpus, c(corpus$session_name))

get_after <- function(x) {

        Token1 <- as.character(x$token)
        Token2 <- Token1[1:length(Token1)+1]
        Token3 <- Token2[1:length(Token1)+1]
        Token4 <- Token3[1:length(Token1)+1]
        Token5 <- Token4[1:length(Token1)+1]
        Token6 <- Token5[1:length(Token1)+1]
        Token7 <- Token6[1:length(Token1)+1]
        Token8 <- Token7[1:length(Token1)+1]
        Token9 <- Token8[1:length(Token1)+1]
        Token10 <- Token9[1:length(Token1)+1]

        x$After <- paste(Token2, Token3, Token4, Token5, Token6, Token7, Token8, Token9, Token10)
}

get_before <- function(x){

        Token1 <- as.character(x$token)
        Token2 <- Token1[0:(length(Token1)-1)]
        Token2 <- append(Token2, "", 0)
        Token3 <- Token2[0:(length(Token2)-1)]
        Token3 <- append(Token3, "", 0)
        Token4 <- Token3[0:(length(Token3)-1)]
        Token4 <- append(Token4, "", 0)
        Token5 <- Token4[0:(length(Token4)-1)]
        Token5 <- append(Token5, "", 0)
        Token6 <- Token5[0:(length(Token5)-1)]
        Token6 <- append(Token6, "", 0)
        Token7 <- Token6[0:(length(Token6)-1)]
        Token7 <- append(Token7, "", 0)
        Token8 <- Token7[0:(length(Token7)-1)]
        Token8 <- append(Token8, "", 0)
        Token9 <- Token8[0:(length(Token8)-1)]
        Token9 <- append(Token9, "", 0)
        Token10 <- Token9[0:(length(Token9)-1)]
        Token10 <- append(Token10, "", 0)

        x$Before <- paste(Token10, Token9, Token8, Token7, Token6, Token5, Token4, Token3, Token2)
}

concat_after <- lapply(corpus_split, get_after)
concat_before <- lapply(corpus_split, get_before)

corpus <- do.call("rbind", corpus_split)

concat_after <- lapply(concat_after, FUN = function(x) gsub("^NA NA NA NA NA$", "###", x, perl = TRUE))
concat_after <- lapply(concat_after, FUN = function(x) gsub("NA", "", x, perl = TRUE))
concat_before <- lapply(concat_before, FUN = function(x) gsub("^      $", "###", x, perl = TRUE))

after <- unlist(concat_after)
before <- unlist(concat_before)

corpus$after <- after
corpus$before <- before

corpus

}
