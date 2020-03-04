#################################################################################
## Parsing Functions                                                           ##
#################################################################################
## Lost previous function to do parsing. Write new functions to parse readings
## into a data frame of book, chapter, verse. These are NOT efficient but get
## the job done for most references.

##' ParseBasic
##' 
##' Parse a basic reference 
##' 
##' @param str a character string in the form
##'     <Chapter>:<VerseStart>[-<VerseEnd>] where the ending verse is optional
##' @return a data frame with 1 row and 2 columns containing the starting and
##'     ending Chapter:Verse
ParseBasic <- function(str) {
    ## Simple case, no commas or semicolons
    result <- data.frame(start = NA, end = NA)
    result[1, ] <- str_split(str, "-")[[1]]
    if (!str_detect(result[1, 2], ":")) {
        ch <- str_split(result[1, 1], ":")[[1]][1]
        result[1, 2] <- paste(ch, result[1, 2], sep = ":")
    }
    return(result)
}

##' ParseFull
##'
##' Parse a full reference
##' 
##' @param str a character string in the form <BookAbbrev>
##'     <Chapter>:<VerseStart>[-<VerseEnd>] [, <VerseStart>-<VerseEnd>] [;
##'     <Chapter>:<VerseStart>[-<VerseEnd>]]
##' @return a data frame with as many rows as there are continuous verses in the
##'     reference and 3 columns containing the starting and ending Chapter:Verse
##'     as well as the book abbreviation
ParseFull <- function(str) {
    ## Only consider the first reading if there is an "or" present
    str <- str_split(str, pattern = " or ")[[1]][1]
    abbrv <- str_extract(str, "^[1-9]?[ ]?[A-Za-z]+ ")[[1]] %>%
        trimws()
    ## Remove letters that denote parts of verses and anything in parentheses or brackets
    str <- str_replace(str, "^[1-9]?[ ]?[A-Za-z]+ ", "") %>%
        str_replace_all("[a-z]", "") %>%
        str_replace_all("\\(.*\\)", "") %>%
        str_replace_all("\\[.*\\]", "") %>%
        trimws()
    num <- str_count(str, "[;,]") + 1
    ## In some rare cases, a book only has 1 chapter and the chapter may be
    ## omitted. If no chapter is given, add chapter 1 to beginning of the
    ## string.
    if (!str_detect(str, ":")) {
        str <- paste0("1:", str)
    }
    result <- data.frame(start = rep(NA, num), end = rep(NA, num))
    str_mult <- str_split(str, ";")[[1]] %>%
        trimws
    lcv <- 1
    for (rr in str_mult) {
        if (!str_detect(rr, "[,]")) {
            ## Basic parsing if there are no commas
            result[lcv,] <- ParseBasic(rr)
            lcv <- lcv + 1
        } else {
            ## Handle commas
            x <- str_split(rr, ",")[[1]] %>%
                trimws()
            ## Loop over x to assign chapter, if not present, and parse basic
            for (jj in seq_along(x)) {
                latest_ch <- map(str_split(x, ":"), function(str)
                    str_extract(str[length(str)-1], "[0-9]+$"))
                if (!str_detect(x[[jj]], ":")) {
                    x[[jj]] <- paste(latest_ch[[jj-1]], x[[jj]], sep = ":")
                }
                result[lcv, ] <- ParseBasic(x[[jj]])
                lcv <- lcv + 1
            }
        }
    }
    result[["Abbrv"]] <- abbrv
    return(result)
}

##' @title getBaseTheme
##'
##' @description
##' \code{getBaseTheme} Defines universal plotting settings to use with ggplot2
##'
##' @import ggplot2
##' 
##' @author Jason Vander Heiden <jason.vanderheiden@@yale.edu>, Stefan Avey <stefan.avey@yale.edu>
##' @keywords aveytoolkit
##' @export
##' @examples
##' p <- ggplot(mtcars, aes(wt, mpg))
##' p <- p + geom_point() + getBaseTheme()
##' plot(p)

getBaseTheme <- function( ) {
    ## Define universal plot settings
    base_theme <- theme_bw() +
        theme(text=element_text(size=14)) +
        theme(plot.title=element_text(size=18)) +
        theme(strip.background=element_rect(fill='white')) +
        theme(strip.text=element_text(size=16, face='bold')) +
        theme(plot.title = element_text(hjust = 0.5)) +
        ## theme(axis.text.x=element_text(size=14, vjust=0.5, hjust=0.5)) +
        ## theme(axis.text.y=element_text(size=14)) +
        theme(axis.title=element_text(size=16, vjust=0.5)) +
        theme(plot.caption = element_text(size = 10, face = "italic"))
    return(base_theme)
}
