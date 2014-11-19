## .help.ESS <-
## function (topic, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
##     try.all.packages = getOption("help.try.all.packages"), help_type = getOption("help_type")) 
## {
##     types <- c("text", "html", "postscript", "ps", "pdf")
##     if (!missing(package)) 
##         if (is.name(y <- substitute(package))) 
##             package <- as.character(y)
##     if (missing(topic)) {
##         if (!missing(package)) {
##             help_type <- if (!length(help_type)) 
##                 "text"
##             else match.arg(tolower(help_type), types)
##             if (interactive() && help_type == "html") {
##                 if (tools:::httpdPort == 0L) 
##                   tools::startDynamicHelp()
##                 if (tools:::httpdPort <= 0L) 
##                   return(library(help = package, lib.loc = lib.loc, 
##                     character.only = TRUE))
##                 browser <- if (.Platform$GUI == "AQUA") {
##                   function(x, ...) {
##                     .Internal(aqua.custom.print("help-files", 
##                       x))
##                     return(invisible(x))
##                   }
##                 }
##                 else getOption("browser")
##                 browseURL(paste("http://127.0.0.1:", tools:::httpdPort, 
##                   "/library/", package, "/html/00Index.html", 
##                   sep = ""), browser)
##                 return(invisible())
##             }
##             else return(library(help = package, lib.loc = lib.loc, 
##                 character.only = TRUE))
##         }
##         if (!missing(lib.loc)) 
##             return(library(lib.loc = lib.loc))
##         topic <- "help"
##         package <- "utils"
##         lib.loc <- .Library
##     }
##     ischar <- tryCatch(is.character(topic) && length(topic) == 
##         1L, error = identity)
##     if (inherits(ischar, "error")) 
##         ischar <- FALSE
##     if (!ischar) {
##         reserved <- c("TRUE", "FALSE", "NULL", "Inf", "NaN", 
##             "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
##         stopic <- deparse(substitute(topic))
##         if (!is.name(substitute(topic)) && !stopic %in% reserved) 
##             stop("'topic' should be a name, length-one character vector or reserved word")
##         topic <- stopic
##     }
##     help_type <- if (!length(help_type)) 
##         "text"
##     else match.arg(tolower(help_type), types)
##     if (help_type %in% c("postscript", "ps")) 
##         warning("Postscript offline help is deprecated", call. = FALSE, 
##             immediate. = TRUE)
##     paths <- index.search(topic, find.package(package, lib.loc, 
##         verbose = verbose))
##     tried_all_packages <- FALSE
##     if (!length(paths) && is.logical(try.all.packages) && !is.na(try.all.packages) && 
##         try.all.packages && missing(package) && missing(lib.loc)) {
##         for (lib in .libPaths()) {
##             packages <- .packages(TRUE, lib)
##             packages <- packages[is.na(match(packages, .packages()))]
##             paths <- c(paths, index.search(topic, file.path(lib, 
##                 packages)))
##         }
##         paths <- paths[paths != ""]
##         tried_all_packages <- TRUE
##     }
##     paths <- unique(paths)
##     attributes(paths) <- list(call = match.call(), topic = topic, 
##         tried_all_packages = tried_all_packages, type = help_type)
##     class(paths) <- "help_files_with_topic"
##     paths
## }

## These functions support the path-shortening routines, and other
## miscellaneous helper functions not needed elsewhere.
##
LLbeta <-
function(p1,p2,p3)
{
    p12 <- sum((p1-p2)^2)
    p23 <- sum((p3-p2)^2) 
    p13 <- sum((p1-p3)^2)
    180*abs(acos((p12 + p23 - p13)/(2*sqrt(p12*p23)))-pi)/pi
}


LLKscore <-
function(p1,p2,p3,pathdist)
{
 
  l12 <-  LineMagnitude(p1[1],p1[2],p2[1],p2[2])/pathdist
  l23 <-  LineMagnitude(p2[1],p2[2],p3[1],p3[2])/pathdist
  l13 <-  LineMagnitude(p1[1],p1[2],p3[1],p3[2])/pathdist
  LLbeta(p1,p2,p3) *l12 *l23/(l12+l23)
}

even <-
function (x) x%%2 == 0

odd <-
function(x) x%%2 == 1

ldist <-
function(p1,p2)
{
  sqrt( sum( (p1-p2)^2))
}

LineMagnitude <-
function(x1, y1, x2, y2) sqrt((x2-x1)^2+(y2-y1)^2)


livenodes <-
function(p,q)
  {
    height <- p*2-1
    width <- q *2-1
    mat <- matrix(0,height,width)



    oddw <- seq(1,width,2)
    mat[,oddw] <- 1
    oddh <- seq(1,height,2)
    mat[oddh,] <- 1
        

     mat
  }
