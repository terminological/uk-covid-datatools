# Shared functions and definitions

#' SQL like filtering in data tables
#' 
#' allow a sql like syntax in data tables. N.b. conflict with data.table's definition of like which is 
#' based on regex.
#' 
#' %	Represents zero or more characters	bl% finds bl, black, blue, and blob
#' _	Represents a single character	h_t finds hot, hat, and hit
#' []	Represents any single character within the brackets	h[oa]t finds hot and hat, but not hit
#' ^	Represents any character not in the brackets	h[^oa]t finds hit, but not hot and hat
#' -	Represents a range of characters	c[a-b]t finds cat and cbt
#' TODO: escaped sql
#'
#' @import dplyr
#' @export
"%like%" = function(vector, like, ignore.case = TRUE, fixed = FALSE) {
  pattern = like %>% stringr::str_replace_all("%",".*") %>% stringr::str_replace_all("_",".")
  if (is.factor(vector)) {
    as.integer(vector) %in% grep(pattern, levels(vector), ignore.case = ignore.case, fixed = fixed)
  } else {
    # most usually character, but integer and numerics will be silently coerced by grepl
    grepl(pattern, vector, ignore.case = ignore.case, fixed = fixed)
  }
}

# 
# identifyCovidOutlier = function(incidences) {
#   surroundingAv = stats::filter(c(1,1,1,0,1,1,1)/6, incidences)
#   incidences
# }


#' Calculates a standard deviation for a ratio distribution of gaussians
#' 
#' @return a SD
#' @export
sdFromRatio = function(mu_x, sig_x, mu_y, sig_y ) {
  #TODO this has conditions. if those are violated I think you have to bootstrap
  # from https://doi.org/10.1007%2Fs00362-012-0429-2
  # and https://en.wikipedia.org/wiki/Ratio_distribution#Means_and_variances_of_random_ratios
  return(sqrt(mu_x^2/mu_y^2*(sig_x^2/mu_x^2+sig_y^2/mu_y^2)))
}

#' Calculates a standard deviation for a product distribution of gaussians
#' 
#' @return a SD
#' @export
sdFromProduct = function(mu_x, sig_x, mu_y=mu_x, sig_y=sig_x ) {
  # from https://math.stackexchange.com/questions/1416518/standard-deviation-of-the-product-of-gaussians
  return(sig_x^2*sig_y^2+sig_x^2*mu_y^2+sig_y^2*mu_x^2)
}

#' Calculates a join list
#' 
#' @param df - a df which may be grouped
#' @param groupVars - the grouping for which we want to create a label as a list of columns quoted by vars(...)
#' @param defaultJoin - if there is no grouping we need one column to join by.
#' @import dplyr
#' @return a join List
#' @export
joinList = function(df,groupVars=NULL,defaultJoin=NULL) {
  grps = df %>% groups()
  joinList = c()
  if (!identical(defaultJoin,NULL)) {
    joinList = c(joinList,defaultJoin)
  }
  if (length(grps)!=0) {
    joinList = c(joinList, sapply(grps,as.character))
  }
  if (!identical(groupVars,NULL)) {
    joinList = c(joinList, as.vector(sapply(groupVars,as_label)))
  }
  return(joinList)
}

#' #' colApply apply the function to the rows of a dataframe row-wise
#' #' 
#' #' @param f - a function which operates on a vector
#' #' @param ... a list of vectors
#' colApply = function(f, ...) {
#'   browser()
#'   if (!all(sapply(list(...), length)==length(list(...)[[1]]))) stop("arguments must all have same length")
#'   m = matrix(c(...),ncol=length(list(...)))
#'   out = apply(m, 1, f, na.rm=TRUE)
#'   return(out)
#' }


#' capture a data frame for an error message
#'
#' @param x a dataframe
#' @export
printDataframeToString <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}


#' Add ribbons to times series from mean and SD
#' 
#' @param meanVar the mean
#' @param sdVar the sd
#' @export
plotRibbons = function(data=NULL, meanVar, sdVar, colourExpr, ...) {
  meanVar = ensym(meanVar)
  sdVar = ensym(sdVar)
  colourExpr = enexpr(colourExpr)
  q = qnorm(c(0.025,0.1,0.9,0.975))
  if (class(colourExpr) == "character") {
    return(list(
      geom_line(data=data,mapping=aes(y=!!meanVar,...),colour=colourExpr),
      geom_ribbon(data=data,mapping=aes(
        ymin=(!!meanVar-q[1]*!!sdVar),
        ymax=(!!meanVar+q[4]*!!sdVar),
        ... #!!!dots
      ),fill=colourExpr,colour = NA,fill="black",alpha = 0.05,show.legend = FALSE),
      geom_ribbon(data=data,mapping=aes(
        ymin=(!!meanVar+q[2]*!!sdVar),
        ymax=(!!meanVar+q[3]*!!sdVar),
        ... #!!!dots
      ),fill=colourExpr,colour = NA,fill="black",alpha = 0.065,show.legend = FALSE)
    ))
  } else {
    return(list(
      geom_line(data=data,mapping=aes(y=!!meanVar,colour=!!colourExpr,...)),
      geom_ribbon(data=data,mapping=aes(
        ymin=(!!meanVar-q[1]*!!sdVar),
        ymax=(!!meanVar+q[4]*!!sdVar),
        group=!!colourExpr,
        ... #!!!dots
      ),colour = NA,fill="black",alpha = 0.05,show.legend = FALSE),
      geom_ribbon(data=data,mapping=aes(
        ymin=(!!meanVar+q[2]*!!sdVar),
        ymax=(!!meanVar+q[3]*!!sdVar),
        group=!!colourExpr,
        ... #!!!dots
      ),colour = NA, fill="black",alpha = 0.065,show.legend = FALSE)
    ))
  }
}

# date craziness ---------------

maybeDMYorMDY = function(dateStringVec) {
  out1 = as.Date(dateStringVec,"%d/%m/%Y")
  out2 = as.Date(dateStringVec,"%m/%d/%Y")
  if(
    sum(is.na(out1)==is.na(dateStringVec)) >= sum(is.na(out2)==is.na(dateStringVec))
  ) {
    return(out1)
  } else {
    return(out2)
  }
}

# ggplot ------------------------------------------------------------------

geom_quantiles <- function(mapping, ...) {
  list(
    stat_summary_bin(mapping,
                 geom = "line",
                 fun.data = function(x) {quantile(x, 0.5)},
                 #orientation = 'x',
                 ...),
    stat_summary_bin(mapping,
               geom = "ribbon",
               fun.min = function(x) quantile(x, 0.025),
               fun.max = function(x) quantile(x, 0.975),
               #orientation = 'x',
               alpha = 0.1, ...),
    stat_summary_bin(mapping,
                 geom = "ribbon",
                 fun.min = function(x) quantile(x, 0.25),
                 fun.max = function(x) quantile(x, 0.75),
                 #orientation = 'x',
                 alpha = 0.15, ...)
  )
}

#' In fix operator to apply ensurer rules
#' @param ensurer an ensurer contract
#' @param object the object
#' @return object if it conforms stops otherwise.
#' @export
`%def%` = function(ensurer,object) {
  return(ensurer(object))
}

#' magrittr pipe
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
NULL

# `%>%`.R6 = function(r6,fn) {
# 
# }


mode <- function(x) {
    tmp = density(x,n = 8192)
    round(tmp$x[tmp$y==max(tmp$y)],3)
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
}