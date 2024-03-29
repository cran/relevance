      
u.true <- function (x) length(x)>0 && is.logical(x) && (!is.na(x)) && all(x)
u.notfalse <-
  function (x) !(length(x)==1 && is.logical(x) && (!is.na(x)) && !x)
u.isnull <- function(x)  length(x)==0||all(is.na(x))
"%nin%" <- function (x,y) !x%in%y

i.last <- function(data, n=1) data[sign(n)*(((ldt <- length(data))-abs(n)+1):ldt)]
RNAMES <- function (x) if (!is.null(dim(x))) row.names(x) else names(x)
## u.debug <- function () u.true(rlvoptions("debug"))
DB <- function (on=TRUE) options(error=if(on) recover else NULL, warn=on)
i.extendrange <- function(range, ext=0.05)  range + c(-1,1)*ext*diff(range)
## -----------------------------------------------------------
i.def <- function(arg, value = TRUE, valuetrue = value, valuefalse = FALSE)
{
  rr <- arg
  if (length(arg)==0 ||
      (mode(arg)%in%c("numeric","character","logical","complex")&&
       all(is.na(arg)))
      )  rr <- value
  else {
    if (length(arg)==1 && is.logical(arg))
      rr <- if (arg) valuetrue else valuefalse
  }
  rr
}
## ----------------------------------------------------------------
u.merge <- function (dd1, dd2 = NA, which=NULL, after=NULL,
                    length=NULL, names=NULL)
{
## Purpose:   merge two vectors or expand a vector by NA s
## -------------------------------------------------------------------------
## Arguments:
##   dd1      first vector or matrix or data.frame (?),
##   dd2      second vector, ...
##   which    is T for indices for which first vector is used
##   after    elements of  dd2  will be inserted after "after" in dd1
##   length   length of the result (will be expanded if necessary)
##   names    names of the result (if length is adequate)
## -------------------------------------------------------------------------
## Author: Werner Stahel, Date: 11 Mar 93, 13:50, and later
  llen <- length
  n1 <- length(dd1)
  nc1 <- ncol(dd1)
  nc2 <- ncol(dd2)
  if (length(nc1)>0) {
    n1 <- nrow(dd1)
    if (!( length(dd2)==1 || is.null(nc2) || nc2==nc1 ))
      stop("unsuitable second argument")
    }
## --- generate  which  vector for all cases
  if (length(which)==0) {
## - after  specified
      if (length(after)==0) stop("specify either  which  or  after")
      if (is.logical(after))  after <- which(after)
      wh <- rep(TRUE,n1+length(after))
      wh[after+1:length(after)] <- FALSE }
  else {
## - which  specified
    if(is.logical(which)) wh <- which
    else {
      if (length(llen)==0)  llen <- n1+length(which)
        wh <- rep(TRUE, llen)
        wh[which] <- FALSE }
  }
## --- merge
  nn <- length(wh)
  n2 <- nn-n1
  if (!(is.null(names)|length(names)==nn))
    warning("argument  names  not used (unsuitable length)")
  if (length(nc1)>0) {
    if (!(length(dd2)==1 || NROW(dd2)==n2))
      stop("unsuitable number of rows")
    rr <- matrix(NA,nn,nc1)
    rr[wh,] <- as.matrix(dd1)
    rr[!wh,] <- if (is.data.frame(dd2)) as.matrix(dd2) else dd2
##-     if (length(names)>0) row.names(rr) <- names else {
##-       if (length(lrn1 <- row.names(dd1))>0)
  }
  else {
    rr <- rep(NA,nn)
    rr[wh] <- dd1
    rr[!wh] <- dd2
    if (length(names)>0) names(rr) <- names
  }
  rr
}
## -----------------------------------------------------------------
## ======================================================================
shortenstring <- function (x, n=50, endstring="..", endchars=NULL)
{ ## from plgraphics
  if (length(endchars)==0) endchars <- pmin(3,ceiling(n/10))
  if (any(li <- 1 >= (ncut <- n-nchar(endstring)-endchars))) {
    warning(":shortenstring: argument 'n' too small for given 'endstring' and 'endchar'")
    endstring <- ifelse(li, ".", endstring)
    endchars <- ifelse(li, 0, endchars)
    ncut <- n-nchar(endstring)-endchars
  }
  if (length(x) && any(n < (lnc <- nchar(x))))
    ifelse(n<lnc & ncut>1, paste(substring(x, 1, ncut), endstring,
          substring(x, lnc-endchars+1, lnc), sep=""), x)
}
## ======================================================================
i.getIfrData <-
  function(object, ...)
{
  if(is.atomic(object)) object <- rbind(object)
  lobj <- as.data.frame(object)
  lnmdf <- names(lobj)
  largs <- list(...)
  if (length(largs$estimate)==0) 
    largs$estimate <-
      i.def(if(is.list(object)) object$estimate, cbind(object)[,1])
  lnm <- names(largs)
  latr <- attributes(object)
  lnmatr <- names(latr)
  for (inm in lnm) {
    ld <- largs[[inm]]
    if (length(ld)==0 || all(is.na(ld))) {
      ld <- if (inm%in%lnmdf) lobj[[inm]] 
            else {
              if (inm%in%lnmatr) latr[[inm]] else NA }
    }
    largs[inm] <- list(ld)
  }
  rr <- data.frame(largs)
  if(length(dim(object)) && nrow(rr)==nrow(object))
    row.names(rr) = row.names(object)
  rr
}
