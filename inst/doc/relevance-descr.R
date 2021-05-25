## ----preliminary, echo=F, message=F-------------------------------------------
## library(plgraphics, lib.loc="/u/stahel/R/regdevelop/pkg/plgraphics.Rcheck")
library(relevance) ##, lib.loc="/u/stahel/R/regdevelop/pkg/relevance.Rcheck")
## options(warn=1)

## ----rlvthres-----------------------------------------------------------------
getOption("rlv.threshold")

## ----twosamples---------------------------------------------------------------
  t.test(sleep[sleep$group == 1, "extra"], sleep[sleep$group == 2, "extra"])
(r.sleep <- 
   twosamples(sleep[sleep$group == 1, "extra"], sleep[sleep$group == 2, "extra"])
)

## ----sleep2-------------------------------------------------------------------
t.oldopt <- options(show.inference = "classical")
r.sleep
options(t.oldopt)  ##  restore the old options

## ----termtable----------------------------------------------------------------
  data(swiss, package="datasets")
  rr <- lm(Fertility ~ . , data = swiss)
  rt <- termtable(rr)
  rt
  names(rt)
  if(interactive()) { ## too much avoidable output for the vignette
    str(rt)  
    print(data.frame(rt)) ## or  print(rt, show="all")
    ## This avoids selection and preparation of columns 
    ## by 'print.inference'. It produces an extensive output.
  }

## ----plotInference, fig.height=5, fig.width=9---------------------------------
plot(rt)

## ----termeffects, fig.width=9, fig.height=6-----------------------------------
data(d.blast)
r.blast <-
  lm(log10(tremor)~location+log10(distance)+log10(charge), data=d.blast)
(rte <- termeffects(r.blast))
print(rte, show=c("classical","coefRls","coefRls.symbol"), single=TRUE)

plot(termeffects(r.blast), single=TRUE)  ## plot all effects

## ----getOption----------------------------------------------------------------
t.opt <- options(show.term.relevance=c("coef", "dropRls", "dropRls.symbol"))
rt
## restore the old options
options(list = t.opt) ## the former options
options(list=relevance.options) ## restore the package's defaults

## ----printlist----------------------------------------------------------------
rr <- print(termeffects(r.blast), print=FALSE)
attr(rr, "head") <- sub("lm", "Linear Regression", attr(rr, "head"))
print(rr)

