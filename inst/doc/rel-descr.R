## ----preliminary, echo=F, message=F-------------------------------------------
## library(plgraphics, lib.loc="/u/stahel/R/regdevelop/pkg/plgraphics.Rcheck")
library(relevance) ##, lib.loc="/u/stahel/R/regdevelop/pkg/relevance.Rcheck")
## options(warn=1)

## ----rlvthres-----------------------------------------------------------------
getOption("rlv.threshold")

## ----twosamples---------------------------------------------------------------
  t.test(sleep[sleep$group == 1, "extra"], sleep[sleep$group == 2, "extra"])
( r.sleep <- 
    twosamples(sleep[sleep$group == 1, "extra"], sleep[sleep$group == 2, "extra"])
)

## ----sleep2-------------------------------------------------------------------
t.oldopt <- options(show.inference = "classical")
r.sleep
options(t.oldopt)  ##  restore the old options

## ----correlation--------------------------------------------------------------
correlation(iris[1:50,1:2], method="spearman")

## ----termtable----------------------------------------------------------------
  data(swiss, package="datasets")
  rr <- lm(Fertility ~ . , data = swiss)
  rt <- termtable(rr)
  rt
  names(rt) ## The result of termtable has 22 columns
  if(interactive()) { ## too much avoidable output for the vignette
    str(rt)  
    data.frame(rt) ## or  print(rt, show="all")
    ## This avoids selection and preparation of columns by 'print.inference'. 
  }

## ----<termtablePrint----------------------------------------------------------
  t.oldopt <- options(show.inference = "classical")
  rt
  options(t.oldopt)  ##  restore the old options

## ----plotInference, fig.height=4, fig.width=9---------------------------------
plot(rt)

## ----termeffects, fig.width=9, fig.height=6-----------------------------------
  data(d.blast)
  r.blast <-
    lm(log10(tremor)~location+log10(distance)+log10(charge), 
       data=d.blast)
  ( rte <- termeffects(r.blast) )

  plot(termeffects(r.blast))  ## plot effects for terms with >1 df

## ----inference----------------------------------------------------------------
  ( rr <- inference(r.blast) )

## ----replication--------------------------------------------------------------
data(d.osc15Onesample)
to <- structure(d.osc15Onesample[,c("effecto","teststatistico","no")],
      names=c("effect","teststatistic","n"))
tr <- structure(d.osc15Onesample[,c("effectr","teststatisticr","nr")],
      names=c("effect","teststatistic","n"))
( rr <- replication(to, tr, rlv.threshold=0.1) )
plot(rr)

## ----replication.plgroups-----------------------------------------------------
plot(attr(rr, "estimate"), refline=c(0,1),
     label2=attr(rr, "rplclass"), xlab="relevance")

## ----show---------------------------------------------------------------------
  showd(d.blast)

## ----getOption----------------------------------------------------------------
  t.opt <- options(show.terms.relevance=c("coef", "dropRls", "dropRls.symbol"))
  rt
  
## restore the old options
  options(t.opt) ## the former options
  options(relevance.options) ## restore the package's defaults

## ----printlist----------------------------------------------------------------
rpr <- print(termeffects(r.blast), print=FALSE)
attr(rpr, "head") <- sub("lm", "Linear Regression", attr(rpr, "head"))
rpr

## ----sleep--------------------------------------------------------------------
data(sleep)
dd <- subset(sleep, group==2)
onesample(60*dd$extra, rlv.threshold=60, standardize=FALSE)

## ----anchoring, fig.height=4, fig.width=9-------------------------------------
data(d.everest)
rr <- twosamples(log(y)~g, data=d.everest, var.equal=TRUE)
print(rr, show="classical")
rr

pltwosamples(log(y)~g, data=d.everest)

## ----blast, fig.height=3------------------------------------------------------
dd <- d.blast[seq(1,388,3),]
dd <- na.omit(dd[dd$location %in% paste("loc",c(1,2,4),sep=""),])
dd$time <- as.numeric(dd$date-min(dd$date))/365

rlm <- lm(log10(tremor)~location+log10(distance)+log10(charge)+time, data=dd,
          contrasts=list(location="contr.sum"))
( rt <- termtable(rlm) )
plot(rt)

