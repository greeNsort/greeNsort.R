## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(greeNsort)

## -----------------------------------------------------------------------------
c(logical=perfcores(), physical=perfcores(logical=FALSE))

## -----------------------------------------------------------------------------
perfmachine()

## -----------------------------------------------------------------------------
perfcompiled()

## -----------------------------------------------------------------------------
perfcheck()

## -----------------------------------------------------------------------------
perfmax()

## -----------------------------------------------------------------------------
perfnow()

## -----------------------------------------------------------------------------
invisible(prunif(32))  ## dummy call to load prunif 
n <- 1e7
p <- perfnow()
invisible(sample(n))
s <- perfthen(p, 'runif')
p <- perfnow()
invisible(runif(n))
r <- perfthen(p, 'runif')
p <- perfnow()
invisible(prunif(n))
pr <- perfthen(p, 'prunif')
rbind(s, r, pr)

## -----------------------------------------------------------------------------
perfmon(refsec = 3, monsec=1, maxsec = 3)

## -----------------------------------------------------------------------------
i <- perfsleep(5)
i

## -----------------------------------------------------------------------------
x <- runif(1e6)    ## a vector with 1e6 uniform random numbers
Knuthsort(x)

## -----------------------------------------------------------------------------
x <- prunif(1e8)
PKnuthsort(x)

## -----------------------------------------------------------------------------
f <- function(n){x <- runif(n); secs(Quicksort2(x))}
s <- sapply(rep(1000, 50), f); vs <- sd(s)/mean(s)
l <- sapply(rep(1000000, 50), f); vl <- sd(l)/mean(l)
c(small=vs, large=vl, ratio=vs/vl)

## -----------------------------------------------------------------------------
x <- prunif(2^25)
y <- x[]  ## now we have two identical copies of the data
k <- Knuthsort(x)
f <- Frogsort2(y)
e <- rbind(k, f)
rbind(e, Ratio=e["Knuthsort",]/e["Frogsort2",])

## -----------------------------------------------------------------------------
cbind(
  size(f)
, secs(f)
, sizesecs(f)
, base(f)
, core(f)
, unco(f)
, dram(f)
)

## -----------------------------------------------------------------------------
pe <- perf(e)
rbind(pe, Ratio=pe[1,]/pe[2,])

## -----------------------------------------------------------------------------
aEnergy(e)

## -----------------------------------------------------------------------------
perfcalibrate(3, minsec = 2, maxsec = 2, save=FALSE)

## -----------------------------------------------------------------------------
.perfbackground

## -----------------------------------------------------------------------------
options(greensort_perf_calc= "raw")
r <- rbind(
  f
, rawperf(f)
, bacperf(f)
, difperf(f)
, adjperf(f)
, amdperf(f)
, optperf(f)
)
rownames(r)[-1] <- c("raw","bac","dif","adj","amd","opt")
r

## -----------------------------------------------------------------------------
require(parallel)
n <- 1e6
pc <- perfcores(logical=FALSE)
p <- 2
th <- pc %/% p
cl <- makeCluster(p)
invisible(clusterEvalQ(cl, require(greeNsort)))
invisible(clusterExport(cl, c("n","th")))
invisible(clusterEvalQ(cl, {
  set.seed(1)
  x <- runif(n)
  NULL
}))
pm <- do.call("rbind", clusterEvalQ(cl, {
  PKnuthsort(x, threads = th)
}))
stopCluster(cl)
pm[,"p"] <- 2
cbind(pm, aTime(pm))

## -----------------------------------------------------------------------------
e <- rbind(Knuthsort=k, Frogsort2=f)
e <- cbind(e, aFootprint(e))
rbind(e, Ratio=e[1,]/e[2,])

## ----elang, echo=FALSE--------------------------------------------------------
main <- structure(list(Energy = c(1, 1.03, 3.23, 2.52, 3.14, 1.98, 4.45, 
75.88), Time = c(1, 1.04, 2.83, 4.2, 3.14, 1.89, 6.52, 71.9), 
    Memory = c(1, 1.05, 1.05, 1.24, 2.85, 6.01, 4.59, 2.8)), row.names = c("C", 
"Rust", "Go", "Fortran", "C#", "Java", "Javascript", "Python"
), class = "data.frame")
extra <- structure(list(Energy = c(1, 1.8), Time = c(1, 3.39), Memory = c(1, 
3.71)), row.names = c("Rust", "Julia"), class = "data.frame")
elang <- rbind(main, Julia=extra["Julia",]*main["Rust",]) 
plot(elang[-1,"Energy"], elang[-1,"Memory"], type="n", xlab="Energy relative to C  (logarithmic)", ylab="Memory relative to C (logarithmic)", log="xy")
text(elang[-1,"Energy"], elang[-1,"Memory"], rownames(elang)[-1])

## -----------------------------------------------------------------------------
elang$eFootprint <- elang$Energy * elang$Memory
elang$tFootprint <- elang$Time * elang$Memory
elang <- elang[order(elang$eFootprint),c("Memory","Time","Energy","tFootprint","eFootprint")]
##require(kableExtra, quietly = TRUE)
#round(elang, 2) %>% kbl(caption = "Language Inefficiency relative to C (ordered by eFootprint)") %>% kable_paper("striped")
round(elang, 2)

## -----------------------------------------------------------------------------
perflabels()["bcdEnergy"]

