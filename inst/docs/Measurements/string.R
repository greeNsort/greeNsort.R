# measurements for the greeNsort innovation report
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk

setwd("/home/jo/SIK/publish/report/greeNsort")
require(greeNsort)

if (file.exists(file.path("inst", "string.RData"))){
  load(file.path("inst", "string.RData"))
}else{
  if (!perfcheck()){
    message("writing new powercap_config.h")
    perfconfig()
    message("please re-compile package")
  }
  if (inherits(try(perfsleep(0)), "try-error")){
    perfgrants()
  }

  D <- c("permut", "ascglobal", "asclocal", "descglobal", "desclocal", "tielog2", "ascall", "descall")
  A <- c("VKnuth","VFrog1"
         , "UKnuth", "UZack","UZackB"
         ,"WQuick2","WQuick2B"
  )
  R <- 1:100
  n <- 2^18

  nD <- length(D)
  nA <- length(A)
  nR <- length(R)

  string <- matrix(NA, nrow=nD*nA*nR, ncol=ncol(perfsleep(0))+1)
  dimnames(string) <- list(algo=1:(nD*nA*nR), perf=c("r", colnames(perfsleep(0))))

  i <- 0L
  lasttim <- proc.time()
  for (r in R){
    print(r)
    set.seed(r)
    for (d in D){
      x <- num2str(testdb[[d,"func"]](n))
      for (a in sample(A)){
        #print(c(r=r, d=d, a=a))
        y <- x[]
        tim <- algodb[[a, "func"]](y)
        i <- i + 1L
        string[i,-1] <- tim
        string[i,1] <- r
        rownames(string)[i] <- rownames(tim)
      }
    }
    thistim <- proc.time()
    print(thistim - lasttim)
    lasttim <- thistim
  }
  rm(x,y)

  Sys.sleep(5)
  if (!exists(".perfbackground"))
    perfcalibrate()

  save.image(file.path("inst", "string.RData"))
  load(file.path("inst", "string.RData"))

  options(greensort_perf_calc='adj')

  summary(string)

  pstring <- data.frame(r=string[,"r"], a=rownames(string), d=NA, perf(string))
  pstring$a <- sub("sort","", pstring$a)
  colnames(pstring)[colnames(pstring)=="X.RAM"] <- "%RAM"
  i <- 0L
  for (r in R){
    print(r)
    set.seed(r)
    for (d in D){
      for (a in sample(A)){
        i <- i + 1L
        pstring$d[i] <- d
      }
    }
  }
  summary(pstring)

  benchmark <- c("permut","tielog2","ascglobal","asclocal","ascall")  #,"descglobal","desclocal","descall")
  tstring <- pstring[pstring$d %in% benchmark,]
  tstring <- aggregate(pstring[,-(1:3)], by = list(r=pstring$r, a=pstring$a), FUN=mean)
  tstring$d <- "TOTAL"
  tstring <- tstring[,colnames(pstring)]
  summary(tstring)

  pstring <- rbind(pstring, tstring)
  rm(tstring)

  mstring <- aggregate(pstring[,-(1:3)], by = list(d=pstring$d, a=pstring$a), FUN=median)
  summary(mstring)

  dlab <- c(
    TOTAL = "T"
    , permut = "p"
    , tielog2 = "t"
    , ascglobal = "g"
    , descglobal = "g"
    , asclocal = "l"
    , desclocal = "l"
    , ascall = "a"
    , descall = "a"
  )

  dcol <- c(
    TOTAL = "black"
    , permut = "white"
    , tielog2 = "yellow"
    , ascglobal = "green"
    , asclocal = "green"
    , ascall = "green"
    , descglobal = "red"
    , desclocal = "red"
    , descall = "red"
  )

  tmp <- aggregate(mstring$"%RAM", by = list(a=mstring$a), FUN=mean)
  strram <- tmp$x
  names(strram) <- tmp$a
  rm(tmp)

  #r <- aggregate(mstring$pcdEnergy, by = list(r=mstring$d), FUN=mean)
  #r <- r[order(r$x),]
  #dput(r$r)
  Df <- c(
    "TOTAL"
    , "ascall"
    , "descall"
    , "ascglobal"
    , "descglobal"
    , "asclocal"
    , "desclocal"
    , "tielog2"
    , "permut"
  )

  strDot <- function(alg=A, kpi="pcdEnergy"){
    oldpar <- par(bg="grey40")
    on.exit(par(oldpar))
    x <- mstring[mstring$a %in% alg,]
    y <- x[x$d=="TOTAL",]
    y <- y[order(y[[kpi]]),]
    dotchart(y[[kpi]], labels=y$a, main=paste("ordered by TOTAL", kpi), xlim=c(0, max(x[[kpi]])), xlab=perfunits(kpi), col=dcol[["TOTAL"]], pch=dlab["TOTAL"])
    for (d in D){
      z <-  x[x$d==d,]
      z <-z[match(y$a, z$a),]
      points(z[[kpi]], seq_len(nrow(z)), col=dcol[d], pch=dlab[d])
    }
  }

  strPlot <- function(
      pro = "VFrog1"
    , kpi = "pcdEnergy"
    , ref = "VKnuth"
    , procol = "green"
    , refcol = "red"
    , refcol2 = "magenta"
  ){
    x <- pstring[pstring$a==pro,]
    ypro <- mstring[mstring$a == pro,]
    yref <- mstring[mstring$a == ref,]
    x$d <- factor(x$d, levels=Df)
    ypro$d <- factor(ypro$d, levels=Df)
    yref$d <- factor(yref$d, levels=Df)
    x$relkpi <- x[[kpi]]/yref[match(x$d, yref$d),kpi]
    ypro$relkpi <- ypro[[kpi]]/yref[[kpi]]
    yref$relkpi <- yref[[kpi]]/yref[[kpi]]

    oldpar <- par(bg="white", las=2, mfrow=c(1,2))
    on.exit(par(oldpar))

    boxplot(split(x[[kpi]], x$d), data=x, xlab="", main=paste(pro, kpi), col=procol
            , ylab=paste0(perfunits(kpi))
            , ylim=range(
              min(ypro[[kpi]])/1.04
              , max(ypro[[kpi]])*strram[ref]/strram[pro]*1.04
              , min(yref[[kpi]])/1.04
              , max(yref[[kpi]])*strram[ref]/strram[pro]*1.04
              , quantile(x[[kpi]], c(0.01, 0.99)))
    )
    z <- yref[,c("d","%RAM",kpi)]
    z <- z[order(z$d),kpi]
    if (length(grep("Energy", kpi)) || kpi=="runTime"){
      z2 <- z * strram[ref]/strram[pro]
      segments(seq_along(z2)-0.5, z2, seq_along(z2)+0.5, z2, col=refcol2, lwd=2)
    }
    segments(seq_along(z)-0.5, z, seq_along(z)+0.5, z, col=refcol, lwd=2)

    boxplot(split(x$relkpi, x$d), data=x, xlab=""
            , main=paste(pro, "/", ref, kpi), col=procol
            , ylab="kpi ratio (logarithmic)"
            , ylim=range(
              min(ypro$relkpi)/1.04
              , max(ypro$relkpi)*strram[ref]/strram[pro]*1.04
              , min(yref$relkpi)/1.04
              , max(yref$relkpi)*strram[ref]/strram[pro]*1.04
              , quantile(x$relkpi, c(0.01, 0.99))
            )
            , log="y"
    )
    z <- yref[,c("d","%RAM","relkpi")]
    z <- z[order(z$d),"relkpi"]
    if (length(grep("Energy", kpi)) || kpi=="runTime"){
      z2 <- z * strram[ref]/strram[pro]
      segments(seq_along(z2)-0.5, z2, seq_along(z2)+0.5, z2, col=refcol2, lwd=2)
    }
    segments(seq_along(z)-0.5, z, seq_along(z)+0.5, z, col=refcol, lwd=2)
  }

  strTab <- function(
    pro = "VFrog1"
    , kpi=c("runTime","pcdEnergy","pcdFootprint")
    , ref = "VKnuth"
  ){
    x <- pstring[pstring$a %in% c(pro,ref),c("a","d","r","%RAM", kpi)]
    y <- mstring[mstring$a %in% c(pro,ref),c("a","d","%RAM", kpi)]
    x$d <- factor(x$d, levels=Df)
    y$d <- factor(y$d, levels=Df)
    x <- x[order(x$a, x$d, x$r),]
    y <- y[order(y$a, y$d),]
    z <- split(y, y$d)
    ret0 <- do.call("rbind", lapply(z, function(z){
      z[z$a==pro,'pcdEnergy'] - z[z$a==ref,'pcdEnergy']
    }))
    rownames(ret0) <- names(z)
    colnames(ret0) <- paste0("d(pcdE)")
    ret0 <- round(ret0, 2)
    ret1 <- do.call("rbind", lapply(z, function(z){
      z[z$a==pro,-(1:2)] / z[z$a==ref,-(1:2)]
    }))
    rownames(ret1) <- names(z)
    colnames(ret1) <- paste0("r(", perflabels(colnames(ret1)), ")")
    ret1 <- round(ret1, 2)
    z <- split(x, x$d)
    ret2 <- do.call("rbind", lapply(z, function(z){
      ret <- do.call("cbind", lapply(kpi, function(k){
        wilcox.test(z[z$a==pro,k], y = z[z$a==ref,k]
                    , alternative = "two.sided"
                    , mu = 0
                    , paired = TRUE
        )$p.value
      }
      )
      )
      colnames(ret) <- paste0("p(",perflabels(kpi), ")")
      ret
    }))
    rownames(ret2) <- names(z)
    ret2 <- round(ret2, 4)
    cbind(ret1,ret0,ret2)
  }

  save.image(file.path("inst", "string.RData"))
}



require(lattice)
require(gplots)

pdffile <- file.path("inst", "string.pdf")
if (!file.exists(pdffile)){

  pdf(pdffile, width=7, height=7, pointsize=10 )

  y <- mstring[mstring$d=="TOTAL",]
  y <- y[order(y$pcdEnergy),]
  dotchart(y$pcdEnergy, labels=y$a, main=paste("TOTAL"), xlim=c(0, max(y$pcdEnergy)), xlab=perflabels("pcdEnergy"))

  strDot(kpi="runTime")
  strDot(kpi="pcdEnergy")
  strDot(kpi="pcdFootprint")

  ref <- c(
      "WQuick2" = "UKnuth"
    , "WQuick2B" = "UKnuth"
    , "WQuick2B" = "WQuick2"
    , "UZack"  = "UKnuth"
    , "UZack"  = "WQuick2"
    , "UZackB"  = "UKnuth"
    , "UZackB"  = "WQuick2B"
    , "UKnuth"  = "WQuick2"
    , "VKnuth" = "UKnuth"
    , "VFrog1" = "UKnuth"
    , "VFrog1" = "VKnuth"
  )
  for (i in seq_along(ref)){
    a <- names(ref)[i]
    r <- ref[i]
    strPlot(a, "runTime", ref=r)
    strPlot(a, "pcdEnergy", ref=r)
  }

  x <- pstring[pstring$d=="TOTAL",]
  y <- mstring[mstring$d=="TOTAL",]
  x$a <- factor(x$a, levels=y$a[order(y$runTime)])
  print(xyplot(runTime ~ r | a,  x, main=paste("TOTAL", "runTime"), ylim=c(0, max(x$runTime))))
  x$a <- factor(x$a, levels=y$a[order(y$pcdEnergy)])
  y <- y[order(y$pcdEnergy),]
  print(xyplot(pcdEnergy ~ r | a,  x, main=paste("TOTAL", "pcdEnergy"), ylim=c(0, max(x$pcdEnergy))))

  y <- pstring[pstring$d!="TOTAL",]
  for (a in A){
    x <- y[y$a==a,]
    print(xyplot(runTime ~ r | d,  x, main=paste("runTime", a), ylim=c(0, max(x$runTime))))
  }
  for (a in A){
    x <- y[y$a==a,]
    print(xyplot(pcdEnergy ~ r | d,  x, main=paste("pcdEnergy", a), ylim=c(0, max(x$pcdEnergy))))
  }

  dev.off()

}


