# measurements for the greeNsort innovation report
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk

setwd("/home/jo/SIK/publish/report/greeNsort")
require(greeNsort)
require(parallel)


if (file.exists(file.path("inst", "parallel.RData"))){
  load(file.path("inst", "parallel.RData"))
}else{
  if (!perfcheck()){
    message("writing new powercap_config.h")
    perfconfig()
    message("please re-compile package")
  }
  if (inherits(try(perfsleep(0)), "try-error")){
    perfgrants()
  }

  D <- c("permut", "ascglobal", "asclocal", "tielog2", "ascall")
  A <- c("PKnuth","PFrog0","PFrog1","PFrog2","PFrog3","PDuckB","PQuick2B")
  P <- c(1,2,4,8)
  T <- c(1,2,4,8)
  M <- c("inner","median","outer")
  R <- 50
  R2 <- 5
  n <- 2^24

  #warmup P <- c(8); T <- c(8);

  L <- vector(mode="list", length(D)*length(A)*length(P)*(length(T)+1)*length(M))
  dim(L) <- c(D=length(D),A=length(A),P=length(P),T=(length(T)+1),M=length(M))
  dimnames(L) <- list(D=D,A=A,P=P,T=c(0,T),M=M)

  m <- c("secs","base","core","unco","dram")
  for (d in D){
    for (a in A){
      for (p in P){
        cp <- as.character(p)
        cl <- makeCluster(p)
        clusterEvalQ(cl, require(greeNsort))
        clusterExport(cl, c("d","a","p","R","R2","n"))
        clusterEvalQ(cl, {
          set.seed(1)
          x <- testdb[d, "func"][[1]](n)
          NULL
        })
        for (t in c(0, T)){
          ct = as.character(t)
          print(c(d=d, a=a, p=p, t=t))
          if (t==0){
            # overhead
            b <- perfnow()
            i <- clusterEvalQ(cl, {
              do.call("rbind", lapply(1:(R+R2), function(r){
                y <- x[]
                perfsleep(0)
              }))
            })
            o <- perfthen(b, rowname="perfsleep", n=0, b=0, p=0, t=0, size=0)
            o[,m] <- o[,m]/(R+R2)
          }else{
            # algorithm
            clusterExport(cl, c("t"))
            b <- perfnow()
            i <- clusterEvalQ(cl, {
              do.call("rbind", lapply(1:(R+R2), function(r){
                y <- x[]
                algodb[a, "func"][[1]](y, threads = t)
              }))
            })
            o <- perfthen(b, rowname=algodb[a,"name"], n=n, b=8, p=p, t=t, size=mean(i[[1]][,"size"]))
            i <- lapply(i, function(y){y[,"p"]<-p; y})
            o[,m] <- o[,m]/(R+R2) - L[[d,a,cp,"0","outer"]][,m]
          }
          L[[d,a,cp,ct,"inner"]] <- i
          md <- apply(do.call("rbind", lapply(i, function(z)z[1:R,,drop=FALSE])), 2, median)
          dim(md) <- dim(o)
          dimnames(md) <- dimnames(o)
          L[[d,a,cp,ct,"median"]] <- md
          L[[d,a,cp,ct,"outer"]] <- o
        }
        stopCluster(cl)
      }
    }
  }

  Sys.sleep(5)
  if (!exists(".perfbackground"))
    perfcalibrate()

  save.image(file.path("inst", "parallel.RData"))
  load(file.path("inst", "parallel.RData"))

  # if (FALSE){
  #   for (d in D){
  #     for (a in A){
  #       for (p in P){
  #         cp <- as.character(p)
  #         for (t in c(0,T)){
  #         }
  #       }
  #     }
  #   }
  # }

  options(greensort_perf_calc='adj')

  inner <- NULL
  outer <- NULL
  median <- NULL

  for (d in D){
    for (a in A){
      for (p in P){
        cp <- as.character(p)
        for (t in c(0,T)){
          ct = as.character(t)
          print(c(d=d, a=a, p=p, t=t))
          if (t>0){
            i <- L[[d,a,cp,ct,"inner"]]
            x <- do.call("rbind", lapply(seq_along(i), function(j){
              y <- i[[j]]
              z <- as.data.frame(perf(y), row.names = FALSE)
              z$t <- t
              z$q <- j
              z$p <- p
              z$r <- seq_along(z$p)
              z$d <- d
              z$a <- a
              z
            }))
            inner <- rbind(inner, x, make.row.names = FALSE)

            y <- L[[d,a,cp,ct,"median"]]
            z <- as.data.frame(perf(y), row.names = FALSE)
            z$t <- t
            z$p <- p
            z$d <- d
            z$a <- a
            median <- rbind(median, z, make.row.names = FALSE)

            y <- L[[d,a,cp,ct,"outer"]]
            z <- as.data.frame(perf(y), row.names = FALSE)
            z$t <- t
            z$p <- p
            z$d <- d
            z$a <- a
            outer <- rbind(outer, z, make.row.names = FALSE)
          }
        }
      }
    }
  }

  summary(inner)
  summary(median)
  summary(outer)

  subset(median, d=="permut" & a=="PKnuth")
  subset(outer, d=="permut" & a=="PKnuth")

  cols <- c("d", "a", "p", "t", "%RAM", "runTime", "lifeTime", "bEnergy", "cdEnergy", "pcdEnergy", "tFootprint", "cdFootprint", "pcdFootprint")
  inner <- inner[c("q", "r", cols)]
  median <- median[,cols]
  outer <- outer[,cols]

  summary(inner)
  summary(median)
  summary(outer)

  # select the first R measurements
  inner <- inner[inner$r <= R,]
  # and average across processes
  inner <- aggregate(inner[,-(1:6)], by = list(d=inner$d, a=inner$a, p=inner$p, t=inner$t, r=inner$r), FUN=mean)
  summary(inner)

  tinner <- aggregate(inner[,-(1:5)], by = list(a=inner$a, p=inner$p, t=inner$t, r=inner$r), FUN=mean)
  tinner$"%RAM" <- tinner$"%RAM" / 5
  tinner <- cbind(data.frame(d="TOTAL"), tinner)

  touter <- aggregate(outer[,-(1:4)], by = list(a=outer$a, p=outer$p, t=outer$t), FUN=mean)
  touter$"%RAM" <- touter$"%RAM" / 5
  touter <- cbind(data.frame(d="TOTAL"), touter)

  tmedian <- aggregate(median[,-(1:4)], by = list(a=median$a, p=median$p, t=median$t), FUN=mean)
  tmedian$"%RAM" <- tmedian$"%RAM" / 5
  tmedian <- cbind(data.frame(d="TOTAL"), tmedian)

  summary(tinner)
  summary(tmedian)
  summary(touter)

  inner <- rbind(inner, tinner)
  outer <- rbind(outer, touter)
  median <- rbind(median, tmedian)

  summary(inner)
  summary(median)
  summary(outer)

  rm(tinner)
  rm(touter)
  rm(tmedian)


  parPlot <- function(algo, kpi="pcdEnergy", ref=NULL, data="TOTAL"
  , color=c("red","yellow","green"), marg=c(8, 8), cex.main=1.2, cex.lab=1.2
  ){
    pl <- colorRampPalette(color)
    x <- median[median$d==data & median$a==algo, c("p","t",kpi)]
    m <- array(NA, dim=c(length(T), length(P)), dimnames=list(T=T,P=P))
    i <- cbind(match(x$t, T),match(x$p, P))
    m[i] <- x[[kpi]]
    if (is.null(ref)){
      m <- m / m["1","1"]
      tit <- paste0(data, "  ", kpi, "\n", algo, "/", algo," 1x1")
    }else{
      y <- median[median$d==data & median$a==ref, c("p","t",kpi)]
      i <- cbind(match(y$t, T),match(y$p, P))
      m[i] <- m[i] / y[[kpi]]
      tit <- paste(data, "  ", kpi, "\n", algo, "/", ref, sep="")
    }
    rownames(m) <- paste(rownames(m), "treads")
    colnames(m) <- paste(colnames(m), "procs")
    #from library(gplots)
    lm <- -log(m)
      oldpar <- par(cex.lab = cex.lab, cex.main = cex.main)
      on.exit(par(oldpar))
      heatmap.2(x = lm
                , Rowv = FALSE
                , dendrogram = "none"
                , col = pl
                , symm = TRUE
                , cellnote = round(m, 2)
                , notecol = "black"
                , notecex = cex.lab * 1.5
                , trace = "none"
                , key = FALSE
                , margins = marg
                , main = tit
      )
  }

  parDot <- function(
      kpi="pcdEnergy"
    , data="TOTAL"
    , ref=NULL
    , algos=c("PKnuth", "PFrog0", "PFrog1", "PFrog2", "PFrog3", "PQuick2B", "PDuckB")
    , para = c("1x2","1x4", "1x8","4x1","4x4", "8x1", "8x8")
    , color=c("yellow","green", "blue", "cyan", "magenta", "red","violet")
    , plot=TRUE
    , cex=1.2
  ){
    oldpar <- par(bg="grey40", cex=cex, no.readonly = TRUE)
    on.exit(par(oldpar))
    y <- subset(median, d==data)
    y$a <- factor(y$a, levels=c("PKnuth", "PFrog0", "PFrog1", "PFrog2", "PFrog3", "PQuick2B", "PDuckB"))
    y <- y[y$a %in% algos,]
    y$para <- factor(paste(y$p, y$t, sep="x"))
    para <- unique(c("1x1", para))
    color <- unique(c("black", color))
    y <- y[y$para %in% para,]
    if (!is.null(ref))
      y[[kpi]] <- y[[kpi]] / subset(y, a==ref & p==1 & t==1)[[kpi]]
    x <- subset(y, para == "1x1")
    x <- x[order(x$a, decreasing = TRUE),]
    xlim <- range(y[[kpi]])
    dotchart(x[[kpi]], labels = x$a, pch="1", xlim=xlim, main=paste(data, kpi), xlab=paste(kpi, if (!is.null(ref))paste("/", ref, "1x1")))
    xgrid <- pretty(xlim, 5)
    xgrid <- xgrid[xlim[1] <= xgrid & xgrid <= xlim[2]]
    abline(v=xgrid, col="grey", lty=3)
    abline(v=1, col="grey", lty=1)
    for (i in seq_along(para)){
      x <- y[y$para == para[i],]
      x <- x[order(x$a, decreasing = TRUE),]
      text(x[[kpi]], seq_along(x[[kpi]]), sub("1x", "", para[i]), col=color[i])
    }
  }

  save.image(file.path("inst", "parallel.RData"))
}


require(lattice)
require(gplots)


pdffile <- file.path("inst", "parallel.pdf")
if (!file.exists(pdffile)){

  pdf(pdffile, width=7, height=7, pointsize=10 )

  parDot(kpi="runTime")
  parDot(kpi="lifeTime")
  parDot(kpi="pcdEnergy")
  parDot(kpi="pcdFootprint")

  parDot(kpi="runTime", ref="PKnuth")
  parDot(kpi="lifeTime", ref="PKnuth")
  parDot(kpi="pcdEnergy", ref="PKnuth")
  parDot(kpi="pcdFootprint", ref="PKnuth")

  for (k in c("lifeTime","pcdEnergy")){
    for (a in A){
      for (d in c("TOTAL", D)){
        parPlot(a, data=d, kpi=k)
      }
    }
  }

  ref <- "PKnuth"
  for (k in c("lifeTime","pcdEnergy","pcdFootprint")){
    for (a in A){
      for (d in c("TOTAL", D)){
        parPlot(a, ref=ref, data=d, kpi=k)
      }
    }
  }

  ref <- "PQuick2B"
  for (k in c("lifeTime","pcdEnergy")){
    for (a in c("PQuick2B","PDuckB")){
      for (d in c("TOTAL", D)){
        parPlot(a, ref=ref, data=d, kpi=k)
      }
    }
  }

  for (d in c("TOTAL", D)){
    y <- inner[inner$d==d,]
    for (a in A){
      x <- y[y$a==a,]
      print(xyplot(lifeTime ~ r | p * t,  x, main=paste(d, "lifeTime", a), ylim=c(0, max(y$lifeTime))))
    }
  }

  for (d in c("TOTAL", D)){
    y <- inner[inner$d==d,]
    for (a in A){
      x <- y[y$a==a,]
      print(xyplot(pcdEnergy ~ r | p * t,  x, main=paste(d, "pcdEnergy", a), ylim=c(0, max(y$pcdEnergy))))
    }
  }

  dev.off()

}

