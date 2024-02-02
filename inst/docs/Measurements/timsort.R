# measurements for the greeNsort innovation report
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk

require(greeNsort)
require(abind)

path <- "../greeNsort.web/"
#path <- "~/"

plotit <- function(ref, set, crit){
  set <- c(set, ref, "Tim")
  S <- S[set]
  C <- C[set]
  C2 <- rgb(t(col2rgb(C[c(ref,"Tim")])), maxColorValue = 255, alpha=0.6*255)
  refval <- M["Knuth",crit,I]
  X <- t(M[set,crit,])
  X <- 100 * X / refval
  ds <- X[,"Tim"] - X[,ref]
  sumtim <- sum(X[,"Tim"])
  sumref <- sum(X[,ref])
  REF <- M["Knuth",,]
  TIM <- 100 * M["Tim",,] / REF[,I]
  REF <- 100 * REF / REF[,I]
  ylim <- c(floor(min(TIM-REF)/10)*10, ceiling(10*max(TIM/REF))*10)
  plot(E,E,ylim=ylim, type="n", axes=FALSE
       , xlab="amount of noise e"
       , ylab="% of Mergesort on totally random data"
       , main=paste(if (ref=='Knuth') "Mergesort" else algodb[[ref,"name"]], " costs ", round(100*sumref/sumtim), "% of total ",if (crit=="secs") "Runtime (~Energy)" else "Footprint"," of Timsort", sep="")
  )
  axis(1, E)
  mtext(c(paste("perfectly ", if (decreasing) "descending" else "ascending", sep=""),"totally random"), 1, line=2, at=c(0,max(E)))
  axis(2, seq(ylim[1], ylim[2], 10))
  do.call("rect", c(as.list(par("usr")[c(1,3,2,4)]), list(col="grey40")))
  abline(h=0, col="grey")
  rect(E-0.45, 0, E+0.45, ds, col=C2[((ds<0)+1)], border=C2[((ds<0)+1)])
  segments(E, X[,"Tim"], E, X[,ref], col=C2[((ds<0)+1)])
  matpoints(E, X, pch=S, cex=1.4, col=C)
}

width <- height <- 8
E <- 0:19
I <- length(E)
N <- max(E)
RR <- 1
n<- 2^N
A <- c("Knuth","Octo","Walk","Grail","Frog1","Frog2","Squid1","Squid2","Tim")
S <- c("M","O","w","g","F","f","S","s","T"); names(S) <- A
C <- c("black","blue","cyan","magenta","green","green","violet","violet","red"); names(C) <- A
C2 <- c(rgb(0,1,0,0.6),rgb(1,0,0,0.6))
L <- vector("list", I)

decreasing <- TRUE
decreasing <- FALSE

for (i in seq_along(E)){
  e <- E[i]
  R <- length(N:e)*RR
  L[[i]] <- array(NA_real_, dim=c(length(A), 2, R), dimnames=list(A, c("secs","sizesecs"), 1:R))
  for (r in 1:R){
    set.seed(r)
    cat("e=", e, " n=", n, " r=", r ,"\n", sep="")
    if (decreasing){
      x <- seq(n-2^e, 0, length=n) + runif(n, 0, 2*(2^e-1))
      #x <- testdb$func$descshuffle(n, 2^(N-e))
      #x <- testdb$func$desclocal(n, 2^(N-e))
      #x <- testdb$func$descglobal(n, 2^e)
    }else{
      x <- seq(0, n-2^e, length=n) + runif(n, 0, 2*(2^e-1))
      #x <- testdb$func$ascshuffle(n, 2^(N-e))
      #x <- testdb$func$asclocal(n, 2^(N-e))
      #x <- testdb$func$ascglobal(n, 2^e)
    }
    for (a in sample(A)){
      y <- x[]
      L[[i]][a,,r] <- perf(algodb[[a,"func"]](y))[c("secs","sizesecs")]
    }
  }
}
M <- lapply(L, function(l){
  apply(l, 1:2, median)
})
M <- do.call("abind", list(M, along=3))
tim <- M["Tim","secs",]
M["Tim","sizesecs",] <- pmin(1.5, (1 + 0.5*(tim-tim[1])/(tim[I]-tim[1]))) * tim * n

if (decreasing){
  Ldesc <- L
  Mdesc <- M
}else{
  Lasc <- L
  Masc <- M
}


if (decreasing){
   L <- Ldesc
   M <- Mdesc
}else{
  L <- Lasc
  M <- Masc
}

ref <- "Knuth"
X <- t(M[c(ref,"Tim"),"secs",])
ds <- X[,"Tim"] - X[,ref]
png(paste(path,if (decreasing) "Desc" else "Asc","TimData.png", sep=""), width=width, height=height, units="in", res=120)
oldpar <- par(mfcol=c(4,5), pty="s")
for (i in seq_along(E)){
  e <- E[i]
  if (decreasing){
    x <- seq(n-2^e, 0, length=n) + runif(n, 0, 2*(2^e-1))
    #x <- testdb$func$descshuffle(n, 2^(N-e))
    #x <- testdb$func$desclocal(n, 2^(N-e))
    #x <- testdb$func$descglobal(n, 2^e)
  }else{
    x <- seq(0, n-2^e, length=n) + runif(n, 0, 2*(2^e-1))
    #x <- testdb$func$ascshuffle(n, 2^(N-e))
    #x <- testdb$func$asclocal(n, 2^(N-e))
    #x <- testdb$func$ascglobal(n, 2^e)
  }
  j <- sort(sample(seq_len(n), min(2^15, n)))
  #j <- 1:64
  xlim <- c(1,max(j))
  ylim <- range(x[j])
  plot(xlim, ylim, type="n", main=e, xlab="position", ylab="key")
  rect(xlim[1], ylim[1], xlim[2], ylim[2], col=C2[((ds[i]<0)+1)], border=C2[((ds[i]<0)+1)])
  points(j, x[j], pch=".", col=rgb(0,0,0,0.2))
}
par(oldpar)
dev.off()


svg(paste(path,if (decreasing) "Desc" else "Asc","TimOctoSecs.svg", sep=""), width=width, height=height)
plotit("Octo", c("Knuth"), "secs")
dev.off()
svg(paste(path,if (decreasing) "Desc" else "Asc","TimOctoSizeSecs.svg", sep=""), width=width, height=height)
plotit("Octo", c("Knuth"), "sizesecs")
dev.off()

svg(paste(path,if (decreasing) "Desc" else "Asc","TimFrog1Secs.svg", sep=""), width=width, height=height)
plotit("Frog1", c("Squid1"), "secs")
dev.off()
svg(paste(path,if (decreasing) "Desc" else "Asc","TimFrog1SizeSecs.svg", sep=""), width=width, height=height)
plotit("Frog1", c("Knuth","Squid1"), "sizesecs")
dev.off()

svg(paste(path,if (decreasing) "Desc" else "Asc","TimSquid2Secs.svg", sep=""), width=width, height=height)
plotit("Squid2", c("Knuth","Frog2"), "secs")
dev.off()
svg(paste(path,if (decreasing) "Desc" else "Asc","TimSquid2SizeSecs.svg", sep=""), width=width, height=height)
plotit("Squid2", c("Knuth","Frog2"), "sizesecs")
dev.off()

svg(paste(path,if (decreasing) "Desc" else "Asc","TimWalkSecs.svg", sep=""), width=width, height=height)
plotit("Walk", c("Knuth","Grail"), "secs")
dev.off()
svg(paste(path,if (decreasing) "Desc" else "Asc","TimWalkSizeSecs.svg", sep=""), width=width, height=height)
plotit("Walk", c("Knuth","Grail"), "sizesecs")
dev.off()




# adjplot <- function(algo=c("Knuth","KnuthA","Frog1","Frog1A","Gecko1","Squid1","Octo","Omit","Tim","Frog2","Squid2","Walk")
#                     , data=c("permut","ascdesc")
#                     , col = list(size=rgb(0,0,0,0.3), secs=rgb(1,0,0,0.9), sizesecs=rgb(0,1,0,0.9))
#                     , xstat = "secs"
#                     , xlim = range(S[xstat,,2])
#                     , ylim = c(0, max(S[,,1]))
# ){
#   M <- h(algo=algo, data=data)
#   S <- apply(M, 2:4, median)
#   S["size",,] <- S["size",,] - 1
#   S["size",,] <- S["size",,] / S["size",1,1]
#   S["secs",,] <- S["secs",,] / S["secs",1,1]
#   S["sizesecs",,] <- S["sizesecs",,] / S["sizesecs",1,1]
#   plot(xlim,ylim, type="n"
#        , xlab=paste(data[2], " / ",algo[1],"(", data[1], ")", sep="")
#        , ylab=paste("buffer (grey), footprint / footprint(", data[1], ") (green)    runtime / runtime(", data[1], ") (red)", sep="")
#   )
#   segments(S[xstat,,2], S["size",,1], S[xstat,,2], S["secs",,1], col=col$size)
#   segments(S[xstat,,2], S["secs",,1], S[xstat,,2], S["sizesecs",,1], col=col$size)
#   text(S[xstat,,2], S["size",,1], algo, col=col$size)
#   text(S[xstat,,2], S["secs",,1], algo, col=col$secs)
#   text(S[xstat,,2], S["sizesecs",,1], algo, col=col$sizesecs)
# }

