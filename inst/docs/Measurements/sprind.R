# measurements SPRIND rejoinder
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk

require(greeNsort)
perfgrants()

# ---- Vergleich Pdqsort Ducksort parallel Ducksort -------------------------------------------------------------

# wiederholt für 3 arten PDucksort zu kompilieren: single pivot, ninther, median aus 64
n <- 2^26
R <- 1:200
A <- c("Pdq","Duck","PDuck","PdqB","DuckB","PDuckB","PFrog2")
L <- vector(mode='list', length(R)*length(A))
dim(L) <- c(length(R),length(A))
dimnames(L) <- list(as.character(R), A)
for (r in R){
  set.seed(r)
  x <-testdb$func$permut(n)
  #x <- prunif(n)
  for (a in sample(A)){
    cat("r=", r, " a=", a, "\n", sep="")
    y <- x[];
    f <- algodb[[a,"func"]]
    if (is.null(as.list(f)$threads)){
      L[[r,a]] <- sperf(f(y))
    }else{
      L[[r,a]] <- sperf(f(y, threads=24))
    }
  }
}
rm(x,y)
#save.image("pdq64_vs_duck8pivot1.RData")
#save.image("pdq64_vs_duck8ninther.RData")
#save.image("pdq64_vs_duck8sample64.RData")
#save.image("pdq64_vs_duck8sample64_win_i7-1355U.RData")


load("pdq64_vs_duck8pivot1.RData")
L1 <- L
load("pdq64_vs_duck8ninther.RData")
L9 <- L
load("pdq64_vs_duck8sample64.RData")
L64 <- L

stat <- "bcdEnergy"
stat <- "runTime"
X1 <- apply(L1, 1:2, function(l){
  l[[1]][,stat]
})
X9 <- apply(L9, 1:2, function(l){
  l[[1]][,stat]
})
colnames(X9)[colnames(X9) %in% c("PDuck","PDuckB")] <- c("PDuck9","PDuckB9")
X64 <- apply(L64, 1:2, function(l){
  l[[1]][,stat]
})
colnames(X64)[colnames(X64) %in% c("PDuck","PDuckB")] <- c("PDuck64","PDuckB64")


X <- (X1+X9+X64)/3
X[,c("PDuck","PDuckB")] <-   X1[,c("PDuck","PDuckB")]
X <- cbind(X, X9[,c("PDuck9","PDuckB9")], X64[,c("PDuck64","PDuckB64","PFrog2")])
X <- X[,intersect(c("Pdq", "Duck", "PDuck", "PDuck9", "PDuck64", "PdqB", "DuckB", "PDuckB", "PDuckB9", "PDuckB64","PFrog2"), colnames(X))]

svg("PDucksort_boxplots.svg", width=12, height=12)
m <- max(X)
boxplot(X, ylim=c(0, m), ylab="runTime (seconds)", main="runTime Pdqsort vs. Ducksort vs. parallel Ducksort")
dev.off()

svg("PDucksort_quantiles.svg", width=12, height=12)
P <- seq(0, 1, 0.1)
Z <- apply(X, 2, quantile, probs=P)
Z <- Z / apply(Z, 1, min)
Z
mcol <- intersect(colnames(Z), c("PdqB","PDuckB","PDuckB9","PDuckB64","PFrog2"))
m <- max(Z[,mcol])
matplot(100*P, Z[,mcol]
, pch=c("P","1","9","D","F")[match(c("PdqB","PDuckB","PDuckB9","PDuckB64","PFrog2"), colnames(Z), nomatch=0)>0], cex=1.3
, xlab="Percentile", ylab="runTime / best runTime", ylim=c(1, m), main="runTime relativ to fastest per percentile", sub="P=PdqB, 1=PDuckB, 9=PDuckB9, D=PDuckB64, F=PFrog2")
dev.off()


# ---- Vergleich für verschieden lange Records  -------------------------------------------------------------

n <- 2^22
#M <- as.integer(c(1:15, seq(16, 30, by=2), seq(32, 48, by=4)))
M <- 1:50
P <- round(2^(seq(log2(0.1), log2(0.5), length=100)), 3)
A <- c("MKnuth","MFrog2","NKnuth","NFrog2")
L <- vector("list", length(P)*length(M)*length(A))
dim(L) <- c(length(P), length(M), length(A))
dimnames(L) <- list(P, M, A)
for (i in seq_along(P)){
  p <- P[i]
  set.seed(i)
  x <- rbind(sample(n))
  for (j in seq_along(M)){
    m <- M[j]
    while(nrow(x)<m)
      x <- rbind(x, x[1,])
    for(a in sample(A)){
      cat("i=", i, " p=", p, " m=", m, " a=", a, "\n", sep="")
      y <- x[]
      L[[i,j,a]] <- sperf(switch(a
      , "MKnuth" = MKnuthsort(y, situation = 'insitu')
      ,"MFrog2" = MFrogsort2(y, p=p, situation = 'insitu')
      ,"NKnuth" = NKnuthsort(y, situation = 'insitu')
      ,"NFrog2" = NFrogsort2(y, p=p, situation = 'insitu')
      ))
      stopifnot(!is.unsorted(y[m,]))
    }
  }
}
rm(x,y)

#save.image("Records.RData")
#save.image("win_i7-1355U_Records.RData")

load("Records.RData")


stat <- "runTime"
stat <- "tFootprint"
X <- apply(L, 1:3, function(x)x[[1]][,stat])
B <- apply(L, 1:3, function(x)x[[1]][,"%RAM"])

i <- as.character(M)[M>9]
ni <- length(X[,i,])
plot(B[,i,],X[,i,], pch=rep(c("K","F","k","f"), rep(ni/4, 4)), col=rep(c("blue","green","blue","green"), rep(ni/4, 4)))
B2 <- apply(B, c(1,3), mean)
matplot(P,B2, pch=c("K","F","k","f"), main="Average RAM consumption given Frogsorts buffer fraction", sub="K=Knuthsort F=Frogsort2 lowercase=keys separated from payload", xlab="buffer fraction", ylab="average algorithm RAM / data RAM during runTime")


for (nm in 1:2){
  if (nm==1){
    AKnuth <- "MKnuth"
    AFrog2 <- "MFrog2"
  }else{
    AKnuth <- "NKnuth"
    AFrog2 <- "NFrog2"
  }
  Y <- O <- M
  for (i in seq_along(M)){
    m <- M[i]
    s <- supsmu(P,X[,i,AFrog2])
    # plot(P, X[,i,AFrog2], pch="1", main=paste0("m=", m))
    # lines(s)
    #s <- list(x=P, y=X[,i,AFrog2])
    w <- which.min(s$y)
    O[i] <- s$x[w]
    Y[i] <- s$y[w]
    # abline(v=O[i], col="green")
    # abline(h=Y[i], col="green")
  }
  if (nm==1){
    YM <- Y
    OM <- O
  }else{
    YN <- Y
    ON <- O
  }
  svg(paste0("Recordsort_data_", stat ,"_", if  (nm==1) "together_with" else "separated_from", "_payload.svg"), width=16, height=16)
  boxplot(X[,,AKnuth], col="red", xlab="number of 32bit-integers in record"
  , ylab=stat, ylim=range(X), main=paste0("Recordsort ", if  (nm==1) "together with" else "separated from", " payload (", stat, " data)")
  , sub="50 boxplots à 50 Knuthsorts 50 smoothed lines for Frogsort2 with different buffer fractions from 10% to 50%")
  for (i in seq_along(P))
    lines(supsmu(M,X[i,,AFrog2]), col=rgb(0,1,0,0.3))
  dev.off()
  print(data.frame(M, P=O, Y))
}
rm(Y,O)


svg(paste0("Recordsort_optimal_buffer_", stat , ".svg"), width=12, height=12)
plot(M, OM, main=paste("Optimal Frogsort buffer for", stat,"given length of record"), xlab="length of record (#32-bit integers)", ylab="optimal buffer-fraction", pch="M"
     , col=greensort_color, ylim=range(c(OM,ON)), sub="solid=sorting with payload  dashed=sorting keys separated from payload")
lines(supsmu(M, OM), col=greensort_color)
points(M, ON, pch="N", col="blue")
lines(supsmu(M, ON), col="blue", lty=2)
dev.off()

# plot(M, YM, main=paste(stat, "given length of record"), xlab="length of record (#32-bit integers)", ylab=stat, ylim=range(X), type="l", col="green", sub="black=Knuth  red=default  blue=50%  green=optimum")
# lines(M, YN, col="green", lty=2, lwd=2)
# lines(M, X[which(P==0.500),,"MFrog2"], col="blue")
# lines(M, X[which(P==0.143),,"MFrog2"], col="red")
# lines(M, colMeans(X[,,"MKnuth"]), col="black")
# lines(M, X[which(P==0.500),,"NFrog2"], col="blue", lty=2, lwd=2)
# lines(M, X[which(P==0.143),,"NFrog2"], col="red", lty=2, lwd=2)
# lines(M, colMeans(X[,,"NKnuth"]), col="black", lty=2, lwd=2)

svg(paste0("Recordsort_smoothed_results_absolute_", stat , ".svg"), width=12, height=12)
plot(supsmu(M, YM), main=paste(stat, "given length of record"), xlab="length of record (#32-bit integers)\nblack=100% red=14%  blue=50%  green=optimum%", ylab=paste(stat, "(smoothed)"), type="l", col="green", ylim=range(X)
, sub="solid=sorting with payload  dashed=sorting keys separated from payload"
)
lines(supsmu(M, YN), col="green", lty=2, lwd=2)
lines(supsmu(M, X[which(P==0.500),,"MFrog2"]), col="blue")
lines(supsmu(M, X[which(P %in% c(0.143, 0.144)),,"MFrog2"]), col="red")
lines(supsmu(M, colMeans(X[,,"MKnuth"])), col="black")
lines(supsmu(M, X[which(P==0.500),,"NFrog2"]), col="blue", lty=2, lwd=2)
lines(supsmu(M, X[which(P %in% c(0.143, 0.144)),,"NFrog2"]), col="red", lty=2, lwd=2)
lines(supsmu(M, colMeans(X[,,"NKnuth"])), col="black", lty=2, lwd=2)
dev.off()

svg(paste0("Recordsort_smoothed_results_relative_", stat , ".svg"), width=12, height=12)
ref <- supsmu(M, colMeans(X[,,"MKnuth"]))$y
refM <- supsmu(M, colMeans(X[,,"MKnuth"]))$y/ref
refN <- supsmu(M, colMeans(X[,,"NKnuth"]))$y/ref
optM <- supsmu(M, YM)$y/ref
optN <- supsmu(M, YN)$y/ref
midM <- supsmu(M, X[which(P==0.500),,"MFrog2"])$y/ref
midN <- supsmu(M, X[which(P==0.500),,"NFrog2"])$y/ref
defM <- supsmu(M, X[which(P %in% c(0.143, 0.144)),,"MFrog2"])$y/ref
defN <- supsmu(M, X[which(P %in% c(0.143, 0.144)),,"NFrog2"])$y/ref
plot(M, optM, main=paste(stat, "relative to Knuthsort given length of record\nblack=100% red=14%  blue=50%  green=optimum%"), xlab="length of record (#32-bit integers)", ylab=paste("smoothed", stat, "/ smoothed Knuthsort"), type="l"
     , col="green", sub="solid=sorting with payload  dashed=sorting keys separated from payload", ylim=range(c(1, optM,optN,midM,midN,defM,defN,refN)))
lines(M, midM, col="blue")
lines(M, defM, col="red")
lines(M,refM)
lines(M,refN, lty=2, lwd=2)
lines(M, optN, col="green", lty=2, lwd=2)
lines(M, midN, col="blue", lty=2, lwd=2)
lines(M, defN, col="red", lty=2, lwd=2)
dev.off()



# --- Throughput ------------------------------------------------------------------------


require(parallel)

P <- 16
n <- 1.3e8
p <- 0.14
r <- 1

cl <- makeCluster(P)
clusterEvalQ(cl, require(greeNsort))
clusterExport(cl, c("n","p","r"))
system.time(clusterEvalQ(cl, {
  set.seed(r)
  x <- runif(n)
  gc()
}))
system.time(tK <- do.call("rbind", clusterEvalQ(cl, {
  ret <- sperf(Knuthsort(x))
  rm(x)
  ret
})))
stopCluster(cl)


cl <- makeCluster(P)
clusterEvalQ(cl, require(greeNsort))
clusterExport(cl, c("n","p","r"))
system.time(clusterEvalQ(cl, {
  set.seed(r)
  x <- runif(n)
  gc()
}))
system.time(tF1 <- do.call("rbind", clusterEvalQ(cl, {
  ret <- sperf(Frogsort1(x))
  rm(x)
  ret
})))
stopCluster(cl)


cl <- makeCluster(P)
clusterEvalQ(cl, require(greeNsort))
clusterExport(cl, c("n","p","r"))
system.time(clusterEvalQ(cl, {
  set.seed(r)
  x <- runif(n)
  gc()
}))
system.time(tF2 <- do.call("rbind", clusterEvalQ(cl, {
  ret <- sperf(Frogsort2(x, p=p))
  rm(x)
  ret
})))
stopCluster(cl)


cl <- makeCluster(P)
clusterEvalQ(cl, require(greeNsort))
clusterExport(cl, c("n","p","r"))
system.time(clusterEvalQ(cl, {
  set.seed(r)
  x <- runif(n)
  gc()
}))
system.time(tW <- do.call("rbind", clusterEvalQ(cl, {
  ret <- sperf(Walksort(x))
  rm(x)
  ret
})))
stopCluster(cl)


cl <- makeCluster(P)
clusterEvalQ(cl, require(greeNsort))
clusterExport(cl, c("n","p","r"))
system.time(clusterEvalQ(cl, {
  set.seed(r)
  x <- runif(n)
  gc()
}))
system.time(tPK <- do.call("rbind", clusterEvalQ(cl, {
  ret <- sperf(PKnuthsort(x, threads=12))
  rm(x)
  ret
})))
stopCluster(cl)


cl <- makeCluster(P)
clusterEvalQ(cl, require(greeNsort))
clusterExport(cl, c("n","p","r"))
system.time(clusterEvalQ(cl, {
  set.seed(r)
  x <- runif(n)
  gc()
}))
system.time(tPF0 <- do.call("rbind", clusterEvalQ(cl, {
  ret <- sperf(PFrogsort0(x, threads=12))
  rm(x)
  ret
})))
stopCluster(cl)


cl <- makeCluster(P)
clusterEvalQ(cl, require(greeNsort))
clusterExport(cl, c("n","p","r"))
system.time(clusterEvalQ(cl, {
  set.seed(r)
  x <- runif(n)
  gc()
}))
system.time(tPF2 <- do.call("rbind", clusterEvalQ(cl, {
  ret <- sperf(PFrogsort2(x, p=p, threads=12))
  rm(x)
  ret
})))
stopCluster(cl)


rbind(
  tF1=colMeans(tK/tF1)
  , tF2=colMeans(tK/tF2)
  , tW=colMeans(tK/tW)
  , tPK=colMeans(tK/tPK)
  , tPF0=colMeans(tK/tPF0)
  , tPF2=colMeans(tK/tPF2)
)

