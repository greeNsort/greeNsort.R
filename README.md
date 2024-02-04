# greeNsort.R

The greeNsort R package (testbed with C-algorithms and R-data generators and C/R powercap RAPL performance measurement)

## Installation under Linux (is described for Ubuntu 22.04

### update your package system

```
sudo apt-get update
sudo apt-get upgrade
```

### check available powercap measures

Note that reading the names should work, but reading the counters requires permissions.

first socket ```package-0``` should be available

```
cat /sys/class/powercap/intel-rapl/intel-rapl:0/name
```

```core``` should be available under Intel and AMD

```
cat /sys/class/powercap/intel-rapl/intel-rapl:0/intel-rapl:0:0/name
```

```dram``` should be available under Intel

```
cat /sys/class/powercap/intel-rapl/intel-rapl:0/intel-rapl:0:1/name
```

if you have a graphic card it should be available under Intel (but not AMD)

```
cat /sys/class/powercap/intel-rapl/intel-rapl:0/intel-rapl:0:2/name
```

### prepare R installation

see https://cran.r-project.org/bin/linux/ubuntu/

```
sudo apt install --no-install-recommends software-properties-common dirmngr
```

add the signing key (by Michael Rutter) for these repos

```
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
```
To verify key, run 

```
gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
```
and check the Fingerprint: E298A3A825C0D65DFD57CBB651716619E084DAB9

check whether lsb_release matches Ubuntu version, 

```
echo $(lsb_release -cs)
```

otherwise replace ```$(lsb_release -cs)``` with the correct one in the following code and add the R 4.0 repo from CRAN

```
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
```


### install R

```
sudo apt-get install r-base
```

### install required Linux libs

```
sudo apt-get install libxml2-dev
sudo apt-get install libssl-dev
sudo apt-get install libfontconfig1-dev
sudo apt-get install libharfbuzz-dev
sudo apt-get install libfribidi-dev
sudo apt-get install libtiff5-dev
```

### install required R packages

open R

```
R
```

and install the required packages

```
install.packages("curl")
install.packages("httr2")
install.packages("systemfonts")
```

... takes a while ...

```
install.packages("devtools")
install.packages(c("abind","gplots","tibble","roxygen2","microbenchmark"))
```

For function prunif and package greeNsort.Rcpp, takes a while ...

```
install.packages(c("Rcpp","RhpcBLASctl"))
install.packages("rTRNG")
```

Quit R without saving data

```
q("no")
```

### configure our R package compilation

Create local R config folder

```
mkdir ~/.R
```

Create makevars file

```
touch ~/.R/makevars
```

Put the following line into the makevars file

```
CFLAGS = -g -ftree-loop-if-convert -ftree-loop-if-convert-stores -fif-conversion -fif-conversion2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g $(LTO)
```

### *temporary* installation

Go to some directory where you unpack ```greeNsort.R``` and ```greeNsort.Rcpp```, such that you have 

```
./greeNsort.R 
./greeNsort.Rcpp 
```

Install them in this sequence

```
R CMD INSTALL greeNsort.Rcpp
R CMD INSTALL greeNsort.R
```

### configure for your machine

open R

```
R
```

load greeNsort

```
require(greeNsort)
```

switch to greeNsort directory

```
setwd("greeNsort.R")
```

update the powercap RAPL config in ```src/powercap_config.h``` by calling 

```
perfconfig()
```

quit R without saving data

```
q("no")
```

replace the ```greeNsort.Rcpp/src/powercap_config.h``` by the new one just created in ```greeNsort.R/src/powercap_config.h```

### final installation

Now install with the powercap RAPL config for your machine

```
R CMD INSTALL greeNsort.Rcpp
R CMD INSTALL greeNsort.R
```

Now the package is installed. Do __*not*__ try to R CMD check the package before you read on

### set permissions

After each reboot, make sure there are read permissions on RAPL counters. If you have the root password, a convenient way is using ```perfgrants()``` in R (otherwise talk to your sysadmin).

assuming you are in R

```
require(greeNsort)
perfgrants()
```

run a Frogsort0 example

```
example(Frogsort0)
```

run the testsuite

```
testsort(out="", verbose=TRUE)
```

see help for the testsuite

```
?testsort
```

### vignettes and package checking

Note that we so far installed from the package source, not as usual from a package build. 

Vignettes are available in source /vignettes and compiled under /doc but not as usual copied to the R lib.

You don't need a proper vignette installation.


### read, apply, certify

read the [*greeNsort-1-quickstart vignette*](/doc/greeNsort-1-quickstart.html) to make yourself familiar with using the package.

read the [*greeNsort-2-measurement vignette*](/doc/greeNsort-2-measurement.html) to understand the measurement technology.


### building vignettes and package checking

For a proper vignette installation and for a successful R CMD check you can rebuild and install but you need first 

- a working LaTeX installation
- a working pandoc installation

Once you have that the following should work but takes a while

```
R CMD build greeNsort.R
R CMD INSTALL greeNsort_1.0.0.tar.gz
R CMD check greeNsort_1.0.0.tar.gz
```

```
R CMD build greeNsort.Rcpp
R CMD INSTALL greeNsort.Rcpp_1.0.0.tar.gz
R CMD check greeNsort.Rcpp_1.0.0.tar.gz
```

### package development

The packages ```greeNsort``` and ```greeNsort.Rcpp``` were developed using RStudio. If you want to modify the packages we recommend installing [RStudio](https://posit.co/download/rstudio-desktop/). Then read the [*greeNsort-3-development vignette*](/doc/greeNsort-3-development.html) to understand the measurement technology.


## Installation under Windows 

Installation under Windows is possible, but currently requires some tricks 

- deactivate RAPL-measurement
- deactivate some C++ algorithms in greeNsort.Rcpp and greeNsort.R algodb

