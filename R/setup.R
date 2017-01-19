## Libraries

if(!"dplyr" %in% installed.packages()){
  install.packages("dplyr")
}
if(!"tidyr" %in% installed.packages()){
  install.packages("tidyr")
}
if(!"ggplot2" %in% installed.packages()){
  install.packages("ggplot2")
}
if(!"pander" %in% installed.packages()){
  install.packages("pander")
}
if(!"withr" %in% installed.packages()){
  install.packages("withr")
}

library(ggplot2)
library(pander)
library(withr)
library(dplyr)
library(dplyr)

## Install knitcitations in this repo if not already installed
if(!"knitcitations" %in% installed.packages()){
  install.packages("knitcitations")
}

# ## Install knitauthors in this repo if not already installed (requires devtools)
if(!"knitauthors" %in% installed.packages()){
  if(!"devtools" %in% installed.packages()){
    install.packages("devtools")
  }
  withr::with_libpaths(new = "../repoPackageLibrary/", devtools::install_github("jamesware/knitauthors",force=T), action= "prefix")
}

## Set up citations
library(knitcitations)
cleanbib()
options("citation_format" = "pandoc")

## set knitr chunk options
if(!"knitr" %in% installed.packages()){
  install.packages("knitr")
}
library(knitr)
opts_chunk$set(fig.path="../figures/", collapse = TRUE, echo=FALSE, warning=TRUE, message=TRUE)#, cache=TRUE)#, results="asis")
library(knitauthors)
