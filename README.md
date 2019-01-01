# Supplemental material for the paper "Community engagement and subgroup meta-knowledge: Some factors in the soul of a community"

This repository contains supplementary material for the paper "Community engagement and subgroup meta-knowledge: Some factors in the soul of a community" to enable the reader to reproduce the analysis.

- **PaperFinalDraft.Rnw** is the LaTeX/knitr file that produces the paper, including all R code for analysis, in knitr chunks.
- **CodeFinalDraft.R** is an R file that contains all the code from the paper, extracted from the .Rnw file using ``purl()``.
- Original Knight Foundation data files are in the **data** directory, along with supplemental data
- R packages are archived in the **packrat** directory
- Style files from Taylor and Francis are named **svjour3.cls** and **svjour3.clo** and the BibTeX bibliography is **SoCbib.bib**

Please note, while this entire repository is reproducible, the code was initially written in 2013, and it has aged poorly. The code won't run with modern versions of the R packages listed, so it's necessary to use the older versions preserved in the packrat snapshot. In order to get the packrat repository to "hydrate" it seems to be necessary to downgrade R to at least 3.2.1. It's also possible you will need to troubleshoot aspects related to C++ and/or FORTRAN filepaths. Some of my personal struggles getting the code to run on a new computer are documented on [RStudio Community](https://community.rstudio.com/t/hydrating-a-packrat-repository-on-a-new-computer/16837/12). Ideally, you could use the following steps:

- clone or download this repo to your local machine
- open the .Rproj
- ``install.packages("packrat")``
- hydrate the packrat repository by running ``packrat::restore()``
- knit the ``Rnw`` file