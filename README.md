
<!-- README.md is generated from README.Rmd. Please edit that file -->

# libminer

<!-- badges: start -->

[![R-CMD-check](https://github.com/sarahgravel/libminer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sarahgravel/libminer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of libminer is to provide an overview of your R library setup.
It is a toy package created as a part of a workshop and not meant for
serious use.

## Installation

You can install the development version of libminer from
[GitHub](https://GitHub.com/) with:

``` r
install.packages("devtools")
devtools::install_GitHub("sarahgravel/libminer")
```

## Example usage

To get a count of installed packages in each of your library locations,
use the `lib_summary()` function:

``` r
library(libminer)
lib_summary()
#>                                                                   Library
#> 1                                      C:/Program Files/R/R-4.4.1/library
#> 2                        C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> 3 C:/Users/GRAVELS/AppData/Local/Temp/RtmpWapGBT/temp_libpath35fc5f727b3a
#>   n_packages
#> 1         29
#> 2        179
#> 3          1
```

And you can use the `lib()` function:

``` r
lib()
#>                       Package
#> libminer             libminer
#> abind                   abind
#> ade4                     ade4
#> askpass               askpass
#> backports           backports
#> base64enc           base64enc
#> BH                         BH
#> bmp                       bmp
#> box                       box
#> brew                     brew
#> brio                     brio
#> broom                   broom
#> bslib                   bslib
#> cachem                 cachem
#> callr                   callr
#> car                       car
#> carData               carData
#> cli                       cli
#> clipr                   clipr
#> colorspace         colorspace
#> commonmark         commonmark
#> covr                     covr
#> cowplot               cowplot
#> cpp11                   cpp11
#> crayon                 crayon
#> credentials       credentials
#> crosstalk           crosstalk
#> curl                     curl
#> data.table         data.table
#> Deriv                   Deriv
#> desc                     desc
#> devtools             devtools
#> diagram               diagram
#> diffobj               diffobj
#> digest                 digest
#> doBy                     doBy
#> downlit               downlit
#> downloader         downloader
#> dplyr                   dplyr
#> DT                         DT
#> dunn.test           dunn.test
#> ellipsis             ellipsis
#> evaluate             evaluate
#> fansi                   fansi
#> farver                 farver
#> fastmap               fastmap
#> fontawesome       fontawesome
#> fs                         fs
#> FSA                       FSA
#> future                 future
#> future.apply     future.apply
#> gclus                   gclus
#> generics             generics
#> gert                     gert
#> ggbiplot             ggbiplot
#> ggplot2               ggplot2
#> gh                         gh
#> gitcreds             gitcreds
#> globals               globals
#> glue                     glue
#> gridExtra           gridExtra
#> gtable                 gtable
#> highr                   highr
#> htmltools           htmltools
#> htmlwidgets       htmlwidgets
#> httpuv                 httpuv
#> httr                     httr
#> httr2                   httr2
#> igraph                 igraph
#> imager                 imager
#> ini                       ini
#> ipred                   ipred
#> isoband               isoband
#> jpeg                     jpeg
#> jquerylib           jquerylib
#> jsonlite             jsonlite
#> knitr                   knitr
#> labeling             labeling
#> later                   later
#> lava                     lava
#> lazyeval             lazyeval
#> libminer.1           libminer
#> lifecycle           lifecycle
#> listenv               listenv
#> lme4                     lme4
#> lmtest                 lmtest
#> magick                 magick
#> magrittr             magrittr
#> MatrixModels     MatrixModels
#> memoise               memoise
#> microbenchmark microbenchmark
#> mime                     mime
#> miniUI                 miniUI
#> minqa                   minqa
#> modelr                 modelr
#> munsell               munsell
#> nloptr                 nloptr
#> NLP                       NLP
#> numDeriv             numDeriv
#> OpenImageR         OpenImageR
#> openssl               openssl
#> parallelly         parallelly
#> pbkrtest             pbkrtest
#> permute               permute
#> pillar                 pillar
#> pixmap                 pixmap
#> pkgbuild             pkgbuild
#> pkgconfig           pkgconfig
#> pkgdown               pkgdown
#> pkgload               pkgload
#> plotrix               plotrix
#> png                       png
#> praise                 praise
#> prettyunits       prettyunits
#> processx             processx
#> prodlim               prodlim
#> profvis               profvis
#> progressr           progressr
#> promises             promises
#> ps                         ps
#> purrr                   purrr
#> quantreg             quantreg
#> R6                         R6
#> ragg                     ragg
#> rappdirs             rappdirs
#> rcmdcheck           rcmdcheck
#> RColorBrewer     RColorBrewer
#> Rcpp                     Rcpp
#> RcppArmadillo   RcppArmadillo
#> RcppEigen           RcppEigen
#> readbitmap         readbitmap
#> rematch2             rematch2
#> remotes               remotes
#> rex                       rex
#> rlang                   rlang
#> rmarkdown           rmarkdown
#> roxygen2             roxygen2
#> rprojroot           rprojroot
#> rstudioapi         rstudioapi
#> rversions           rversions
#> sass                     sass
#> scales                 scales
#> sessioninfo       sessioninfo
#> shape                   shape
#> shapeR                 shapeR
#> shiny                   shiny
#> slam                     slam
#> sourcetools       sourcetools
#> sp                         sp
#> SparseM               SparseM
#> SQUAREM               SQUAREM
#> stringi               stringi
#> stringr               stringr
#> sys                       sys
#> systemfonts       systemfonts
#> testthat             testthat
#> textshaping       textshaping
#> tibble                 tibble
#> tidyr                   tidyr
#> tidyselect         tidyselect
#> tiff                     tiff
#> tinytex               tinytex
#> tm                         tm
#> urlchecker         urlchecker
#> usethis               usethis
#> utf8                     utf8
#> vctrs                   vctrs
#> vegan                   vegan
#> viridisLite       viridisLite
#> waldo                   waldo
#> wavethresh         wavethresh
#> whisker               whisker
#> withr                   withr
#> xfun                     xfun
#> xml2                     xml2
#> xopen                   xopen
#> xtable                 xtable
#> yaml                     yaml
#> zip                       zip
#> zoo                       zoo
#> base                     base
#> boot                     boot
#> class                   class
#> cluster               cluster
#> codetools           codetools
#> compiler             compiler
#> datasets             datasets
#> foreign               foreign
#> graphics             graphics
#> grDevices           grDevices
#> grid                     grid
#> KernSmooth         KernSmooth
#> lattice               lattice
#> MASS                     MASS
#> Matrix                 Matrix
#> methods               methods
#> mgcv                     mgcv
#> nlme                     nlme
#> nnet                     nnet
#> parallel             parallel
#> rpart                   rpart
#> spatial               spatial
#> splines               splines
#> stats                   stats
#> stats4                 stats4
#> survival             survival
#> tcltk                   tcltk
#> tools                   tools
#> utils                   utils
#>                                                                                LibPath
#> libminer       C:/Users/GRAVELS/AppData/Local/Temp/RtmpWapGBT/temp_libpath35fc5f727b3a
#> abind                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> ade4                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> askpass                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> backports                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> base64enc                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> BH                                    C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> bmp                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> box                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> brew                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> brio                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> broom                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> bslib                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> cachem                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> callr                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> car                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> carData                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> cli                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> clipr                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> colorspace                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> commonmark                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> covr                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> cowplot                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> cpp11                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> crayon                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> credentials                           C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> crosstalk                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> curl                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> data.table                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> Deriv                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> desc                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> devtools                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> diagram                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> diffobj                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> digest                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> doBy                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> downlit                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> downloader                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> dplyr                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> DT                                    C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> dunn.test                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> ellipsis                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> evaluate                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> fansi                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> farver                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> fastmap                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> fontawesome                           C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> fs                                    C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> FSA                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> future                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> future.apply                          C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> gclus                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> generics                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> gert                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> ggbiplot                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> ggplot2                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> gh                                    C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> gitcreds                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> globals                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> glue                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> gridExtra                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> gtable                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> highr                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> htmltools                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> htmlwidgets                           C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> httpuv                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> httr                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> httr2                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> igraph                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> imager                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> ini                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> ipred                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> isoband                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> jpeg                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> jquerylib                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> jsonlite                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> knitr                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> labeling                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> later                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> lava                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> lazyeval                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> libminer.1                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> lifecycle                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> listenv                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> lme4                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> lmtest                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> magick                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> magrittr                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> MatrixModels                          C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> memoise                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> microbenchmark                        C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> mime                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> miniUI                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> minqa                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> modelr                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> munsell                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> nloptr                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> NLP                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> numDeriv                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> OpenImageR                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> openssl                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> parallelly                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> pbkrtest                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> permute                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> pillar                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> pixmap                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> pkgbuild                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> pkgconfig                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> pkgdown                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> pkgload                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> plotrix                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> png                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> praise                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> prettyunits                           C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> processx                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> prodlim                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> profvis                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> progressr                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> promises                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> ps                                    C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> purrr                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> quantreg                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> R6                                    C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> ragg                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> rappdirs                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> rcmdcheck                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> RColorBrewer                          C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> Rcpp                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> RcppArmadillo                         C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> RcppEigen                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> readbitmap                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> rematch2                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> remotes                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> rex                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> rlang                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> rmarkdown                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> roxygen2                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> rprojroot                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> rstudioapi                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> rversions                             C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> sass                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> scales                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> sessioninfo                           C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> shape                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> shapeR                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> shiny                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> slam                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> sourcetools                           C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> sp                                    C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> SparseM                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> SQUAREM                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> stringi                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> stringr                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> sys                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> systemfonts                           C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> testthat                              C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> textshaping                           C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> tibble                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> tidyr                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> tidyselect                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> tiff                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> tinytex                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> tm                                    C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> urlchecker                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> usethis                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> utf8                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> vctrs                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> vegan                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> viridisLite                           C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> waldo                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> wavethresh                            C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> whisker                               C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> withr                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> xfun                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> xml2                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> xopen                                 C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> xtable                                C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> yaml                                  C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> zip                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> zoo                                   C:/Users/GRAVELS/AppData/Local/R/win-library/4.4
#> base                                                C:/Program Files/R/R-4.4.1/library
#> boot                                                C:/Program Files/R/R-4.4.1/library
#> class                                               C:/Program Files/R/R-4.4.1/library
#> cluster                                             C:/Program Files/R/R-4.4.1/library
#> codetools                                           C:/Program Files/R/R-4.4.1/library
#> compiler                                            C:/Program Files/R/R-4.4.1/library
#> datasets                                            C:/Program Files/R/R-4.4.1/library
#> foreign                                             C:/Program Files/R/R-4.4.1/library
#> graphics                                            C:/Program Files/R/R-4.4.1/library
#> grDevices                                           C:/Program Files/R/R-4.4.1/library
#> grid                                                C:/Program Files/R/R-4.4.1/library
#> KernSmooth                                          C:/Program Files/R/R-4.4.1/library
#> lattice                                             C:/Program Files/R/R-4.4.1/library
#> MASS                                                C:/Program Files/R/R-4.4.1/library
#> Matrix                                              C:/Program Files/R/R-4.4.1/library
#> methods                                             C:/Program Files/R/R-4.4.1/library
#> mgcv                                                C:/Program Files/R/R-4.4.1/library
#> nlme                                                C:/Program Files/R/R-4.4.1/library
#> nnet                                                C:/Program Files/R/R-4.4.1/library
#> parallel                                            C:/Program Files/R/R-4.4.1/library
#> rpart                                               C:/Program Files/R/R-4.4.1/library
#> spatial                                             C:/Program Files/R/R-4.4.1/library
#> splines                                             C:/Program Files/R/R-4.4.1/library
#> stats                                               C:/Program Files/R/R-4.4.1/library
#> stats4                                              C:/Program Files/R/R-4.4.1/library
#> survival                                            C:/Program Files/R/R-4.4.1/library
#> tcltk                                               C:/Program Files/R/R-4.4.1/library
#> tools                                               C:/Program Files/R/R-4.4.1/library
#> utils                                               C:/Program Files/R/R-4.4.1/library
#>                   Version    Priority
#> libminer       0.0.0.9000        <NA>
#> abind               1.4-8        <NA>
#> ade4               1.7-22        <NA>
#> askpass             1.2.0        <NA>
#> backports           1.5.0        <NA>
#> base64enc           0.1-3        <NA>
#> BH               1.84.0-0        <NA>
#> bmp                   0.3        <NA>
#> box                 1.2.0        <NA>
#> brew               1.0-10        <NA>
#> brio                1.1.5        <NA>
#> broom               1.0.6        <NA>
#> bslib               0.8.0        <NA>
#> cachem              1.1.0        <NA>
#> callr               3.7.6        <NA>
#> car                 3.1-2        <NA>
#> carData             3.0-5        <NA>
#> cli                 3.6.3        <NA>
#> clipr               0.8.0        <NA>
#> colorspace          2.1-1        <NA>
#> commonmark          1.9.1        <NA>
#> covr                3.6.4        <NA>
#> cowplot             1.1.3        <NA>
#> cpp11               0.4.7        <NA>
#> crayon              1.5.3        <NA>
#> credentials         2.0.1        <NA>
#> crosstalk           1.2.1        <NA>
#> curl                5.2.3        <NA>
#> data.table         1.15.4        <NA>
#> Deriv               4.1.6        <NA>
#> desc                1.4.3        <NA>
#> devtools            2.4.5        <NA>
#> diagram             1.6.5        <NA>
#> diffobj             0.3.5        <NA>
#> digest             0.6.37        <NA>
#> doBy               4.6.22        <NA>
#> downlit             0.4.4        <NA>
#> downloader            0.4        <NA>
#> dplyr               1.1.4        <NA>
#> DT                   0.33        <NA>
#> dunn.test           1.3.6        <NA>
#> ellipsis            0.3.2        <NA>
#> evaluate           0.24.0        <NA>
#> fansi               1.0.6        <NA>
#> farver              2.1.2        <NA>
#> fastmap             1.2.0        <NA>
#> fontawesome         0.5.2        <NA>
#> fs                  1.6.4        <NA>
#> FSA                 0.9.5        <NA>
#> future             1.34.0        <NA>
#> future.apply       1.11.2        <NA>
#> gclus               1.3.2        <NA>
#> generics            0.1.3        <NA>
#> gert                2.1.1        <NA>
#> ggbiplot            0.6.2        <NA>
#> ggplot2             3.5.1        <NA>
#> gh                  1.4.1        <NA>
#> gitcreds            0.1.2        <NA>
#> globals            0.16.3        <NA>
#> glue                1.7.0        <NA>
#> gridExtra             2.3        <NA>
#> gtable              0.3.5        <NA>
#> highr                0.11        <NA>
#> htmltools         0.5.8.1        <NA>
#> htmlwidgets         1.6.4        <NA>
#> httpuv             1.6.15        <NA>
#> httr                1.4.7        <NA>
#> httr2               1.0.5        <NA>
#> igraph              2.0.3        <NA>
#> imager              1.0.2        <NA>
#> ini                 0.3.1        <NA>
#> ipred              0.9-15        <NA>
#> isoband             0.2.7        <NA>
#> jpeg               0.1-10        <NA>
#> jquerylib           0.1.4        <NA>
#> jsonlite            1.8.8        <NA>
#> knitr                1.48        <NA>
#> labeling            0.4.3        <NA>
#> later               1.3.2        <NA>
#> lava                1.8.0        <NA>
#> lazyeval            0.2.2        <NA>
#> libminer.1     0.0.0.9000        <NA>
#> lifecycle           1.0.4        <NA>
#> listenv             0.9.1        <NA>
#> lme4             1.1-35.5        <NA>
#> lmtest             0.9-40        <NA>
#> magick              2.8.4        <NA>
#> magrittr            2.0.3        <NA>
#> MatrixModels        0.5-3        <NA>
#> memoise             2.0.1        <NA>
#> microbenchmark      1.5.0        <NA>
#> mime                 0.12        <NA>
#> miniUI            0.1.1.1        <NA>
#> minqa               1.2.8        <NA>
#> modelr             0.1.11        <NA>
#> munsell             0.5.1        <NA>
#> nloptr              2.1.1        <NA>
#> NLP                 0.3-0        <NA>
#> numDeriv       2016.8-1.1        <NA>
#> OpenImageR          1.3.0        <NA>
#> openssl             2.2.1        <NA>
#> parallelly         1.38.0        <NA>
#> pbkrtest            0.5.3        <NA>
#> permute             0.9-7        <NA>
#> pillar              1.9.0        <NA>
#> pixmap             0.4-13        <NA>
#> pkgbuild            1.4.4        <NA>
#> pkgconfig           2.0.3        <NA>
#> pkgdown             2.1.1        <NA>
#> pkgload             1.4.0        <NA>
#> plotrix             3.8-4        <NA>
#> png                 0.1-8        <NA>
#> praise              1.0.0        <NA>
#> prettyunits         1.2.0        <NA>
#> processx            3.8.4        <NA>
#> prodlim        2024.06.25        <NA>
#> profvis             0.3.8        <NA>
#> progressr          0.14.0        <NA>
#> promises            1.3.0        <NA>
#> ps                  1.7.7        <NA>
#> purrr               1.0.2        <NA>
#> quantreg             5.98        <NA>
#> R6                  2.5.1        <NA>
#> ragg                1.3.2        <NA>
#> rappdirs            0.3.3        <NA>
#> rcmdcheck           1.4.0        <NA>
#> RColorBrewer        1.1-3        <NA>
#> Rcpp               1.0.13        <NA>
#> RcppArmadillo    14.0.0-1        <NA>
#> RcppEigen       0.3.4.0.2        <NA>
#> readbitmap          0.1.5        <NA>
#> rematch2            2.1.2        <NA>
#> remotes             2.5.0        <NA>
#> rex                 1.2.1        <NA>
#> rlang               1.1.4        <NA>
#> rmarkdown            2.29        <NA>
#> roxygen2            7.3.2        <NA>
#> rprojroot           2.0.4        <NA>
#> rstudioapi         0.16.0        <NA>
#> rversions           2.1.2        <NA>
#> sass                0.4.9        <NA>
#> scales              1.3.0        <NA>
#> sessioninfo         1.2.2        <NA>
#> shape             1.4.6.1        <NA>
#> shapeR              1.0-1        <NA>
#> shiny               1.9.1        <NA>
#> slam               0.1-53        <NA>
#> sourcetools       0.1.7-1        <NA>
#> sp                  2.1-4        <NA>
#> SparseM            1.84-2        <NA>
#> SQUAREM            2021.1        <NA>
#> stringi             1.8.4        <NA>
#> stringr             1.5.1        <NA>
#> sys                 3.4.2        <NA>
#> systemfonts         1.1.0        <NA>
#> testthat          3.2.1.1        <NA>
#> textshaping         0.4.0        <NA>
#> tibble              3.2.1        <NA>
#> tidyr               1.3.1        <NA>
#> tidyselect          1.2.1        <NA>
#> tiff               0.1-12        <NA>
#> tinytex              0.52        <NA>
#> tm                 0.7-14        <NA>
#> urlchecker          1.0.1        <NA>
#> usethis             3.0.0        <NA>
#> utf8                1.2.4        <NA>
#> vctrs               0.6.5        <NA>
#> vegan             2.6-6.1        <NA>
#> viridisLite         0.4.2        <NA>
#> waldo               0.5.2        <NA>
#> wavethresh          4.7.3        <NA>
#> whisker             0.4.1        <NA>
#> withr               3.0.1        <NA>
#> xfun                 0.47        <NA>
#> xml2                1.3.6        <NA>
#> xopen               1.0.1        <NA>
#> xtable              1.8-4        <NA>
#> yaml               2.3.10        <NA>
#> zip                 2.3.1        <NA>
#> zoo                1.8-12        <NA>
#> base                4.4.1        base
#> boot               1.3-30 recommended
#> class              7.3-22 recommended
#> cluster             2.1.6 recommended
#> codetools          0.2-20 recommended
#> compiler            4.4.1        base
#> datasets            4.4.1        base
#> foreign            0.8-86 recommended
#> graphics            4.4.1        base
#> grDevices           4.4.1        base
#> grid                4.4.1        base
#> KernSmooth        2.23-24 recommended
#> lattice            0.22-6 recommended
#> MASS             7.3-60.2 recommended
#> Matrix              1.7-0 recommended
#> methods             4.4.1        base
#> mgcv                1.9-1 recommended
#> nlme              3.1-164 recommended
#> nnet               7.3-19 recommended
#> parallel            4.4.1        base
#> rpart              4.1.23 recommended
#> spatial            7.3-17 recommended
#> splines             4.4.1        base
#> stats               4.4.1        base
#> stats4              4.4.1        base
#> survival            3.6-4 recommended
#> tcltk               4.4.1        base
#> tools               4.4.1        base
#> utils               4.4.1        base
#>                                                        Depends
#> libminer                                                  <NA>
#> abind                                             R (>= 1.5.0)
#> ade4                                               R (>= 2.10)
#> askpass                                                   <NA>
#> backports                                         R (>= 3.0.0)
#> base64enc                                         R (>= 2.9.0)
#> BH                                                        <NA>
#> bmp                                                       <NA>
#> box                                               R (>= 3.6.0)
#> brew                                                      <NA>
#> brio                                                R (>= 3.6)
#> broom                                               R (>= 3.5)
#> bslib                                              R (>= 2.10)
#> cachem                                                    <NA>
#> callr                                               R (>= 3.4)
#> car                           R (>= 3.5.0), carData (>= 3.0-0)
#> carData                                           R (>= 3.5.0)
#> cli                                                 R (>= 3.4)
#> clipr                                                     <NA>
#> colorspace                               R (>= 3.0.0), methods
#> commonmark                                                <NA>
#> covr                                     R (>= 3.1.0), methods
#> cowplot                                           R (>= 3.5.0)
#> cpp11                                             R (>= 3.5.0)
#> crayon                                                    <NA>
#> credentials                                               <NA>
#> crosstalk                                                 <NA>
#> curl                                              R (>= 3.0.0)
#> data.table                                        R (>= 3.1.0)
#> Deriv                                                     <NA>
#> desc                                                R (>= 3.4)
#> devtools                      R (>= 3.0.2), usethis (>= 2.1.6)
#> diagram                                     R (>= 2.01), shape
#> diffobj                                           R (>= 3.1.0)
#> digest                                            R (>= 3.3.0)
#> doBy                                     R (>= 4.2.0), methods
#> downlit                                           R (>= 4.0.0)
#> downloader                                                <NA>
#> dplyr                                             R (>= 3.5.0)
#> DT                                                        <NA>
#> dunn.test                                                 <NA>
#> ellipsis                                            R (>= 3.2)
#> evaluate                                          R (>= 4.0.0)
#> fansi                                             R (>= 3.1.0)
#> farver                                                    <NA>
#> fastmap                                                   <NA>
#> fontawesome                                       R (>= 3.3.0)
#> fs                                                  R (>= 3.6)
#> FSA                                               R (>= 3.5.0)
#> future                                                    <NA>
#> future.apply                  R (>= 3.2.0), future (>= 1.28.0)
#> gclus                                     R (>= 2.10), cluster
#> generics                                            R (>= 3.2)
#> gert                                                      <NA>
#> ggbiplot                                 R (>= 3.5.0), ggplot2
#> ggplot2                                             R (>= 3.5)
#> gh                                                  R (>= 3.6)
#> gitcreds                                            R (>= 3.4)
#> globals                                           R (>= 3.1.2)
#> glue                                                R (>= 3.6)
#> gridExtra                                                 <NA>
#> gtable                                              R (>= 3.5)
#> highr                                             R (>= 3.3.0)
#> htmltools                                        R (>= 2.14.1)
#> htmlwidgets                                               <NA>
#> httpuv                                           R (>= 2.15.1)
#> httr                                                R (>= 3.5)
#> httr2                                               R (>= 4.0)
#> igraph                                   methods, R (>= 3.5.0)
#> imager                                  R (>= 4.0.0), magrittr
#> ini                                                       <NA>
#> ipred                                              R (>= 2.10)
#> isoband                                                   <NA>
#> jpeg                                              R (>= 2.9.0)
#> jquerylib                                                 <NA>
#> jsonlite                                               methods
#> knitr                                             R (>= 3.3.0)
#> labeling                                                  <NA>
#> later                                                     <NA>
#> lava                                                R (>= 3.0)
#> lazyeval                                          R (>= 3.1.0)
#> libminer.1                                                <NA>
#> lifecycle                                           R (>= 3.6)
#> listenv                                           R (>= 3.1.2)
#> lme4                      R (>= 3.6.0), Matrix, methods, stats
#> lmtest                                R (>= 3.0.0), stats, zoo
#> magick                                                    <NA>
#> magrittr                                          R (>= 3.4.0)
#> MatrixModels                                      R (>= 3.6.0)
#> memoise                                                   <NA>
#> microbenchmark                                    R (>= 3.2.0)
#> mime                                                      <NA>
#> miniUI                                                    <NA>
#> minqa                                                     <NA>
#> modelr                                              R (>= 3.2)
#> munsell                                                   <NA>
#> nloptr                                                    <NA>
#> NLP                                               R (>= 3.2.0)
#> numDeriv                                         R (>= 2.11.1)
#> OpenImageR                                         R(>= 3.2.3)
#> openssl                                                   <NA>
#> parallelly                                                <NA>
#> pbkrtest                        R (>= 4.2.0), lme4 (>= 1.1.31)
#> permute                                          R (>= 2.14.0)
#> pillar                                                    <NA>
#> pixmap                                                    <NA>
#> pkgbuild                                            R (>= 3.5)
#> pkgconfig                                                 <NA>
#> pkgdown                                           R (>= 4.0.0)
#> pkgload                                           R (>= 3.4.0)
#> plotrix                                           R (>= 3.5.0)
#> png                                               R (>= 2.9.0)
#> praise                                                    <NA>
#> prettyunits                                         R(>= 2.10)
#> processx                                          R (>= 3.4.0)
#> prodlim                                           R (>= 2.9.0)
#> profvis                                             R (>= 3.0)
#> progressr                                         R (>= 3.5.0)
#> promises                                                  <NA>
#> ps                                                  R (>= 3.4)
#> purrr                                             R (>= 3.5.0)
#> quantreg                            R (>= 3.5), stats, SparseM
#> R6                                                  R (>= 3.0)
#> ragg                                                      <NA>
#> rappdirs                                            R (>= 3.2)
#> rcmdcheck                                                 <NA>
#> RColorBrewer                                      R (>= 2.0.0)
#> Rcpp                                                      <NA>
#> RcppArmadillo                                     R (>= 3.3.0)
#> RcppEigen                                         R (>= 3.6.0)
#> readbitmap                                                <NA>
#> rematch2                                                  <NA>
#> remotes                                           R (>= 3.0.0)
#> rex                                                       <NA>
#> rlang                                             R (>= 3.5.0)
#> rmarkdown                                           R (>= 3.0)
#> roxygen2                                            R (>= 3.6)
#> rprojroot                                         R (>= 3.0.0)
#> rstudioapi                                                <NA>
#> rversions                                                 <NA>
#> sass                                                      <NA>
#> scales                                              R (>= 3.6)
#> sessioninfo                                        R (>= 2.10)
#> shape                                              R (>= 2.01)
#> shapeR                                            R (>= 3.0.2)
#> shiny                                    R (>= 3.0.2), methods
#> slam                                              R (>= 3.4.0)
#> sourcetools                                       R (>= 3.0.2)
#> sp                                       R (>= 3.5.0), methods
#> SparseM                                   R (>= 2.15), methods
#> SQUAREM                                             R (>= 3.0)
#> stringi                                             R (>= 3.4)
#> stringr                                             R (>= 3.6)
#> sys                                                       <NA>
#> systemfonts                                       R (>= 3.2.0)
#> testthat                                          R (>= 3.6.0)
#> textshaping                                       R (>= 3.2.0)
#> tibble                                            R (>= 3.4.0)
#> tidyr                                               R (>= 3.6)
#> tidyselect                                          R (>= 3.4)
#> tiff                                              R (>= 2.9.0)
#> tinytex                                                   <NA>
#> tm                                R (>= 3.4.0), NLP (>= 0.2-0)
#> urlchecker                                          R (>= 3.3)
#> usethis                                             R (>= 3.6)
#> utf8                                               R (>= 2.10)
#> vctrs                                             R (>= 3.5.0)
#> vegan                permute (>= 0.9-0), lattice, R (>= 4.1.0)
#> viridisLite                                        R (>= 2.10)
#> waldo                                               R (>= 3.6)
#> wavethresh                                   R (>= 2.10), MASS
#> whisker                                                   <NA>
#> withr                                             R (>= 3.6.0)
#> xfun                                              R (>= 3.2.0)
#> xml2                                              R (>= 3.6.0)
#> xopen                                               R (>= 3.1)
#> xtable                                           R (>= 2.10.0)
#> yaml                                                      <NA>
#> zip                                                       <NA>
#> zoo                                        R (>= 3.1.0), stats
#> base                                                      <NA>
#> boot                             R (>= 3.0.0), graphics, stats
#> class                               R (>= 3.0.0), stats, utils
#> cluster                                           R (>= 3.5.0)
#> codetools                                           R (>= 2.1)
#> compiler                                                  <NA>
#> datasets                                                  <NA>
#> foreign                                           R (>= 4.0.0)
#> graphics                                                  <NA>
#> grDevices                                                 <NA>
#> grid                                                      <NA>
#> KernSmooth                                 R (>= 2.5.0), stats
#> lattice                                           R (>= 4.0.0)
#> MASS           R (>= 4.4.0), grDevices, graphics, stats, utils
#> Matrix                                   R (>= 4.4.0), methods
#> methods                                                   <NA>
#> mgcv                            R (>= 3.6.0), nlme (>= 3.1-64)
#> nlme                                              R (>= 3.5.0)
#> nnet                                R (>= 3.0.0), stats, utils
#> parallel                                                  <NA>
#> rpart                R (>= 2.15.0), graphics, stats, grDevices
#> spatial                   R (>= 3.0.0), graphics, stats, utils
#> splines                                                   <NA>
#> stats                                                     <NA>
#> stats4                                                    <NA>
#> survival                                          R (>= 3.5.0)
#> tcltk                                                     <NA>
#> tools                                                     <NA>
#> utils                                                     <NA>
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                    Imports
#> libminer                                                                                                                                                                                                                                                                                                                                                                                                                                        fs,\npurrr
#> abind                                                                                                                                                                                                                                                                                                                                                                                                                                       methods, utils
#> ade4                                                                                                                                                                                                                                                                                                                                                                                   graphics, grDevices, methods, stats, utils, MASS, pixmap, sp,\nRcpp
#> askpass                                                                                                                                                                                                                                                                                                                                                                                                                                       sys (>= 2.1)
#> backports                                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
#> base64enc                                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
#> BH                                                                                                                                                                                                                                                                                                                                                                                                                                                    <NA>
#> bmp                                                                                                                                                                                                                                                                                                                                                                                                                                                   <NA>
#> box                                                                                                                                                                                                                                                                                                                                                                                                                                                  tools
#> brew                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> brio                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> broom                                                                                                                                                                                                                                                                                                                      backports, dplyr (>= 1.0.0), generics (>= 0.0.2), glue,\nlifecycle, purrr, rlang, stringr, tibble (>= 3.0.0), tidyr (>=\n1.0.0)
#> bslib                                                                                                                                                                                                                                                                                base64enc, cachem, fastmap (>= 1.1.1), grDevices, htmltools\n(>= 0.5.8), jquerylib (>= 0.1.3), jsonlite, lifecycle, memoise\n(>= 2.0.1), mime, rlang, sass (>= 0.4.9)
#> cachem                                                                                                                                                                                                                                                                                                                                                                                                                           rlang, fastmap (>= 1.2.0)
#> callr                                                                                                                                                                                                                                                                                                                                                                                                                       processx (>= 3.6.1), R6, utils
#> car                                                                                                                                                                                                                                                                                                                          abind, MASS, mgcv, nnet, pbkrtest (>= 0.4-4), quantreg,\ngrDevices, utils, stats, graphics, lme4 (>= 1.1-27.1), nlme,\nscales
#> carData                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
#> cli                                                                                                                                                                                                                                                                                                                                                                                                                                                  utils
#> clipr                                                                                                                                                                                                                                                                                                                                                                                                                                                utils
#> colorspace                                                                                                                                                                                                                                                                                                                                                                                                                      graphics, grDevices, stats
#> commonmark                                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
#> covr                                                                                                                                                                                                                                                                                                                                                                            digest, stats, utils, jsonlite, rex, httr, crayon, withr (>=\n1.0.2), yaml
#> cowplot                                                                                                                                                                                                                                                                                                                                                                               ggplot2 (>= 3.4.0), grid, gtable, grDevices, methods, rlang,\nscales
#> cpp11                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
#> crayon                                                                                                                                                                                                                                                                                                                                                                                                                           grDevices, methods, utils
#> credentials                                                                                                                                                                                                                                                                                                                                                                                        openssl (>= 1.3), sys (>= 2.1), curl, jsonlite, askpass
#> crosstalk                                                                                                                                                                                                                                                                                                                                                                                                     htmltools (>= 0.3.6), jsonlite, lazyeval, R6
#> curl                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> data.table                                                                                                                                                                                                                                                                                                                                                                                                                                         methods
#> Deriv                                                                                                                                                                                                                                                                                                                                                                                                                                              methods
#> desc                                                                                                                                                                                                                                                                                                                                                                                                                                        cli, R6, utils
#> devtools       cli (>= 3.3.0), desc (>= 1.4.1), ellipsis (>= 0.3.2), fs (>=\n1.5.2), lifecycle (>= 1.0.1), memoise (>= 2.0.1), miniUI (>=\n0.1.1.1), pkgbuild (>= 1.3.1), pkgdown (>= 2.0.6), pkgload (>=\n1.3.0), profvis (>= 0.3.7), rcmdcheck (>= 1.4.0), remotes (>=\n2.4.2), rlang (>= 1.0.4), roxygen2 (>= 7.2.1), rversions (>=\n2.1.1), sessioninfo (>= 1.2.2), stats, testthat (>= 3.1.5),\ntools, urlchecker (>= 1.0.1), utils, withr (>= 2.5.0)
#> diagram                                                                                                                                                                                                                                                                                                                                                                                                                                    stats, graphics
#> diffobj                                                                                                                                                                                                                                                                                                                                                                                                    crayon (>= 1.3.2), tools, methods, utils, stats
#> digest                                                                                                                                                                                                                                                                                                                                                                                                                                               utils
#> doBy                                                                                                                                                                                                                                                                                                                                             boot, broom, cowplot, Deriv, dplyr, ggplot2, MASS, Matrix,\nmodelr, microbenchmark, rlang, tibble, tidyr,
#> downlit                                                                                                                                                                                                                                                                                                                                                                           brio, desc, digest, evaluate, fansi, memoise, rlang, vctrs,\nwithr, yaml
#> downloader                                                                                                                                                                                                                                                                                                                                                                                                                                   utils, digest
#> dplyr                                                                                                                                                                                                                                           cli (>= 3.4.0), generics, glue (>= 1.3.2), lifecycle (>=\n1.0.3), magrittr (>= 1.5), methods, pillar (>= 1.9.0), R6,\nrlang (>= 1.1.0), tibble (>= 3.2.0), tidyselect (>= 1.2.0),\nutils, vctrs (>= 0.6.4)
#> DT                                                                                                                                                                                                                                                                                                                                     htmltools (>= 0.3.6), htmlwidgets (>= 1.3), httpuv, jsonlite\n(>= 0.9.16), magrittr, crosstalk, jquerylib, promises
#> dunn.test                                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
#> ellipsis                                                                                                                                                                                                                                                                                                                                                                                                                                  rlang (>= 0.3.0)
#> evaluate                                                                                                                                                                                                                                                                                                                                                                                                                                           methods
#> fansi                                                                                                                                                                                                                                                                                                                                                                                                                                     grDevices, utils
#> farver                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
#> fastmap                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
#> fontawesome                                                                                                                                                                                                                                                                                                                                                                                                       rlang (>= 1.0.6), htmltools (>= 0.5.1.1)
#> fs                                                                                                                                                                                                                                                                                                                                                                                                                                                 methods
#> FSA                                                                                                                                                                                                                                                                                                                                                                      graphics, grDevices, stats, tools, utils, car, dunn.test,\nlmtest, plotrix, withr
#> future                                                                                                                                                                                                                                                                                                                                                           digest, globals (>= 0.16.1), listenv (>= 0.8.0), parallel,\nparallelly (>= 1.38.0), utils
#> future.apply                                                                                                                                                                                                                                                                                                                                                                                                          globals (>= 0.16.1), parallel, utils
#> gclus                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
#> generics                                                                                                                                                                                                                                                                                                                                                                                                                                           methods
#> gert                                                                                                                                                                                                                                                                                                                                                       askpass, credentials (>= 1.2.1), openssl (>= 2.0.3),\nrstudioapi (>= 0.11), sys, zip (>= 2.1.0)
#> ggbiplot                                                                                                                                                                                                                                                                                                                                                                                                                                            scales
#> ggplot2                                                                                                                                                                                                                                                                  cli, glue, grDevices, grid, gtable (>= 0.1.1), isoband,\nlifecycle (> 1.0.1), MASS, mgcv, rlang (>= 1.1.0), scales (>=\n1.3.0), stats, tibble, vctrs (>= 0.6.0), withr (>= 2.5.0)
#> gh                                                                                                                                                                                                                                                                                                                                                                      cli (>= 3.0.1), gitcreds, glue, httr2, ini, jsonlite,\nlifecycle, rlang (>= 1.0.0)
#> gitcreds                                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
#> globals                                                                                                                                                                                                                                                                                                                                                                                                                                          codetools
#> glue                                                                                                                                                                                                                                                                                                                                                                                                                                               methods
#> gridExtra                                                                                                                                                                                                                                                                                                                                                                                                         gtable, grid, grDevices, graphics, utils
#> gtable                                                                                                                                                                                                                                                                                                                                                                                                        cli, glue, grid, lifecycle, rlang (>= 1.1.0)
#> highr                                                                                                                                                                                                                                                                                                                                                                                                                                       xfun (>= 0.18)
#> htmltools                                                                                                                                                                                                                                                                                                                                                                       base64enc, digest, fastmap (>= 1.1.0), grDevices, rlang (>=\n1.0.0), utils
#> htmlwidgets                                                                                                                                                                                                                                                                                                                                                        grDevices, htmltools (>= 0.5.7), jsonlite (>= 0.9.16), knitr\n(>= 1.8), rmarkdown, yaml
#> httpuv                                                                                                                                                                                                                                                                                                                                                                                              later (>= 0.8.0), promises, R6, Rcpp (>= 1.0.7), utils
#> httr                                                                                                                                                                                                                                                                                                                                                                                                 curl (>= 5.0.2), jsonlite, mime, openssl (>= 0.8), R6
#> httr2                                                                                                                                                                                                                                                                                                                       cli (>= 3.0.0), curl (>= 5.2.2), glue, lifecycle, magrittr,\nopenssl, R6, rappdirs, rlang (>= 1.1.0), vctrs (>= 0.6.3),\nwithr
#> igraph                                                                                                                                                                                                                                                                                                                                            cli, graphics, grDevices, lifecycle, magrittr, Matrix,\npkgconfig (>= 2.0.0), rlang, stats, utils, vctrs
#> imager                                                                                                                                                                                                                                                                                                                                                     Rcpp (>= 1.0.0), methods, stringr, png, jpeg, readbitmap,\ngrDevices, purrr, downloader, igraph
#> ini                                                                                                                                                                                                                                                                                                                                                                                                                                                   <NA>
#> ipred                                                                                                                                                                                                                                                                                                                                                                                               rpart (>= 3.1-8), MASS, survival, nnet, class, prodlim
#> isoband                                                                                                                                                                                                                                                                                                                                                                                                                                        grid, utils
#> jpeg                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> jquerylib                                                                                                                                                                                                                                                                                                                                                                                                                                        htmltools
#> jsonlite                                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
#> knitr                                                                                                                                                                                                                                                                                                                                                               evaluate (>= 0.15), highr (>= 0.11), methods, tools, xfun (>=\n0.44), yaml (>= 2.1.19)
#> labeling                                                                                                                                                                                                                                                                                                                                                                                                                                   stats, graphics
#> later                                                                                                                                                                                                                                                                                                                                                                                                                              Rcpp (>= 0.12.9), rlang
#> lava                                                                                                                                                                                                                                                                                                                                                cli, future.apply, graphics, grDevices, methods, numDeriv,\nprogressr, stats, survival, SQUAREM, utils
#> lazyeval                                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
#> libminer.1                                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
#> lifecycle                                                                                                                                                                                                                                                                                                                                                                                                           cli (>= 3.4.0), glue, rlang (>= 1.1.0)
#> listenv                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
#> lme4                                                                                                                                                                                                                                                                                                                               graphics, grid, splines, utils, parallel, MASS, lattice, boot,\nnlme (>= 3.1-123), minqa (>= 1.1.15), nloptr (>= 1.0.4)
#> lmtest                                                                                                                                                                                                                                                                                                                                                                                                                                            graphics
#> magick                                                                                                                                                                                                                                                                                                                                                                                                                   Rcpp (>= 0.12.12), magrittr, curl
#> magrittr                                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
#> MatrixModels                                                                                                                                                                                                                                                                                                                                                                                                             stats, methods, Matrix (>= 1.6-0)
#> memoise                                                                                                                                                                                                                                                                                                                                                                                                                          rlang (>= 0.4.10), cachem
#> microbenchmark                                                                                                                                                                                                                                                                                                                                                                                                                             graphics, stats
#> mime                                                                                                                                                                                                                                                                                                                                                                                                                                                 tools
#> miniUI                                                                                                                                                                                                                                                                                                                                                                                                          shiny (>= 0.13), htmltools (>= 0.3), utils
#> minqa                                                                                                                                                                                                                                                                                                                                                                                                                                     Rcpp (>= 0.9.10)
#> modelr                                                                                                                                                                                                                                                                                                                                                   broom, magrittr, purrr (>= 0.2.2), rlang (>= 1.0.6), tibble,\ntidyr (>= 0.8.0), tidyselect, vctrs
#> munsell                                                                                                                                                                                                                                                                                                                                                                                                                                colorspace, methods
#> nloptr                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
#> NLP                                                                                                                                                                                                                                                                                                                                                                                                                                                  utils
#> numDeriv                                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
#> OpenImageR                                                                                                                                                                                                                                                                                                                                                     Rcpp (>= 0.12.17), graphics, grDevices, grid, shiny, jpeg,\npng, tiff, R6, lifecycle, tools
#> openssl                                                                                                                                                                                                                                                                                                                                                                                                                                            askpass
#> parallelly                                                                                                                                                                                                                                                                                                                                                                                                                          parallel, tools, utils
#> pbkrtest                                                                                                                                                                                                                                                                                                                                                                                    broom, dplyr, MASS, methods, numDeriv, Matrix (>= 1.2.3), doBy
#> permute                                                                                                                                                                                                                                                                                                                                                                                                                                              stats
#> pillar                                                                                                                                                                                                                                                                                                                                                 cli (>= 2.3.0), fansi, glue, lifecycle, rlang (>= 1.0.2), utf8\n(>= 1.1.0), utils, vctrs (>= 0.5.0)
#> pixmap                                                                                                                                                                                                                                                                                                                                                                                                                        methods, graphics, grDevices
#> pkgbuild                                                                                                                                                                                                                                                                                                                                                                                              callr (>= 3.2.0), cli (>= 3.4.0), desc, processx, R6
#> pkgconfig                                                                                                                                                                                                                                                                                                                                                                                                                                            utils
#> pkgdown                                                                                                                                                      bslib (>= 0.5.1), callr (>= 3.7.3), cli (>= 3.6.1), desc (>=\n1.4.0), digest, downlit (>= 0.4.4), fontawesome, fs (>= 1.4.0),\nhttr2 (>= 1.0.2), jsonlite, openssl, purrr (>= 1.0.0), ragg,\nrlang (>= 1.1.0), rmarkdown (>= 2.27), tibble, whisker, withr\n(>= 2.4.3), xml2 (>= 1.3.1), yaml
#> pkgload                                                                                                                                                                                                                                                                                                                      cli (>= 3.3.0), desc, fs, glue, lifecycle, methods, pkgbuild,\nprocessx, rlang (>= 1.1.1), rprojroot, utils, withr (>= 2.4.3)
#> plotrix                                                                                                                                                                                                                                                                                                                                                                                                                  grDevices, graphics, stats, utils
#> png                                                                                                                                                                                                                                                                                                                                                                                                                                                   <NA>
#> praise                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
#> prettyunits                                                                                                                                                                                                                                                                                                                                                                                                                                           <NA>
#> processx                                                                                                                                                                                                                                                                                                                                                                                                                          ps (>= 1.2.0), R6, utils
#> prodlim                                                                                                                                                                                                                                                                                                                                                     Rcpp (>= 0.11.5), stats, data.table, grDevices, graphics,\ndiagram, survival, KernSmooth, lava
#> profvis                                                                                                                                                                                                                                                                                                                                                                                   htmlwidgets (>= 0.3.2), purrr, rlang (>= 0.4.9), stringr,\nvctrs
#> progressr                                                                                                                                                                                                                                                                                                                                                                                                                                    digest, utils
#> promises                                                                                                                                                                                                                                                                                                                                                                             fastmap (>= 1.1.0), later, magrittr (>= 1.5), R6, Rcpp, rlang,\nstats
#> ps                                                                                                                                                                                                                                                                                                                                                                                                                                                   utils
#> purrr                                                                                                                                                                                                                                                                                                                                                       cli (>= 3.6.1), lifecycle (>= 1.0.3), magrittr (>= 1.5.0),\nrlang (>= 1.1.1), vctrs (>= 0.6.3)
#> quantreg                                                                                                                                                                                                                                                                                                                                                                                           methods, graphics, Matrix, MatrixModels, survival, MASS
#> R6                                                                                                                                                                                                                                                                                                                                                                                                                                                    <NA>
#> ragg                                                                                                                                                                                                                                                                                                                                                                                                        systemfonts (>= 1.0.3), textshaping (>= 0.3.0)
#> rappdirs                                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
#> rcmdcheck                                                                                                                                                                                                                                                                                        callr (>= 3.1.1.9000), cli (>= 3.0.0), curl, desc (>= 1.2.0),\ndigest, pkgbuild, prettyunits, R6, rprojroot, sessioninfo (>=\n1.1.1), utils, withr, xopen
#> RColorBrewer                                                                                                                                                                                                                                                                                                                                                                                                                                          <NA>
#> Rcpp                                                                                                                                                                                                                                                                                                                                                                                                                                        methods, utils
#> RcppArmadillo                                                                                                                                                                                                                                                                                                                                                                                                       Rcpp (>= 1.0.8), stats, utils, methods
#> RcppEigen                                                                                                                                                                                                                                                                                                                                                                                                                   Rcpp (>= 0.11.0), stats, utils
#> readbitmap                                                                                                                                                                                                                                                                                                                                                                                                                            bmp, jpeg, png, tiff
#> rematch2                                                                                                                                                                                                                                                                                                                                                                                                                                            tibble
#> remotes                                                                                                                                                                                                                                                                                                                                                                                                                       methods, stats, tools, utils
#> rex                                                                                                                                                                                                                                                                                                                                                                                                                                               lazyeval
#> rlang                                                                                                                                                                                                                                                                                                                                                                                                                                                utils
#> rmarkdown                                                                                                                                                                                                                                        bslib (>= 0.2.5.1), evaluate (>= 0.13), fontawesome (>=\n0.5.0), htmltools (>= 0.5.1), jquerylib, jsonlite, knitr (>=\n1.43), methods, tinytex (>= 0.31), tools, utils, xfun (>=\n0.36), yaml (>= 2.1.19)
#> roxygen2                                                                                                                                                                                                                                                      brew, cli (>= 3.3.0), commonmark, desc (>= 1.2.0), knitr,\nmethods, pkgload (>= 1.0.2), purrr (>= 1.0.0), R6 (>= 2.1.2),\nrlang (>= 1.0.6), stringi, stringr (>= 1.0.0), utils, withr,\nxml2
#> rprojroot                                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
#> rstudioapi                                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
#> rversions                                                                                                                                                                                                                                                                                                                                                                                                                     curl, utils, xml2 (>= 1.0.0)
#> sass                                                                                                                                                                                                                                                                                                                                                                                 fs (>= 1.2.4), rlang (>= 0.4.10), htmltools (>= 0.5.1), R6,\nrappdirs
#> scales                                                                                                                                                                                                                                                                                                                               cli, farver (>= 2.0.3), glue, labeling, lifecycle, munsell (>=\n0.5), R6, RColorBrewer, rlang (>= 1.0.0), viridisLite
#> sessioninfo                                                                                                                                                                                                                                                                                                                                                                                                                   cli (>= 3.1.0), tools, utils
#> shape                                                                                                                                                                                                                                                                                                                                                                                                                           stats, graphics, grDevices
#> shapeR                                                                                                                                                                                                                                                                                                                                                                                             plotrix, jpeg, pixmap, wavethresh, methods, vegan, MASS
#> shiny                                                                                   utils, grDevices, httpuv (>= 1.5.2), mime (>= 0.3), jsonlite\n(>= 0.9.16), xtable, fontawesome (>= 0.4.0), htmltools (>=\n0.5.4), R6 (>= 2.0), sourcetools, later (>= 1.0.0), promises\n(>= 1.1.0), tools, crayon, rlang (>= 0.4.10), fastmap (>=\n1.1.1), withr, commonmark (>= 1.7), glue (>= 1.3.2), bslib (>=\n0.6.0), cachem (>= 1.1.0), lifecycle (>= 0.2.0)
#> slam                                                                                                                                                                                                                                                                                                                                                                                                                                                 stats
#> sourcetools                                                                                                                                                                                                                                                                                                                                                                                                                                           <NA>
#> sp                                                                                                                                                                                                                                                                                                                                                                                                        utils, stats, graphics, grDevices, lattice, grid
#> SparseM                                                                                                                                                                                                                                                                                                                                                                                                                             graphics, stats, utils
#> SQUAREM                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
#> stringi                                                                                                                                                                                                                                                                                                                                                                                                                                tools, utils, stats
#> stringr                                                                                                                                                                                                                                                                                                                                      cli, glue (>= 1.6.1), lifecycle (>= 1.0.3), magrittr, rlang\n(>= 1.0.0), stringi (>= 1.5.3), vctrs (>= 0.4.0)
#> sys                                                                                                                                                                                                                                                                                                                                                                                                                                                   <NA>
#> systemfonts                                                                                                                                                                                                                                                                                                                                                                                                                                      lifecycle
#> testthat                                                                                                 brio (>= 1.1.3), callr (>= 3.7.3), cli (>= 3.6.1), desc (>=\n1.4.2), digest (>= 0.6.33), evaluate (>= 0.21), jsonlite (>=\n1.8.7), lifecycle (>= 1.0.3), magrittr (>= 2.0.3), methods,\npkgload (>= 1.3.2.1), praise (>= 1.0.0), processx (>= 3.8.2),\nps (>= 1.7.5), R6 (>= 2.5.1), rlang (>= 1.1.1), utils, waldo\n(>= 0.5.1), withr (>= 2.5.0)
#> textshaping                                                                                                                                                                                                                                                                                                                                                                                                              lifecycle, systemfonts (>= 1.1.0)
#> tibble                                                                                                                                                                                                                                                                                                                fansi (>= 0.4.0), lifecycle (>= 1.0.0), magrittr, methods,\npillar (>= 1.8.1), pkgconfig, rlang (>= 1.0.2), utils, vctrs\n(>= 0.4.2)
#> tidyr                                                                                                                                                                                                                                                cli (>= 3.4.1), dplyr (>= 1.0.10), glue, lifecycle (>= 1.0.3),\nmagrittr, purrr (>= 1.0.1), rlang (>= 1.1.1), stringr (>=\n1.5.0), tibble (>= 2.1.1), tidyselect (>= 1.2.0), utils, vctrs\n(>= 0.5.2)
#> tidyselect                                                                                                                                                                                                                                                                                                                                               cli (>= 3.3.0), glue (>= 1.3.0), lifecycle (>= 1.0.3), rlang\n(>= 1.0.4), vctrs (>= 0.5.2), withr
#> tiff                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> tinytex                                                                                                                                                                                                                                                                                                                                                                                                                                     xfun (>= 0.29)
#> tm                                                                                                                                                                                                                                                                                                                                                                                  Rcpp, parallel, slam (>= 0.1-37), stats, tools, utils,\ngraphics, xml2
#> urlchecker                                                                                                                                                                                                                                                                                                                                                                                                                          cli, curl, tools, xml2
#> usethis                                                                                                                                                     cli (>= 3.0.1), clipr (>= 0.3.0), crayon, curl (>= 2.7), desc\n(>= 1.4.2), fs (>= 1.3.0), gert (>= 1.4.1), gh (>= 1.2.1), glue\n(>= 1.3.0), jsonlite, lifecycle (>= 1.0.0), purrr, rappdirs,\nrlang (>= 1.1.0), rprojroot (>= 1.2), rstudioapi, stats, utils,\nwhisker, withr (>= 2.3.0), yaml
#> utf8                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> vctrs                                                                                                                                                                                                                                                                                                                                                                                         cli (>= 3.4.0), glue, lifecycle (>= 1.0.3), rlang (>= 1.1.0)
#> vegan                                                                                                                                                                                                                                                                                                                                                                                                                                  MASS, cluster, mgcv
#> viridisLite                                                                                                                                                                                                                                                                                                                                                                                                                                           <NA>
#> waldo                                                                                                                                                                                                                                                                                                                                                                   cli, diffobj (>= 0.3.4), fansi, glue, methods, rematch2, rlang\n(>= 1.0.0), tibble
#> wavethresh                                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
#> whisker                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
#> withr                                                                                                                                                                                                                                                                                                                                                                                                                                  graphics, grDevices
#> xfun                                                                                                                                                                                                                                                                                                                                                                                                                               grDevices, stats, tools
#> xml2                                                                                                                                                                                                                                                                                                                                                                                                                        cli, methods, rlang (>= 1.1.0)
#> xopen                                                                                                                                                                                                                                                                                                                                                                                                                                             processx
#> xtable                                                                                                                                                                                                                                                                                                                                                                                                                                        stats, utils
#> yaml                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> zip                                                                                                                                                                                                                                                                                                                                                                                                                                                   <NA>
#> zoo                                                                                                                                                                                                                                                                                                                                                                                                       utils, graphics, grDevices, lattice (>= 0.20-27)
#> base                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> boot                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> class                                                                                                                                                                                                                                                                                                                                                                                                                                                 MASS
#> cluster                                                                                                                                                                                                                                                                                                                                                                                                                  graphics, grDevices, stats, utils
#> codetools                                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
#> compiler                                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
#> datasets                                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
#> foreign                                                                                                                                                                                                                                                                                                                                                                                                                              methods, utils, stats
#> graphics                                                                                                                                                                                                                                                                                                                                                                                                                                         grDevices
#> grDevices                                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
#> grid                                                                                                                                                                                                                                                                                                                                                                                                                                      grDevices, utils
#> KernSmooth                                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
#> lattice                                                                                                                                                                                                                                                                                                                                                                                                            grid, grDevices, graphics, stats, utils
#> MASS                                                                                                                                                                                                                                                                                                                                                                                                                                               methods
#> Matrix                                                                                                                                                                                                                                                                                                                                                                                                    grDevices, graphics, grid, lattice, stats, utils
#> methods                                                                                                                                                                                                                                                                                                                                                                                                                                       utils, stats
#> mgcv                                                                                                                                                                                                                                                                                                                                                                                                      methods, stats, graphics, Matrix, splines, utils
#> nlme                                                                                                                                                                                                                                                                                                                                                                                                                       graphics, stats, utils, lattice
#> nnet                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> parallel                                                                                                                                                                                                                                                                                                                                                                                                                                   tools, compiler
#> rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
#> spatial                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
#> splines                                                                                                                                                                                                                                                                                                                                                                                                                                    graphics, stats
#> stats                                                                                                                                                                                                                                                                                                                                                                                                                           utils, grDevices, graphics
#> stats4                                                                                                                                                                                                                                                                                                                                                                                                                            graphics, methods, stats
#> survival                                                                                                                                                                                                                                                                                                                                                                                                  graphics, Matrix, methods, splines, stats, utils
#> tcltk                                                                                                                                                                                                                                                                                                                                                                                                                                                utils
#> tools                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
#> utils                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
#>                                                                     LinkingTo
#> libminer                                                                 <NA>
#> abind                                                                    <NA>
#> ade4                                                      Rcpp, RcppArmadillo
#> askpass                                                                  <NA>
#> backports                                                                <NA>
#> base64enc                                                                <NA>
#> BH                                                                       <NA>
#> bmp                                                                      <NA>
#> box                                                                      <NA>
#> brew                                                                     <NA>
#> brio                                                                     <NA>
#> broom                                                                    <NA>
#> bslib                                                                    <NA>
#> cachem                                                                   <NA>
#> callr                                                                    <NA>
#> car                                                                      <NA>
#> carData                                                                  <NA>
#> cli                                                                      <NA>
#> clipr                                                                    <NA>
#> colorspace                                                               <NA>
#> commonmark                                                               <NA>
#> covr                                                                     <NA>
#> cowplot                                                                  <NA>
#> cpp11                                                                    <NA>
#> crayon                                                                   <NA>
#> credentials                                                              <NA>
#> crosstalk                                                                <NA>
#> curl                                                                     <NA>
#> data.table                                                               <NA>
#> Deriv                                                                    <NA>
#> desc                                                                     <NA>
#> devtools                                                                 <NA>
#> diagram                                                                  <NA>
#> diffobj                                                                  <NA>
#> digest                                                                   <NA>
#> doBy                                                                     <NA>
#> downlit                                                                  <NA>
#> downloader                                                               <NA>
#> dplyr                                                                    <NA>
#> DT                                                                       <NA>
#> dunn.test                                                                <NA>
#> ellipsis                                                                 <NA>
#> evaluate                                                                 <NA>
#> fansi                                                                    <NA>
#> farver                                                                   <NA>
#> fastmap                                                                  <NA>
#> fontawesome                                                              <NA>
#> fs                                                                       <NA>
#> FSA                                                                      <NA>
#> future                                                                   <NA>
#> future.apply                                                             <NA>
#> gclus                                                                    <NA>
#> generics                                                                 <NA>
#> gert                                                                     <NA>
#> ggbiplot                                                                 <NA>
#> ggplot2                                                                  <NA>
#> gh                                                                       <NA>
#> gitcreds                                                                 <NA>
#> globals                                                                  <NA>
#> glue                                                                     <NA>
#> gridExtra                                                                <NA>
#> gtable                                                                   <NA>
#> highr                                                                    <NA>
#> htmltools                                                                <NA>
#> htmlwidgets                                                              <NA>
#> httpuv                                                            later, Rcpp
#> httr                                                                     <NA>
#> httr2                                                                    <NA>
#> igraph                                                       cpp11 (>= 0.4.7)
#> imager                                                                   Rcpp
#> ini                                                                      <NA>
#> ipred                                                                    <NA>
#> isoband                                                                  <NA>
#> jpeg                                                                     <NA>
#> jquerylib                                                                <NA>
#> jsonlite                                                                 <NA>
#> knitr                                                                    <NA>
#> labeling                                                                 <NA>
#> later                                                                    Rcpp
#> lava                                                                     <NA>
#> lazyeval                                                                 <NA>
#> libminer.1                                                               <NA>
#> lifecycle                                                                <NA>
#> listenv                                                                  <NA>
#> lme4           Rcpp (>= 0.10.5), RcppEigen (>= 0.3.3.9.4), Matrix (>=\n1.2-3)
#> lmtest                                                                   <NA>
#> magick                                                                   Rcpp
#> magrittr                                                                 <NA>
#> MatrixModels                                                             <NA>
#> memoise                                                                  <NA>
#> microbenchmark                                                           <NA>
#> mime                                                                     <NA>
#> miniUI                                                                   <NA>
#> minqa                                                                    Rcpp
#> modelr                                                                   <NA>
#> munsell                                                                  <NA>
#> nloptr                                                                   <NA>
#> NLP                                                                      <NA>
#> numDeriv                                                                 <NA>
#> OpenImageR                                     Rcpp, RcppArmadillo (>= 0.8.0)
#> openssl                                                                  <NA>
#> parallelly                                                               <NA>
#> pbkrtest                                                                 <NA>
#> permute                                                                  <NA>
#> pillar                                                                   <NA>
#> pixmap                                                                   <NA>
#> pkgbuild                                                                 <NA>
#> pkgconfig                                                                <NA>
#> pkgdown                                                                  <NA>
#> pkgload                                                                  <NA>
#> plotrix                                                                  <NA>
#> png                                                                      <NA>
#> praise                                                                   <NA>
#> prettyunits                                                              <NA>
#> processx                                                                 <NA>
#> prodlim                                                                  Rcpp
#> profvis                                                                  <NA>
#> progressr                                                                <NA>
#> promises                                                          later, Rcpp
#> ps                                                                       <NA>
#> purrr                                                                     cli
#> quantreg                                                                 <NA>
#> R6                                                                       <NA>
#> ragg                                                 systemfonts, textshaping
#> rappdirs                                                                 <NA>
#> rcmdcheck                                                                <NA>
#> RColorBrewer                                                             <NA>
#> Rcpp                                                                     <NA>
#> RcppArmadillo                                                            Rcpp
#> RcppEigen                                                                Rcpp
#> readbitmap                                                               <NA>
#> rematch2                                                                 <NA>
#> remotes                                                                  <NA>
#> rex                                                                      <NA>
#> rlang                                                                    <NA>
#> rmarkdown                                                                <NA>
#> roxygen2                                                                cpp11
#> rprojroot                                                                <NA>
#> rstudioapi                                                               <NA>
#> rversions                                                                <NA>
#> sass                                                                     <NA>
#> scales                                                                   <NA>
#> sessioninfo                                                              <NA>
#> shape                                                                    <NA>
#> shapeR                                                                   <NA>
#> shiny                                                                    <NA>
#> slam                                                                     <NA>
#> sourcetools                                                              <NA>
#> sp                                                                       <NA>
#> SparseM                                                                  <NA>
#> SQUAREM                                                                  <NA>
#> stringi                                                                  <NA>
#> stringr                                                                  <NA>
#> sys                                                                      <NA>
#> systemfonts                                                  cpp11 (>= 0.2.1)
#> testthat                                                                 <NA>
#> textshaping                          cpp11 (>= 0.2.1), systemfonts (>= 1.0.0)
#> tibble                                                                   <NA>
#> tidyr                                                        cpp11 (>= 0.4.0)
#> tidyselect                                                               <NA>
#> tiff                                                                     <NA>
#> tinytex                                                                  <NA>
#> tm                                                                   BH, Rcpp
#> urlchecker                                                               <NA>
#> usethis                                                                  <NA>
#> utf8                                                                     <NA>
#> vctrs                                                                    <NA>
#> vegan                                                                    <NA>
#> viridisLite                                                              <NA>
#> waldo                                                                    <NA>
#> wavethresh                                                               <NA>
#> whisker                                                                  <NA>
#> withr                                                                    <NA>
#> xfun                                                                     <NA>
#> xml2                                                                     <NA>
#> xopen                                                                    <NA>
#> xtable                                                                   <NA>
#> yaml                                                                     <NA>
#> zip                                                                      <NA>
#> zoo                                                                      <NA>
#> base                                                                     <NA>
#> boot                                                                     <NA>
#> class                                                                    <NA>
#> cluster                                                                  <NA>
#> codetools                                                                <NA>
#> compiler                                                                 <NA>
#> datasets                                                                 <NA>
#> foreign                                                                  <NA>
#> graphics                                                                 <NA>
#> grDevices                                                                <NA>
#> grid                                                                     <NA>
#> KernSmooth                                                               <NA>
#> lattice                                                                  <NA>
#> MASS                                                                     <NA>
#> Matrix                                                                   <NA>
#> methods                                                                  <NA>
#> mgcv                                                                     <NA>
#> nlme                                                                     <NA>
#> nnet                                                                     <NA>
#> parallel                                                                 <NA>
#> rpart                                                                    <NA>
#> spatial                                                                  <NA>
#> splines                                                                  <NA>
#> stats                                                                    <NA>
#> stats4                                                                   <NA>
#> survival                                                                 <NA>
#> tcltk                                                                    <NA>
#> tools                                                                    <NA>
#> utils                                                                    <NA>
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Suggests
#> libminer                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             knitr,\nrmarkdown,\ntestthat (>= 3.0.0)
#> abind                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <NA>
#> ade4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ade4TkGUI, adegraphics, adephylo, ape, CircStats, deldir,\nlattice, spdep, splancs, waveslim, progress, foreach, parallel,\ndoParallel, iterators
#> askpass                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             testthat
#> backports                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
#> base64enc                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
#> BH                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <NA>
#> bmp                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         pixmap, testthat
#> box                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               devtools, knitr (>= 1.40), rmarkdown, R6, rlang, roxygen2 (>=\n7.2.1), shiny, stringr, testthat (>= 3.1.7)
#> brew                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     testthat (>= 3.0.0)
#> brio                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               covr, testthat (>= 3.0.0)
#> broom          AER, AUC, bbmle, betareg, biglm, binGroup, boot, btergm (>=\n1.10.6), car, carData, caret, cluster, cmprsk, coda, covr, drc,\ne1071, emmeans, epiR, ergm (>= 3.10.4), fixest (>= 0.9.0), gam\n(>= 1.15), gee, geepack, ggplot2, glmnet, glmnetUtils, gmm,\nHmisc, irlba, interp, joineRML, Kendall, knitr, ks, Lahman,\nlavaan, leaps, lfe, lm.beta, lme4, lmodel2, lmtest (>= 0.9.38),\nlsmeans, maps, MASS, mclust, mediation, metafor, mfx, mgcv,\nmlogit, modeldata, modeltests (>= 0.1.6), muhaz, multcomp,\nnetwork, nnet, orcutt (>= 2.2), ordinal, plm, poLCA, psych,\nquantreg, rmarkdown, robust, robustbase, rsample, sandwich,\nspdep (>= 1.1), spatialreg, speedglm, spelling, survey,\nsurvival (>= 3.6-4), systemfit, testthat (>= 2.1.0), tseries,\nvars, zoo
#> bslib                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               bsicons, curl, fontawesome, future, ggplot2, knitr, magrittr,\nrappdirs, rmarkdown (>= 2.7), shiny (> 1.8.1), testthat,\nthematic, withr
#> cachem                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              testthat
#> callr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         asciicast (>= 2.3.1), cli (>= 1.1.0), mockery, ps, rprojroot,\nspelling, testthat (>= 3.2.0), withr (>= 2.3.0)
#> car                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            alr4, boot, coxme, effects, knitr, leaps, lmtest, Matrix,\nMatrixModels, mvtnorm, rgl (>= 0.111.3), rio, sandwich,\nSparseM, survival, survey
#> carData                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       car (>= 3.0-0)
#> cli                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      callr, covr, crayon, digest, glue (>= 1.6.0), grDevices,\nhtmltools, htmlwidgets, knitr, methods, mockery, processx, ps\n(>= 1.3.4.9000), rlang (>= 1.0.2.9003), rmarkdown, rprojroot,\nrstudioapi, testthat, tibble, whoami, withr
#> clipr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      covr, knitr, rmarkdown, rstudioapi (>= 0.5), testthat (>=\n2.0.0)
#> colorspace                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             datasets, utils, KernSmooth, MASS, kernlab, mvtnorm, vcd,\ntcltk, shiny, shinyjs, ggplot2, dplyr, scales, grid, png, jpeg,\nknitr, rmarkdown, RColorBrewer, rcartocolor, scico, viridis,\nwesanderson
#> commonmark                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              curl, testthat, xml2
#> covr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           R6, curl, knitr, rmarkdown, htmltools, DT (>= 0.2), testthat,\nrlang, rstudioapi (>= 0.2), xml2 (>= 1.0.0), parallel, memoise,\nmockery, covr
#> cowplot                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Cairo, covr, dplyr, forcats, gridGraphics (>= 0.4-0), knitr,\nlattice, magick, maps, PASWR, patchwork, rmarkdown, ragg,\ntestthat (>= 1.0.0), tidyr, vdiffr (>= 0.3.0), VennDiagram
#> cpp11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               bench, brio, callr, cli, covr, decor, desc, ggplot2, glue,\nknitr, lobstr, mockery, progress, rmarkdown, scales, Rcpp,\ntestthat (>= 3.2.0), tibble, utils, vctrs, withr
#> crayon                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  mockery, rstudioapi, testthat, withr
#> credentials                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       testthat, knitr, rmarkdown
#> crosstalk                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   shiny, ggplot2, testthat (>= 2.1.0), sass, bslib
#> curl                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          spelling, testthat (>= 1.0.0), knitr, jsonlite, later,\nrmarkdown, httpuv (>= 1.4.4), webutils
#> data.table                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            bit64 (>= 4.0.0), bit (>= 4.0.4), R.utils, xts, zoo (>=\n1.8-1), yaml, knitr, markdown
#> Deriv                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   testthat (>= 0.11.0)
#> desc                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      callr, covr, gh, spelling, testthat, whoami, withr
#> devtools                                                                                                                                                                                                                                                                                                                                                                                                                            BiocManager (>= 1.30.18), callr (>= 3.7.1), covr (>= 3.5.1),\ncurl (>= 4.3.2), digest (>= 0.6.29), DT (>= 0.23), foghorn (>=\n1.4.2), gh (>= 1.3.0), gmailr (>= 1.0.1), httr (>= 1.4.3),\nknitr (>= 1.39), lintr (>= 3.0.0), MASS, mockery (>= 0.4.3),\npingr (>= 2.0.1), rhub (>= 1.1.1), rmarkdown (>= 2.14),\nrstudioapi (>= 0.13), spelling (>= 2.2)
#> diagram                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
#> diffobj                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     knitr, rmarkdown
#> digest                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             tinytest, simplermarkdown
#> doBy                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         geepack, knitr, lme4, markdown, multcomp, pbkrtest (>=\n0.4-8.1), survival, testthat (>= 2.1.0)
#> downlit                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     covr, htmltools, jsonlite, MASS, MassSpecWavelet, pkgload,\nrmarkdown, testthat (>= 3.0.0), xml2
#> downloader                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          testthat
#> dplyr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   bench, broom, callr, covr, DBI, dbplyr (>= 2.2.1), ggplot2,\nknitr, Lahman, lobstr, microbenchmark, nycflights13, purrr,\nrmarkdown, RMySQL, RPostgreSQL, RSQLite, stringi (>= 1.7.6),\ntestthat (>= 3.1.5), tidyr (>= 1.3.0), withr
#> DT                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 knitr (>= 1.8), rmarkdown, shiny (>= 1.6), bslib, future,\ntestit, tibble
#> dunn.test                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
#> ellipsis                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      covr, testthat
#> evaluate                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           covr, ggplot2, lattice, rlang, testthat (>= 3.0.0), withr
#> fansi                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             unitizer, knitr, rmarkdown
#> farver                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             covr, testthat (>= 3.0.0)
#> fastmap                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  testthat (>= 2.1.1)
#> fontawesome                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              covr, dplyr (>= 1.0.8), knitr (>= 1.31), testthat (>= 3.0.0),\nrsvg
#> fs                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             covr, crayon, knitr, pillar (>= 1.0.0), rmarkdown, spelling,\ntestthat (>= 3.0.0), tibble (>= 1.1.0), vctrs (>= 0.3.0), withr
#> FSA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               DescTools, dplyr, fishmethods, FSAdata, knitr, marked, nlme,\nnlstools, pkgdown, plyr, psych, Rcapture, rmarkdown, testthat,\ntibble, covr
#> future                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 methods, RhpcBLASctl, R.rsp, markdown
#> future.apply                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     datasets, stats, tools, listenv (>= 0.8.0), R.rsp, markdown
#> gclus                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       knitr, rmarkdown
#> generics                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   covr, pkgload, testthat (>= 3.0.0), tibble, withr
#> gert                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    spelling, knitr, rmarkdown, testthat
#> ggbiplot                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 corrplot, dplyr, MASS, broom, tidyr
#> ggplot2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     covr, dplyr, ggplot2movies, hexbin, Hmisc, knitr, mapproj,\nmaps, multcomp, munsell, nlme, profvis, quantreg, ragg (>=\n1.2.6), RColorBrewer, rmarkdown, rpart, sf (>= 0.7-3), svglite\n(>= 2.1.2), testthat (>= 3.1.2), vdiffr (>= 1.0.6), xml2
#> gh                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         covr, knitr, mockery, rmarkdown, rprojroot, spelling,\ntestthat (>= 3.0.0), withr
#> gitcreds                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  codetools, covr, knitr, mockery, oskeyring, rmarkdown,\ntestthat (>= 3.0.0), withr
#> globals                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
#> glue                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             crayon, DBI (>= 1.2.0), dplyr, knitr, magrittr, rlang,\nrmarkdown, RSQLite, testthat (>= 3.2.0), vctrs (>= 0.3.0),\nwaldo (>= 0.3.0), withr
#> gridExtra                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ggplot2, egg, lattice, knitr, testthat
#> gtable                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         covr, ggplot2, knitr, profvis, rmarkdown, testthat (>= 3.0.0)
#> highr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                knitr, markdown, testit
#> htmltools                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Cairo, markdown, ragg, shiny, testthat, withr
#> htmlwidgets                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         testthat
#> httpuv                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      callr, curl, testthat, websocket
#> httr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            covr, httpuv, jpeg, knitr, png, readr, rmarkdown, testthat\n(>= 0.8.0), xml2
#> httr2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          askpass, bench, clipr, covr, docopt, httpuv, jose, jsonlite,\nknitr, later, promises, rmarkdown, testthat (>= 3.1.8), tibble,\nwebfakes, xml2
#> igraph                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ape (>= 5.7-0.1), callr, decor, digest, graph, igraphdata,\nknitr, rgl, rmarkdown, scales, stats4, tcltk, testthat, vdiffr,\nwithr
#> imager                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             knitr, rmarkdown, ggplot2, dplyr, scales, testthat, raster,\nspatstat.geom, magick, Cairo
#> ini                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 testthat
#> ipred                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         mvtnorm, mlbench, TH.data, randomForest, party
#> isoband                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         covr, ggplot2, knitr, magick, microbenchmark, rmarkdown, sf,\ntestthat, xml2
#> jpeg                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <NA>
#> jquerylib                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           testthat
#> jsonlite                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  httr, vctrs, testthat, knitr, rmarkdown, R.rsp, sf
#> knitr                                                                                                                                                                                                                                                                                                                                                                                                              bslib, codetools, DBI (>= 0.4-1), digest, formatR, gifski,\ngridSVG, htmlwidgets (>= 0.7), jpeg, JuliaCall (>= 0.11.1),\nmagick, markdown (>= 1.3), png, ragg, reticulate (>= 1.4), rgl\n(>= 0.95.1201), rlang, rmarkdown, sass, showtext, styler (>=\n1.2.0), targets (>= 0.6.0), testit, tibble, tikzDevice (>=\n0.10), tinytex (>= 0.46), webshot, rstudioapi, svglite
#> labeling                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
#> later                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  knitr, rmarkdown, testthat (>= 2.1.0)
#> lava                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             KernSmooth, Rgraphviz, data.table, ellipse, fields, geepack,\ngraph, knitr, rmarkdown, igraph (>= 0.6), lavaSearch2, lme4 (>=\n1.1.35.1), MASS, Matrix (>= 1.6.3), mets (>= 1.1), nlme,\noptimx, polycor, quantreg, rgl, targeted (>= 0.4), testthat (>=\n0.11), visNetwork
#> lazyeval                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        knitr, rmarkdown (>= 0.2.65), testthat, covr
#> libminer.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
#> lifecycle                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                covr, crayon, knitr, lintr, rmarkdown, testthat (>= 3.0.1),\ntibble, tidyverse, tools, vctrs, withr
#> listenv                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             R.utils, R.rsp, markdown
#> lme4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   knitr, rmarkdown, MEMSS, testthat (>= 0.8.1), ggplot2,\nmlmRev, optimx (>= 2013.8.6), gamm4, pbkrtest, HSAUR3,\nnumDeriv, car, dfoptim, mgcv, statmod, rr2, semEff, tibble,\nmerDeriv
#> lmtest                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              car, strucchange, sandwich, dynlm, stats4, survival, AER
#> magick                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         av (>= 0.3), spelling, jsonlite, methods, knitr, rmarkdown,\nrsvg, webp, pdftools, ggplot2, gapminder, IRdisplay, tesseract\n(>= 2.0), gifski
#> magrittr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             covr, knitr, rlang, rmarkdown, testthat
#> MatrixModels                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
#> memoise                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              digest, aws.s3, covr, googleAuthR, googleCloudStorageR, httr,\ntestthat
#> microbenchmark                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ggplot2, multcomp, RUnit
#> mime                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <NA>
#> miniUI                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> minqa                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <NA>
#> modelr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          compiler, covr, ggplot2, testthat (>= 3.0.0)
#> munsell                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ggplot2, testthat
#> nloptr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      knitr, rmarkdown, covr, tinytest
#> NLP                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <NA>
#> numDeriv                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
#> OpenImageR                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  testthat, knitr, rmarkdown, covr
#> openssl                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         curl, testthat (>= 2.1.0), digest, knitr, rmarkdown,\njsonlite, jose, sodium
#> parallelly                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
#> pbkrtest                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     markdown, knitr
#> permute                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              vegan (>= 2.0-0), testthat (>= 0.5), parallel, knitr,\nrmarkdown, bookdown, sessioninfo
#> pillar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         bit64, DBI, debugme, DiagrammeR, dplyr, formattable, ggplot2,\nknitr, lubridate, nanotime, nycflights13, palmerpenguins,\nrmarkdown, scales, stringi, survival, testthat (>= 3.1.1),\ntibble, units (>= 0.7.2), vdiffr, withr
#> pixmap                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> pkgbuild                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                covr, cpp11, knitr, mockery, Rcpp, rmarkdown, testthat (>=\n3.0.0), withr (>= 2.3.0)
#> pkgconfig                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             covr, testthat, disposables (>= 1.0.3)
#> pkgdown                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         covr, diffviewer, evaluate (>= 0.24.0), gert, gt, htmltools,\nhtmlwidgets, knitr, lifecycle, magick, methods, pkgload (>=\n1.0.2), quarto, rsconnect, rstudioapi, rticles, sass, testthat\n(>= 3.1.3), tools
#> pkgload                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          bitops, jsonlite, mathjaxr, pak, Rcpp, remotes, rstudioapi,\ntestthat (>= 3.2.1.1), usethis
#> plotrix                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
#> png                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <NA>
#> praise                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              testthat
#> prettyunits                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        codetools, covr, testthat
#> processx                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                callr (>= 3.7.3), cli (>= 3.3.0), codetools, covr, curl,\ndebugme, parallel, rlang (>= 1.0.2), testthat (>= 3.0.0),\nwebfakes, withr
#> prodlim                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
#> profvis                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          knitr, ggplot2, rmarkdown, testthat (>= 3.0.0), devtools,\nshiny, htmltools
#> progressr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              graphics, tcltk, beepr, cli, crayon, pbmcapply, progress,\npurrr, foreach, plyr, doFuture, future, future.apply, furrr,\nRPushbullet, rstudioapi, shiny, commonmark, base64enc, tools
#> promises                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           future (>= 1.21.0), knitr, purrr, rmarkdown, spelling,\ntestthat, vembedr
#> ps                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          callr, covr, curl, pillar, pingr, processx (>= 3.1.0), R6,\nrlang, testthat (>= 3.0.0), webfakes
#> purrr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    covr, dplyr (>= 0.7.8), httr, knitr, lubridate, rmarkdown,\ntestthat (>= 3.0.0), tibble, tidyselect
#> quantreg                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       interp, rgl, logspline, nor1mix, Formula, zoo, R.rsp, conquer
#> R6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            testthat, pryr
#> ragg                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               covr, graphics, grid, testthat (>= 3.0.0)
#> rappdirs                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          roxygen2, testthat (>= 3.0.0), covr, withr
#> rcmdcheck                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        covr, knitr, mockery, processx, ps, rmarkdown, svglite,\ntestthat, webfakes
#> RColorBrewer                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
#> Rcpp                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      tinytest, inline, rbenchmark, pkgKitten (>= 0.1.2)
#> RcppArmadillo                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       tinytest, Matrix (>= 1.3.0), pkgKitten, reticulate, slam
#> RcppEigen                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Matrix, inline, tinytest, pkgKitten, microbenchmark
#> readbitmap                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  pixmap, testthat
#> rematch2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      covr, testthat
#> remotes                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      brew, callr, codetools, covr, curl, git2r (>= 0.23.0), knitr,\nmockery, pingr, pkgbuild (>= 1.0.1), rmarkdown, rprojroot,\ntestthat (>= 3.0.0), webfakes, withr
#> rex                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             covr, dplyr, ggplot2, Hmisc, knitr, magrittr, rmarkdown,\nroxygen2, rvest, stringr, testthat
#> rlang                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            cli (>= 3.1.0), covr, crayon, fs, glue, knitr, magrittr,\nmethods, pillar, rmarkdown, stats, testthat (>= 3.0.0), tibble,\nusethis, vctrs (>= 0.2.3), withr
#> rmarkdown                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   digest, dygraphs, fs, rsconnect, downlit (>= 0.4.0), katex\n(>= 1.4.0), sass (>= 0.4.0), shiny (>= 1.6.0), testthat (>=\n3.0.3), tibble, vctrs, cleanrmd, withr (>= 2.4.2), xml2
#> roxygen2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            covr, R.methodsS3, R.oo, rmarkdown (>= 2.16), testthat (>=\n3.1.2), yaml
#> rprojroot                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       covr, knitr, lifecycle, mockr, rlang, rmarkdown, testthat (>=\n3.0.0), withr
#> rstudioapi                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           testthat, knitr, rmarkdown, clipr, covr
#> rversions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            covr, mockery, testthat
#> sass                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          testthat, knitr, rmarkdown, withr, shiny, curl
#> scales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        bit64, covr, dichromat, ggplot2, hms (>= 0.5.0), stringi,\ntestthat (>= 3.0.0)
#> sessioninfo                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     callr, covr, mockery, reticulate, rmarkdown, testthat, withr
#> shape                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <NA>
#> shapeR                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> shiny                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       datasets, DT, Cairo (>= 1.5-5), testthat (>= 3.0.0), knitr\n(>= 1.6), markdown, rmarkdown, ggplot2, reactlog (>= 1.0.0),\nmagrittr, yaml, future, dygraphs, ragg, showtext, sass
#> slam                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <NA>
#> sourcetools                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         testthat
#> sp                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         RColorBrewer, gstat, deldir, knitr, rmarkdown, sf, terra,\nraster
#> SparseM                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                knitr
#> SQUAREM                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               setRNG
#> stringi                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
#> stringr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              covr, dplyr, gt, htmltools, htmlwidgets, knitr, rmarkdown,\ntestthat (>= 3.0.0), tibble
#> sys                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        unix (>= 1.4), spelling, testthat
#> systemfonts                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               covr, knitr, rmarkdown, testthat (>= 2.1.0), tools
#> testthat                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 covr, curl (>= 0.9.5), diffviewer (>= 0.1.0), knitr,\nrmarkdown, rstudioapi, shiny, usethis, vctrs (>= 0.1.0), xml2
#> textshaping                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           covr, knitr, rmarkdown
#> tibble                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    bench, bit64, blob, brio, callr, cli, covr, crayon (>=\n1.3.4), DiagrammeR, dplyr, evaluate, formattable, ggplot2,\nhere, hms, htmltools, knitr, lubridate, mockr, nycflights13,\npkgbuild, pkgload, purrr, rmarkdown, stringi, testthat (>=\n3.0.2), tidyr, withr
#> tidyr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                covr, data.table, knitr, readr, repurrrsive (>= 1.1.0),\nrmarkdown, testthat (>= 3.0.0)
#> tidyselect                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 covr, crayon, dplyr, knitr, magrittr, rmarkdown, stringr,\ntestthat (>= 3.1.1), tibble (>= 2.1.3)
#> tiff                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <NA>
#> tinytex                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   testit, rstudioapi
#> tm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     antiword, filehash, methods, pdftools, Rcampdf, Rgraphviz,\nRpoppler, SnowballC, testthat, tm.lexicon.GeneralInquirer
#> urlchecker                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              covr
#> usethis                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               covr, knitr, magick, pkgload (>= 1.3.2.1), rmarkdown,\nroxygen2 (>= 7.1.2), spelling (>= 1.2), styler (>= 1.2.0),\ntestthat (>= 3.1.8)
#> utf8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         cli, covr, knitr, rlang, rmarkdown, testthat (>= 3.0.0),\nwithr
#> vctrs                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            bit64, covr, crayon, dplyr (>= 0.8.5), generics, knitr,\npillar (>= 1.4.4), pkgdown (>= 2.0.1), rmarkdown, testthat (>=\n3.0.0), tibble (>= 3.1.3), waldo (>= 0.2.0), withr, xml2,\nzeallot
#> vegan                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       parallel, tcltk, knitr, markdown
#> viridisLite                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           hexbin (>= 1.27.0), ggplot2 (>= 1.0.1), testthat, covr
#> waldo                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             covr, R6, testthat (>= 3.0.0), withr, xml2
#> wavethresh                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
#> whisker                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             markdown
#> withr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  callr, DBI, knitr, methods, rlang, rmarkdown (>= 2.12),\nRSQLite, testthat (>= 3.0.0)
#> xfun                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                testit, parallel, codetools, methods, rstudioapi, tinytex (>=\n0.30), mime, markdown (>= 1.5), knitr (>= 1.47), htmltools,\nremotes, pak, rhub, renv, curl, xml2, jsonlite, magick, yaml,\nqs, rmarkdown
#> xml2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             covr, curl, httr, knitr, magrittr, mockery, rmarkdown,\ntestthat (>= 3.0.0)
#> xopen                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ps, testthat (>= 3.0.0)
#> xtable                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             knitr, plm, zoo, survival
#> yaml                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   RUnit
#> zip                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      covr, processx, R6, testthat, withr
#> zoo                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  AER, coda, chron, ggplot2 (>= 3.0.0), mondate, scales,\nstinepack, strucchange, timeDate, timeSeries, tis, tseries, xts
#> base                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 methods
#> boot                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          MASS, survival
#> class                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <NA>
#> cluster                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         MASS, Matrix
#> codetools                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
#> compiler                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
#> datasets                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
#> foreign                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
#> graphics                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
#> grDevices                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         KernSmooth
#> grid                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <NA>
#> KernSmooth                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     MASS, carData
#> lattice                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           KernSmooth, MASS, latticeExtra, colorspace
#> MASS                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           lattice, nlme, nnet, survival
#> Matrix                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        MASS, datasets, sfsmisc, tools
#> methods                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            codetools
#> mgcv                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                parallel, survival, MASS
#> nlme                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Hmisc, MASS, SASmixed
#> nnet                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    MASS
#> parallel                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             methods
#> rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               survival
#> spatial                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 MASS
#> splines                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Matrix, methods
#> stats                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               MASS, Matrix, SuppDists, methods, stats4
#> stats4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#> survival                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
#> tcltk                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <NA>
#> tools                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 codetools, methods, xml2, curl, commonmark, knitr, xfun,\nmathjaxr, V8
#> utils                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             methods, xml2, commonmark, knitr, jsonlite
#>                                                             Enhances
#> libminer                                                        <NA>
#> abind                                                           <NA>
#> ade4                                                            <NA>
#> askpass                                                         <NA>
#> backports                                                       <NA>
#> base64enc                                                        png
#> BH                                                              <NA>
#> bmp                                                             <NA>
#> box                                                       rstudioapi
#> brew                                                            <NA>
#> brio                                                            <NA>
#> broom                                                           <NA>
#> bslib                                                           <NA>
#> cachem                                                          <NA>
#> callr                                                           <NA>
#> car                                                             <NA>
#> carData                                                         <NA>
#> cli                                                             <NA>
#> clipr                                                           <NA>
#> colorspace                                                      <NA>
#> commonmark                                                      <NA>
#> covr                                                            <NA>
#> cowplot                                                         <NA>
#> cpp11                                                           <NA>
#> crayon                                                          <NA>
#> credentials                                                     <NA>
#> crosstalk                                                       <NA>
#> curl                                                            <NA>
#> data.table                                                      <NA>
#> Deriv                                                           <NA>
#> desc                                                            <NA>
#> devtools                                                        <NA>
#> diagram                                                         <NA>
#> diffobj                                                         <NA>
#> digest                                                          <NA>
#> doBy                                                            <NA>
#> downlit                                                         <NA>
#> downloader                                                      <NA>
#> dplyr                                                           <NA>
#> DT                                                              <NA>
#> dunn.test                                                       <NA>
#> ellipsis                                                        <NA>
#> evaluate                                                        <NA>
#> fansi                                                           <NA>
#> farver                                                          <NA>
#> fastmap                                                         <NA>
#> fontawesome                                                     <NA>
#> fs                                                              <NA>
#> FSA                                                             <NA>
#> future                                                          <NA>
#> future.apply                                                    <NA>
#> gclus                                                           <NA>
#> generics                                                        <NA>
#> gert                                                            <NA>
#> ggbiplot                                                        <NA>
#> ggplot2                                                           sp
#> gh                                                              <NA>
#> gitcreds                                                        <NA>
#> globals                                                         <NA>
#> glue                                                            <NA>
#> gridExtra                                                       <NA>
#> gtable                                                          <NA>
#> highr                                                           <NA>
#> htmltools                                                      knitr
#> htmlwidgets                                           shiny (>= 1.1)
#> httpuv                                                          <NA>
#> httr                                                            <NA>
#> httr2                                                           <NA>
#> igraph                                                          <NA>
#> imager                                                          <NA>
#> ini                                                             <NA>
#> ipred                                                           <NA>
#> isoband                                                         <NA>
#> jpeg                                                            <NA>
#> jquerylib                                                       <NA>
#> jsonlite                                                        <NA>
#> knitr                                                           <NA>
#> labeling                                                        <NA>
#> later                                                           <NA>
#> lava                                                            <NA>
#> lazyeval                                                        <NA>
#> libminer.1                                                      <NA>
#> lifecycle                                                       <NA>
#> listenv                                                         <NA>
#> lme4                                                            <NA>
#> lmtest                                                          <NA>
#> magick                                                          <NA>
#> magrittr                                                        <NA>
#> MatrixModels                                                    <NA>
#> memoise                                                         <NA>
#> microbenchmark                                                  <NA>
#> mime                                                            <NA>
#> miniUI                                                          <NA>
#> minqa                                                           <NA>
#> modelr                                                          <NA>
#> munsell                                                         <NA>
#> nloptr                                                          <NA>
#> NLP                                         udpipe, spacyr, cleanNLP
#> numDeriv                                                        <NA>
#> OpenImageR                                                      <NA>
#> openssl                                                         <NA>
#> parallelly                                                      <NA>
#> pbkrtest                                                        <NA>
#> permute                                                         <NA>
#> pillar                                                          <NA>
#> pixmap                                                          <NA>
#> pkgbuild                                                        <NA>
#> pkgconfig                                                       <NA>
#> pkgdown                                                         <NA>
#> pkgload                                                         <NA>
#> plotrix                                                         <NA>
#> png                                                             <NA>
#> praise                                                          <NA>
#> prettyunits                                                     <NA>
#> processx                                                        <NA>
#> prodlim                                                         <NA>
#> profvis                                                         <NA>
#> progressr                                                       <NA>
#> promises                                                        <NA>
#> ps                                                              <NA>
#> purrr                                                           <NA>
#> quantreg                                                        <NA>
#> R6                                                              <NA>
#> ragg                                                            <NA>
#> rappdirs                                                        <NA>
#> rcmdcheck                                                       <NA>
#> RColorBrewer                                                    <NA>
#> Rcpp                                                            <NA>
#> RcppArmadillo                                                   <NA>
#> RcppEigen                                                       <NA>
#> readbitmap                                                      <NA>
#> rematch2                                                        <NA>
#> remotes                                                         <NA>
#> rex                                                             <NA>
#> rlang                                                          winch
#> rmarkdown                                                       <NA>
#> roxygen2                                                        <NA>
#> rprojroot                                                       <NA>
#> rstudioapi                                                      <NA>
#> rversions                                                       <NA>
#> sass                                                            <NA>
#> scales                                                          <NA>
#> sessioninfo                                                     <NA>
#> shape                                                           <NA>
#> shapeR                                                          <NA>
#> shiny                                                           <NA>
#> slam                                           Matrix, SparseM, spam
#> sourcetools                                                     <NA>
#> sp                                                              <NA>
#> SparseM                                                         <NA>
#> SQUAREM                                                         <NA>
#> stringi                                                         <NA>
#> stringr                                                         <NA>
#> sys                                                             <NA>
#> systemfonts                                                     <NA>
#> testthat                                                        <NA>
#> textshaping                                                     <NA>
#> tibble                                                          <NA>
#> tidyr                                                           <NA>
#> tidyselect                                                      <NA>
#> tiff                                                            <NA>
#> tinytex                                                         <NA>
#> tm                                                              <NA>
#> urlchecker                                                      <NA>
#> usethis                                                         <NA>
#> utf8                                                            <NA>
#> vctrs                                                           <NA>
#> vegan                                                           <NA>
#> viridisLite                                                     <NA>
#> waldo                                                           <NA>
#> wavethresh                                                      <NA>
#> whisker                                                         <NA>
#> withr                                                           <NA>
#> xfun                                                            <NA>
#> xml2                                                            <NA>
#> xopen                                                           <NA>
#> xtable                                                          <NA>
#> yaml                                                            <NA>
#> zip                                                             <NA>
#> zoo                                                             <NA>
#> base                                              chron, date, round
#> boot                                                            <NA>
#> class                                                           <NA>
#> cluster                             mvoutlier, fpc, ellipse, sfsmisc
#> codetools                                                       <NA>
#> compiler                                                        <NA>
#> datasets                                                        <NA>
#> foreign                                                         <NA>
#> graphics                                                         vcd
#> grDevices                                                       <NA>
#> grid                                                            <NA>
#> KernSmooth                                                      <NA>
#> lattice                                                   chron, zoo
#> MASS                                                            <NA>
#> Matrix                                                SparseM, graph
#> methods                                                         <NA>
#> mgcv                                                            <NA>
#> nlme                                                            <NA>
#> nnet                                                            <NA>
#> parallel                                                  snow, Rmpi
#> rpart                                                           <NA>
#> spatial                                                         <NA>
#> splines                                                         <NA>
#> stats          Kendall, coin, multcomp, pcaPP, pspearman, robustbase
#> stats4                                                          <NA>
#> survival                                                        <NA>
#> tcltk                                                           <NA>
#> tools                                                           <NA>
#> utils                                                           <NA>
#>                                               License License_is_FOSS
#> libminer                           MIT + file LICENSE            <NA>
#> abind                              MIT + file LICENSE            <NA>
#> ade4                                       GPL (>= 2)            <NA>
#> askpass                            MIT + file LICENSE            <NA>
#> backports                               GPL-2 | GPL-3            <NA>
#> base64enc                               GPL-2 | GPL-3            <NA>
#> BH                                            BSL-1.0            <NA>
#> bmp                                        GPL (>= 2)            <NA>
#> box                                MIT + file LICENSE            <NA>
#> brew                                       GPL (>= 2)            <NA>
#> brio                               MIT + file LICENSE            <NA>
#> broom                              MIT + file LICENSE            <NA>
#> bslib                              MIT + file LICENSE            <NA>
#> cachem                             MIT + file LICENSE            <NA>
#> callr                              MIT + file LICENSE            <NA>
#> car                                        GPL (>= 2)            <NA>
#> carData                                    GPL (>= 2)            <NA>
#> cli                                MIT + file LICENSE            <NA>
#> clipr                                           GPL-3            <NA>
#> colorspace                BSD_3_clause + file LICENSE            <NA>
#> commonmark                BSD_2_clause + file LICENSE            <NA>
#> covr                               MIT + file LICENSE            <NA>
#> cowplot                                         GPL-2            <NA>
#> cpp11                              MIT + file LICENSE            <NA>
#> crayon                             MIT + file LICENSE            <NA>
#> credentials                        MIT + file LICENSE            <NA>
#> crosstalk                          MIT + file LICENSE            <NA>
#> curl                               MIT + file LICENSE            <NA>
#> data.table                     MPL-2.0 | file LICENSE            <NA>
#> Deriv                                      GPL (>= 3)            <NA>
#> desc                               MIT + file LICENSE            <NA>
#> devtools                           MIT + file LICENSE            <NA>
#> diagram                                    GPL (>= 2)            <NA>
#> diffobj                                 GPL-2 | GPL-3            <NA>
#> digest                                     GPL (>= 2)            <NA>
#> doBy                                       GPL (>= 2)            <NA>
#> downlit                            MIT + file LICENSE            <NA>
#> downloader                                      GPL-2            <NA>
#> dplyr                              MIT + file LICENSE            <NA>
#> DT                               GPL-3 | file LICENSE            <NA>
#> dunn.test                                       GPL-2            <NA>
#> ellipsis                           MIT + file LICENSE            <NA>
#> evaluate                           MIT + file LICENSE            <NA>
#> fansi                                   GPL-2 | GPL-3            <NA>
#> farver                             MIT + file LICENSE            <NA>
#> fastmap                            MIT + file LICENSE            <NA>
#> fontawesome                        MIT + file LICENSE            <NA>
#> fs                                 MIT + file LICENSE            <NA>
#> FSA                                        GPL (>= 2)            <NA>
#> future                                  LGPL (>= 2.1)            <NA>
#> future.apply                               GPL (>= 2)            <NA>
#> gclus                                      GPL (>= 2)            <NA>
#> generics                           MIT + file LICENSE            <NA>
#> gert                               MIT + file LICENSE            <NA>
#> ggbiplot                                        GPL-2            <NA>
#> ggplot2                            MIT + file LICENSE            <NA>
#> gh                                 MIT + file LICENSE            <NA>
#> gitcreds                           MIT + file LICENSE            <NA>
#> globals                                 LGPL (>= 2.1)            <NA>
#> glue                               MIT + file LICENSE            <NA>
#> gridExtra                                  GPL (>= 2)            <NA>
#> gtable                             MIT + file LICENSE            <NA>
#> highr                                             GPL            <NA>
#> htmltools                                  GPL (>= 2)            <NA>
#> htmlwidgets                        MIT + file LICENSE            <NA>
#> httpuv                      GPL (>= 2) | file LICENSE            <NA>
#> httr                               MIT + file LICENSE            <NA>
#> httr2                              MIT + file LICENSE            <NA>
#> igraph                                     GPL (>= 2)            <NA>
#> imager                                         LGPL-3            <NA>
#> ini                                             GPL-3            <NA>
#> ipred                                      GPL (>= 2)            <NA>
#> isoband                            MIT + file LICENSE            <NA>
#> jpeg                                    GPL-2 | GPL-3            <NA>
#> jquerylib                          MIT + file LICENSE            <NA>
#> jsonlite                           MIT + file LICENSE            <NA>
#> knitr                                             GPL            <NA>
#> labeling               MIT + file LICENSE | Unlimited            <NA>
#> later                              MIT + file LICENSE            <NA>
#> lava                                            GPL-3            <NA>
#> lazyeval                                        GPL-3            <NA>
#> libminer.1                         MIT + file LICENSE            <NA>
#> lifecycle                          MIT + file LICENSE            <NA>
#> listenv                                 LGPL (>= 2.1)            <NA>
#> lme4                                       GPL (>= 2)            <NA>
#> lmtest                                  GPL-2 | GPL-3            <NA>
#> magick                             MIT + file LICENSE            <NA>
#> magrittr                           MIT + file LICENSE            <NA>
#> MatrixModels                               GPL (>= 2)            <NA>
#> memoise                            MIT + file LICENSE            <NA>
#> microbenchmark            BSD_2_clause + file LICENSE            <NA>
#> mime                                              GPL            <NA>
#> miniUI                                          GPL-3            <NA>
#> minqa                                           GPL-2            <NA>
#> modelr                                          GPL-3            <NA>
#> munsell                            MIT + file LICENSE            <NA>
#> nloptr                                    LGPL (>= 3)            <NA>
#> NLP                                             GPL-3            <NA>
#> numDeriv                                        GPL-2            <NA>
#> OpenImageR                                      GPL-3            <NA>
#> openssl                            MIT + file LICENSE            <NA>
#> parallelly                              LGPL (>= 2.1)            <NA>
#> pbkrtest                                   GPL (>= 2)            <NA>
#> permute                                         GPL-2            <NA>
#> pillar                             MIT + file LICENSE            <NA>
#> pixmap                                          GPL-2            <NA>
#> pkgbuild                           MIT + file LICENSE            <NA>
#> pkgconfig                          MIT + file LICENSE            <NA>
#> pkgdown                            MIT + file LICENSE            <NA>
#> pkgload                                         GPL-3            <NA>
#> plotrix                                    GPL (>= 2)            <NA>
#> png                                     GPL-2 | GPL-3            <NA>
#> praise                             MIT + file LICENSE            <NA>
#> prettyunits                        MIT + file LICENSE            <NA>
#> processx                           MIT + file LICENSE            <NA>
#> prodlim                                    GPL (>= 2)            <NA>
#> profvis                          GPL-3 | file LICENSE            <NA>
#> progressr                                  GPL (>= 3)            <NA>
#> promises                           MIT + file LICENSE            <NA>
#> ps                                 MIT + file LICENSE            <NA>
#> purrr                              MIT + file LICENSE            <NA>
#> quantreg                                   GPL (>= 2)            <NA>
#> R6                                 MIT + file LICENSE            <NA>
#> ragg                               MIT + file LICENSE            <NA>
#> rappdirs                           MIT + file LICENSE            <NA>
#> rcmdcheck                          MIT + file LICENSE            <NA>
#> RColorBrewer                       Apache License 2.0            <NA>
#> Rcpp                                       GPL (>= 2)            <NA>
#> RcppArmadillo                              GPL (>= 2)            <NA>
#> RcppEigen                   GPL (>= 2) | file LICENSE            <NA>
#> readbitmap                                 GPL (>= 2)            <NA>
#> rematch2                           MIT + file LICENSE            <NA>
#> remotes                            MIT + file LICENSE            <NA>
#> rex                                MIT + file LICENSE            <NA>
#> rlang                              MIT + file LICENSE            <NA>
#> rmarkdown                                       GPL-3            <NA>
#> roxygen2                           MIT + file LICENSE            <NA>
#> rprojroot                          MIT + file LICENSE            <NA>
#> rstudioapi                         MIT + file LICENSE            <NA>
#> rversions                          MIT + file LICENSE            <NA>
#> sass                               MIT + file LICENSE            <NA>
#> scales                             MIT + file LICENSE            <NA>
#> sessioninfo                                     GPL-2            <NA>
#> shape                                      GPL (>= 3)            <NA>
#> shapeR                                     GPL (>= 2)            <NA>
#> shiny                            GPL-3 | file LICENSE            <NA>
#> slam                                            GPL-2            <NA>
#> sourcetools                        MIT + file LICENSE            <NA>
#> sp                                         GPL (>= 2)            <NA>
#> SparseM                                    GPL (>= 2)            <NA>
#> SQUAREM                                    GPL (>= 2)            <NA>
#> stringi                                  file LICENSE             yes
#> stringr                            MIT + file LICENSE            <NA>
#> sys                                MIT + file LICENSE            <NA>
#> systemfonts                        MIT + file LICENSE            <NA>
#> testthat                           MIT + file LICENSE            <NA>
#> textshaping                        MIT + file LICENSE            <NA>
#> tibble                             MIT + file LICENSE            <NA>
#> tidyr                              MIT + file LICENSE            <NA>
#> tidyselect                         MIT + file LICENSE            <NA>
#> tiff                                    GPL-2 | GPL-3            <NA>
#> tinytex                            MIT + file LICENSE            <NA>
#> tm                                              GPL-3            <NA>
#> urlchecker                                      GPL-3            <NA>
#> usethis                            MIT + file LICENSE            <NA>
#> utf8           Apache License (== 2.0) | file LICENSE            <NA>
#> vctrs                              MIT + file LICENSE            <NA>
#> vegan                                           GPL-2            <NA>
#> viridisLite                        MIT + file LICENSE            <NA>
#> waldo                              MIT + file LICENSE            <NA>
#> wavethresh                                 GPL (>= 2)            <NA>
#> whisker                                         GPL-3            <NA>
#> withr                              MIT + file LICENSE            <NA>
#> xfun                               MIT + file LICENSE            <NA>
#> xml2                               MIT + file LICENSE            <NA>
#> xopen                              MIT + file LICENSE            <NA>
#> xtable                                     GPL (>= 2)            <NA>
#> yaml                      BSD_3_clause + file LICENSE            <NA>
#> zip                                MIT + file LICENSE            <NA>
#> zoo                                     GPL-2 | GPL-3            <NA>
#> base                                  Part of R 4.4.1            <NA>
#> boot                                        Unlimited            <NA>
#> class                                   GPL-2 | GPL-3            <NA>
#> cluster                                    GPL (>= 2)            <NA>
#> codetools                                         GPL            <NA>
#> compiler                              Part of R 4.4.1            <NA>
#> datasets                              Part of R 4.4.1            <NA>
#> foreign                                    GPL (>= 2)            <NA>
#> graphics                              Part of R 4.4.1            <NA>
#> grDevices                             Part of R 4.4.1            <NA>
#> grid                                  Part of R 4.4.1            <NA>
#> KernSmooth                                  Unlimited            <NA>
#> lattice                                    GPL (>= 2)            <NA>
#> MASS                                    GPL-2 | GPL-3            <NA>
#> Matrix                      GPL (>= 2) | file LICENCE            <NA>
#> methods                               Part of R 4.4.1            <NA>
#> mgcv                                       GPL (>= 2)            <NA>
#> nlme                                       GPL (>= 2)            <NA>
#> nnet                                    GPL-2 | GPL-3            <NA>
#> parallel                              Part of R 4.4.1            <NA>
#> rpart                                   GPL-2 | GPL-3            <NA>
#> spatial                                 GPL-2 | GPL-3            <NA>
#> splines                               Part of R 4.4.1            <NA>
#> stats                                 Part of R 4.4.1            <NA>
#> stats4                                Part of R 4.4.1            <NA>
#> survival                                  LGPL (>= 2)            <NA>
#> tcltk                                 Part of R 4.4.1            <NA>
#> tools                                 Part of R 4.4.1            <NA>
#> utils                                 Part of R 4.4.1            <NA>
#>                License_restricts_use OS_type MD5sum NeedsCompilation Built
#> libminer                        <NA>    <NA>   <NA>             <NA> 4.4.1
#> abind                           <NA>    <NA>   <NA>               no 4.4.1
#> ade4                            <NA>    <NA>   <NA>              yes 4.4.1
#> askpass                         <NA>    <NA>   <NA>              yes 4.4.1
#> backports                       <NA>    <NA>   <NA>              yes 4.4.0
#> base64enc                       <NA>    <NA>   <NA>              yes 4.4.0
#> BH                              <NA>    <NA>   <NA>               no 4.4.0
#> bmp                             <NA>    <NA>   <NA>               no 4.4.1
#> box                             <NA>    <NA>   <NA>              yes 4.4.1
#> brew                            <NA>    <NA>   <NA>               no 4.4.1
#> brio                            <NA>    <NA>   <NA>              yes 4.4.1
#> broom                           <NA>    <NA>   <NA>               no 4.4.1
#> bslib                           <NA>    <NA>   <NA>               no 4.4.1
#> cachem                          <NA>    <NA>   <NA>              yes 4.4.1
#> callr                           <NA>    <NA>   <NA>               no 4.4.1
#> car                             <NA>    <NA>   <NA>               no 4.4.1
#> carData                         <NA>    <NA>   <NA>               no 4.4.1
#> cli                             <NA>    <NA>   <NA>              yes 4.4.1
#> clipr                           <NA>    <NA>   <NA>               no 4.4.1
#> colorspace                      <NA>    <NA>   <NA>              yes 4.4.1
#> commonmark                      <NA>    <NA>   <NA>              yes 4.4.1
#> covr                            <NA>    <NA>   <NA>              yes 4.4.1
#> cowplot                         <NA>    <NA>   <NA>               no 4.4.1
#> cpp11                           <NA>    <NA>   <NA>               no 4.4.1
#> crayon                          <NA>    <NA>   <NA>               no 4.4.1
#> credentials                     <NA>    <NA>   <NA>               no 4.4.1
#> crosstalk                       <NA>    <NA>   <NA>               no 4.4.1
#> curl                            <NA>    <NA>   <NA>              yes 4.4.1
#> data.table                      <NA>    <NA>   <NA>              yes 4.4.1
#> Deriv                           <NA>    <NA>   <NA>               no 4.4.1
#> desc                            <NA>    <NA>   <NA>               no 4.4.1
#> devtools                        <NA>    <NA>   <NA>               no 4.4.1
#> diagram                         <NA>    <NA>   <NA>               no 4.4.0
#> diffobj                         <NA>    <NA>   <NA>              yes 4.4.1
#> digest                          <NA>    <NA>   <NA>              yes 4.4.1
#> doBy                            <NA>    <NA>   <NA>               no 4.4.1
#> downlit                         <NA>    <NA>   <NA>               no 4.4.1
#> downloader                      <NA>    <NA>   <NA>               no 4.4.1
#> dplyr                           <NA>    <NA>   <NA>              yes 4.4.1
#> DT                              <NA>    <NA>   <NA>               no 4.4.1
#> dunn.test                       <NA>    <NA>   <NA>               no 4.4.0
#> ellipsis                        <NA>    <NA>   <NA>              yes 4.4.1
#> evaluate                        <NA>    <NA>   <NA>               no 4.4.1
#> fansi                           <NA>    <NA>   <NA>              yes 4.4.1
#> farver                          <NA>    <NA>   <NA>              yes 4.4.1
#> fastmap                         <NA>    <NA>   <NA>              yes 4.4.1
#> fontawesome                     <NA>    <NA>   <NA>               no 4.4.1
#> fs                              <NA>    <NA>   <NA>              yes 4.4.1
#> FSA                             <NA>    <NA>   <NA>               no 4.4.1
#> future                          <NA>    <NA>   <NA>               no 4.4.1
#> future.apply                    <NA>    <NA>   <NA>               no 4.4.1
#> gclus                           <NA>    <NA>   <NA>               no 4.4.1
#> generics                        <NA>    <NA>   <NA>               no 4.4.1
#> gert                            <NA>    <NA>   <NA>              yes 4.4.1
#> ggbiplot                        <NA>    <NA>   <NA>               no 4.4.1
#> ggplot2                         <NA>    <NA>   <NA>               no 4.4.1
#> gh                              <NA>    <NA>   <NA>               no 4.4.1
#> gitcreds                        <NA>    <NA>   <NA>               no 4.4.1
#> globals                         <NA>    <NA>   <NA>               no 4.4.0
#> glue                            <NA>    <NA>   <NA>              yes 4.4.1
#> gridExtra                       <NA>    <NA>   <NA>               no 4.4.1
#> gtable                          <NA>    <NA>   <NA>               no 4.4.1
#> highr                           <NA>    <NA>   <NA>               no 4.4.1
#> htmltools                       <NA>    <NA>   <NA>              yes 4.4.1
#> htmlwidgets                     <NA>    <NA>   <NA>               no 4.4.1
#> httpuv                          <NA>    <NA>   <NA>              yes 4.4.1
#> httr                            <NA>    <NA>   <NA>               no 4.4.1
#> httr2                           <NA>    <NA>   <NA>               no 4.4.1
#> igraph                          <NA>    <NA>   <NA>              yes 4.4.1
#> imager                          <NA>    <NA>   <NA>              yes 4.4.1
#> ini                             <NA>    <NA>   <NA>               no 4.4.1
#> ipred                           <NA>    <NA>   <NA>              yes 4.4.1
#> isoband                         <NA>    <NA>   <NA>              yes 4.4.1
#> jpeg                            <NA>    <NA>   <NA>              yes 4.4.0
#> jquerylib                       <NA>    <NA>   <NA>               no 4.4.1
#> jsonlite                        <NA>    <NA>   <NA>              yes 4.4.1
#> knitr                           <NA>    <NA>   <NA>               no 4.4.1
#> labeling                        <NA>    <NA>   <NA>               no 4.4.0
#> later                           <NA>    <NA>   <NA>              yes 4.4.1
#> lava                            <NA>    <NA>   <NA>               no 4.4.1
#> lazyeval                        <NA>    <NA>   <NA>              yes 4.4.1
#> libminer.1                      <NA>    <NA>   <NA>               no 4.4.1
#> lifecycle                       <NA>    <NA>   <NA>               no 4.4.1
#> listenv                         <NA>    <NA>   <NA>               no 4.4.1
#> lme4                            <NA>    <NA>   <NA>              yes 4.4.1
#> lmtest                          <NA>    <NA>   <NA>              yes 4.4.1
#> magick                          <NA>    <NA>   <NA>              yes 4.4.1
#> magrittr                        <NA>    <NA>   <NA>              yes 4.4.1
#> MatrixModels                    <NA>    <NA>   <NA>               no 4.4.1
#> memoise                         <NA>    <NA>   <NA>               no 4.4.1
#> microbenchmark                  <NA>    <NA>   <NA>              yes 4.4.1
#> mime                            <NA>    <NA>   <NA>              yes 4.4.0
#> miniUI                          <NA>    <NA>   <NA>               no 4.4.1
#> minqa                           <NA>    <NA>   <NA>              yes 4.4.1
#> modelr                          <NA>    <NA>   <NA>               no 4.4.1
#> munsell                         <NA>    <NA>   <NA>               no 4.4.1
#> nloptr                          <NA>    <NA>   <NA>              yes 4.4.1
#> NLP                             <NA>    <NA>   <NA>               no 4.4.1
#> numDeriv                        <NA>    <NA>   <NA>               no 4.4.0
#> OpenImageR                      <NA>    <NA>   <NA>              yes 4.4.1
#> openssl                         <NA>    <NA>   <NA>              yes 4.4.1
#> parallelly                      <NA>    <NA>   <NA>              yes 4.4.1
#> pbkrtest                        <NA>    <NA>   <NA>               no 4.4.1
#> permute                         <NA>    <NA>   <NA>               no 4.4.1
#> pillar                          <NA>    <NA>   <NA>               no 4.4.1
#> pixmap                          <NA>    <NA>   <NA>               no 4.4.0
#> pkgbuild                        <NA>    <NA>   <NA>               no 4.4.1
#> pkgconfig                       <NA>    <NA>   <NA>               no 4.4.1
#> pkgdown                         <NA>    <NA>   <NA>               no 4.4.1
#> pkgload                         <NA>    <NA>   <NA>               no 4.4.1
#> plotrix                         <NA>    <NA>   <NA>               no 4.4.0
#> png                             <NA>    <NA>   <NA>              yes 4.4.0
#> praise                          <NA>    <NA>   <NA>               no 4.4.1
#> prettyunits                     <NA>    <NA>   <NA>               no 4.4.1
#> processx                        <NA>    <NA>   <NA>              yes 4.4.1
#> prodlim                         <NA>    <NA>   <NA>              yes 4.4.1
#> profvis                         <NA>    <NA>   <NA>              yes 4.4.1
#> progressr                       <NA>    <NA>   <NA>               no 4.4.1
#> promises                        <NA>    <NA>   <NA>              yes 4.4.1
#> ps                              <NA>    <NA>   <NA>              yes 4.4.1
#> purrr                           <NA>    <NA>   <NA>              yes 4.4.1
#> quantreg                        <NA>    <NA>   <NA>              yes 4.4.1
#> R6                              <NA>    <NA>   <NA>               no 4.4.1
#> ragg                            <NA>    <NA>   <NA>              yes 4.4.1
#> rappdirs                        <NA>    <NA>   <NA>              yes 4.4.1
#> rcmdcheck                       <NA>    <NA>   <NA>               no 4.4.1
#> RColorBrewer                    <NA>    <NA>   <NA>               no 4.4.0
#> Rcpp                            <NA>    <NA>   <NA>              yes 4.4.1
#> RcppArmadillo                   <NA>    <NA>   <NA>              yes 4.4.1
#> RcppEigen                       <NA>    <NA>   <NA>              yes 4.4.1
#> readbitmap                      <NA>    <NA>   <NA>               no 4.4.1
#> rematch2                        <NA>    <NA>   <NA>               no 4.4.1
#> remotes                         <NA>    <NA>   <NA>               no 4.4.1
#> rex                             <NA>    <NA>   <NA>               no 4.4.1
#> rlang                           <NA>    <NA>   <NA>              yes 4.4.1
#> rmarkdown                       <NA>    <NA>   <NA>               no 4.4.1
#> roxygen2                        <NA>    <NA>   <NA>              yes 4.4.1
#> rprojroot                       <NA>    <NA>   <NA>               no 4.4.1
#> rstudioapi                      <NA>    <NA>   <NA>               no 4.4.1
#> rversions                       <NA>    <NA>   <NA>               no 4.4.1
#> sass                            <NA>    <NA>   <NA>              yes 4.4.1
#> scales                          <NA>    <NA>   <NA>              yes 4.4.1
#> sessioninfo                     <NA>    <NA>   <NA>               no 4.4.1
#> shape                           <NA>    <NA>   <NA>               no 4.4.0
#> shapeR                          <NA>    <NA>   <NA>               no 4.4.1
#> shiny                           <NA>    <NA>   <NA>               no 4.4.1
#> slam                            <NA>    <NA>   <NA>              yes 4.4.1
#> sourcetools                     <NA>    <NA>   <NA>              yes 4.4.1
#> sp                              <NA>    <NA>   <NA>              yes 4.4.1
#> SparseM                         <NA>    <NA>   <NA>              yes 4.4.1
#> SQUAREM                         <NA>    <NA>   <NA>               no 4.4.0
#> stringi                         <NA>    <NA>   <NA>              yes 4.4.0
#> stringr                         <NA>    <NA>   <NA>               no 4.4.1
#> sys                             <NA>    <NA>   <NA>              yes 4.4.1
#> systemfonts                     <NA>    <NA>   <NA>              yes 4.4.1
#> testthat                        <NA>    <NA>   <NA>              yes 4.4.1
#> textshaping                     <NA>    <NA>   <NA>              yes 4.4.1
#> tibble                          <NA>    <NA>   <NA>              yes 4.4.1
#> tidyr                           <NA>    <NA>   <NA>              yes 4.4.1
#> tidyselect                      <NA>    <NA>   <NA>              yes 4.4.1
#> tiff                            <NA>    <NA>   <NA>              yes 4.4.0
#> tinytex                         <NA>    <NA>   <NA>               no 4.4.1
#> tm                              <NA>    <NA>   <NA>              yes 4.4.1
#> urlchecker                      <NA>    <NA>   <NA>               no 4.4.1
#> usethis                         <NA>    <NA>   <NA>               no 4.4.1
#> utf8                            <NA>    <NA>   <NA>              yes 4.4.1
#> vctrs                           <NA>    <NA>   <NA>              yes 4.4.1
#> vegan                           <NA>    <NA>   <NA>              yes 4.4.1
#> viridisLite                     <NA>    <NA>   <NA>               no 4.4.1
#> waldo                           <NA>    <NA>   <NA>               no 4.4.1
#> wavethresh                      <NA>    <NA>   <NA>              yes 4.4.1
#> whisker                         <NA>    <NA>   <NA>               no 4.4.1
#> withr                           <NA>    <NA>   <NA>               no 4.4.1
#> xfun                            <NA>    <NA>   <NA>              yes 4.4.1
#> xml2                            <NA>    <NA>   <NA>              yes 4.4.1
#> xopen                           <NA>    <NA>   <NA>               no 4.4.1
#> xtable                          <NA>    <NA>   <NA>               no 4.4.1
#> yaml                            <NA>    <NA>   <NA>              yes 4.4.1
#> zip                             <NA>    <NA>   <NA>              yes 4.4.1
#> zoo                             <NA>    <NA>   <NA>              yes 4.4.1
#> base                            <NA>    <NA>   <NA>             <NA> 4.4.1
#> boot                            <NA>    <NA>   <NA>               no 4.4.1
#> class                           <NA>    <NA>   <NA>              yes 4.4.1
#> cluster                         <NA>    <NA>   <NA>              yes 4.4.1
#> codetools                       <NA>    <NA>   <NA>               no 4.4.1
#> compiler                        <NA>    <NA>   <NA>             <NA> 4.4.1
#> datasets                        <NA>    <NA>   <NA>             <NA> 4.4.1
#> foreign                         <NA>    <NA>   <NA>              yes 4.4.1
#> graphics                        <NA>    <NA>   <NA>              yes 4.4.1
#> grDevices                       <NA>    <NA>   <NA>              yes 4.4.1
#> grid                            <NA>    <NA>   <NA>              yes 4.4.1
#> KernSmooth                      <NA>    <NA>   <NA>              yes 4.4.1
#> lattice                         <NA>    <NA>   <NA>              yes 4.4.1
#> MASS                            <NA>    <NA>   <NA>              yes 4.4.1
#> Matrix                          <NA>    <NA>   <NA>              yes 4.4.1
#> methods                         <NA>    <NA>   <NA>              yes 4.4.1
#> mgcv                            <NA>    <NA>   <NA>              yes 4.4.1
#> nlme                            <NA>    <NA>   <NA>              yes 4.4.1
#> nnet                            <NA>    <NA>   <NA>              yes 4.4.1
#> parallel                        <NA>    <NA>   <NA>              yes 4.4.1
#> rpart                           <NA>    <NA>   <NA>              yes 4.4.1
#> spatial                         <NA>    <NA>   <NA>              yes 4.4.1
#> splines                         <NA>    <NA>   <NA>              yes 4.4.1
#> stats                           <NA>    <NA>   <NA>              yes 4.4.1
#> stats4                          <NA>    <NA>   <NA>             <NA> 4.4.1
#> survival                        <NA>    <NA>   <NA>              yes 4.4.1
#> tcltk                           <NA>    <NA>   <NA>              yes 4.4.1
#> tools                           <NA>    <NA>   <NA>              yes 4.4.1
#> utils                           <NA>    <NA>   <NA>              yes 4.4.1
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
