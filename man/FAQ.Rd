\name{FAQ}
\alias{FAQ}
\title{Frequently Asked Questions}
\description{
  Answers to common installation and usage questions for StepReg.
}
\details{
  \itemize{
    \item \strong{Q: How do I install StepReg efficiently?} \cr
      A: You can install StepReg using the following command:

      \code{pak::pkg_install("StepReg")}.
      \cr
      or
      \cr
      \code{pak::pkg_install("JunhuiLi1017/StepReg")}.
      \cr
      or
      \cr
      \code{install.packages("StepReg")}.
      \cr
      or
      \cr
      \code{devtools::install_github("JunhuiLi1017/StepReg")}.
      \cr
      \cr
      we recommend using \code{pak::pkg_install("StepReg")} or \code{pak::pkg_install("JunhuiLi1017/StepReg")} to install StepReg to avoid the dependency issues.
      \cr
    \item \strong{Q: What system dependencies might I need when installing StepReg?} \cr
      A: Some dependencies of StepReg require additional system libraries to be installed on your computer. If you see an error message about missing system packages, you may need to manually install them using your system package manager.
      \cr
      For common dependencies, you may need to install:
      \cr
      \itemize{
      \cr
        \item cairo-devel (required for gdtools)
        \cr
        \item ImageMagick-c++-devel and ImageMagick-devel (required for magick)
        \cr
        \item libwebp-devel (required for ragg)
        \cr
        \item X11 library (required for graphics on macOS): If the X11 library is missing, please install XQuartz first.
        \cr
      }
      \cr
      installation examples:
      \cr
      Ubuntu/Debian:
      \code{sudo apt-get install libcairo2-dev libwebp-dev libmagick++-dev}
      \cr
      Red Hat/CentOS:
      \code{sudo yum install cairo-devel libwebp-devel ImageMagick-c++-devel ImageMagick-devel}
      \cr
      macOS:
      You can use Homebrew to install the dependencies (cairo, imagemagick, and webp):
      \code{brew install cairo imagemagick webp}
      \cr
    \item \strong{Q: How to use StepReg?} \cr
      A: You can refer to the [StepReg vignette](https://CRAN.R-project.org/package=StepReg/vignettes/StepReg.html) for detailed examples.
      \cr
    \item \strong{Q: How to use the interactive Shiny application?} \cr
      A: You can refer to the [StepReg Shiny application](https://junhuili1017.shinyapps.io/StepReg/) for detailed examples.
      \cr
  }
}