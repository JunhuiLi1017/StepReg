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
      or
      \code{pak::pkg_install("JunhuiLi1017/StepReg")}.
      or
      \code{install.packages("StepReg")}.
      or
      \code{devtools::install_github("JunhuiLi1017/StepReg")}.
      we recommend using \code{pak::pkg_install("StepReg")} to install StepReg to avoid the dependency issues.
    \item \strong{Q: What system dependencies might I need when installing StepReg?} \cr
      A: Some dependencies of StepReg require additional system libraries to be installed on your computer. If you see an error message about missing system packages, you may need to manually install them using your system package manager.
      For common dependencies, you may need to install:
      - cairo-devel (required for gdtools)
      - ImageMagick-c++-devel and ImageMagick-devel (required for magick)
      - libwebp-devel (required for ragg)
      - X11 library (required for graphics on macOS): If the X11 library is missing (often required for graphics on macOS), please install XQuartz first.
      installation examples:
      Ubuntu/Debian:
      \code{sudo apt-get install libcairo2-dev libwebp-dev libmagick++-dev}
      Red Hat/CentOS:
      \code{sudo yum install cairo-devel libwebp-devel ImageMagick-c++-devel ImageMagick-devel}
      macOS:
      \code{brew install cairo imagemagick webp}
  }
}