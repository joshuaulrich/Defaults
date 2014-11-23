\name{Defaults-package}
\alias{Defaults-package}
\alias{Defaults}
\docType{package}
\title{
Create Global Function Defaults
}
\description{
Set, Get, and Import Global Function Defaults
}
\details{
\tabular{ll}{
Package: \tab Defaults\cr
Type: \tab Package\cr
Version: \tab 1.0-5\cr
Date: \tab 2007-07-25\cr
LazyLoad: \tab yes\cr
License: \tab GPL 2 or later\cr
Packaged: \tab Sun Jul 25 10:03:19 2007; jryan\cr
Built: \tab R 2.5.0; ; 2007-07-25 10:03:38; unix\cr
}

Index:
\preformatted{
useDefaults             Enable Global Default Check by Function
unDefaults              Disable Global Default Check by Function

getDefaults             Show Global Defaults List by Function
setDefaults             Create Global Defaults List by Function
unsetDefaults           Remove All Global Defaults by Function

importDefaults          Import Global Default Argument Values
}
\pkg{Defaults} makes the use of
globally specified defaults, on a per-function
basis, available to \emph{all} functions
written in \R.

Through the use of \code{importDefaults}, which can
be either hard coded into a function by the
function author, or dynamically added to
\emph{any} visible function through
\code{useDefaults}, a user can now
override the author specified defaults as returned
by \code{formals}.
}
\author{
Jeffrey A. Ryan

Maintainer: Jeff Ryan <jeff.a.ryan@gmail.com>
}
\keyword{ package }
\examples{
formals(sd)
try(sd(c(1:10,NA)),silent=TRUE)   # fails

useDefaults(sd)
setDefaults(sd, na.rm=TRUE) 
getDefaults(sd)

sd(c(1:10,NA))                    # works!

unsetDefaults(sd, confirm=FALSE)  # removes previously set default
unDefaults(sd)                    # returns sd to original

}
