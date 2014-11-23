\name{useDefaults}
\alias{useDefaults}
\alias{unDefaults}
\title{ Enable and Disable Global Defaults By Function }
\description{
Allows for the use of globally managed default
values for formal function arguments. Adds
the ability to pre-specify a value for any formal
argument as if it were specified in the function call.
} 
\usage{
useDefaults(name)
unDefaults(name)
}
\arguments{
  \item{name}{ name of function, quoted or unquoted }
}
\details{
Defaults are set inside the named function with
a call to \code{importDefaults}.  This may be hard
coded into the function by the author, or may be
dynamically added with a call to \code{useDefaults}.
If the function is defined in the Global environment
\code{useDefaults} first copies the original to a hidden
function in the global namespace, then creates a modified version
with the same name as the original in the current 
environment. For functions defined in packages, a replacement
modified function is simply written to the Global environment,
effectively overriding the original.

\code{useDefaults} replaces all formal functional
arguments with all non-NULL globally specified ones
after first checking that these global defaults have
not been overridden with new values in the function 
call.

This check at present does not match non-named arguments, therefore
it is \emph{possible} that a value specified in the function call
will be ignored and replaced by the global one.

The order of lookup is as follows, with the lookup
halted once a specified value is found:

\enumerate{
    \item{}{1. Check for arguments specified 
       in the actual call}

    \item{}{2. Check for arguments specified by setDefaults}

    \item{}{3. Use original function defaults. (if any)}
}

Setting default values is accompished via
\code{setDefaults}, with the values being written
to \R's \code{options} list as a named list set
to the function's name appended with a .Default,
all managed automatically.  It is possible to view
and delete defaults with the functions
\code{getDefaults} and \code{unsetDefaults}, 
respectively.

To return a function enabled by \code{useDefaults}
to its original state, call \code{unDefaults}.
Conceptually this is similar to \code{debug} and
\code{undebug}, though implemented entirely in \R.
}
\value{
None. Called for its side effect of enabling
or disabling the Defaults mechanism
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{importDefaults}},
          \code{\link{setDefaults}},
          \code{\link{formals}},
          \code{\link{body}},
          \code{\link{as.function}}
}
\examples{
my.fun <- function(x=2,y=1) { x ^ y }
my.fun()            #returns 2
my.fun(x=2,y=10)    #returns 1024

setDefaults(my.fun,x=2,y=3)
useDefaults(my.fun)
my.fun

my.fun()            #returns 8
my.fun(y=10)        #returns 1024
my.fun(x=2,y=10)    #returns 1024

unDefaults(my.fun)
my.fun
my.fun()            #returns 2

getDefaults(my.fun)
unsetDefaults(my.fun,confirm=FALSE)  
getDefaults(my.fun)
}
\keyword{ utilities }
