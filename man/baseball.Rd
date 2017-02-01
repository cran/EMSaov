\name{baseball}
\alias{baseball}
\docType{data}
\title{
Nested factorial design of Measurement of velocity
}
\description{
There was on a measurement of velocity of a baseball throw in meters per second. Three groups of subjects were involved, two being subjected to special experimental training and the third acting as a control with no special training.  Each group has 7 subjects and each subject was given a pretest and posttest. 
}
\usage{data("baseball")}
\format{
  A data frame with 42 observations on the following 4 variables.
  \describe{
    \item{\code{velocity}}{a numeric vector}
    \item{\code{test}}{a factor with levels \code{Pre} \code{Post}}
    \item{\code{Group}}{a factor with levels \code{I} \code{II} \code{III}}
    \item{\code{Subject}}{a numeric vector}
  }
}
\references{
Example 11.4 in Fundamental Concepts in the Design of Experiments (3rd ed.) - Charles R. Hicks
}
\examples{
data(baseball)
## maybe str(baseball) ; plot(baseball) ...
}
\keyword{datasets}
