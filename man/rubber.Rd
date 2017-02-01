\name{rubber}
\alias{rubber}
\docType{data}
\title{
Split-split plot design of Curerate index
}
\description{
A study of the cure rate index on some samples of rubber. Three laboratories, three temperatures and three types of mix were involved. Once a temperature was set, all three mixes were subjected to that temperature and then another temperature was set and again all three mixes were involved, finally the third temperature was set. 
}
\usage{data("rubber")}
\format{
  A data frame with 108 observations on the following 5 variables.
  \describe{
    \item{\code{cure}}{a numeric vector}
    \item{\code{Rep}}{a factor with levels \code{I} \code{II} \code{III} \code{IV}}
    \item{\code{Lap}}{a numeric vector}
    \item{\code{Temp}}{a numeric vector}
    \item{\code{Mix}}{a factor with levels \code{A} \code{B} \code{C}}
  }
}
\references{
Fundamental Concepts in the Design of Experiments (3rd ed.) - Charles R. Hicks
}
\examples{
data(rubber)
## maybe str(rubber) ; plot(rubber) ...
}
\keyword{datasets}
