\name{film}
\alias{film}
\docType{data}
\title{
Dry-film thickness
}
\description{
Two days in a given month were randomly selected in which to run an experiment. three operators were selected at random from a large pool of available operators. The experiment consisted of measuring the dry-film thickness of varnish in mils for three different gate settings: 2, 4, and 6.
}
\usage{data("film")}
\format{
  A data frame with 36 observations on the following 4 variables.
  \describe{
    \item{\code{thickness}}{a numeric vector}
    \item{\code{Gate}}{a numeric vector}
    \item{\code{Operator}}{a factor with levels \code{A} \code{B} \code{C}}
    \item{\code{Day}}{a numeric vector}
  }
}

\references{
Fundamental Concepts in the Design of Experiments (3rd ed.) - Charles R. Hicks
}
\examples{
data(film)
## maybe str(film) ; plot(film) ...
}
\keyword{datasets}
