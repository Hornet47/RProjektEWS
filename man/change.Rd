% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/change.R
\name{change}
\alias{change}
\title{Kategorisiere dosage form}
\usage{
change(a)
}
\arguments{
\item{a}{die attribut, die kategorisiert werden muss.}
}
\value{
ein kategorisierte frame.
}
\description{
\code{change} kategorisiert \code{dosage_form} aus dem data frame.
}
\examples{
change(c("TABLET, EXTENDED", "SOLUTION"))

}
