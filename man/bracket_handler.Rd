% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_fn_args_addin.R
\name{bracket_handler}
\alias{bracket_handler}
\title{bracket_handler}
\usage{
bracket_handler(curval, inval, start, stop)
}
\arguments{
\item{curval}{the inputted character}

\item{inval}{the current value for the special character count}

\item{start}{what increases the value}

\item{stop}{what decreases the value}
}
\value{
inval a numeric vector of length 1
}
\description{
Convenience function, increases inval for start, reduces it for stop
}
