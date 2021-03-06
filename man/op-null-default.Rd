% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-pipe.R
\name{op-null-default}
\alias{op-null-default}
\alias{\%||\%}
\title{Default value for \code{NULL}}
\usage{
x \%||\% y
}
\arguments{
\item{x, y}{If \code{x} is NULL, will return \code{y}; otherwise returns \code{x}.}
}
\value{
A vector of \code{x} or \code{y}
}
\description{
This infix function makes it easy to replace \code{NULL}s with a default
value. It's inspired by the way that Ruby's or operation (\code{||})
works.
}
