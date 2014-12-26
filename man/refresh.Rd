% Please edit documentation in R/rnotebook.R
\name{refresh}
\alias{refresh}
\title{Update a notebook file}
\usage{
refresh(file)
}
\arguments{
\item{file}{the notebook filename}
}
\value{
The notebook filename (invisibly). The content of the notebook is
  updated as a side effect.
}
\description{
Parse and recompile a notebook file through \pkg{knitr}.
}
\examples{
f = rnotebook::newnb()
rnotebook::refresh(f)  # compile the notebook
file.show(f)
}
