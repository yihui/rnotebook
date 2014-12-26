% Please edit documentation in R/rnotebook.R
\name{export}
\alias{export}
\title{Export a notebook to Markdown}
\usage{
export(file, output, raw = FALSE)
}
\arguments{
\item{file}{the notebook filename}

\item{output}{the output filename; by default, the filename is the input
filename \code{file} with the file extension replaced by \file{.md}
(\code{raw = FALSE}) or \file{.Rmd} (\code{raw = TRUE})}

\item{raw}{whether to export the notebook as raw R Markdown or only Markdown
output; for the latter case, the notebook must have been compiled via
\code{\link{refresh}()} beforehand}
}
\value{
The output filename is returned invisibly.
}
\description{
Export a notebook file to Markdown (\file{.md}) or R Markdown (\file{.Rmd}).
}
\examples{
f = rnotebook::newnb()
rnotebook::refresh(f)  # compile the notebook
file.show(f)

f1 = rnotebook::export(f)  # export to markdown
file.edit(f1)

f2 = rnotebook::export(f, raw = TRUE)  # export to R Markdown
file.edit(f2)

unlink(c(f, f1, f2))
}
