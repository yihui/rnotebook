% Please edit documentation in R/rnotebook.R
\name{newnb}
\alias{newnb}
\title{Create a new notebook file}
\usage{
newnb(file = tempfile("rnotebook", ".", ".Rnb"), title = getOption("rnotebook.title", 
    "An R Notebook"), author = getOption("rnotebook.author"), date = Sys.Date(), 
    ..., body)
}
\arguments{
\item{file}{the notebook filename}

\item{title}{the title of the notebook}

\item{author}{the author of the notebook}

\item{date}{the date for the notebook}

\item{...}{other metadata for the notebook}

\item{body}{the body of the notebook; a default list of content is used if
not provided, otherwise it should be a list of sub-lists, with each
sub-list being a list of elements \code{type} (\code{text} or \code{code}),
\code{src} (source), and \code{out} (output); for \code{type = 'code'},
\code{src} is a list of two elements \code{options} (chunk options) and
\code{code} (source code)}
}
\value{
The notebook filename (a temporary file by default, with the prefix
  \code{rnotebook}). The metadata and content are written to the file as a
  JSON string.
}
\description{
Create a notebook file based on the metadata (e.g.,title and author, etc) and
the content.
}
\examples{
f = rnotebook::newnb()
rnotebook::refresh(f)  # compile the notebook
file.show(f)

f2 = rnotebook::newnb('foo-test.Rnb', author = 'R. A. Fisher', body = list(
  list(type = 'text', src = 'Hello **world**.', out = NULL),
  list(type = 'code', src = list(
    options = '', code = 'fisher.test(matrix(c(3, 1, 1, 3), 2))'
  ), out = '')
))
file.show(f2)
rnotebook::refresh(f2)

# serve notebooks using the servr package (click the .Rnb filenames)
if (!requireNamespace('servr') || packageVersion('servr') < '0.1.10')
  install.packages('servr', repos = 'http://yihui.name/xran')
if (interactive()) servr::notebook()
}
