#' Update a notebook file
#'
#' Parse and recompile a notebook file through \pkg{knitr}.
#' @param file the notebook filename
#' @return The notebook filename (invisibly). The content of the notebook is
#'   updated as a side effect.
#' @export
refresh = function(file) {
  json = lint_nb(file)
  body = json$body
  if ((n <- length(body)) == 0) return()

  # arrange text and code chunks for knitr
  cells = lapply(body, function(cell) {
    if (cell$type == 'text') {
      knitr:::parse_inline(cell$src, knitr::all_patterns$md)
    } else {
      header = cell$src$options
      knitr:::parse_block(cell$src$code, header, header)
    }
  })

  # a lazy method to restore all knitr objects such as opts_chunk/opts_knit/knit_hooks/...
  on.exit(unloadNamespace('knitr'), add = TRUE)
  oopts = options(
    useFancyQuotes = FALSE, width = knitr::opts_knit$get('width'),
    knitr.in.progress = TRUE, device = knitr:::pdf_null
  )
  on.exit(options(oopts), add = TRUE)
  knitr::opts_knit$set(encoding = 'UTF-8', progress = FALSE)
  knitr::render_markdown()

  for (i in seq_len(n)) {
    res = knitr:::process_group(cells[[i]])
    # if output and source are identical, avoid duplicate storage
    body[[i]]['out'] = list(
      if (!identical(res, paste(body[[i]][['src']], collapse = '\n'))) res
    )
  }

  json$body = body
  write_json(json, file)
  invisible(file)
}

#' Export a notebook to Markdown
#'
#' Export a notebook file (\file{.Rnb}) to Markdown (\file{.md}).
#' @inheritParams refresh
export = function(file) {

}

#' Create a new notebook file
#'
#' Create a notebook file based on the metadata (e.g.,title and author, etc) and
#' the content.
#' @inheritParams refresh
#' @param title the title of the notebook
#' @param author the author of the notebook
#' @param date the date for the notebook
#' @param ... other metadata for the notebook
#' @param body the body of the notebook; a default list of content is used if
#'   not provided, otherwise it should be a list of sub-lists, with each
#'   sub-list being a list of elements \code{type} (\code{text} or \code{code}),
#'   \code{src} (source), and \code{out} (output); for \code{type = 'code'},
#'   \code{src} is a list of two elements \code{options} (chunk options) and
#'   \code{code} (source code)
#' @return The notebook filename (a temporary file by default, with the prefix
#'   \code{rnotebook}). The metadata and content are written to the file as a
#'   JSON string.
#' @export
create = function(
  file = tempfile('rnotebook', '.', '.Rnb'),
  title = getOption('rnotebook.title', 'An R Notebook'),
  author = getOption('rnotebook.author'),
  date = Sys.Date(),
  ...,
  body
) {
  if (missing(body))
    body = list(
      list(type = 'text', src = 'A _sample_ paragraph.', out = ''),
      list(type = 'code', src = list(options = 'tidy = TRUE', code = '1 + 1; x = dnorm(0)'), out = ''),
      list(type = 'text', src = c('We know the density of N(0, 1)', 'at 0 is `r x`.'), out = '')
    )
  info = list(
    frontmatter = list(title = title, author = author, date = as.character(date), ...),
    body = body
  )
  write_json(info, file)
  invisible(file)
}
