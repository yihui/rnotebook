# use pretty JSON for I/O?
pretty_json = function() {
  getOption('rnotebook.json.pretty', 2L)
}

# the specs of the notebook JSON format
lint_nb = function(file) {
  data = jsonlite::fromJSON(
    file,
    simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE
  )

  if (!is.list(data)) stop('The notebook source must be created from a list')
  if (length(data) != 2 || !identical(names(data), c('frontmatter', 'body')))
    stop("The notebook source must be of length 2 with names 'frontmatter' and 'body'")

  body = data$body
  if (!is.list(body)) stop('The notebook body must be created from a list')

  n = length(body)
  for (i in seq_len(n)) {
    b = body[[i]]
    if (!is.list(b) || !identical(names(b), c('type', 'src', 'out')))
      stop('The element #', i, " must be a list with names 'type', 'src', and 'out'")

    if (length(b$type) != 1 || !(b$type %in% c('text', 'code')))
      stop('The type of the element #', i, " must be either 'text' or 'code'")
    if (b$type == 'text' && !is.character(b$src))
      stop('The source of the text chunk #', i, ' must be a character vector')

    if (b$type == 'code') {
      if (!is.list(b$src))
        stop('The source of the code chunk #', i, ' must be a list')
      if (!identical(names(b$src), c('options', 'code')))
        stop('The source of the code chunk #', i, " must be a list with names 'options' and 'code'")
      if (length(b$src$options) != 1 || !is.character(b$src$options))
        stop('The chunk options for the code chunk #', i, ' must be a character vector of length 1')
      b$src$options = jsonlite::unbox(b$src$options)
      if (!is.character(b$src$code))
        stop('The source code of the chunk #', i, ' must be a character vector')
      if (!is.null(b$out) && !is.character(b$out))
        stop('The output of the chunk #', i, ' must be either NULL or a character vector')
    }
    b$type = jsonlite::unbox(b$type)
    body[[i]] = b
  }

  data$body = body
  data
}

# write a JSON list to file with UTF-8 and a few custom options
write_json = function(json, file) {
  json = jsonlite::toJSON(json, null = 'null', pretty = pretty_json(), auto_unbox = TRUE)
  json = to_utf8(json)
  writeLines(json, file, useBytes = TRUE)
}

to_utf8 = function(text) {
  text = iconv(text, to = 'UTF-8')
  if (any(is.na(text))) stop('Failed to convert the character string to UTF-8')
  text
}
