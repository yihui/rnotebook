# rnotebook: R Notebook based on JSON and R Markdown

Traditional dynamic documents based on **knitr** are normally compiled to output documents. The separation between the source document and output document has its pros and cons. The source document is often cleaner than the output document, and easier to maintain. Some people may prefer the output to be embedded right in the source document, so they only need to maintain one single document, in which they can see both the source and output.

This package tries to defined a JSON-based format for dynamic documents in R. There is no formal definition for an "R Notebook", and we use this term here because what we did in this package is similar to IPython notebooks, which is also based on JSON.

The R Notebook in this package is a JSON file, which is basically a JavaScript object with meta information, code chunks, and text chunks. Here is an example (created from `rnotebook::create()`):

```json
{
  "frontmatter": {
    "title": "An R Notebook",
    "author": "Yihui Xie",
    "date": "2014-12-26"
  },
  "body": [
    {
      "type": "text",
      "src": "A _sample_ paragraph.",
      "out": ""
    },
    {
      "type": "code",
      "src": {
        "options": "tidy = TRUE",
        "code": "1 + 1; x = dnorm(0)"
      },
      "out": ""
    },
    {
      "type": "text",
      "src": [
        "We know the density of N(0, 1)",
        "at 0 is `r x`."
      ],
      "out": ""
    }
  ]
}
```

The format should be clear if you are familiar with **knitr** documents and JSON. When compiling this document, the output is written into the `out` elements of the `body` cells instead of a new file. That is how the source and output are combined in one single file.

This package is just my proof of concept, and it may or may not be a worthwhile project in the future, given the popularity of R Markdown v2.
