#!/usr/bin/env Rscript
# Escape non-ASCII characters in a text file to Unicode escape sequences.
#
# Reads a UTF-8 file and rewrites every character whose code point is greater
# than 127 as a \\uXXXX (BMP) or \\UXXXXXXXX (non-BMP) escape. ASCII
# characters, including existing backslash escapes like \\n or \\\\, are left
# unchanged.
#
# Usage:
#   Rscript tools/escape_non_ascii.R <file> [-o <output>]

escape_non_ascii <- function(text) {
  chars <- strsplit(text, "")[[1]]
  codes <- utf8ToInt(text)

  out <- vapply(seq_along(chars), function(i) {
    code <- codes[i]
    if (code > 0xFFFF) {
      sprintf("\\U%08x", code)
    } else if (code > 0x7F) {
      sprintf("\\u%04x", code)
    } else {
      chars[i]
    }
  }, character(1))

  paste0(out, collapse = "")
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  out_idx <- which(args == "-o" | args == "--output")
  output <- NULL
  if (length(out_idx) > 0) {
    output <- args[out_idx[1] + 1]
    args <- args[-c(out_idx[1], out_idx[1] + 1)]
  }

  if (length(args) < 1) {
    stop("Usage: Rscript escape_non_ascii.R <file> [-o <output>]")
  }

  input <- args[1]
  output <- output %||% input

  # Read and write as raw bytes to avoid any platform-specific line-ending
  # translation (e.g. CRLF -> CR-CRLF on Windows).
  bytes <- readBin(input, "raw", file.info(input)$size)
  content <- rawToChar(bytes)
  Encoding(content) <- "UTF-8"

  escaped <- escape_non_ascii(content)

  writeBin(charToRaw(escaped), output)

  message("Escaped non-ASCII characters -> ", output)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

main()
