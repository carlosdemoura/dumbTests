#' Main wrapper
#'
#' Wrapper that automates the seed setting and adjustment of the text in the
#' main functions of the packages.
#'
#' @param f function to be wrapped.
#'
#' @return wrapped function.
decorator = function(f) {

  wrapper = function(max.char = NULL, flag = " ", seed = NULL) {
    stopifnot(
      "seed must be double or NULL"  = is.double(seed) | is.null(seed),
      "max.char must be int or NULL" = any(is.int(max.char), is.null(max.char)),
      "flag must be a character"     = is.character(flag)
    )

    if (!is.null(seed)) set.seed(seed)
    x = f()

    if (!is.null(max.char)) {
      x$text = x$text |>
        substring(1, max.char) |>
        adjust_text(flag)
    }
    return(x)
  }

  return(wrapper)
}


#' Remove last characters from text and adjust it
#'
#' Cuts everything on a string after last time a flag character is present.
#'
#' @param text string; text to be cropped.
#' @param flag character; everything after the last time this character appears will be removed.
#'
#' @return string; text cropped and adjusted.
adjust_text = function(text, flag = " ") {
  split = text |>
    strsplit(paste0("[", flag ,"]")) |>
    purrr::pluck(1) |>
    utils::head(-1)

  if (length(split)) {
    text = split |>
      paste0(collapse = flag) |>
      paste0(" ...")
  }

  text
}
#adjust_text(text = "some-text\n some-more-text", flag  = "\n")  # shoud return "some-text".


#' Verify if a variable is as integer number
#'
#' @param x numeric.
#'
#' @return boolean;
#' * `TRUE`  if `x` is an integer number,
#' * `FALSE` if it is not.
#'
#' @export
is.int = function(x) {
  if (is.numeric(x)) {
    if (x == round(x)) {
      return(TRUE)
    }
  }

  return(FALSE)
}
