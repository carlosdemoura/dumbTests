#' Get a song lyric by Lupicínio every day
#'
#' Draw a random song lyric by Brazilian musician Lupicínio Rodrigues from
#' <www.letras.mus.br/> based on an seed.
#'
#' @inheritParams Pessoa
#'
#' @return list; contains author, title, text, and source.
#'
#' @examples Lupicinio(max.char = 1000)
#' # should return the first 1000 ish characters of today's Lupicínio song lyric.
#'
#' @export
#'
#' @import rvest
#' @import purrr
Lupicinio = decorator(function() {
  songs = "https://www.letras.mus.br/lupcinio-rodrigues/" |>
    rvest::read_html() |>
    rvest::html_elements("a.songList-table-songName") |>
    rvest::html_attr("href")

  url = paste0("https://www.letras.mus.br", sample(songs, 1))

  page = url |>
    rvest::read_html()

  text = page |>
    rvest::html_elements("div.lyric-original") |>
    rvest::html_text2()

  about = page |>
    rvest::html_elements("div.title-content") |>
    rvest::html_text2() |>
    strsplit("\n") |>
    purrr::pluck(1)

  list(author = about[2],
       title = about[1],
       text = text,
       source = url)
})
