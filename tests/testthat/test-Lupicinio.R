test_that("Lupicinio() works", {
  adjust_encoding = function(x) {
    x |>
      iconv(from = "latin1", to = "UTF-8") |>
      iconv(from = "UTF-8", "ASCII", sub = "")
  }

  expected = list(
    author = "Lupicínio Rodrigues",
    title  = "Carteiro",
    text   = "Quem vê aquele carteiro\nO dia inteiro feliz a cantar\nDeixando missivas diversas\nEm cada lugar\n\nNão pode julgar se ele sabe\nQue a pasta onde cabem\nAs cartas de amor\nTrazem junto as notícias de luto\nInfelicidade, infortunas e dor\n\nAquele carteiro que sempre me trouxe\nNotícias do meu grande bem\nO mesmo carteiro, hoje vem me avisar\nQue quem amo tem um outro alguém\nE a cantar bem contente\nSai indiferente\nSem ver que eu fico a chorar\nO carteiro devia saber\nO que vai entregar",
    source = "https://www.letras.mus.br/lupcinio-rodrigues/1410841/"
  ) |>
    sapply(adjust_encoding)

  actual = Lupicinio(seed = 0) |>
    sapply(adjust_encoding)

  expect_equal(actual, expected)
})
