library(radagio)
data(archie)

res1 <- adagio(archie, bottomup = TRUE, preproc = TRUE, cleanup = FALSE)
list.files(res1$filepath)
res2 <- adagio(archie, bottomup = TRUE, preproc = FALSE,
               tempfolderidentifier = "bohoo", cleanup = TRUE)
list.files(res2$filepath)

test_that("cleanup works", {
  expect_true(length(list.files(res1$filepath)) == 7)
  expect_true(length(list.files(res2$filepath)) == 0)
})


res3 <- adagio(archie, bottomup = FALSE, preproc = TRUE)
res4 <- adagio(archie, bottomup = FALSE, preproc = FALSE, plotfig = TRUE)

test_that("lists are produced", {
  expect_true(is.list(res3))
  expect_true(is.list(res4))
})

test_that("errors are caught", {
  m <- archie
  m[1, 3] <- NA
  expect_error(adagio(m))
  m <- archie
  m <- m[1:6, 1:4]
  expect_error(adagio(m))
  m <- archie
  colnames(m) <- rownames(m) <- NULL
  expect_warning(adagio(m))
})
