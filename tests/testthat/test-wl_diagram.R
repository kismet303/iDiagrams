data <- data.frame(endpoint = c(paste0("endpoint", 1:5)),
                   wins = c(5,5,5,5,5),
                   ties = c(50,40,30,20,10),
                   losses = c(5,5,5,5,5))

data2 <- data.frame(endpoint = c(paste0("endpoint", 1:5)),
                   ties = c(50,40,30,20,10),
                   losses = c(5,5,5,5,5))

test_that("error checks work", {
  expect_error(wl_diagram(data = data[1:4,], topline ="N=10*6 = 60"))
  expect_error(wl_diagram(data = data2, topline ="N=10*6 = 60"))
})

test_that("function runs", {
  d <- wl_diagram(data = data, topline ="N=10*6 = 60")
  expect_equal(class(d),  c("grViz","htmlwidget"))

})
