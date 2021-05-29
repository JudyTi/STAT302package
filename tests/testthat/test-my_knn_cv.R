# within my_knn_cv.R

data <- na.omit(my_penguins)

# Correct situation
test_that("my_knn_cv correctly returns a list", {
  expect_is(my_knn_cv(data[, 3:6], data$species, 1, 5), "list")
})

# Error
test_that("incorrect input type throws error", {
  expect_error(my_knn_cv(my_penguins[, 3:6], 1))
  expect_error(my_knn_cv(my_penguins[, 3:6], "a string"))
})
