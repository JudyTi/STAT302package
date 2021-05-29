# within my_rf_cv.R

# Correct situation
test_that("my_rf_cv works mathematically and returns a numeric output", {
  expect_is(my_rf_cv(5), "numeric")
})

# Error
test_that("non-numeric input throws error", {
  expect_error(my_rf_cv("a string"))
})
