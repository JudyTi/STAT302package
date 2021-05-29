# within my_t.test.R
# The result from my_t.test function
x <- my_gapminder$lifeExp
my_two_side <- my_t.test(x, "two.sided", 60)
my_greater <- my_t.test(x, "greater", 60)
my_less <- my_t.test(x, "less", 60)

# The result from t.test function
test_two_side <- t.test(x, alternative = "two.sided", mu = 60)
test_greater <- t.test(x, alternative = "greater", mu = 60)
test_less <- t.test(x, alternative = "less", mu = 60)

# Test return type: list
test_that("my_t.test correctly returns a list", {
  expect_is(my_two_side, "list")
  expect_is(my_greater, "list")
  expect_is(my_less, "list")
})
# Test test_stat
test_that("my_t.test correctly returns numeric test statistic in the list", {
  expect_equal(my_two_side$test_stat, as.numeric(test_two_side$statistic))
  expect_equal(my_greater$test_stat, as.numeric(test_greater$statistic))
  expect_equal(my_less$test_stat, as.numeric(test_less$statistic))
})
# Test df
test_that("my_t.test correctly returns degrees of freedom in the list", {
  expect_equal(my_two_side$df, as.numeric(test_two_side$parameter))
  expect_equal(my_greater$df, as.numeric(test_greater$parameter))
  expect_equal(my_less$df, as.numeric(test_less$parameter))
})
# Test alternative
test_that("my_t.test correctly returns alternative in the list", {
  expect_match(my_two_side$alternative, "two.sided")
  expect_match(my_greater$alternative, "greater")
  expect_match(my_less$alternative, "less")
})
# Test p-value
test_that("my_t.test correctly returns p value in the list", {
  expect_equal(my_two_side$p_val, test_two_side$p.value)
  expect_equal(my_greater$p_val, test_greater$p.value)
  expect_equal(my_less$p_val, test_less$p.value)
})

# Error
test_that("non-numeric input of x and mu throws error", {
  expect_error(my_t.test(x = "a string", alternative = "greater", mu = "a string"))
})
test_that("non-string/incorrect string input of alternative throws error", {
  expect_error(my_t.test(x, 1, 1))
  expect_error(my_t.test(x, "error", 1))
})
