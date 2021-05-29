# within my_lm.R

# the outputs from my function and stats package function
my_output <- my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)
test_output <- summary(lm(formula = lifeExp ~ gdpPercap + continent,
                          data = my_gapminder))

# Correct situation
test_that("my_lm correctly returns a table", {
  expect_is(my_output, "table")
})
test_that("the result was all correct", {
  expect_equal(my_output, as.table(test_output$coefficients))
})

# Error
test_that("non-formula, non-data frame input type throws error", {
  expect_error(my_lm(1, my_gapminder))
  expect_error(my_lm(lifeExp ~ gdpPercap + continent, 1))
  expect_error(my_lm(lifeExp ~ gdpPercap + continent, "a string"))
})
