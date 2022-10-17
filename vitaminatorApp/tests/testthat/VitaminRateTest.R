library(testthat) 

test_that("Test Triglyceride red",{
  expect_equal(rateVitaminValue("Triglyceride", 10), "red")
})

test_that("Test Triglyceride green",{
  expect_equal(rateVitaminValue("Triglyceride", 2), "green")
})

test_that("Test Vitamin B12 green",{
  expect_equal(rateVitaminValue("Vitamin B12", 60), "green")
})

test_that("Test Vitamin B12 yellow",{
  expect_equal(rateVitaminValue("Vitamin B12", 45), "yellow")
})

test_that("Test Vitamin B12 yellow",{
  expect_equal(rateVitaminValue("Vitamin B12", 15), "red")
})

test_that("Test Vitamin D green",{
  expect_equal(rateVitaminValue("Vitamin D", 31), "green")
})

test_that("HS-Omega3-Index yellow",{
  expect_equal(rateVitaminValue("HS-Omega3-Index", 12), "yellow")
})

