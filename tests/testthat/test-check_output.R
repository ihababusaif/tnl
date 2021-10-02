library(testthat)        # load testthat package
library(tnl)            # load our package

# Test whether the output is a list
test_that("tnl() returns a list", {
  output_tnl<-tnl(2,7,1)
  expect_type(output_tnl, "list")

})

## Test whether the output return the right number
test_that("tnl() returns the right output", {
  output_tnl <-  output_tnl<-tnl(7,7,1)
  expect_equal(output_tnl$tt, 1)
})
## Test whether the output contains the right number
test_that("tnl() returns a list with correct number", {
  output_tnl <- output_tnl<-tnl(2,7,1)
  expect_length(output_tnl$prob, 1)
  expect_length(output_tnl$tt, 1)
})
## Test whether the output should not exceed one.
test_that("tnl() returns number should not exceed one", {
  output_tnl <- output_tnl<-tnl(2,7,1)
  expect_lte(output_tnl$prob, 1)
  expect_lte(output_tnl$tt, 1)
})
## Test whether the output should exceed zero.
test_that("tnl() returns number should exceed zero", {
  output_tnl <- output_tnl<-tnl(2,7,1)
  expect_lte(0, output_tnl$prob)
  expect_lte(0, output_tnl$tt)
})

## Test errors
test_that("errors", {
    expect_error(tnl(1,7,2),"k must be >= l" )
})
test_that("errors", {
  expect_error(tnl(3,4,2),"n must be > 2l")
})
