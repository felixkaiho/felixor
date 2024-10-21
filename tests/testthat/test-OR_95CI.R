test_that("OR_95CI function works", {
  expect_equal(OR_95CI(1.00551, 0.25341, 0.05, 3), "2.733 (1.663, 4.491)")
  expect_error(OR_95CI("cat"))
})
