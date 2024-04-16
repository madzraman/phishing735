test_that("add works on two vectors", {

    expect_equal(add(1:5,6:10), c(7,9,11,13,15))

})

test_that("simple errors for bad input", {

    expect_error(add())
    expect_error(add(1:5))
    expect_error(add(1:5,6:10,"yes"))

})
