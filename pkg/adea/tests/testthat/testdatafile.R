library("adea")

data("cardealers1")
test_that("data set cardealers1", {
    expect_true(is.data.frame(cardealers1))
    expect_equal(nrow(cardealers1), 4)
    expect_equal(ncol(cardealers1), 2)
    })

data("cardealers2")
test_that("data set cardealers2", {
    expect_true(is.data.frame(cardealers2))
    expect_equal(nrow(cardealers2), 6)
    expect_equal(ncol(cardealers2), 3)
    })

data("cardealers3")
test_that("data set cardealers3", {
    expect_true(is.data.frame(cardealers3))
    expect_equal(nrow(cardealers3), 6)
    expect_equal(ncol(cardealers3), 3)
    })


data("cardealers4")
test_that("data set cardealers4", {
    expect_true(is.data.frame(cardealers4))
    expect_equal(nrow(cardealers4), 6)
    expect_equal(ncol(cardealers4), 4)
    })

data("tokyo_libraries")
test_that("data set tokyo_libraries", {
    expect_true(is.data.frame(tokyo_libraries))
    expect_equal(nrow(tokyo_libraries), 23)
    expect_equal(ncol(tokyo_libraries), 6)
    })

data("tokyo_libraries")
test_that("data set tokyo_libraries", {
    expect_true(is.data.frame(tokyo_libraries))
    expect_equal(nrow(tokyo_libraries), 23)
    expect_equal(ncol(tokyo_libraries), 6)
    })
