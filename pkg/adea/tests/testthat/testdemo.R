skip_on_cran()

library("adea")

demo("cardealers")

test_that("cardealers demo", {
    expect_s3_class(sol.adea, "adea")
    expect_equal(sol.adea$load$load, 0.66666667)
    expect_equal(names(sol.adea$load$iinput), c("Employees"))
    expect_s3_class(sol.ah, "adeahierarchical")
    expect_equal(length(sol.ah$models), 4)
    expect_equal(sol.ah$models[[4]]$load$load, 0.66666667)
    expect_equal(names(sol.ah$models[[4]]$load$iinput), c("Employees"))
    expect_s3_class(sol.ap, "adeaparametric")
    expect_equal(length(sol.ap$models), 4)
    expect_equal(sol.ap$models[[4]]$load$load, 0.66666667)
    expect_equal(names(sol.ap$models[[4]]$load$iinput), c("Employees"))
})

demo("spanishuniversities2018")
test_that("spanishuniversities2018 demo", {
    expect_s3_class(sol.dea, "Farrell")
    expect_equal(sol.dea$eff[1], 0.77542788)
    expect_equal(sol.dea$ux[1], 0.00091240876)
    expect_s3_class(sol.adea, "adea")
    expect_equal(sol.adea$load$load, 0.58985841)
    expect_equal(as.numeric(sol.adea$load$ratios$output[7]), 0.75717137)
})

demo("tokyo_libraries")
test_that("cardealers demo", {
    expect_s3_class(sol.adea, "adea")
    expect_equal(sol.adea$load$load, 0.455467)
    expect_equal(names(sol.adea$load$iinput), c("Area.I1"))
    expect_s3_class(sol.adea, "adea")
    expect_equal(sol.adea$load$load, 0.455467)
    expect_equal(names(sol.adea$load$iinput), c("Area.I1"))
})
