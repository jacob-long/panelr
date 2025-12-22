# Tests for spline/poly support in wbm

# Skip if splines not available (though it's in base R)
skip_if_not_installed("splines")

library(splines)

data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
wages <- wages[1:200,] # Reduce runtime

# Basic spline test -----------------------------------------------------------
context("wbm with splines")

test_that("wbm works with ns() in formula", {
  wb <- wbm(lwage ~ ns(exp, df = 3) | blk, data = wages)
  expect_s4_class(wb, "wbm")
  # Should have both within and between spline columns
  fe <- lme4::fixef(wb)
  expect_true(any(grepl("ns_exp.w", names(fe))))
  expect_true(any(grepl("ns_exp.b", names(fe))))
})

test_that("wbm works with bs() in formula", {
  suppressWarnings({
  wb <- wbm(lwage ~ bs(exp, df = 3) | blk, data = wages)
  })
  expect_s4_class(wb, "wbm")
  fe <- lme4::fixef(wb)
  expect_true(any(grepl("bs_exp.w", names(fe))))
  expect_true(any(grepl("bs_exp.b", names(fe))))
})

test_that("wbm works with poly() in formula", {
  wb <- wbm(lwage ~ poly(exp, degree = 2) | blk, data = wages)
  expect_s4_class(wb, "wbm")
  fe <- lme4::fixef(wb)
  expect_true(any(grepl("poly_exp.w", names(fe))))
  expect_true(any(grepl("poly_exp.b", names(fe))))
})

# Test basis_utils functions --------------------------------------------------
context("basis_utils helpers")

test_that("is_matrix_term detects matrix-returning functions", {
  expect_true(is_matrix_term("ns(exp, df = 3)", wages))
  expect_true(is_matrix_term("bs(exp, df = 3)", wages))
  expect_true(is_matrix_term("poly(exp, degree = 2)", wages))
  expect_false(is_matrix_term("exp", wages))
  expect_false(is_matrix_term("log(lwage)", wages))
})

test_that("extract_fn_name extracts function names", {
  expect_equal(extract_fn_name("ns(exp, df = 3)"), "ns")
  expect_equal(extract_fn_name("bs(exp, df = 3)"), "bs")
  expect_equal(extract_fn_name("poly(exp, degree = 2)"), "poly")
  expect_null(extract_fn_name("exp"))
})

test_that("extract_basis_variable extracts the primary variable", {
  expect_equal(extract_basis_variable("ns(exp, df = 3)"), "exp")
  expect_equal(extract_basis_variable("bs(age, df = 5)"), "age")
  expect_equal(extract_basis_variable("poly(lwage, degree = 3)"), "lwage")
})

test_that("is_known_basis_fn identifies registered functions", {
  expect_true(is_known_basis_fn("ns"))
  expect_true(is_known_basis_fn("bs"))
  expect_true(is_known_basis_fn("poly"))
  expect_false(is_known_basis_fn("log"))
  expect_false(is_known_basis_fn("exp"))
})

test_that("evaluate_basis_term extracts correct attributes", {
  result <- evaluate_basis_term("ns(exp, df = 3)", wages)
  expect_equal(result$fn_name, "ns")
  expect_equal(result$var_name, "exp")
  expect_equal(result$ncol, 3)
  expect_true("knots" %in% names(result$attrs) || "Boundary.knots" %in% names(result$attrs))
})

test_that("reconstruct_basis_call modifies variable correctly", {
  original <- "ns(exp, df = 3)"
  new_call <- reconstruct_basis_call(original, "exp - mean_exp", NULL)
  expect_true(grepl("exp - mean_exp", new_call))
  expect_true(grepl("ns", new_call))
})

test_that("generate_basis_colnames creates expected names", {
  cols <- generate_basis_colnames("ns", "exp", 3, "w")
  expect_equal(cols, c("ns_exp.w1", "ns_exp.w2", "ns_exp.w3"))
  
  cols_between <- generate_basis_colnames("ns", "exp", 3, "b")
  expect_equal(cols_between, c("ns_exp.b1", "ns_exp.b2", "ns_exp.b3"))
})

test_that("detect_matrix_terms returns correct logical vector", {
  terms <- c("exp", "ns(exp, df = 3)", "log(lwage)")
  result <- detect_matrix_terms(terms, wages)
  expect_equal(unname(result), c(FALSE, TRUE, FALSE))
})

# Test process_matrix_term ----------------------------------------------------
context("process_matrix_term")

test_that("process_matrix_term returns expected structure", {
  result <- process_matrix_term("ns(exp, df = 3)", wages)
  
  expect_type(result, "list")
  expect_equal(result$fn_name, "ns")
  expect_equal(result$var_name, "exp")
  expect_equal(result$ncol, 3)
  expect_equal(length(result$within_cols), 3)
  expect_equal(length(result$between_cols), 3)
  expect_true(grepl("w1$", result$within_cols[1]))
  expect_true(grepl("b1$", result$between_cols[1]))
})
