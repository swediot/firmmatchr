test_that("normalization works basic", {
    expect_equal(normalize_company_name("Acme GmbH"), "acme")
    expect_equal(normalize_company_name("  Bad Space  "), "bad space")
})

test_that("normalization handles german characters", {
    expect_equal(normalize_company_name("Müller AG"), "mueller")
    expect_equal(normalize_company_name("Großhandel"), "grosshandel")
})

test_that("normalization removes legal forms", {
    expect_equal(normalize_company_name("Tech Limited"), "tech")
    expect_equal(normalize_company_name("Service SpA"), "service")
    expect_equal(normalize_company_name("Holding Group"), "") # Might result in empty string if only stop words
})
