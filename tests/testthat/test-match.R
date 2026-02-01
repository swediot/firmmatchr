test_that("match_companies runs end-to-end", {
    # Mock data
    dict <- data.frame(
        id = c("1", "2", "3"),
        name = c("Apple Inc", "Microsoft Corp", "Google LLC"),
        stringsAsFactors = FALSE
    )

    queries <- data.frame(
        qid = c("q1", "q2", "q3", "q4"),
        qname = c("Apple Inc", "Micro soft", "Google", "Facebook"),
        stringsAsFactors = FALSE
    )

    # Run pipeline
    # Suppress messages to keep test output clean
    suppressMessages({
        res <- match_companies(queries, dict,
            query_col = "qname", dict_col = "name",
            unique_id_col = "qid", dict_id_col = "id",
            threshold_jw = 0.7
        )
    })

    # Check results
    expect_true(is.data.frame(res))
    expect_true(nrow(res) > 0)
    expect_true(all(c("query_id", "dict_id", "match_type") %in% names(res)))

    # Expect Exact match for Apple
    apple_match <- res[query_id == "q1"]
    expect_equal(nrow(apple_match), 1)
    expect_equal(apple_match$match_type, "Perfect")
})
