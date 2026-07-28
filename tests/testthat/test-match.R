test_that("match_companies runs end-to-end", {
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

    res <- quiet_match(queries, dict,
        query_col = "qname", dict_col = "name",
        unique_id_col = "qid", dict_id_col = "id",
        threshold_jw = 0.7
    )

    expect_true(is.data.frame(res))
    expect_true(nrow(res) > 0)
    expect_true(all(
        c(
            "query_id", "query_name", "dict_id", "dict_name",
            "match_type", "n_dict_ids", "dict_id_all"
        ) %in% names(res)
    ))

    apple_match <- res[query_id == "q1"]
    expect_equal(nrow(apple_match), 1)
    expect_equal(apple_match$match_type, "Perfect")
})

# Fixture used by the duplicate-handling tests: three dictionary rows collapse
# onto "meier", and one row normalizes to the empty string.
dup_dict <- function() {
    data.frame(
        orbis_id = c("D1", "D2", "D3", "D4", "D5"),
        company_name = c(
            "Meier GmbH", "Meier AG", "Meier Holding",
            "BMW AG", "Holding Group"
        ),
        stringsAsFactors = FALSE
    )
}

test_that("collapsed dictionary duplicates do not break the pipeline", {
    queries <- data.frame(
        query_id = c("q1", "q2"),
        company_name = c("Meier", "BMW"),
        stringsAsFactors = FALSE
    )

    res <- quiet_match(queries, dup_dict(), threshold_jw = 0.75)

    # One row per query, not one row per duplicate dictionary entry.
    expect_equal(nrow(res), 2L)
    expect_equal(nrow(res[query_id == "q1"]), 1L)

    meier <- res[query_id == "q1"]
    expect_equal(meier$dict_id, "D1") # first row of the group is representative
    expect_equal(meier$n_dict_ids, 3L)
    expect_equal(meier$dict_id_all, "D1|D2|D3")

    bmw <- res[query_id == "q2"]
    expect_equal(bmw$n_dict_ids, 1L)
    expect_equal(bmw$dict_id_all, "D4")
})

test_that("every original dictionary row stays linkable after collapsing", {
    queries <- data.frame(
        query_id = "q1", company_name = "Meier",
        stringsAsFactors = FALSE
    )

    res <- quiet_match(queries, dup_dict(), threshold_jw = 0.75)

    cw <- dict_crosswalk(res)
    # D5 normalizes to "" and is dropped; the other four survive.
    expect_equal(nrow(cw), 4L)
    expect_setequal(cw$dict_id, c("D1", "D2", "D3", "D4"))
    expect_equal(cw[dict_id == "D3"]$dict_id_rep, "D1")
    expect_equal(sum(cw$is_representative), 2L)

    ex <- expand_matches(res)
    expect_equal(nrow(ex), 3L)
    expect_setequal(ex$dict_id, c("D1", "D2", "D3"))
    expect_setequal(
        ex$dict_name,
        c("Meier GmbH", "Meier AG", "Meier Holding")
    )
    expect_equal(sum(ex$is_representative), 1L)
})

test_that("on_duplicate = 'error' restores the strict behaviour", {
    queries <- data.frame(
        query_id = "q1", company_name = "Meier",
        stringsAsFactors = FALSE
    )

    expect_error(
        suppressWarnings(suppressMessages(
            match_companies(queries, dup_dict(), on_duplicate = "error")
        )),
        "duplicate normalized name"
    )
})

test_that("duplicate query names and ids are handled", {
    dict <- data.frame(
        orbis_id = c("D1", "D2"),
        company_name = c("BMW AG", "Siemens AG"),
        stringsAsFactors = FALSE
    )

    # Same name twice, and a repeated id: every input row still gets a row out.
    queries <- data.frame(
        query_id = c("q1", "q1", "q2"),
        company_name = c("BMW", "BMW", "Siemens"),
        stringsAsFactors = FALSE
    )

    res <- quiet_match(queries, dict, threshold_jw = 0.75)

    expect_equal(nrow(res), 3L)
    expect_equal(nrow(res[query_id == "q1"]), 2L)
    expect_true(all(res[query_id == "q1"]$dict_id == "D1"))
})

test_that("names that normalize away are dropped, not matched to each other", {
    # Both dictionary rows normalize to "" -- without the guard they would
    # "exactly match" any query that also normalizes to "".
    dict <- data.frame(
        orbis_id = c("D1", "D2"),
        company_name = c("Holding Group", "GmbH"),
        stringsAsFactors = FALSE
    )
    queries <- data.frame(
        query_id = c("q1", "q2"),
        company_name = c("Group", "BMW"),
        stringsAsFactors = FALSE
    )

    expect_error(
        suppressWarnings(suppressMessages(match_companies(queries, dict))),
        "No usable dictionary rows"
    )
})

test_that("normalization can be disabled per side", {
    dict <- data.frame(
        orbis_id = c("D1", "D2"),
        company_name = c("meier", "bmw"),
        stringsAsFactors = FALSE
    )
    queries <- data.frame(
        query_id = "q1",
        company_name = "Meier GmbH",
        stringsAsFactors = FALSE
    )

    # Dictionary already clean, queries still messy.
    res <- quiet_match(queries, dict, dict_norm = FALSE, query_norm = TRUE)
    expect_equal(res$dict_id, "D1")
    expect_equal(res$match_type, "Perfect")

    # Neither side normalized: "Meier GmbH" no longer matches "meier" exactly.
    res2 <- quiet_match(
        queries, dict,
        dict_norm = FALSE, query_norm = FALSE, threshold_jw = 0.99
    )
    expect_equal(nrow(res2), 0L)
})

test_that("a mixed-language register matches end-to-end", {
    dict <- data.frame(
        orbis_id = c("D1", "D2", "D3", "D4"),
        company_name = c(
            "M\u00fcller Handels GmbH",          # de
            "Soci\u00e9t\u00e9 G\u00e9n\u00e9rale S.A.",  # fr
            "Ferrero S.p.A.",                    # it
            "The Boring Company LLC"             # en
        ),
        stringsAsFactors = FALSE
    )
    queries <- data.frame(
        query_id = c("q1", "q2", "q3", "q4"),
        company_name = c(
            "MUELLER HANDELS",       # umlaut spelled out
            "Societe Generale",      # accents dropped
            "FERRERO SPA",           # dots dropped
            "Boring Company"         # article dropped
        ),
        stringsAsFactors = FALSE
    )

    res <- quiet_match(queries, dict, threshold_jw = 0.9)

    expect_equal(nrow(res), 4L)
    expect_true(all(res$match_type == "Perfect"))
    expect_equal(res[query_id == "q1"]$dict_id, "D1")
    expect_equal(res[query_id == "q2"]$dict_id, "D2")
    expect_equal(res[query_id == "q3"]$dict_id, "D3")
    expect_equal(res[query_id == "q4"]$dict_id, "D4")
})

test_that("lang is threaded through to the normalizer", {
    dict <- data.frame(
        orbis_id = "D1", company_name = "Casa Roma",
        stringsAsFactors = FALSE
    )
    queries <- data.frame(
        query_id = "q1", company_name = "Sa Casa di Roma",
        stringsAsFactors = FALSE
    )

    # With Romance rules "sa"/"di" are stripped, so this is an exact match.
    res <- quiet_match(queries, dict, lang = c("fr", "it"), threshold_jw = 0.99)
    expect_equal(res$match_type, "Perfect")

    # With German rules only, they survive and the strings differ.
    res_de <- quiet_match(queries, dict, lang = "de", threshold_jw = 0.99)
    expect_equal(nrow(res_de), 0L)
})

test_that("an empty result keeps the full schema", {
    res <- quiet_match(
        data.frame(query_id = "z1", company_name = "Zzzzqqqx"),
        data.frame(orbis_id = "D1", company_name = "Aaaawwwy"),
        threshold_jw = 0.99, threshold_rarity = 2
    )

    expect_equal(nrow(res), 0L)
    expect_true(all(
        c(
            "query_id", "query_name", "dict_id", "dict_name",
            "match_type", "n_dict_ids", "dict_id_all"
        ) %in% names(res)
    ))
})

test_that("dict_crosswalk errors when the attribute is absent", {
    expect_error(dict_crosswalk(data.frame(a = 1)), "No dictionary crosswalk")
})
