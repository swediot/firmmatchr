test_that("normalization works basic", {
    expect_equal(normalize_company_name("Acme GmbH"), "acme")
    expect_equal(normalize_company_name("  Bad Space  "), "bad space")
})

test_that("normalization handles german characters", {
    expect_equal(normalize_company_name("M\u00fcller AG"), "mueller")
    expect_equal(normalize_company_name("Gro\u00dfhandel"), "grosshandel")
    expect_equal(normalize_company_name("Sch\u00e4fer & S\u00f6hne"), "schaefer soehne")
})

test_that("normalization removes legal forms", {
    expect_equal(normalize_company_name("Tech Limited"), "tech")
    expect_equal(normalize_company_name("Service SpA"), "service")
    expect_equal(normalize_company_name("Holding Group"), "") # only stop words
})

test_that("french conventions are handled", {
    # Accented legal forms are recognised (transliteration runs first).
    expect_equal(normalize_company_name("Soci\u00e9t\u00e9 G\u00e9n\u00e9rale"), "generale")
    expect_equal(normalize_company_name("Nestl\u00e9 S\u00e0rl"), "nestle")
    expect_equal(normalize_company_name("\u00c9tablissements Dupont et Cie"), "dupont")
    expect_equal(
        normalize_company_name("Cr\u00e9dit Agricole SAS Succursale"),
        "credit agricole"
    )
})

test_that("italian conventions are handled", {
    expect_equal(normalize_company_name("Ditta Rossi S.p.A."), "rossi")
    expect_equal(
        normalize_company_name("Societ\u00e0 Cooperativa Bianchi S.r.l."),
        "bianchi"
    )
    expect_equal(
        normalize_company_name("Gruppo Ferrero S.p.A. Italia"),
        "ferrero italia" # country kept: it identifies as often as it pads
    )
})

test_that("english conventions are handled", {
    expect_equal(normalize_company_name("The Boring Company LLC"), "boring")
    expect_equal(normalize_company_name("Apple Inc."), "apple")
    expect_equal(normalize_company_name("Acme Holdings PLC International"), "acme")
})

test_that("dotted acronyms are joined, not split", {
    # "S.A.R.L." must become the token "sarl" so it is stripped as a legal
    # form, rather than four stray letters.
    expect_equal(normalize_company_name("Dupont S.A.R.L."), "dupont")
    expect_equal(normalize_company_name("Rossi S.p.A."), "rossi")
    expect_equal(normalize_company_name("G.m.b.H. Sch\u00e4fer"), "schaefer")
    # A dot after a multi-letter token is still just a separator.
    expect_equal(normalize_company_name("Siemens GmbH & Co. KG"), "siemens")
})

test_that("apostrophes join rather than split", {
    # "Loreal" is far more common than "Oreal" as the apostrophe-free spelling,
    # so the elided article must be kept.
    expect_equal(normalize_company_name("L'Or\u00e9al"), "loreal")
    expect_equal(normalize_company_name("L'Or\u00e9al"), normalize_company_name("Loreal"))
    expect_equal(normalize_company_name("McDonald's"), "mcdonalds")
    expect_equal(normalize_company_name("Banca d'Italia"), "banca ditalia")
    # Typographic apostrophe behaves like the ASCII one.
    expect_equal(
        normalize_company_name("L\u2019Or\u00e9al"),
        normalize_company_name("L'Or\u00e9al")
    )
})

test_that("identifying words are not stripped", {
    expect_equal(normalize_company_name("Deutsche Bank AG"), "deutsche bank")

    # Country names are identity as often as filler, so they are kept: these
    # would otherwise collapse to "credit" and "air", which can exact-match
    # unrelated firms.
    expect_equal(normalize_company_name("Credit Suisse AG"), "credit suisse")
    expect_equal(normalize_company_name("Air France"), "air france")

    # Structural branch markers are still removed.
    expect_equal(
        normalize_company_name("Bosch Niederlassung Deutschland"),
        "bosch deutschland"
    )
    expect_equal(normalize_company_name("Dupont Succursale"), "dupont")
})

test_that("extra_stopwords strips corpus-specific filler", {
    expect_equal(
        normalize_company_name("Toyota Deutschland GmbH", extra_stopwords = "deutschland"),
        "toyota"
    )
    # Applied on top of the built-ins, case-insensitively.
    expect_equal(
        normalize_company_name("Acme Standort Zurich", extra_stopwords = c("ZURICH")),
        "acme"
    )
    # No-ops are safe.
    expect_equal(normalize_company_name("BMW AG", extra_stopwords = NULL), "bmw")
    expect_equal(normalize_company_name("BMW AG", extra_stopwords = character(0)), "bmw")
    expect_equal(normalize_company_name("BMW AG", extra_stopwords = c("", NA)), "bmw")
})

test_that("lang narrows which conventions apply", {
    # "sa" and "di" are Romance tokens; German-only leaves them alone.
    expect_equal(normalize_company_name("Sa Casa di Roma", lang = "de"), "sa casa di roma")
    expect_equal(normalize_company_name("Sa Casa di Roma", lang = c("fr", "it")), "casa roma")

    # Without "de", umlauts fall back to plain transliteration.
    expect_equal(normalize_company_name("M\u00fcller", lang = "en"), "muller")
    expect_equal(normalize_company_name("M\u00fcller", lang = "de"), "mueller")

    expect_error(normalize_company_name("x", lang = "es"))
})

test_that("cross-language spellings converge", {
    same <- function(a, b) {
        expect_equal(normalize_company_name(a), normalize_company_name(b))
    }
    same("Nestl\u00e9 S.A.", "NESTLE SA")
    same("Soci\u00e9t\u00e9 G\u00e9n\u00e9rale", "Societe Generale S.A.")
    same("Ferrero S.p.A.", "FERRERO SPA")
    same("M\u00fcller GmbH", "MUELLER Gmbh")
})

test_that("edge cases are safe", {
    expect_identical(normalize_company_name(character(0)), character(0))
    expect_true(is.na(normalize_company_name(NA_character_)))
    expect_equal(normalize_company_name(""), "")
    expect_equal(normalize_company_name("   "), "")
    # Vectorized
    expect_equal(
        normalize_company_name(c("BMW AG", "Apple Inc.")),
        c("bmw", "apple")
    )
})

test_that("company_name_stopwords reports what is stripped", {
    all_sw <- company_name_stopwords()
    expect_true(all(c("lang", "type", "token") %in% names(all_sw)))
    expect_setequal(unique(all_sw$lang), c("de", "fr", "it", "en"))
    expect_setequal(unique(all_sw$type), c("legal_form", "noise"))

    it_only <- company_name_stopwords("it")
    expect_setequal(unique(it_only$lang), "it")
    expect_true("srl" %in% it_only$token)
    expect_false("gmbh" %in% it_only$token)

    # Every listed token must actually normalize away.
    expect_true(all(normalize_company_name(all_sw$token) == ""))
})
