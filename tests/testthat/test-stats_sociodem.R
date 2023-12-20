# manually created data
dat <- data.frame(candidate = c("CandidateA", "CandidateB", "CandidateC", "CandidateD"),
                  parties = c("PartyA", "PartyB", "PartyA", "PartyC"),
                  gender = c("Male", "Female", "Female", "Female"))

# result of applying function
outdat <- stats_sociodem(dat, gender, parties)

# manual reconstruction of expected result
outdat_expected  <- tibble::tibble(gender = c("Female", "Male"),
                                   PartyA_n = as.integer(c(1, 1)),
                                   PartyA_per = c(50, 50),
                                   PartyB_n = as.integer(c(1, NA)),
                                   PartyB_per = c(100, NA),
                                   PartyC_n = as.integer(c(1, NA)),
                                   PartyC_per = c(100, NA),
                                   all_n = as.integer(c(3, 1)),
                                   all_per = c(75, 25))

# two versions of the same test
testthat::expect_true(identical(outdat, outdat_expected))
testthat::expect_identical(outdat, outdat_expected)
