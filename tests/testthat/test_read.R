context("file reader")

test_that("metadata functions work", {
    x <- vx_read(vx_example_file(1))
    expect_identical(teams(x), c("Attacking Aardvarks", "Blocking Badgers"))
    expect_identical(teams(plays(x)), c("Attacking Aardvarks", "Blocking Badgers"))
    expect_identical(home_team(x), "Attacking Aardvarks")
    expect_identical(home_team(plays(x)), "Attacking Aardvarks")
    expect_identical(visiting_team(plays(x)), "Blocking Badgers")
    expect_identical(visiting_team(plays(x)), "Blocking Badgers")
})
