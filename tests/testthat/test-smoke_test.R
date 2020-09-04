# test_df loaded from fixtures data

test_that("create report", {
  create_report(test_df, "Alice", tempfile())
})
