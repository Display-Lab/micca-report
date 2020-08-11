# test_df loaded from fixtures data

test_that("main runs", {
  main(test_df, "Alice", tempfile())
})
