test_that("end ranges calculate as expected", {
  QEnd1 <- quarter_end(2000, 1)
  QEnd2 <- quarter_end(2000, 2)
  QEnd3 <- quarter_end(2000, 3)
  QEnd4 <- quarter_end(2000, 4)

  expect_equal(QEnd1, lubridate::ymd('2000-04-01'))
  expect_equal(QEnd2, lubridate::ymd('2000-07-01'))
  expect_equal(QEnd3, lubridate::ymd('2000-10-01'))
  expect_equal(QEnd4, lubridate::ymd('2001-01-01'))
})

test_that("begin ranges calculate as expected", {
  QB1 <- quarter_begin(2000, 1)
  QB2 <- quarter_begin(2000, 2)
  QB3 <- quarter_begin(2000, 3)
  QB4 <- quarter_begin(2000, 4)

  expect_equal(QB1, lubridate::ymd('2000-01-01'))
  expect_equal(QB2, lubridate::ymd('2000-04-01'))
  expect_equal(QB3, lubridate::ymd('2000-07-01'))
  expect_equal(QB4, lubridate::ymd('2000-10-01'))
})
