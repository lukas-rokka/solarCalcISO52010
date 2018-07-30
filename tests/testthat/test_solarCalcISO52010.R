# test_data.csv holds climate data from the ISO 52010 standard,
# as well as validation cases based on ISO_FDIS_52010_SS_ISO-TC163-SC2-WG15_N0075_20160705.xlsm
# station DRYCOLD.TMY (Denver (Col, USA)): latitude = 39.76, longitude = -104.86, time zone = -7
# I_tot_s1: azimuth=90, tilt angle=90
# I_tot_s2: azimuth=-90, tilt angle=90
# I_tot_s3: azimuth=-35, tilt angle=0
# I_tot_s4: azimuth=45, tilt angle=30

context("read test data")

df <- read.csv("test_data.csv.gz")
df$timestamp = seq.POSIXt(as.POSIXct("2014-01-01 01:00", tz="Etc/GMT+7"), 
                          as.POSIXct("2015-01-01 00:00", tz="Etc/GMT+7"), by="hour")
df$albedo = 0.2

context("test add_dayOfYear_hourOfDay()")

df1 <- add_dayOfYear_hourOfDay(df)

test_that("time related variables matches with validation data", {
  expect_equal(df1$n_hour, df$n_hour)
  expect_equal(df1$n_day, df$n_day)
})


context("test tidyISO52010()")

df2 <- solarCalcISO52010:::tidyISO52010(
  df1, 
  lat=39.76, lng=-104.86, tz=-7, t_shift=0.5, 
  surfaceAzimuths = c(90, -90, -35, 45), surfaceTilts = c(90, 90, 0, 30),
  interp_perez = 0) # the validation data is calc with binned Perez data
df2$I_tot_s1 = df2$I_tot_dir_s1 + df2$I_tot_dif_s1
df2$I_tot_s2 = df2$I_tot_dir_s2 + df2$I_tot_dif_s2
df2$I_tot_s3 = df2$I_tot_dir_s3 + df2$I_tot_dif_s3
df2$I_tot_s4 = df2$I_tot_dir_s4 + df2$I_tot_dif_s4
df2$alpha_sol = df2$alpha_sol*180/pi

test_that("solar altidue (alpha_sol) matches with validation data", {
  expect_true(mean(abs(df2$alpha_sol - df$alpha_sol)) < 0.03)
})


test_that("total solar irradiance matches with validation data", {
  # There are some deviation due to small differences in numerical precision between 
  # this calculation and the validation data calculated with Excel. 
  # Espically the "eps", used for the Perez table, is sensitive. 
  # An small change in "eps" can result in up to 10 % deviations in I_tot. 
  expect_true(abs(mean(df2$I_tot_s1 - df$I_tot_s1)) < 0.03)
  expect_true(abs(mean(df2$I_tot_s2 - df$I_tot_s2)) < 0.03)
  expect_true(abs(mean(df2$I_tot_s3 - df$I_tot_s3)) < 0.03)
  expect_true(abs(mean(df2$I_tot_s4 - df$I_tot_s4)) < 0.03)
})

# df_15m <- camsRad::cams_get_radiation(60, 16, "2014-01-01", "2014-12-31", "PT15M")
# 
# df_15m <- solarCalcISO52010:::tidyISO52010(
#   df_15m,
#   lat=60, lng=16, tz=0, t_shift=0.125, 
#   surfaceAzimuths = c(90, -90), surfaceTilts = c(90, 90),
#   albedo=0.2, interp_perez = 1,
#   col_G_dir = "BNI", col_G_dif = "DHI"
#  )
# 
# df_15m$I_tot_s1 = df_15m$I_tot_dir_s1 + df_15m$I_tot_dif_s1
# df_15m$I_tot_s2 = df_15m$I_tot_dir_s2 + df_15m$I_tot_dif_s2
# 
