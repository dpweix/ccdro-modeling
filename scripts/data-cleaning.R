library("tidyverse")
theme_set(theme_bw())
library("here")
library("lubridate")


# Clean Data --------------------------------------------------------------

# Names of raw data files
file_names <- 
  here("data") |> 
  list.files() |> 
  str_subset("Both.csv")

# Reorder 10th experiment
file_names[11] <- file_names[2]
file_names <- file_names[-2]

# Import raw data
dat_raw <-
  file_names |> 
  map(\(x) {
    read_csv(here("data", x), skip = 1)
  })

# Create clean data frames
dat_cln <-
  dat_raw |> 
  map(\(x) {
    x |> 
      transmute(
        date_time = mdy_hms(paste(Date, Time)),
        p_f = `Approx. Feed Pump Power Consumption (kW)`,
        p_r = `Approx. Recirc. Pump Power Consumption (kW)`,
        f_p = `Permeate Flow (L/min)`,
        f_r = `Reject Flow (L/min)`,
        c_p = `Permeate Conductivity (uS/cm)`,
        c_r = `Reject Conductivity (mS.cm)`,
        ph_f = `Feed pH`,
        t_f = `Feed Temperature (C)`)
  })

# Save all clean data as list of data frames
saveRDS(dat_cln, file = here("data", "open-loop.rds"))

# Save each open loop experiment as a csv
1:10 |> 
  walk(\(i) {
    write_csv(dat_cln[[i]],file = here("data", paste0("open-loop-", i, ".csv")))
  })

 
