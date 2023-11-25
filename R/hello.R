library(tidyverse)
library(simmer)
library(simmer.plot)

set.seed(1)

# Time unit  = minutes
if(exists("env")) env |> reset()
env <- simmer("pathway")

# create the distribution functions
# patient arrivals
patients_per_month <- 400
rate <- patients_per_month * 12 / 365 / 24 / 60
dist_patient_arrival <- function() rexp(1, rate)
#dist_patient_arrival()

# initial backlog of patients
dist_starting_backlog <- at(rep(0,1000)) # 1000 patients backlog
#dist_starting_backlog()

dist_pre_op_ward_los <- function() rnorm(1, 60*12, sd = 6)
#dist_pre_op_ward_los()

dist_operating_time <- function() rnorm(1, 90, sd = 6)
#dist_operating_time()

dist_post_op_ward_los <- function() rnorm(1, 60*36, sd = 6)
#dist_post_op_ward_los()

# OP outcoming result. 1 = admit to wl, 2 = OP followup, 3 = discharged
dist_op_outcome <- function() sample(1:3, 1, FALSE, c(0.10, 0.225, 0.675))
#dist_op_outcome()

# create some schedules to close resources overnight
op_clinic_schedule <- schedule(
  c(60*8, 60*16),
  c(1, 0),
  period = 60*24
)
theatre_schedule <- schedule(
  c(60*8, 60*16),
  c(1, 0),
  period = 60*24
)

# create the patient pathway branches
branch_discharge_from_op <- trajectory("discharged from OP appt") |>
  set_attribute("discharged home", 1) |>
  log_("Discharged from OP appt")

branch_followup_later <- trajectory("book OP followup") |>
  set_attribute("OP_fup_booked", 1) |>
  log_("Follow-up OP appt booked") |>
  rollback("op clinic") # rollback to tagged resource

branch_admit <- trajectory("admit for treatment") |>
  set_attribute("admitted_for_treatment", 1) |>

  # take a pre-op bed
  set_attribute("moved_to_pre_op_bed", 1) |>
  seize("bed") |>
  timeout(dist_pre_op_ward_los) |>
  release("bed") |>

  # operate
  set_attribute("moved_to_theatre", 1) |>
  seize("theatre") |>
  timeout(dist_operating_time) |>
  release("theatre") |>
  log_("Im recovering") |>

  # take a recovery ward bed
  set_attribute("moved_to_post_op_bed", 1) |>
  seize("bed") |>
  timeout(dist_post_op_ward_los) |>
  release("bed") |>
  set_attribute("discharged home", 1) |>
  log_("Discharged from bed")


# create the overall patient pathway
patient <- trajectory("patient pathway") |>
#  log_("Referred in") |>
  ## add an intake activity
  seize("OP clinic", 1, tag = "op clinic") |>
  timeout(function() rnorm(1, mean = 30, sd = 6)) |>
  release("OP clinic", 1) |>

  # branch into admission and discharge
  branch(dist_op_outcome, FALSE,
         branch_admit,
         branch_followup_later,
         branch_discharge_from_op)


sim <- env |>
  add_resource("OP clinic", op_clinic_schedule, mon = 2) |>
  add_resource("bed", 2, mon = 2) |> # 2 beds
  add_resource("theatre", capacity = theatre_schedule, queue_size = 0, mon = 2) |>
  add_generator("backlog patient", patient, dist_starting_backlog, mon = 2) |>
  add_generator("new patient", patient, dist_patient_arrival, mon = 2)

env |> run(60*24*5)#365*5) # 60 * 24 * 365 = 1 year

sim_arrivals <- sim |>
  get_mon_arrivals(per_resource = TRUE) |>
#  get_mon_arrivals() |>
  arrange(start_time)

sim_attributes <- sim |>
  get_mon_attributes()

sim_resources <- sim |>
  get_mon_resources()

plot(patient, verbose = TRUE)

plot(sim_resources, metric = "usage", items = c("server", "queue"), steps = FALSE)
#plot(sim_resources, metric = "usage", items = c("server", "queue"), steps = TRUE)
# plot(sim_resources, metric = "usage", items = "server", steps = TRUE)
#plot(sim_resources, metric = "usage", items = "queue", steps = TRUE)
#plot(sim_resources, metric = "utilization")

#plot(sim_attributes)

# plot(sim_arrivals, metric = "activity_time")
# plot(sim_arrivals, metric = "waiting_time")
# plot(sim_arrivals, metric = "flow_time")

# time spent in system
# ggplot(sim_arrivals, aes((end_time - start_time)/60/24)) +
#   geom_histogram() +
#   xlab("Time spent in the system (days)") +
#   ylab("Number of patients")
