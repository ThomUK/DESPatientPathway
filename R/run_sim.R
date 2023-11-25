#' Run the model simulation
#'
#' @param model_config list. A config object containing variables for model adjustment
#'
#' @return simulation
#' @export
#' @noRd
#'
run_sim <- function(model_config){

  # read the model config to a short variable name
  mc <- model_config()

  # Time unit  = minutes
  env <- simmer("pathway")

  # create the distribution functions
  # patient arrivals
  rate <- mc$pat_referral_rate * 12 / 365 / 24 / 60 # monthly -> annual, then calculate patients per minute
  dist_patient_arrival <- function() rexp(1, rate)
  #dist_patient_arrival()

  # initial backlog of patients
  dist_starting_backlog <- at(rep(0, mc$pat_backlog_size))
  #dist_starting_backlog()

  dist_pre_op_ward_los <- function() rnorm(1, 60*12, sd = 6)
  #dist_pre_op_ward_los()

  dist_operating_time <- function() rnorm(1, 90, sd = 6)
  #dist_operating_time()

  dist_post_op_ward_los <- function() rnorm(1, 60*36, sd = 6)
  #dist_post_op_ward_los()

  # OP outcoming result. 1 = admit to wl, 2 = OP followup, 3 = discharged
  # mc$op_conversion_rate is the percentage of OP appointments requiring admission
  # mc$op_fup_rate is the percentage of OP appointments which are followups
  scaled_fup_rate <- (1 - mc$op_conversion_rate) * mc$op_fup_rate # scale the rate for a 3-way branch
  scaled_disch_rate <- (1 - mc$op_conversion_rate) * (1 - mc$op_fup_rate) # scale the rate for a 3-way branch

  dist_op_outcome <- function() sample(1:3, 1, FALSE, c(mc$op_conversion_rate, scaled_fup_rate, scaled_disch_rate))
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
    timeout(function() rnorm(1, mean = mc$op_clinic_length, sd = 6)) |>
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

  env |> run(60*24*7 * mc$forecast_length) # 60 * 24 * 7 = 1 week

  return(sim)
}
