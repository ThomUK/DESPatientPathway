#' Run the model simulation
#'
#' @param model_config list. A config object containing variables for model adjustment
#'
#' @return simulation
#' @export
#' @noRd
#'
run_sim <- function(model_config) {
  # read the model config to a short variable name
  mc <- model_config()

  # Time unit  = weeks
  env <- simmer::simmer("pathway")

  #### create the distribution functions ####
  ## continuous distributions ##
  # patient arrivals
  rate <- mc$pat_referral_rate * 12 / 52 # monthly -> annual, then calculate patients per week
  dist_patient_arrival <- function() rexp(1, rate)
  # dist_patient_arrival()

  # initial backlog of patients
  dist_starting_backlog <- simmer::at(rep(0, mc$pat_backlog_size))
  # dist_starting_backlog()

  dist_pre_op_ward_los <- function() rexp(1, 1 / (mc$pre_op_los / 7)) # days to weeks
  # dist_pre_op_ward_los()

  dist_post_op_ward_los <- function() rexp(1, 1 / (mc$post_op_los / 7)) # days to weeks
  # dist_post_op_ward_los()

  ## discrete distributions ##
  # OP did not attend (DNA) rate. 0 = attended, 1 = did not attend
  dist_op_dna <- function() sample(0:1, 1, FALSE, c(100 - mc$op_dna_rate, mc$op_dna_rate))
  dist_op_dna()

  # OP outcoming result. 1 = admit to wl, 2 = OP followup, 3 = discharged
  op_disch_rate <- (100 - mc$op_admit_rate - mc$op_fup_rate)

  dist_op_outcome <- function() sample(1:3, 1, FALSE, c(mc$op_admit_rate, mc$op_fup_rate, op_disch_rate))
  # dist_op_outcome()

  # create the patient pathway branches
  branch_op_dna <- simmer::trajectory("op did not attend") |>
    # the dna consumes the same clinic resource as an attendance
    simmer::log_("OP: DNA") |>
    simmer::set_attribute("OP appt DNA, patient rebooked", 1) |>
    simmer::timeout(1 / mc$op_clinic_slots) |>
    simmer::release("OP Clinic", 1) |>
    simmer::rollback("op_clinic") # rollback to tagged resource

  branch_discharge_from_op <- simmer::trajectory("discharged from OP appt") |>
    simmer::log_("OP outcome: Discharge") |>
    simmer::set_attribute("OP outcome: discharged home", 1)

  branch_followup_later <- simmer::trajectory("book OP followup") |>
    simmer::log_("OP outcome: Follow-up later") |>
    simmer::set_attribute("OP outcome: followup booked", 1) |>
    simmer::timeout(4) |> # followup in 4 weeks
    simmer::rollback("op_clinic") # rollback to tagged resource

  branch_admit <- simmer::trajectory("admit for treatment") |>
    simmer::log_("OP outcome: Admit") |>
    simmer::set_attribute("OP outcome: admit for treatment", 1) |>
    # take a pre-op bed
    simmer::seize("Bed") |>
    simmer::log_("Pre-op bed") |>
    simmer::set_attribute("IP moved to pre-op bed", 1) |>
    simmer::timeout(dist_pre_op_ward_los) |>
    simmer::release("Bed") |>
    # operate
    simmer::seize("Theatre") |>
    simmer::log_("Theatre") |>
    simmer::set_attribute("IP moved to theatre", 1) |>
    simmer::timeout(1 / mc$theatre_slots) |>
    simmer::release("Theatre") |>
    # take a recovery ward bed
    simmer::seize("Bed") |>
    simmer::log_("Post-op bed") |>
    simmer::set_attribute("IP moved to post-op bed", 1) |>
    simmer::timeout(dist_post_op_ward_los) |>
    simmer::release("Bed") |>
    simmer::log_("IP discharged") |>
    simmer::set_attribute("IP discharged home", 1)


  # create the overall patient pathway
  patient_trajectory <- simmer::trajectory("patient pathway") |>
    #  log_("Referred in") |>
    ## add an intake activity
    simmer::seize("OP Clinic", 1, tag = "op_clinic") |>

    # create a branch to model OP DNAs
    simmer::branch(
      dist_op_dna,
      continue = FALSE,
      branch_op_dna
    ) |>

    # if no DNA, continue with the OP appointment
    simmer::log_("OP: Attended") |>
    simmer::set_attribute("OP appt attended", 1) |>
    simmer::timeout(1 / mc$op_clinic_slots) |>
    simmer::release("OP Clinic", 1) |>

    # branch into admission and discharge
    simmer::branch(
      dist_op_outcome,
      continue = FALSE,
      branch_admit,
      branch_followup_later,
      branch_discharge_from_op
    )


  sim <- env |>
    simmer::add_resource("OP Clinic", capacity = 1) |>
    simmer::add_resource("Bed", mc$total_beds) |>
    simmer::add_resource("Theatre", capacity = 1) |>
    simmer::add_generator("backlog patient ", patient_trajectory, dist_starting_backlog, mon = 2) |>
    simmer::add_generator("new patient ", patient_trajectory, dist_patient_arrival, mon = 2)

  env |> simmer::run(mc$forecast_length)

  res <- list(
    sim = sim,
    patient_trajectory = patient_trajectory
  )
  return(res)
}
