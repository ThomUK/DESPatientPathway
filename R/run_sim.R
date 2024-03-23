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
  env <- simmer("pathway")

  #### create the distribution functions ####
  ## continuous distributions ##
  # patient arrivals
  rate <- mc$pat_referral_rate * 12 / 52 # monthly -> annual, then calculate patients per week
  dist_patient_arrival <- function() rexp(1, rate)
  # dist_patient_arrival()

  # initial backlog of patients
  dist_starting_backlog <- at(rep(0, mc$pat_backlog_size))
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
  branch_op_dna <- trajectory("op did not attend") |>
    # the dna consumes the same clinic resource as an attendance
    log_("OP: DNA") |>
    set_attribute("OP appt DNA", 1) |>
    timeout(1 / mc$op_clinic_slots) |>
    release("OP Clinic", 1) |>
    rollback("op_clinic") # rollback to tagged resource

  branch_discharge_from_op <- trajectory("discharged from OP appt") |>
    log_("OP outcome: Discharge") |>
    set_attribute("discharged home", 1)

  branch_followup_later <- trajectory("book OP followup") |>
    log_("OP outcome: Follow-up later") |>
    set_attribute("OP_fup_booked", 1) |>
    rollback("op_clinic") # rollback to tagged resource

  branch_admit <- trajectory("admit for treatment") |>
    log_("OP outcome: Admit") |>
    set_attribute("admitted_for_treatment", 1) |>
    # take a pre-op bed
    seize("Bed") |>
    log_("Pre-op bed") |>
    set_attribute("moved_to_pre_op_bed", 1) |>
    timeout(dist_pre_op_ward_los) |>
    release("Bed") |>
    # operate
    seize("Theatre") |>
    log_("Theatre") |>
    set_attribute("moved_to_theatre", 1) |>
    timeout(1 / mc$theatre_slots) |>
    release("Theatre") |>
    # take a recovery ward bed
    seize("Bed") |>
    log_("Post-op bed") |>
    set_attribute("moved_to_post_op_bed", 1) |>
    timeout(dist_post_op_ward_los) |>
    release("Bed") |>
    log_("IP discharged") |>
    set_attribute("discharged home", 1)


  # create the overall patient pathway
  patient <- trajectory("patient pathway") |>
    #  log_("Referred in") |>
    ## add an intake activity
    seize("OP Clinic", 1, tag = "op_clinic") |>

    # create a branch to model OP DNAs
    branch(
      dist_op_dna,
      continue = FALSE,
      branch_op_dna
    ) |>

    # if no DNA, continue with the OP appointment
    log_("OP: Attended") |>
    set_attribute("OP appt attended", 1) |>
    timeout(1 / mc$op_clinic_slots) |>
    release("OP Clinic", 1) |>

    # branch into admission and discharge
    branch(
      dist_op_outcome,
      continue = FALSE,
      branch_admit,
      branch_followup_later,
      branch_discharge_from_op
    )


  sim <- env |>
    add_resource("OP Clinic", capacity = 1, mon = 2) |>
    add_resource("Bed", mc$total_beds, mon = 2) |>
    add_resource("Theatre", capacity = 1, mon = 2) |>
    add_generator("backlog patient", patient, dist_starting_backlog, mon = 2) |>
    add_generator("new patient", patient, dist_patient_arrival, mon = 2)

  env |> run(mc$forecast_length)

  res <- list(
    sim = sim,
    patient = patient
  )
  return(res)
}
