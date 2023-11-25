
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DES patient pathway model

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This patient pathway model is designed to simulate a typical acute
hospital outpatient, admission, treatment, and followup pathways. It
uses Discrete Event Simulation (DES) methodology, and is implemented
using the open-source {simmer} R package.

It is designed to be used by hospital management and clinicians to
investigate service configuration options. It models a single pathway,
and is configurable so that users can experiment with “what if”
questions.

It presents results graphically wherever possible (some of this is still
in development).

## Model configuration options

The model uses a blend of hard-coded assumtions, and a set of
configurable options. Over time (or by request), options that are
currently hard-coded may be made user-configurable.

#### Examples of configurable items (some are under development):

- Number of weeks to forecast ahead (default is 2 years)
- Patient referral (arrival) rate
- Conversion rate (from OP appointment to admission / treatment waiting
  list)
- Outpatient appointment followup rate (what proportion of OP appts are
  new vs. followups)
- Existing service waiting list (backlog size)
- Outpatient clinics:
  - Number of clinics (schedule)
  - OP clinic appointment length
- Pre-operative ward:
  - Number of beds
  - Patient average length-of-stay
- Post-operative ward:
  - Number of beds
  - Patient average length-of-stay
- Operating theatre
  - Operating hours (schedule)
  - Average procedure length

#### Examples of currently hard-coded items:

- Distributions (variation) of length-of-stay, and theatre procedure
  length
- OP clinics and Operating theatres are currently assumed to be the same
  every day (ie. 7 day service). The ability to define weekly capacities
  is planned in due course.  
- Appointment non-attendance (DNA - did not attend) is not currently
  modelled. Attendance is assumed to be 100%

## Installation

You can install the development version of DES patient pathway model
like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library("DES patient pathway model")
## basic example code
```
