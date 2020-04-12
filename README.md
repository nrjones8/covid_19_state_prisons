
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covid\_19\_state\_prisons

<!-- badges: start -->

<!-- badges: end -->

## What’s in this repo?

The goal of covid\_19\_state\_prisons is to automate the collection of
data from various state Department of Corrections into one repository.
Specifically, we focused on collecting data related to the number of
people who have been tested for NCoV-19 within state correctional
facilities as well as historical data regarding state facilities. As a
result, this repo has data pertaining to the following

  - Number of incarcerated people tested in correctional facilities by
    state (If data is available by the state’s DOC)
  - Number of incarcerated people who tested positive for NCoV-19 in
    correctional facilities by state
  - Number of incarcerated people who tested negative for NCoV-19 in
    correctional facilities by state
  - Historical Population data for state prisons (if available).
  - Web scrapers built to extract data from varying state DOCs

While we make every effort to ensure that the information scraped is
accurate and updated everyday, there are the following caveats. First,
not all state DOC’s update their information everyday or even every
business day. As a result, while the scraper for data pertaining to
NCoV-19 is run daily, the data retrieved by the scraper may not reflect
daily changes. While we make every effort to ensure that states who do
not update their covid data daily are clearly marked, this all
ultimately depends on the frequency that a state’s department of
correction updates their data.

## Special thanks

This repository is the product of the collaboration of the following
people: [Jacob Kaplan](https://github.com/jacobkap), [Connor
Concannon](https://github.com/concannon), [Aaron
Littman](https://github.com/amlittman), [Hima
Lakkaraju](https://github.com/lvhimabindu), [Wes
Weaver](https://github.com/wesweaver), and Oren Gur
