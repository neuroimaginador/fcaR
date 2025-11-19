# Data for Tourist Destination in Las Vegas

The dataset `vegas` is the binary translation of the Las Vegas Strip
dataset (@moro2017stripping), which records more than 500 TripAdvisor
reviews of hotels in Las Vegas Strip. The uninformative attributes (such
as the user continent or the weekday of the review) are removed.

## Usage

``` r
vegas
```

## Format

A matrix with 504 rows and 25 binary columns. Column names are related
to different features of the hotels:

- Period of Stay:

  4 categories are present in the original data, which produces as many
  binary variables: `Period of stay=Dec-Feb`, `Period of stay=Mar-May`,
  `Period of stay=Jun-Aug` and `Period of stay=Sep-Nov`.

- Traveler type:

  Five binary categories are created from the original data:
  `Traveler type=Business`, `Traveler type=Couples`,
  `Traveler type=Families`, `Traveler type=Friends` and
  `Traveler type=Solo`.

- Pool, Gym, Tennis court, Spa, Casino, Free internet:

  Binary variables for the services offered by each destination hotel

- Stars:

  Five binary variables are created, according to the number of stars of
  the hotel, `Stars=3`, `Stars=3.5`, `Stars=4`, `Stars=4.5` and
  `Stars=5`.

- Score:

  The score assigned in the review, from `Score=1` to `Score=5`.

## Source

Moro, S., Rita, P., & Coelho, J. (2017). Stripping customers' feedback
on hotels through data mining: The case of Las Vegas Strip. Tourism
Management Perspectives, 23, 41-52.
