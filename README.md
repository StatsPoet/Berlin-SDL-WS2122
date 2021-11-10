Deep and Statistical Learning WS 21-22
================

# Analysis of Berlins Airbnb Data using statistical and deep learning methods.

## Data

-   [Visual representation](http://insideairbnb.com/berlin)
-   [Get the data](http://insideairbnb.com/get-the-data.html)

Data last downloaded 10.11.21

## Possible analisis:

### Fundamentals:

[Taken from the about page of Insider
Airbnb](http://insideairbnb.com/about.html)

By analyzing publicly available information about a city’s Airbnb’s
listings, Inside Airbnb provides filters and key metrics so you can see
how Airbnb is being used to compete with the residential housing market.

With Inside Airbnb, you can ask fundamental questions about Airbnb in
any neighbourhood, or across the city as a whole. Questions such as:

-   “How many listings are in my neighbourhood and where are they?”
-   “How many houses and apartments are being rented out frequently to
    tourists and not to long-term residents?”
-   “How much are hosts making from renting to tourists (compare that to
    long-term rentals)?”
-   “Which hosts are running a business with multiple listings and where
    they?”

The tools are presented simply, and can also be used to answer more
complicated questions, such as:

-   “Show me all the highly available listings in Bedford-Stuyvesant in
    Brooklyn, New York City, which are for the ‘entire home or
    apartment’ that have a review in the last 6 months AND booked
    frequently AND where the host has other listings.”

These questions (and the answers) get to the core of the debate for many
cities around the world, with Airbnb claiming that their hosts only
occasionally rent the homes in which they live.

In addition, many city or state legislation or ordinances that address
residential housing, short term or vacation rentals, and zoning usually
make reference to allowed use, including:

-   how many nights a dwelling is rented per year
-   minimum nights stay
-   whether the host is present
-   how many rooms are being rented in a building
-   the number of occupants allowed in a rental whether the listing is
    licensed

The Inside Airbnb tool or data can be used to answer some of these
questions.

``` r
library(dplyr)
library(here)
library(stringr)
library(data.table)
library(scoringutils)
library(purrr)
library(tidyr)
```
