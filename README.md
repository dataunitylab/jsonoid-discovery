# JSONoid Discovery

[![CI](https://github.com/michaelmior/jsonoid-discovery/actions/workflows/ci.yml/badge.svg)](https://github.com/michaelmior/jsonoid-discovery/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/michaelmior/jsonoid-discovery/branch/main/graph/badge.svg?token=VhcWABnQhU)](https://codecov.io/gh/michaelmior/jsonoid-discovery)

Schema discovery for JSON using monoids.

Currently this package uses unreleased code via GitHub Packages.
Although all the code is public, you must [create a personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token).
This token should be stored in a file named `.env` in the root of the repository with a line `GITHUB_TOKEN=YOUR_TOKEN_HERE`.
Only the `read:packages` permission should be required.

## Datasets

* [GDP](http://api.worldbank.org/countries/USA/indicators/NY.GDP.MKTP.CD?per_page=5000&format=json)
* [Nobel Prize](http://api.nobelprize.org/v1/prize.json)
* [USGS - Earthquakes](https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson)
* [Rick and Morty characters](https://rickandmortyapi.com/api/character/)
* [TVmaze](https://api.tvmaze.com/singlesearch/shows?q=mr-robot&embed=episodes)
