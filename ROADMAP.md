# Development Roadmap

## Before GDG meeting 7/11/24

1. Finish implementing player name fuzzy search.
2. Add all the possible query options targeting the Statcast endpoint.
    - Use a computation expression to provide a type-safe interface.
3. Implement at least one of the HTML datasources, probably league batting stats.
4. Think about ad-hoc data analysis and storage; a few options:
    - [Deedle](https://fslab.org/Deedle/): preferred
    - [DuckDB](https://duckdb.org/): backup

## Later

1. Add the rest of the HTML datasources.
2. Add visualization support.
3. Build the library for production use and publish it to NuGet.
4. Refine data analysis needs and update libraries accordingly.
5. Make a dashboard app to demonstrate features of the library.
