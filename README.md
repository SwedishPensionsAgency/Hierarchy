Hierarchy
=========

The Hierarchy package aims to simplify the work with hierarchical data structures in R.

## Todo

- S4: subclass of data frame
- Validate (add this to S4 constructor, so that it validates the format)
- Hid delimiter (allow "-", ".", etc.)
- to_json: converts object to an json string.
- aggreggate: calculates the sum of all children

## Data structure

E.g.

    { ---- Variable ---- }                  { ----- Dimensions ------ }             { --- Metrics --- }
    Hid               Name                  Year                  Month             Weight
    1                 KPI                   2012                  12
    1.1               Inventarier           2012                  12
    1.1.1             Möbler                2012                  12
    1.1.1.1           Matbord               2012                  12                1.48
    1.1.1.2           Säng                  2012                  12                2.90
    1.1.1.3           Taklampa              2012                  12                2.21
    1.2               Hälso- och sjukvård   2012                  12
    1.2.1             Läkemedel             2012                  12
    1.2.1.1           Sjukvårdsartiklar     2012                  12                0.98
    1.2.1.2           Naturläkemedel        2012                  12                3.03


   
