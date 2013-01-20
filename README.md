Hierarchy
=========

The purpose of this package is to simplify the work with hierarchical data structures in R.

TODO:
- S4: subclass of data frame
- to_json: converts object to an json string.
- aggreggate: calculates the sum of all children

## Data structure

E.g.

                      { ----- attributes ----- }  
    Hid               Name                  Year       Value
    1                 KPI                   2012
    1.1               Inventarier           2012
    1.1.1             Möbler                2012
    1.1.1.1           Matbord               2012       1.48
    1.1.1.2           Säng                  2012       2.90
    1.1.1.3           Taklampa              2012       2.21
    1.2               Hälso- och sjukvård   2012
    1.2.1             Läkemedel             2012
    1.2.1.1           Sjukvårdsartiklar     2012       0.98
    1.2.1.2           Naturläkemedel        2012       3.03


   
