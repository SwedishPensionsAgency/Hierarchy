Hierarchy
=========

The Hierarchy package aims to simplify the work with hierarchical data structures in R.

## Todo

- S4: subclass of data frame
- Validate (add this to S4 constructor, so that it validates the format)
- Hid delimiter (allow "-", ".", etc.)
- to_json: converts object to an json string
- aggregate: calculates the sum of all children
- Implement regexp support in aggregate function

## Example

Let's say that we have a data frame of the following structure:

    { - Id - }  { ---- Labels ---- }  { - Dimensions - }    { --- Metrics --- }
    Id          Name                  Year       Month      Weight  Consumption
    1           KPI                   
    1.1         Inventarier          
    1.1.1       Möbler               
    1.1.1.1     Matbord               2012      12          1.48    40000
    1.1.1.2     Säng                  2012      12          2.90    20000
    1.1.1.3     Taklampa              2012      12          2.21    5000
    1.2         Hälso- och sjukvård  
    1.2.1       Läkemedel             
    1.2.1.1     Sjukvårdsartiklar     2012      12          0.98    10000
    1.2.1.2     Naturläkemedel        2012      12          3.03    12000


Then we can group them within an hierarchical object:

    h <- Hierarchy(data, 
                   id = "Id",
                   labels = "Name",
                   dimensions = c("Year", "Month"), 
                   metrics = c("Weight", "Consumption"))


And thus, calculate the aggregate sum of all children of an hierarchical object:

    aggregate(h, id = "1.1", "sum")
    
    Id      Name                Year      Month       Weight    Consumption
    1.1     Inventarier         2012      12          5,11      65000
    
It is also possible to aggregate several objects at the same time (with *):

    aggregate(h, id = "1.*", "sum")
    
    Id      Name                Year      Month       Weight    Consumption
    1.1     Inventarier         2012      12          5,11      65000
    1.2     Hälso- och sjukvård 2012      12          4.01      22000
    
    
