Hierarchy
=========

 ***Package is under heavy development***

The Hierarchy package aims to simplify the work with hierarchical data structures in R. 
It is inspired by [Ancestry](https://github.com/stefankroes/ancestry) - a Ruby and Rails gem/plugin by Stefan Henzen.
Hierarchy is created and maintained by Thomas Reinholdsson (<reinholdsson@gmail.com>).


## Installation

Use `devtools` for easy installation

```r
library(devtools)
install_github('Hierarchy', 'reinholdsson')
```

## Usage

### Path Enumeration

Let's say that we have a data frame `cpi` of the following structure:

    id          name                  weight  consumption
    1           KPI                   
    1.1         Inventarier          
    1.1.1       Möbler               
    1.1.1.1     Matbord               1.48    40000
    1.1.1.2     Säng                  2.90    20000
    1.1.1.3     Taklampa              2.21    5000
    1.2         Hälso- och sjukvård  
    1.2.1       Läkemedel             
    1.2.1.1     Sjukvårdsartiklar     0.98    10000
    1.2.1.2     Naturläkemedel        3.03    12000


Then we define the hierarchical structure to be a path enumeration 

    a <- path_enum$new(cpi)

And thus, calculate the aggregate sum of all endnodes, given a specific path id:

    a$endnodes_aggregate("1.1", c("weight", "consumption"), sum)
    
    id      name                weight    consumption
    1.1     Inventarier         6.59      65000


## Development

The *master* branch is the development branch, and might therefore be a bit instable. Stable releases are marked with tags, e.g. v1.0, where the first number represents a new stable release and the second number imply new bug fixes within the given release version.


## License

Hierarchy is licensed under the AGPLv3, the terms of which are included in the file LICENSE.
