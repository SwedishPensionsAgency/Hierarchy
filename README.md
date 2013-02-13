Hierarchy
=========

 *** Work in progress ***

The Hierarchy package aims to simplify the work with hierarchical data structures in R. 
It is inspired by [Ancestry](https://github.com/stefankroes/ancestry) - a Ruby and Rails gem/plugin by Stefan Henzen.
Hierarchy is created and maintained by Thomas Reinholdsson (<reinholdsson@gmail.com>).


## Installation

Use `devtools` for easy installation

    library(devtools)
    install_github('Hierarchy', 'reinholdsson')

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

    a <- Hierarchy:::path_enum$new(cpi, metrics = c("weight", "consumption"))
    
#### Examples of method calls

Get node from id

    a$node("1.1")

Check if it has a parent

    a$has_parent("1.1")

Get children ids

    a$children_ids("1.1")

Get children

    a$children("1.1")

Get all descendants

    a$descendants("1.1")

Get endnodes (the end points of the subtree)

    a$endnodes("1.1")
    
Sum of endnodes:

    a$aggregate("1.1", function(x) sum(x, na.rm = TRUE))

    # id      name                weight    consumption
    # 1.1     Inventarier         6.59      65000


## Development

The *master* branch is the development branch, and might therefore be a bit instable. Stable releases are marked with tags, e.g. v1.0, where the first number represents a new stable release and the second number imply new bug fixes within the given release version.


## License

Hierarchy is licensed under the AGPLv3, the terms of which are included in the file LICENSE.
