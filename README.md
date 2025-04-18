# mongoTable::mongoTable()

This R function generates one-dimensional and two-dimensional frequency tables from data stored in a MongoDB database. Rather than retrieving the entire dataset through a `find()` operation, this function leverages MongoDB's aggregation capabilities to compile frequency data directly on the server, followed by post-processing in R.

## Usage
```R
mongoTable(connection, x, y = NULL, query = "{}",lowerize = FALSE, limit = NULL, sort = FALSE, decreasing = TRUE)
```

## Arguments

| Name | Description|
| :--- | :--- |
| connection   | character. A mongo connection object initiated with mongolite::mongo().|
| x            |  character. A field variable for which frequencies should be counted.|
| y            |  character. An optional second field variable for which frequencies should be counted.|
| query        |  character. An optional MongoDB query for data subset selection (e.g.: ’{\"year\": 2024}’). |
| lowerize     | logical. All levels in one dimensional tables will be lowerized. |
| limit     | integer. Defines the maximum length/dimensions of output.|
| sort       |  logical. If TRUE, the output is sorted by frequency.|
| decreasing  | logical. If TRUE and sort==TRUE, the output is returned with decreasing frequencies. If TRUE and sort==FALSE, level names are returned in decreasing manner.|

## Installation with the devtools package
```R
if(require(devtools)!=TRUE) install.packages("devtools")
devtools::install_github("ingmarboeschen/mongoTable")
```

## Examples
```R
## use mongolite::mongo() to connect to a MongoDB instance (demo server)
mon <- mongolite::mongo("mtcars", url = "mongodb+srv://readwrite:test@cluster0-84vdt.mongodb.net/test")
if(mon$count() > 0) mon$drop()
mon$insert(mtcars)
stopifnot(mon$count() == nrow(mtcars))

## Create a one-dimensional frequency table
# for all x
mongoTable(connection = "mon", x = "cyl")

# for all x matching a query (cars with mpg greater than 20)
mongoTable(connection="mon", x="cyl", query = '{\"mpg\": {\"$gt": 20}}')

## Create a two-dimensional frequency table
# for all x and y
mongoTable(con = "mon", x = "cyl", y = "gear")

# for all x and y matching a query (cars with mpg greater than 20)
mongoTable(con="mon", x = "cyl", y = "gear", query = '{\"mpg\": {\"$gt": 20}}')

```
