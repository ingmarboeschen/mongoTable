# mongoTable

Contains the function mongoTable() to create one and two dimensional frequency tables on a mongoDB connection initiated with the mongolite package.

## Usage
mongoTable(
connection,
x,
y = NULL,
query = "{}",
lowerize = FALSE,
limit = NULL,
sort = FALSE,
decreasing = TRUE
)

## Arguments
connection   &nbsp; character. A mongo connection object initiated with mongolite::mongo().

x       &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;       character. A field variable for which frequencies should be counted.

y       &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;       character. An optional second field variable for which frequencies should be counted.

query    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;      character. An optional MongoDB query for data subset selection (e.g.: ’{\"year\": 2024}’).

lowerize &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;    logical. All levels in one dimensional tables will be lowerized.

limit  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;    integer. Defines the maximum length/dimensions of output.

sort     &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;    logical. If TRUE, the output is sorted by frequency.

decreasing  &nbsp; logical. If TRUE and sort==TRUE, the output is returned with decreasing frequencies. If TRUE and sort==FALSE, level names are returned in decreasing manner.

## Examples
\# use mongolite::mongo() to connect to a MongoDB instance (demo server)

mon <- mongolite::mongo("mtcars", url = "mongodb+srv://readwrite:test@cluster0-84vdt.mongodb.net/test")

if(mon$count() > 0) mon$drop()

mon$insert(mtcars)

stopifnot(mon$count() == nrow(mtcars))

############################################
## Create a one dimensional frequency table
\# for all x

mongoTable(connection = "mon", x = "cyl")

\# create a one dimensional frequency table for all x matching a query

mongoTable(connection="mon", x="cyl", query = '{\"mpg\": {\"$gt": 20}}')

############################################
## Create a two dimensional frequency table
\# for all x and y

mongoTable(con = "mon", x = "cyl", y = "gear")

\# for all x and y matching a query

mongoTable(con="mon", x = "cyl", y = "gear", query = '{\"mpg\": {\"$gt": 20}}')
