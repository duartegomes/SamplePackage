
#' @title Add Calculated Columns to data chunks
#' @description This function allows users to add new columns to data chunks by specifying
#' a calculation.  The function builds upon the underlying syntax of R so that users don't need
#' to learn a new syntax. Despite this we will give an introduction to the main syntax here.
#' @export
#' @param Data This is a required field and is usually a TAP chunk but it can be any item of class "data.table" or "data.frame".
#' If the user submits a data frame then this will first be converted to a data table before
#' the calculated column is added.This is because data tables are far more efficient in R than a
#' data frame. The resulting table that is returned by this function will also be a data table even
#' if a data frame was submitted.
#' @param Filter Optional filter to apply to the chunk.  The calculation is only defined on the rows that meet
#' the criteria in this filter.  If you are using this function to change a column that already exists then rows that don't
#' match the filter will not be changed and only rows that match the filter will have a new value placed in the column.
#' If you are using this function to create a new column then rows that don't match the filter will contain NA and the rows
#' that match will have the results of the calculation.
#' @param Calculation This is a required field and is an equation that follows the syntax of R. The Calculation follows the general
#' design of <column_name> := <calculation>. <column_name> can be a new column or an existing column, in which case the existing data will
#' be overwritten. The <calculation> can be stand alone or it can refer to other columns in the data.
#' @param By This is an optional field that allows the <calculation> to be computed for certain groups and then each member of that group
#' will be given the result of the calculation.
#' @param Ret This defaults to TRUE and when set as TRUE the function returns the entire data submitted with the calculated column added. When
#' set to FALSE the function will not return the entire data set.  As long as the original data was submitted as a data table the function
#' will modify the original file to have the calculation added.  This avoids the user's script having to receive and copy large amounts of
#' data and so is more efficient.  There are some examples shown below to demonstrate this functionality.
#' @author JTA - The Data Scientists
#' @details This function allows users to add a new calculated column to a data table or to replace an existing column with a calculated
#' value.
#'
#' @section Parameter Order:
#' The function expects the parameters to be passed in the following order: Data, Filter, Calculation and By.  A common scenario
#' will be to use just Data and Calculation without a filter or a by clause and in this case the system will need to understand that the filter is blank.
#' There are two ways to do this:
#' \tabular{ll}{
#' Leave the filter blank: \tab CalculateColumn(iris, , newcol := 1)\cr
#' Name the parameters:    \tab CalculateColumn(Data = iris, Calculation = newcol := 1)\cr
#' }
#' We recommend always naming the parameters for all TAPChunks functions to avoid errors in
#' interpretation.  The first example that we gave requires a extra comma to specify that filter is blank.
#' The second example does not require the extra comma because we explicitly name the parameters.
#' @section Receiving the result:
#' The function has two mechanisms for returning the results.  It can either send the results
#' back for the user to store or it can just add the calculation to the data without requiring the user
#' to explicitly store it.  This second option will only work when the data is presented as a data table.
#' When data is presented as a data frame the function must return the data.
#' @section Defining the calculation:
#' The key part of the Calculation definition is in the calculation parameter.  This parameter always starts with the name of the column to be altered or
#' created followed by the := operator and then the calculation.  \cr
#' Calculations can contain constants, variable names, column names, operators, functions and parenthesis. \cr
#' The system can manipulate various types of data and there are six elemental (also called \emph{atomic}) data types as follows:\cr
#' \enumerate{
#' \item{\strong{Raw:} This is a single byte value ranging from 0 to 255.  It is often expressed in binary or in hexadecimal. This type is very rarely used as it cannot
#' be manipulated using the standard mathematical functions such as add and subtract.  }
#' \item{\strong{Logical:} This is one of two boolean states - FALSE and TRUE. }
#' \item{\strong{Integer:} This is a 32 bit integer able to hold numbers in the range -2,147,483,647 to 2,147,483,647 inclusive.}
#' \item{\strong{Numeric:} This is a floating point number following the IEC 60559 standard. It uses 53 bit of precision and holds numbers from 2e-308 to 2e+308.}
#' \item{\strong{Complex:} This is a complex number formed of Real and Imaginary parts.  These are both Numeric.}
#' \item{\strong{String:} This is a sequence of characters.}
#' }  \cr
#' \strong{Constants:}\cr
#' Constants may be used as a calculation or in a calculation.  The constant may be any of the \emph{atomic} data types listed above.
#' Here are some examples of calculations that use constants:
#' \preformatted{
#' # We can set a column to be a numeric constant
#' CalculateColumn(iris, Calculation = x := 2.5) }
#' \tabular{rrrrlr}{
#' \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \tab \strong{x}\cr
#' 5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa \tab 2.5\cr
#' 4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa \tab 2.5\cr
#' 4.7 \tab 3.2 \tab 1.3 \tab 0.2 \tab setosa \tab 2.5\cr
#' 4.6 \tab 3.1 \tab 1.5 \tab 0.2 \tab setosa \tab 2.5\cr
#' 5.0 \tab 3.6 \tab 1.4 \tab 0.2 \tab setosa \tab 2.5
#' }
#' \preformatted{
#' # We can set a column to be a string constant
#' CalculateColumn(iris, Filter = Species=="setosa", Calculation = y := "setosa")}
#' \tabular{rrrrll}{
#' \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \tab \strong{y}\cr
#' 5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa \tab setosa\cr
#' 4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa \tab setosa\cr
#' 4.7 \tab 3.2 \tab 1.3 \tab 0.2 \tab setosa \tab setosa\cr
#' 4.6 \tab 3.1 \tab 1.5 \tab 0.2 \tab setosa \tab setosa\cr
#' 5.0 \tab 3.6 \tab 1.4 \tab 0.2 \tab setosa \tab setosa
#' }
#' \preformatted{
#' # We can use a constant in a calculation
#' CalculateColumn(iris, Calculation = x := Sepal.Length * 2.5)}
#' \tabular{rrrrlr}{
#' \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \tab \strong{x}\cr
#' 5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa \tab 12.75\cr
#' 4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa \tab 12.25\cr
#' 4.7 \tab 3.2 \tab 1.3 \tab 0.2 \tab setosa \tab 11.75\cr
#' 4.6 \tab 3.1 \tab 1.5 \tab 0.2 \tab setosa \tab 11.50\cr
#' 5.0 \tab 3.6 \tab 1.4 \tab 0.2 \tab setosa \tab 12.50
#' }
#'
#' \strong{Variable Names:} \cr
#' If you have defined a variable in your script then it may be used in a calculation: \cr
#' \preformatted{
#' #We Define a variable...
#' weight <- 0.2345
#' # ...and use our variable in the calculation
#' CalculateColumn(iris, Calculation = r := Petal.Width * weight)
#' }
#' \tabular{rrrrlr}{
#' \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \tab \strong{r}\cr
#' 5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa \tab 0.0469\cr
#' 4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa \tab 0.0469\cr
#' 4.7 \tab 3.2 \tab 1.3 \tab 0.2 \tab setosa \tab 0.0469\cr
#' 4.6 \tab 3.1 \tab 1.5 \tab 0.2 \tab setosa \tab 0.0469\cr
#' 5.0 \tab 3.6 \tab 1.4 \tab 0.2 \tab setosa \tab 0.0469
#' }
#' \strong{Column Names:} \cr
#' Calculations may refer to columns that already exist in the data submitted to the function.
#' Here are some examples of calculations that use column names
#' \preformatted{
#' # We can copy a column to a new calculation
#' CalculateColumn(iris, Calculation = new_species := Species)}
#' \tabular{rrrrll}{
#' \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \tab \strong{new_species}\cr
#' 5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa \tab setosa\cr
#' 4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa \tab setosa\cr
#' 4.7 \tab 3.2 \tab 1.3 \tab 0.2 \tab setosa \tab setosa\cr
#' 4.6 \tab 3.1 \tab 1.5 \tab 0.2 \tab setosa \tab setosa\cr
#' 5.0 \tab 3.6 \tab 1.4 \tab 0.2 \tab setosa \tab setosa
#' }
#' \preformatted{
#' # We can create calculations combining columns
#' CalculateColumn(iris, Calculation = A := Petal.Length * Petal.Width)
#' }
#' \tabular{rrrrlr}{
#' \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \tab \strong{A}\cr
#' 5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa \tab 0.28\cr
#' 4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa \tab 0.28\cr
#' 4.7 \tab 3.2 \tab 1.3 \tab 0.2 \tab setosa \tab 0.26\cr
#' 4.6 \tab 3.1 \tab 1.5 \tab 0.2 \tab setosa \tab 0.30\cr
#' 5.0 \tab 3.6 \tab 1.4 \tab 0.2 \tab setosa \tab 0.28
#' }
#' \strong{Operators:}\cr
#' We have already shown some examples that use operators to combine pre-existing columns and we now present here the most common operators that you may wish to use
#' in a calculation. They are listed in precedence groups, from highest to lowest and the function will apply higher operations before
#' applying lower operations. For example 1 + 2 * 3 resolves to 7. Parentheses will always overide this behaviour so that (1 + 2) * 3 resolves to 9.  These are the main
#' operators:
#' \tabular{lll}{
#' \strong{Symbols}\tab \strong{Meaning}       \tab \strong{Example}  \cr
#' ^               \tab Exponentiation         \tab 2^10 = 1024       \cr
#' - +             \tab Unary minus and plus   \tab +5 * -5 = -25     \cr
#' * /             \tab Multiply and divide    \tab 25 / 5 * 100 = 500\cr
#' + -             \tab Add and subtract       \tab 10 + 5 - 3 = 12   \cr
#' < > <= >= == != \tab Ordering and comparison\tab                   \cr
#'                 \tab Less Than            : \tab 3 <  10 = TRUE    \cr
#'                 \tab Greater Than         : \tab 3 >  10 = FALSE   \cr
#'                 \tab Less Than or Equal   : \tab 3 <= 10 = TRUE    \cr
#'                 \tab Greater Than or Equal: \tab 3 >= 10 = FALSE   \cr
#'                 \tab Equal                : \tab 3 == 10 = FALSE   \cr
#'                 \tab Not Equal            : \tab 3 != 10 = TRUE    \cr
#' !               \tab Negation               \tab !FALSE = TRUE     \cr
#' &               \tab Logical AND            \tab TRUE & FALSE = FALSE\cr
#' |               \tab Logical OR             \tab TRUE | FALSE 0 TRUE\cr
#' }
#' It is important to understand the concept of coercion when using operators.  In general the operators ^ - + * and / require operands that are numbers; in this context a number is
#' either integer, numeric or complex.  The other operators < > <= >= == != work with numbers and strings.  The system will use coercion to convert the data
#' to try and make sense of other data types. For example TRUE + FALSE is valid.  The system will interpret FALSE and the integer 0 and TRUE as the integer 1.  This means that
#' the system will consider that FALSE < TRUE is a true statement. \cr \cr
#' \strong{Functions:}\cr
#' Calculations may also use functions and these functions may be those that are defined in R or they may be defined by the user.  When using a function it is important
#' to consider the cardinality of the inputs and the result.  Some functions are designed to transform a single value to another single value, for example sqrt(n). In this
#' example when we use the function sqrt the column created will calculate a value for each row.  Other functions, however, take a set of numbers and aggregate them into
#' one single value. When we use these functions the calculation can aggregate the entire column and will place the result in every row of the result.  This is best demonstrated
#' with examples:
#' \preformatted{
#' # Here the function translates a single value to a single value
#' CalculateColumn(iris, Calculation = ROOT := sqrt(Sepal.Length))}
#' \tabular{rrrrlr}{
#' \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \tab \strong{ROOT}\cr
#' 5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa \tab 2.258318\cr
#' 4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa \tab 2.213594\cr
#' 4.7 \tab 3.2 \tab 1.3 \tab 0.2 \tab setosa \tab 2.167948\cr
#' 4.6 \tab 3.1 \tab 1.5 \tab 0.2 \tab setosa \tab 2.144761\cr
#' 5.0 \tab 3.6 \tab 1.4 \tab 0.2 \tab setosa \tab 2.236068
#' }
#' \preformatted{
#' # Here the function aggregates all the input data to a single value
#' CalculateColumn(iris, Calculation = Mean.Sepal.Length := mean(Sepal.Length))}
#' \tabular{rrrrlr}{
#' \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \tab \strong{Mean.Sepal.Length}\cr
#' 5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa \tab 5.843333\cr
#' 4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa \tab 5.843333\cr
#' 4.7 \tab 3.2 \tab 1.3 \tab 0.2 \tab setosa \tab 5.843333\cr
#' 4.6 \tab 3.1 \tab 1.5 \tab 0.2 \tab setosa \tab 5.843333\cr
#' 5.0 \tab 3.6 \tab 1.4 \tab 0.2 \tab setosa \tab 5.843333
#' }
#' Aggregating functions can be used to great effect when combined with grouping the data.  This is explained in the section on grouping data below. \cr
#' For your convenience we have summarised some of the most common functions here:
#' \tabular{lll}{
#' \strong{Function}\tab \strong{Effect} \tab \strong{Notes} \cr
#' abs(x)   \tab Absolute value.\tab\cr
#' diff(x)  \tab Differentiated or iterated differences.\tab\cr
#' grep(s)  \tab Pattern matching on a string.\tab\cr
#' length(x)\tab Number of rows.\tab\cr
#' sign(x)  \tab Signs of the values.\tab\cr
#' order(x) \tab Order the value appears in a sorted list.\tab\cr
#' round(x) \tab Rounding. \tab Also ceiling(x), floor(x), signif(x) and trunc(x).\cr
#' log(x)   \tab Natural logarithm. \tab Also log10(x) and log2(x).\cr
#' exp(x)   \tab Exponential.\tab\cr
#' cos(x)   \tab Cosine. \tab Also sin(x), tan(x), acos(x), asin(x) and atan(x).\cr
#' sqrt(x)  \tab Square root.\tab\cr
#' sum(x)   \tab Sum.\tab\cr
#' cumsum(x)\tab Cumulative Sum.  \tab Also cumprod(x), cummin(x), cummax(x).\cr
#' mean(x)  \tab Arithmetic mean. \tab Also median(x), min(x), max(x), sd(x), var(x).\cr
#' rnorm(x) \tab Random numbers with a Gaussian distribution.\tab\cr
#' runif(x) \tab Random numbers with a uniform distribution.\tab\cr
#' }
#'
#' \strong{Parentheses:}\cr
#' Parentheses may be used to control order of opertions in the calculation.
#' @section Combining Calculations with Filters:
#' The examples given so far have applied the calculation to every row in the submitted data, however a filter can be used to update just a subset of rows.
#' The filter definition follows the same syntax as the main calculation but it must return a logical value of either TRUE or FALSE.  The calculation is
#' then only performed on the rows for which the filter returns TRUE. Here are some examples:
#' \preformatted{
#' file <- CalculateColumn(iris, Filter = Species == "setosa"    , Calculation = Species.Code := 1)
#' file <- CalculateColumn(iris, Filter = Species == "versicolor", Calculation = Species.Code := 2)
#' file <- CalculateColumn(iris, Filter = Species == "virginica" , Calculation = Species.Code := 3)
#' # Here we take the Sepal.Length for those plants that have a Sepal Length below 5
#' # We then compute the sum of those values
#' file <- CalculateColumn(iris, Filter = Sepal.Length < 5, Calculation = w := Sepal.Length)
#'         CalculateColumn(file, Filter = Sepal.Length < 5, Calculation = x := sum(Sepal.Length))
#' }
#' \tabular{rrrrlrr}{
#' \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \tab \strong{w} \tab \strong{x}\cr
#' 5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa \tab  NA \tab    NA\cr
#' 4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa \tab 4.9 \tab 103.2\cr
#' 4.7 \tab 3.2 \tab 1.3 \tab 0.2 \tab setosa \tab 4.7 \tab 103.2\cr
#' 4.6 \tab 3.1 \tab 1.5 \tab 0.2 \tab setosa \tab 4.6 \tab 103.2\cr
#' 5.0 \tab 3.6 \tab 1.4 \tab 0.2 \tab setosa \tab  NA \tab    NA
#' }
#' Note that the system leaves the value NA for rows for which the filter resolved to FALSE. It is important to be aware that when the value NA is used in a calculation
#' it will cause the result to also be NA
#' \preformatted{
#' # Here we take the Sepal.Length for those plants that have a Sepal Length below 5
#' # We then compute the sum of those values but we get an issue with NA values
#' CalculateColumn(iris, Filter = Sepal.Length < 5, Calculation = w := Sepal.Length)
#' CalculateColumn(iris,                          , Calculation = x := sum(w))
#' }
#' \tabular{rrrrlrr}{
#' \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \tab \strong{w} \tab \strong{x}\cr
#' 5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa \tab  NA \tab NA\cr
#' 4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa \tab 4.9 \tab NA\cr
#' 4.7 \tab 3.2 \tab 1.3 \tab 0.2 \tab setosa \tab 4.7 \tab NA\cr
#' 4.6 \tab 3.1 \tab 1.5 \tab 0.2 \tab setosa \tab 4.6 \tab NA\cr
#' 5.0 \tab 3.6 \tab 1.4 \tab 0.2 \tab setosa \tab  NA \tab NA
#' }
#' Most functions have a mechanism to ignore the NA values and in the above example x:=sum(w, na.rm = TRUE) works
#' @section Grouping Calculations:
#' When we wish to perform the calculation for different groups of data we can use the By parameter to do this.  Supose that we want
#' to add a calculated column that has the mean Sepal.Length for each of the species.  We have already seen a way to achieve this using
#' filters and the solution would look like this:
#' \preformatted{
#' # In the following code we calculate the mean for each species
#' # If we convert the file to a data table first the function will automatically keep previous calculations
#' file <- as.data.table(iris)
#' CalculateColumn(file, Filter = Species == "setosa"    , Calculation = m := mean(Sepal.Length))
#' CalculateColumn(file, Filter = Species == "versicolor", Calculation = m := mean(Sepal.Length))
#' CalculateColumn(file, Filter = Species == "virginica" , Calculation = m := mean(Sepal.Length))
#' }
#' \tabular{rrrrlr}{
#' \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \tab \strong{m}\cr
#' 5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa     \tab 5.006\cr
#' 4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa     \tab 5.006\cr
#' 5.5 \tab 2.3 \tab 4.0 \tab 1.3 \tab versicolor \tab 5.936\cr
#' 6.5 \tab 2.8 \tab 4.6 \tab 1.5 \tab versicolor \tab 5.936\cr
#' 6.2 \tab 3.4 \tab 5.4 \tab 2.3 \tab virginica  \tab 6.588\cr
#' 5.9 \tab 3.0 \tab 5.1 \tab 1.8 \tab virginica  \tab 6.588
#' }
#' This solution worked because we have very few members in the Species column so it doesn't hurt to define a calculation line for each member of Species.
#' Sometimes we may have a lot more than the three examples we show in this example and then we should use the By parameter which causes the function to repeat the calculation
#' for each member listed in the By parameter.  Here is an example:
#' \preformatted{
#' CalculateColumn(iris,, mean := mean(Sepal.Length), By = Species)
#' }
#' @family Chunk Manipulators
#' @examples calculateColumn(iris, , newcol := 1)
#' CalculateColumn(Data = iris, Calculation = newcol := 1)

#' @seealso \code{\link{TAPChunks}}

CalculateColumn <- function(Data = NULL, Filter = 1 == 1, Calculation = NULL, By = NULL, Ret = TRUE) {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  f1 <- substitute(Filter)
  if (typeof(f1) != "language") {
    TAPChunks:::catError("Syntax error in the Filter definition.")
  }

  c1 <- substitute(Calculation)
  if (typeof(c1) != "language" | !grepl(":=", deparse(c1))) {
    TAPChunks:::catError("Syntax error in Calculation definition.")
  }

  b1 <- substitute(By)

  e <- try(tb <- typeof(b1), silent = T)
  e <- strsplit(e, ":")[[1]][2]
  if (!exists("tb")) TAPChunks:::catError(paste("Syntax error in the By definition:", e, "."))
  if (!(tb == "symbol" | tb == "NULL")) TAPChunks:::catError("Syntax error in the By definition.")

  if (is.null(b1)) {
    e <- try({
      ret <- Data[eval(f1), eval(c1)][]
      if (Ret) return(ret) else return("OK")
    }
    ,
    silent = T
    )
    e <- strsplit(e, ":")[[1]][2]

    TAPChunks:::catError(paste("Syntax error:", e, "."))
  } else {
    e <- try({
      ret <- Data[eval(f1), eval(c1), eval(b1)][]
      if (Ret) return(ret) else return("OK")
    }
    ,
    silent = T
    )

    e <- strsplit(e, ":")[[1]][2]

    TAPChunks:::catError(paste("Syntax error:", e, "."))
  }
}
