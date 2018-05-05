#' Example data files
#'
#' Example data files provided as part of the volleyxml package
#'
#' @param choice numeric: which data file to return?
#' \itemize{
#'   \item{1 - a largely fictitious example}
#' }
#' @return path to the file
#'
#' @seealso \code{\link{vx_read}}
#'
#' @examples
#' myfile <- vx_example_file()
#' x <- vx_read(myfile)
#' summary(x)
#' 
#' @export
vx_example_file <- function(choice=1) {
    assert_that(is.numeric(choice))
    switch(as.character(choice),
           "1"=system.file("extdata/example_data.xml",package="volleyxml"),
           stop("unrecognized 'choice' value (", choice, ")")
           )
}

# @references The example data files came from \url{http://}
