#' Get team names from volleyxml object
#'
#' @param x volleyxml or data.frame: a volleyxml object as returned by \code{vx_read}, or the plays component of that object
#'
#' @return character vector of team names
#'
#' @seealso \code{\link{vx_read}}
#' 
#' @examples
#' \dontrun{
#'   x <- vx_read(vx_example_file())
#'   teams(x)
#' }
#' @export
teams <- function(x) {
    if ("meta" %in% names(x)) {
        x$meta$teams$team
    } else {
        c(na.omit(unique(x$home_team)), na.omit(unique(x$visiting_team)))
    }
}

#' @rdname teams
#' @export
home_team <- function(x) {
    if ("meta" %in% names(x)) {
        x$meta$teams$team[x$meta$teams$home_away_team=="*"]
    } else {
        na.omit(unique(x$home_team))
    }
}

#' @rdname teams
#' @export
visiting_team <- function(x) {
    if ("meta" %in% names(x)) {
        x$meta$teams$team[x$meta$teams$home_away_team=="a"]
    } else {
        na.omit(unique(x$visiting_team))
    }
}
