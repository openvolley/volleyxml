## iStatVball 2 | SoloStats Live | AOC VBStats | Volleyball Ace


#' Read XML volleyball scouting file
#'
#' Read XML volleyball scouting file
#' 
#' @param filename string: name of the file to read 
#'
#' @return volleyxml object (list with components meta, plays, and raw)
#'
#' @examples
#' x <- vx_read(vx_example_file())
#'
#' @export
vx_read <- function(filename) {
    assert_that(is.string(filename))
    x <- read_xml(filename)
    xraw <- as.character(x)
    ##xml_find_all(x, "//d1:session") %>% as.character
    ##[1] "<session>\n  <startTimeUtc>2018-03-17 14:11:16.25 +1000</startTimeUtc>\n  <dataProvider>\n    <name>iStatVball 2</name>\n    <version>2.19.4.2</version>\n  </dataProvider>\n  <teams>\n    <team>\n      <key>1</key>\n      <value>First VI 2018</value>\n    </team>\n    <team>\n      <key>2</key>\n      <value>TGS</value>\n    </team>\n  </teams>\n</session>"
    ## match metadata
    meta <- list()
    ## match date
    this <- xml_find_all(x, "//d1:session/d1:startTimeUtc")
    assert_that(length(this)==1)
    dt <- lubridate::ymd_hms(xml_text(this))
    meta$match <- tibble(date=as.Date(dt), time=dt)
    ## teams
    tms <- xml_find_all(x, "//d1:teams/d1:team")
    assert_that(length(tms)==2)
    meta$teams <- bind_rows(lapply(1:2, function (tm) {
        tibble(team_id=xml_text(xml_find_first(tms[[tm]], ".//d1:key")),
               team=xml_text(xml_find_first(tms[[tm]], ".//d1:value")))
    }))
    meta$teams$home_away_team <- c("*", "a") ## assume team 1 is home team
    ## play by play
    plays <- xml_find_all(x, "//d1:moment")
    play2df <- function(p) {
        ts <- xml_find_all(p, ".//d1:startTimeMs")
        assert_that(length(ts)==1)
        ts <- as.numeric(xml_text(ts))
        tgs <- as_list(xml_find_all(p, ".//d1:tags"))
        assert_that(length(tgs)==1)
        tgs <- tgs[[1]]
        pdf <- list(video_time=ts/1000)
        for (tg in tgs) {
            this_key <- tg$key[[1]]
            this_value <- tg$value[[1]]
            if (is.null(this_key) || is.null(this_value)) stop("null")
            pdf[[this_key]] <- this_value
                                        #switch(this_key,
                                        #       type=
            ## type values: attack, beginMatch, beginSet, block, dig, endMatch, endSet, freeBall, pass, scoreAdjustment, serve, set, substitution timeout
            ## rotation: 1-6 (setter pos?)
            ## playerJersey: player number
            ## quality: 0-3
            ## amount: 1 (is for score adjustments)
            ## result: ace, assist, block, error, kill
            ## playerIn, playerOut: jersey numbers
            ## team1Score, team2Score: these seem to be populated only on endSet and endMatch, not per point
            ##   but can infer team scores from type scoreAdjustment (see team and amount columns)            
        }
        as_tibble(pdf)
    }
    plays <- bind_rows(lapply(plays, play2df))
    plays <- dplyr::rename(plays, skill="type", player_number="playerJersey", evaluation_code="quality", evaluation="result") ##setter_position="rotation",
    plays <- dplyr::mutate_at(plays, c("amount", "team1Score", "team2Score"), .funs=as.integer)
    plays$player_id <- plays$player_number ## use player number as player_id in the absence of anything else
    ## validations
    ## all team entries must be "1", "2", or NA
    assert_that(all(plays$team %in% c("1", "2", NA_character_)))
    ## postprocess plays frame
    ## track scoreAdjustment to give team scores
    home_team_score <- visiting_team_score <- set_number <- rep(NA_integer_, nrow(plays))
    saidx <- plays$skill %eq% "scoreAdjustment"
    bsidx <- plays$skill %eq% "beginSet"
    setnum <- NA_integer_
    for (ri in seq_len(nrow(plays))[-1]) {
        if (bsidx[ri]) {
            home_team_score[ri] <- 0L
            visiting_team_score[ri] <- 0L
            setnum <- if (is.na(setnum)) 1L else setnum+1L
        } else if (saidx[ri]) {
            ## was score adjustment row
            this_team <- plays$team[ri]
            if (this_team %eq% "1") {
                home_team_score[ri] <- home_team_score[ri-1]+plays$amount[ri]
                visiting_team_score[ri] <- visiting_team_score[ri-1]
            } else if (this_team %eq% "2") {
                visiting_team_score[ri] <- visiting_team_score[ri-1]+plays$amount[ri]
                home_team_score[ri] <- home_team_score[ri-1]
            } else {
                warning("scoreAdjustment not associated with valid team identifier")
            }
        } else {
            home_team_score[ri] <- home_team_score[ri-1]
            visiting_team_score[ri] <- visiting_team_score[ri-1]
        }
        set_number[ri] <- setnum
    }
    plays$home_team_score <- home_team_score
    plays$visiting_team_score <- visiting_team_score
    plays$set_number <- set_number
    ## should now be finished with scoreAdjustment rows and the amount column
    plays <- plays[!plays$skill %eq% "scoreAdjustment", ]
    if (!all(is.na(plays$amount))) {
        warning("non-NA entries in amount column that are not associated with scoreAdjustment")
    } else {
        plays <- plays[, setdiff(names(plays), "amount")] ## drop amount col
    }
    ## substitute team names into team col
    plays$team[plays$team %eq% "1"] <- meta$teams$team[1]
    plays$team[plays$team %eq% "2"] <- meta$teams$team[2]
    plays$home_team <- meta$teams$team[1]
    plays$visiting_team <- meta$teams$team[2]
    plays$row_id <- seq_len(nrow(plays))
    ## update meta with sets won
    temp <- group_by(dplyr::filter(plays, !is.na(.data$set_number)), .data$set_number)
    temp <- dplyr::summarize(temp, home_team_score=as.integer(max(.data$home_team_score, na.rm=TRUE)), visiting_team_score=as.integer(max(.data$visiting_team_score, na.rm=TRUE)))
    meta$result <- temp
    temp <- mutate(temp, won_by_home=case_when(.data$home_team_score>.data$visiting_team_score~TRUE,
                                                  .data$visiting_team_score>.data$home_team_score~FALSE,
                                                  TRUE~NA))
    meta$teams$sets_won <- c(sum(temp$won_by_home, na.rm=TRUE), sum(!temp$won_by_home, na.rm=TRUE))
    meta$teams$won_match <- if (diff(meta$teams$sets_won)<0) c(TRUE, FALSE) else if (diff(meta$teams$sets_won)>0) c(FALSE, TRUE) else c(NA, NA)
    ## add match id
    temp <- meta$match
    temp$home_team <- meta$teams$team[meta$teams$home_away_team=="*"]
    temp$visiting_team <- meta$teams$team[meta$teams$home_away_team=="a"]
    meta$match_id <- digest(temp)
    ## double check that any entries in team1Score and team2Score match our score columns
    tsok <- TRUE
    chk1 <- mutate(dplyr::filter(plays, .data$skill %eq% "endSet" & !is.na(.data$team1Score)), ok=.data$team1Score %eq% .data$home_team_score)
    chk2 <- mutate(dplyr::filter(plays, .data$skill %eq% "endSet" & !is.na(.data$team2Score)), ok=.data$team2Score %eq% .data$visiting_team_score)
    chk <- c(chk1$row_id[!chk1$ok], chk2$row_id[!chk2$ok])
    if (length(chk)>0) {
        tsok <- FALSE
        warning("score mismatches at row(s): ", paste(chk, collapse=", "))
    }
    chk <- dplyr::filter(plays, .data$skill %eq% "endMatch")
    if (nrow(chk)<1) {
        warning("missing endMatch row")
    } else if (nrow(chk)>1) {
        warning("multiple endMatch rows")
    } else {
        if (!chk$team1Score %eq% meta$teams$sets_won[1] || !chk$team2Score %eq% meta$teams$sets_won[2]) {
            tsok <- FALSE
            warning("match score in endMatch row conflicts with sum of set wins in play-by-play data")
        }
    }
    ## any other team1Score, team2Score entries?
    chk <- dplyr::filter(plays, !.data$skill %in% c("endSet", "endMatch") & (!is.na(.data$team1Score) | !is.na(.data$team2Score)))
    if (nrow(chk)>0) {
        tsok <- FALSE
        warning("team1Score or team2Score entries in unexpected row(s): ", paste(chk$row_id, collapse=", "))
    }
    if (tsok) {
        ## drop team1Score, team2Score cols
        plays <- plays[, setdiff(names(plays), c("team1Score", "team2Score"))]
    }
    colorder <- c("row_id", "video_time", "set_number", "home_team", "visiting_team", "team", "rotation", "player_id", "player_number", "skill", "evaluation_code", "evaluation", "home_team_score", "visiting_team_score")
    colorder <- c(colorder, setdiff(colorder, names(plays)))
    plays <- plays[, colorder]
    out <- list(meta=meta, plays=plays, raw=xraw)
    class(out) <- c("volleyxml", class(out))
    out
}


#' Extract the play-by-play data component from a volleyxml object
#'
#' Extract the play-by-play data component from a volleyxml object
#'
#' @param x volleyxml: a volleyxml object as returned by \code{vx_read}
#'
#' @return The plays component of x (a data.frame)
#'
#' @seealso \code{\link{vx_read}}
#'
#' @examples
#' \dontrun{
#'   x <- vx_read(vx_example_file())
#'   head(plays(x))
#' }
#' @export
plays <- function(x) {
    if (!(inherits(x, "volleyxml"))) stop("x must be a volleyxml object")
    x$plays
}

#' A simple summary of a volleyball match
#'
#' @param object volleyxml: volleyxml object as returned by \code{vx_read}
#' @param ... : additional arguments (currently these have no effect)
#'
#' @return list of summary items
#'
#' @seealso \code{\link{vx_read}}
#' @examples
#' x <- vx_read(vx_example_file())
#' summary(x)
#'
#' @method summary volleyxml
#' @export
summary.volleyxml <- function(object, ...) {
    out <- list(date=object$meta$match$date) ##,league=object$meta$match$league)
    out$teams <- object$meta$teams[, c("team", "sets_won")] ##"coach","assistant",
    temp <- object$meta$result$home_team_score>object$meta$result$visiting_team_score
    out$set_scores <- as.data.frame(object$meta$result[,c("home_team_score","visiting_team_score")])
    ## make extra sure that set_scores has home team assigned correctly
    if (object$meta$teams$home_away_team[1]!="*") { out$set_scores <- out$set_scores[,2:1] }
    out$set_scores <- na.omit(out$set_scores)
    class(out) <- "summary.volleyxml"
    out
}

#' Print method for summary.volleyxml
#'
#' @param x summary.volleyxml: a summary.volleyxml object as returned by \code{summary.volleyxml}
#' @param ... : additional arguments (currently these have no effect)
#' @seealso \code{\link{summary.volleyxml}}
#' @method print summary.volleyxml
#' @export
print.summary.volleyxml <- function(x, ...) {
    out <- sprintf("Match summary:\nDate: %s\n",x$date)
    out <- sprintf("%sTeams: %s\n       vs\n       %s\n",out,x$teams$team[1],x$teams$team[2])
    out <- sprintf("%sResult: %d-%d (%s)\n",out,x$teams$sets_won[1],x$teams$sets_won[2],paste(x$set_scores[,1],x$set_scores[,2],sep="-",collapse=", "))
    cat(out)
    invisible(out)
}
