## Accumulate messages for later display
## Internal function, not exported
## severity: 1=critical, 2=informative, may lead to misinterpretation of data, 3=minor, esp. those that might have resulted from selective post-processing of combo codes
##collect_messages <- function(msgs,msg_text,line_nums,raw_lines,severity,fatal=FALSE) {
##    if (missing(line_nums)) line_nums <- NA
##    if (missing(raw_lines)) raw_lines <- "[unknown]"
##    if (missing(severity)) severity <- NA
##    vt <- rep(NA_integer_,length(line_nums))
##    if (!missing(raw_lines)) vt <- video_time_from_raw(raw_lines)
##    if (fatal) {
##        lnt <- as.character(line_nums)
##        lnt[is.na(lnt)] <- "[unknown]"
##        txt <- paste0("line ",lnt,": ",msg_text," (line in file is: \"",raw_lines,"\")")
##        if (fatal) stop(paste(txt,collapse=" / "))
##    } else {
##        msgs[[length(msgs)+1]] <- list(file_line_number=line_nums,video_time=vt,message=msg_text,file_line=raw_lines,severity=severity)
##    }
##    msgs
##}

is.notempty.string <- function(x) {
    (is.character(x) && length(x)==1) && !is.na(x) && nchar(x)>0
}

## equality with NAs considered false
`%eq%` <- function(x,y) x==y & !is.na(x) & !is.na(y)

## convenience function to replace NAs
na.replace <- function(x,replace_with) {x[is.na(x)] <- replace_with; x}

most_common_value <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

