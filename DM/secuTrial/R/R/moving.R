## ----------------------------------------------------------------------
## FUNCTIONS FOR MOVING COLUMNS
## ----------------------------------------------------------------------

#' Move a column in a data frame by index.
#'
#' This function moves a given column (defined by the column index)
#' to a specific position in the data frame.
#'
#' @param df data frame
#' @param col.idx old column index
#' @param new.col.idx new column index
#' @return Data frame with shuffled columns.
#' @export
#' @seealso move.column.after
#' @examples
#' move.column.to.pos(data.frame(a=NA, b=NA, c=NA),1,3)
#' ##   b  c  a
#' ##1 NA NA NA
#' @author Pascal Benkert
move.column.to.pos <- function(df, col.idx, new.col.idx) {
  ## assertions
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(col.idx%%1==0) ## check for integer
  assertthat::assert_that(new.col.idx%%1==0)
  assertthat::assert_that(col.idx <= ncol(df))
  assertthat::assert_that(new.col.idx <= ncol(df))
  ## move to begin
  if(new.col.idx == 1) {
    df <- df[, c(col.idx, (1:ncol(df))[-col.idx])]
  }
  ## move to end
  else if(new.col.idx == ncol(df)) {
    df <- df[, c((1:ncol(df))[-col.idx],col.idx)]
  }
  else {
    ## move forward
    if(new.col.idx<col.idx) {
      df <- df[, c(1:new.col.idx-1,col.idx, (new.col.idx:ncol(df))[(new.col.idx:ncol(df)) != col.idx])]
    }
    ## move backwards
    else {
      df <- df[, c((1:new.col.idx)[-col.idx], col.idx , ((new.col.idx+1):ncol(df)))]
    }
  }
  return(df)
}

## ----------------------------------------------------------------------

#' Move a column in a data frame after a specified column.
#'
#' This function moves a given column or a vector of columns (defined by column name(s))
#' after a specific position in the data frame (defined by column name).
#'
#' @param df data frame
#' @param col.name column name or vector of column names to move
#' @param col.name.after column name after which to place \code{col.name} ("first" to place at beginning)
#' @return Data frame with shuffled columns.
#' @export
#' @seealso move.column.to.pos
#' @examples
#' move.column.after(data.frame(a=NA, b=NA, c=NA, d=NA),c("c","d"),"a")
#' ##   a  c  d  b
#' ##1 NA NA NA NA
#' @author Pascal Benkert
move.column.after <- function(df, col.name, col.name.after) {
  ## assertions
  assertthat::assert_that(is.data.frame(df))
  if(!(col.name.after %in% names(df)) & col.name.after != "first") stop(paste("Unknown column",col.name.after))
  if(!is.na(match(col.name.after, col.name))) stop("Reference column cannot be moved.")
  ## iterate from last to first to keep order
  range <- 1:length(col.name)
  for(i in range) {
    ## index of col.name
    col.idx <- match(col.name[i], names(df))
    if(is.na(col.idx)) stop(paste("Unknown column",col.name[i]))
    ## index to move (one after col.name.after)
    new.col.idx <- 1 + (i-1)
    ref.col.idx <- match(col.name.after, names(df)) + (i-1)
    if (col.name.after != "first") new.col.idx <- ref.col.idx +  as.numeric(col.idx > ref.col.idx) # last term handles artefact in move.column.to.pos that reference is move in different diretions depending on whether col comes from position   before or after ref
    df <- move.column.to.pos(df, col.idx, new.col.idx)
  }
  df
}

