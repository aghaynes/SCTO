#' Convert internal id to patient id.
#'
#' This function converts the internal id \code{mnppid} used in most tables
#' to the real patient id (\code{mnpaid}).
#'
#' @param mnppid internal id
#' @return patient id
#' @export
#' @examples
#' \dontrun{
#' mnppid2mnpaid(978) # 10001
#' }
#' @seealso mnpaid2mnppid
mnppid2mnpaid <- function(mnppid) {
  patient <- get("patient") # declare table
  patient$mnpaid[match(mnppid, patient$mnppid)]
}

#' Convert patient id to internal id.
#'
#' This function converts the patient id \code{mnpaid}
#' to the internal id (\code{mnppid}) used in most tables.
#'
#' @param mnpaid patient id
#' @return internal id
#' @export
#' @examples
#' \dontrun{
#' mnpaid2mnppid(10001) # 978
#' }
#' @seealso mnppid2mnpaid
mnpaid2mnppid <- function(mnpaid) {
  patient <- get("patient") # declare table
  patient$mnppid[match(mnpaid, patient$mnpaid)]
}

#' Helper function to strip away center tag.
#'
#' Example: "Universitätsspital Basel (SWISS-AF)"
#'
#' @param x center string
#' @return cleaned center string
#' @seealso mnppid2center
remove.center.tag <- function(x) {
  x <- as.character(x)
  pos <- which(strsplit(x, "")[[1]]=="(")
  if(length(pos) == 1) x <- trim(substr(x, 1, pos-1))
  x
}


#' Convert center id to center.
#'
#' This function returns the center for a given internal id \code{mnppid}.
#'
#' @param mnppid internal id
#' @return Center name as string.
#' @export
mnppid2center <- function(mnppid) {
  center <- get("center") # declare table
  patient <- get("patient") # declare table
  ## first convert mnppid to mnpctrid
  mnpctrid <- patient$mnpctrid[match(mnppid, patient$mnppid)]
  ## then convert center id to center and remove center tag
  as.factor(unlist(lapply(center$mnpctrname[match(mnpctrid, center$mnpctrid)], remove.center.tag)))
}


#' Add patient id.
#'
#' This function adds the patient id \code{mnpaid}
#' to the specified table.
#'
#' @param tab table to add center
#' @param id variable name (default: center)
#' @return Table with center add after mnppid.
#' @export
#' @seealso mnppid2mnpaid
add.pat.id <- function(tab, id="pat.id") {
  tab[[id]] <- mnppid2mnpaid(tab$mnppid)
  tab <- move.column.after(tab, id, "first")
  tab
}

#' Add center.
#'
#' This function adds the center information to the specified table.
#' The column is added after \code{pat.id} if present and after
#' \code{mnppid} otherwise.
#'
#' @param tab table to add patient id
#' @param id patient id name (default: pat.id)
#' @return Table with additional center column.
#' @export
#' @seealso mnppid2mnpaid
add.center <- function(tab, id="center") {
  tab[[id]] <- mnppid2center(tab$mnppid)
  if("pat.id" %in% names(tab)) tab <- move.column.after(tab, id, "pat.id")
  else tab <- move.column.after(tab, id, "mnppid")
  tab
}
