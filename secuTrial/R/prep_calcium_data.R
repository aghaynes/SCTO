
## calcium dataset
library("lava") # v.1.6.3
## mapvalues()
library("plyr") # v.1.8.4

## todate
## makes a date of the input
todate <- function(x) {
  return(as.Date(x, origin = "1899-12-30"))
}

## round according to commonly accepted rounding
## i.e. 0.5 -> 1 instead of 0
expected_round <- function(num, digits=1) {
  return(round((num+0.00000000000001), digits = digits))
}

## add data
data(calcium)
calcium_st <- calcium

## visit dates 
calcium_st$ctime <- todate(calcium_st$ctime)
calcium_st$ctime <- format(calcium_st$ctime, "%d.%m.%Y")

## adjust col names
names(calcium_st) <- c("bmd.bmd", "bmd.grouping", "patid", "visit", "bmd.age", "visitdate")

## centre
calcium_st$centre <- "Universitätsspital Basel (BMD)"

## add entry date (today)
calcium_st$entrydate <- Sys.Date()
calcium_st$entrydate <- format(calcium_st$entrydate, "%d.%m.%Y")

## round age
calcium_st$bmd.age <- expected_round(calcium_st$bmd.age, digits=5)

## spell grouping out
calcium_st$bmd.grouping <- mapvalues(calcium_st$bmd.grouping, c("C","P"), c("Calcium","Placebo"))

## reorder
calcium_st <- calcium_st[c("patid","centre","entrydate","visitdate","bmd.bmd","bmd.grouping","bmd.age")]

## write data
write.table(calcium_st, "calcium_secuTrial.csv", row.names=F, quote=F, sep=";")

