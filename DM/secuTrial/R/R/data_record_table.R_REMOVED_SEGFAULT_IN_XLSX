#' Get dataframe from table in secuTrial export directory 
#' 
#' TODO
#'
#' @param exportdir
#' @param table
#' @param columns
#' @param centres
#' @param filetype
#' @return data frame from export table
#' @export
getDataFrameFromExportTable <- function(exportdir, table, columns = NULL, centres = NULL, filetype = "xls") {
  
  if(is.na(table) | length(table) > 1)
    stop("Exactly one table needs to be specified!")
  
  ## CENTERS
  ctr <- read.delim(paste0(exportdir,"/ctr.", filetype), na.strings=c(""))
  ctr$X <- NULL
  ## CONNECTION TABLE
  cn <- read.delim(paste0(exportdir,"/cn.", filetype), na.strings=c(""))
  cn$X <- NULL
  tmp <- merge(cn,ctr,by="mnpctrid", all.x = T)
  
  ## READ TABLE
  dat <- read.delim(file = paste0(exportdir, "/", table, ".", filetype), na.strings=c("","N/A"))
  dat$X <- NULL
  dat <- merge(tmp, dat, by="mnppid", all.x=T)
  
  ## FILTER FOR CENTRES
  if(!is.null(centres))
  {
    dat <- dat[which(dat$mnpctrname %in% centres), ]
  }

  ## FILTER FOR COLUMNS
  if(!is.null(columns)) {
    notfound <- columns[which(!(columns %in%names(dat)))]
    if(length(notfound) > 0)
    {
      warning(paste("Columns not found in export:", notfound))
    }
    
    dat <- dat[,c("mnpaid",columns)]
  }
  
  ## SET PATIENT ID
  names(dat)[names(dat)=="mnpaid"] <- "patient_id"
  
  return(dat)
}

#' Get information on tables specified in data record table
#'
#' TODO
#'
#' @param drt data record table (xls file)
#' @return data frame with information on tables.
#' @export
getTablesFromDRT <- function(drt) 
{
  ## get table prefix
  prefix <- paste0("mnp",tolower(read.xlsx2(drt, 1, startRow=1)[1,2]))
  x <- read.xlsx2(drt, 2, startRow=2)
  
  ## select form and table informations
  x <- x[nchar(as.character(x[,3])) > 0, 2:3]
  
  ## extract subforms
  i <- 1
  while(i <= nrow(x)) 
  {
    tmp <- strsplit(as.character(x[i,2]), ",\n")[[1]]
    if(length(tmp) > 1) 
    {
      y <- expand.grid(x[i,1], tmp)
      
      if(i > 1) {
        y <- rbind(x[1:(i-1), ], y)
      }
      if(i < nrow(x))
      {
        y <- rbind(y, x[(i+1):nrow(x), ])
      }
      x <- y
    }
    i <- i + 1
  }
  
  df <- data.frame(Form=as.character(x[,1]), Table=as.character(x[,2]), Repetition=factor(rep("no",nrow(x)), levels=c("no","yes")), stringsAsFactors = F)
  
  df$Repetition[grep(df$Table, pattern="^emnp")] <- "yes"
  ## cut table prefix (mnpXX,emnpXX)
  df$Table[df$Repetition=="no"] <- substring(df$Table[df$Repetition=="no"], nchar(prefix) + 1)
  df$Table[df$Repetition=="yes"] <- substring(df$Table[df$Repetition=="yes"], nchar(prefix) + 2)

  return(df)
}

#' Get items specified in secuTrial data record table
#' 
#' TODO
#'
#' @param drt data record table (xls file)
#' @param tables the name of a specific table 
#' @return data frame with information on all items.
#' @export
getItemsFromDRT <- function(drt, tables = NULL)
{
  getColIdx <- function(data, colname) 
  {
    as.vector(which(lapply(data, function(x, y=colname) grep(pattern = y, x)) > 0))
  }
  
  ## get all tables from DRT
  tmp <- getTablesFromDRT(drt)
  df <- data.frame(Table=character(), 
                   Form=character(),
                   Question=character(), 
                   Item=character(), 
                   Column=character(), 
                   Type=character(), 
                   Format=character(), 
                   Unit=character())
  ## delete 
  if(!is.null(tables)) {
    idx <- which(!(tmp$Table %in% tables))
    tmp <- tmp[-idx, ]
  }
  tables <- tmp
  
  if(nrow(tables) < 1)
    stop("No table found!")

  ## loop over tables
  for(i in 1:nrow(tables))
  {
      sn <- ifelse(nchar(tables$Form[i])>29, paste0(substr(tables$Form[i],1,28),"."), tables$Form[i]) 
      
        ## read current table/sheet
      x <- try(read.xlsx2(file = drt, sheetName = sn, startRow=1, stringsAsFactors=F), silent = F)
      if(class(x) == "try-error")
        stop(paste("Problems while reading sheet", sn, "from data record table", drt))
      
      ## extract indices of columns
      dbCol <- getColIdx(data=x,colname = "Database column|Datenbankspalte")
      if(length(dbCol) != 1)
        stop(paste(length(dbCol),"indices found for database column"))
      
      questionCol <- getColIdx(data=x,colname = "^Question$|^Frage$")
      if(length(questionCol) != 1)
        stop(paste(length(questionCol),"indices found for question column"))
      
      itemCol <- getColIdx(data=x,colname = "^Item$")
      if(length(itemCol) != 1)
        stop(paste(length(itemCol),"indices found for item column"))
      
      unitCol <- getColIdx(data=x,colname = "Unit./.format|Einheit./.Format")
      if(length(unitCol) != 1)
        stop(paste(length(unitCol),"indices found for unit column"))
      
      optionsCol <- getColIdx(data=x,colname = "lookup.table|Lookuptabelle")
      if(length(optionsCol) != 1)
        stop(paste(length(optionsCol),"indices found for format column"))
      
      typeCol <- getColIdx(data=x,colname = "Item.type|Itemtyp")
      if(length(typeCol) != 1)
        stop(paste(length(typeCol),"indices found for type column"))
      
      idx <- which(nchar(as.character(x[,dbCol])) > 0 & !(x[,dbCol] %in% c("Database column","Datenbankspalte")))
      tmp <- data.frame(Table=rep(tables$Table[i], length(idx)),
                        Form=rep(tables$Form[i], length(idx)),
                        Question=as.character(x[idx,questionCol]), 
                        Item=as.character(x[idx,itemCol]), 
                        Column=as.character(x[idx,dbCol]), 
                        Format=as.character(x[idx,optionsCol]), 
                        Type=simplifyDatatypes(x[idx,typeCol]),
                        Unit=as.character(x[idx,unitCol]), 
                        stringsAsFactors=F)
      
      ## copy questions in corresponding lines
      for(j in 1:nrow(tmp))
      {
        if(nchar(as.character(tmp$Question[j])) > 0)
          lastQ <- tmp$Question[j]
        else
          tmp$Question[j] <- lastQ
      }
      
      ## remove HTML from item name
      tmp$Item <- gsub("<.*?>", "", tmp$Item)
      
      ## extract format informations
      idx <- grep("Number |Date |Date-Interval |Text ", tmp$Type)
      if(length(idx) > 0) 
      {
        for(j in 1:length(idx))
        {
          fmt <-  strsplit(tmp$Type[idx[j]], " ")[[1]]
          tmp$Format[idx[j]] <- paste(fmt[2:length(fmt)], collapse=" ")
          tmp$Type[idx[j]] <- fmt[1]
        }
      }
      tmp$Format <- gsub(tmp$Format, pattern = "\n", replacement = "", fixed = T)
      
      ## add current table to other tables 
      df <- merge(df, tmp, all=T, sort=F)
    }
  return(df)
}

simplifyDatatypes <- function(types) 
{
  types <- gsub(types, pattern = "checked ", replacement = "", fixed = T)
  types <- gsub(types, pattern = " (Date format: hide)", replacement = "", fixed = T)
  types <- gsub(types, pattern = " (Date format: display)", replacement = "", fixed = T)
  types <- gsub(types, pattern = " (Date format: display with optional fields)", replacement = "", fixed = T)
  types <- gsub(types, pattern = "Datetime", replacement = "Date", fixed = T)
  types <- gsub(types, pattern = " (nur berechnet)", replacement = "", fixed = T)
  types[grep("*Radio*", types)] <- "Single choice"
  types[grep("*Popup*", types)] <- "Single choice"
  types[grep("*Score*", types)] <- "Number"
  types[grep("*CheckBox*", types)] <- "Checkbox"
  types[grep("*Text*", types)] <- "Text"
  
  return(types)
}

formatColumn <- function(col, type, fmt, unit)
{
  print(paste(type, fmt, unit))
  
  if(type == "Date-Interval") 
  {
    if(fmt == "Y-M-D")
    {
      tmp <- sprintf("%07d", col) ##completion
      years <- as.numeric(substr(tmp,1,3))
      months <- as.numeric(substr(4,5))
      days <- as.numeric(substr(6,7))
      
      if(unit == "years")
      {
        col <- years + months/12 + days/365.25   
      }
    }
  }
  return(col)
}

writeExportToXLS <- function(drt, exportdir, destination, tables = NULL, centres = NULL)
{
  ## WORKAROUND for openxlsx
  Sys.setenv(R_ZIPCMD="zip")
  
  tabs <- getTablesFromDRT(drt)
  items <- getItemsFromDRT(drt, tables)
  
  ## delete 
  if(!is.null(tables)) {
    idx <- which(!(tabs$Table %in% tables))
    tabs <- tabs[-idx, ]
  }
  
  if(nrow(tabs) < 1)
    stop("Not table found in data record table!")
  
  ## Create empty sheets
  sheets <- vector("list", nrow(tabs)+1)
  
  ## All tables separately (TODO: rectangular)
  for(i in 1:nrow(tabs)) {
    sheets[[i+1]] <- getTableFromExport(exportdir, table = tabs$Table[i], columns = items$Column[items$Table == tabs$Table[i]], centres)
    names(sheets)[i+1] <- tabs$Form[i] 
  }
  items$Table <- NULL
  sheets[[1]] <- items
  names(sheets)[1] <- "Codebook" 
  
  openxlsx::write.xlsx(x = sheets, file = destination, row.names=F, col.names = T)  
}