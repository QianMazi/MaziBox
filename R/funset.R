# ----- Others -----

#' assets return demo dataset.
#'
#' A dataset containing stock index and bond index daily return data since 2009.
#'
#' @format A data frame with 1627 rows and 3 variables:
#' \describe{
#'   \item{date}{date type}
#'   \item{stock}{stock index return}
#'   \item{bond}{bond index return}
#' }
"rtndemo"

#' test
#'
"EE_CT"

#' ladder position port function
#'
#' @param assetRtn a data frame for stock and bond return.
#' @param ruledf a data frame for position rule.
#' @param rebalance rebalance frequency, default value is NULL.
#' @return a data frame, containing return,postion,nav.
#' @author han.qian
#' @export
#' @examples
#' assetRtn <- rtndemo
#' ruledf <- data.frame(nav = c(0,1.05,1.10,1.20),pos = c(0.15,0.25,0.35,0.5))
#' re <- ladderNAV(assetRtn,ruledf)
#' rebalance <- '2 years'
#' re <- ladderNAV(assetRtn,ruledf,rebalance)
ladderNAV <- function(assetRtn,ruledf,rebalance=NULL){
  assetRtn[,c('pos','rtn','nav_rebalance','nav')] <- 0

  if(is.null(rebalance)){
    for(i in 1:nrow(assetRtn)){
      if(i==1){
        assetRtn$pos[i] <- ruledf$pos[1]
      }else{
        postmp <- findInterval(assetRtn$nav_rebalance[i-1],ruledf$nav)
        assetRtn$pos[i] <- ruledf$pos[postmp]
      }
      assetRtn$rtn[i] <- assetRtn$stock[i]*assetRtn$pos[i]+assetRtn$bond[i]*(1-assetRtn$pos[i])
      assetRtn$nav_rebalance[i] <- prod(1+assetRtn$rtn[1:i])
      assetRtn$nav[i] <- prod(1+assetRtn$rtn[1:i])
    }
  }else{
    reDate <- seq.Date(min(assetRtn$date),max(assetRtn$date),by = rebalance)
    reDate <- QDataGet::trday.nearby(reDate,by=0)
    for( i in 1:nrow(assetRtn)){
      if(assetRtn$date[i] %in% reDate){
        assetRtn$pos[i] <- ruledf$pos[1]
        j <- i
      }else{
        postmp <- findInterval(assetRtn$nav_rebalance[i-1],ruledf$nav)
        assetRtn$pos[i] <- ruledf$pos[postmp]
      }
      assetRtn$rtn[i] <- assetRtn$stock[i]*assetRtn$pos[i]+assetRtn$bond[i]*(1-assetRtn$pos[i])
      assetRtn$nav_rebalance[i] <- prod(1+assetRtn$rtn[j:i])
      assetRtn$nav[i] <- prod(1+assetRtn$rtn[1:i])
    }
  }

  if(is.null(rebalance)){
    assetRtn <- assetRtn[,c("date","stock","bond","pos","rtn","nav")]
  }
  return(assetRtn)
}

#' get old NP YOY
#'
#' @export
gf.old_NP_YOY <- function(TS,is1q=TRUE,filt=10000000,rm_neg=FALSE,src=c("all","fin","for"),clear_result=TRUE){
  src <- match.arg(src)
  if(src=="fin") {
    src_filt <- "src='fin'"
  } else if(src=="fin"){
    src_filt <- "src='for'"
  } else {
    src_filt <- "1>0"
  }
  check.TS(TS)
  PeriodMark <- ifelse(is1q,2,3)
  TS$date <- rdate2int(TS$date)
  qr <- paste(
    "select b.date, b.stockID, InfoPublDate,EndDate,NP_YOY as 'factorscore', src, NP_LYCP
    from LC_PerformanceGrowth a, yrf_tmp b
    where a.id=(
    select id from LC_PerformanceGrowth
    where stockID=b.stockID and InfoPublDate<=b.date and",src_filt,"
    and PeriodMark=",PeriodMark,"
    order by InfoPublDate desc, EndDate DESC
    limit 1,1);
    "
  )
  con <- db.local()
  dbWriteTable(con,name="yrf_tmp",value=TS[,c("date","stockID")],row.names = FALSE,overwrite = TRUE)
  re <- DBI::dbGetQuery(con,qr)
  DBI::dbDisconnect(con)
  re <- merge.x(TS,re,by=c("date","stockID"))
  re <- transform(re, date=intdate2r(date))

  # -- filtering
  if(rm_neg){
    re[!is.na(re$NP_LYCP) & re$NP_LYCP<filt, "factorscore"] <- NA
  } else {
    re[!is.na(re$NP_LYCP) & abs(re$NP_LYCP)<filt, "factorscore"] <- NA
  }

  if(clear_result){# drop cols: InfoPublDate,EndDate, src, NP_LYCP
    return(re[,c(names(TS),"factorscore")])
  } else {
    return(re)
  }
}

#' get reserved publ date according to rptDate
#'
#' @export
rptDate.rsvDate <- function(rptTS){
  check.rptTS(rptTS)
  result <- rptTS.getFin_ts(rptTS, funchar ='
                            "rsvDate",report(128002,RDate),
                            "chgDate_1",report(128003,RDate),
                            "chgDate_2",report(128004,RDate),
                            "chgDate_3",report(128005,RDate)')
  result$date <- result$rsvDate
  ind <- !is.na(result$chgDate_1) & result$chgDate_1 > 0
  result$date[ind] <- result$chgDate_1[ind]
  ind <- !is.na(result$chgDate_2) & result$chgDate_2 > 0
  result$date[ind] <- result$chgDate_2[ind]
  ind <- !is.na(result$chgDate_3) & result$chgDate_3 > 0
  result$date[ind] <- result$chgDate_3[ind]
  result <- result[, c("date","stockID")]
  result$date <- intdate2r(result$date)
  result$date <- trday.nearest(result$date, dir = 1) # ensure trday
  return(result)
}


#' get forecast report info according to rptTS
#'
#' @return data frame
#' @export
rptDate.forecastInfo <- function(rptTS){
  check.rptTS(rptTS)
  mazi_tmp <- rptTS[,c("rptDate","stockID")]
  mazi_tmp$rptDate <- rdate2int(mazi_tmp$rptDate)

  qr <- paste0("select A.* from LC_PerformanceGrowth A, mazi_tmp B
               where A.src = 'for' and A.PeriodMark=3
               and A.id = (select id from LC_PerformanceGrowth
               where stockID=B.stockID and EndDate <= B.rptDate and src='for' and PeriodMark=3
               order by InfoPublDate desc, EndDate DESC
               limit 1)")

  # run qr codes
  con <- db.local()
  RSQLite::dbWriteTable(con, name = "mazi_tmp", value = mazi_tmp, overwrite = TRUE)
  result <- RSQLite::dbGetQuery(con, qr)
  RSQLite::dbDisconnect(con)

  # organize output format
  result$InfoPublDate <- intdate2r(result$InfoPublDate)
  result$EndDate <- intdate2r(result$EndDate)
  return(result)
}

# ----- Internal using functions -----

#' Get tradingday by passing in vector inputs.
#'
#' @param begTvec begT vector.
#' @param endTvec endT vector.
#' @return A vector that row-bind all the trading days between begT and endT.
#' @export
trday.get.vec <- function(begTvec, endTvec){
  if(length(begTvec)!=length(endTvec)) stop("The lengths of begTvec and endTvec do not match.")
  begT.min <- min(begTvec)
  endT.max <- max(endTvec)
  T.df <- data.frame("begT" = begTvec, "endT" = endTvec)
  # get the market trading days
  begTT <- max(begT.min,as.Date("1990-12-19"))
  begTT <- QUtility::rdate2int(begTT)
  endTT <- QUtility::rdate2int(endT.max)
  qr <- paste("select TradingDate from QT_TradingDay where SecuMarket=83 and IfTradingDay=1 and TradingDate between ",begTT,"and",endTT)
  trday <- QDataGet::queryAndClose.dbi(QDataGet::db.local(),qr)
  trday$TradingDate <- QUtility::intdate2r(trday$TradingDate)
  T.df <- dplyr::rowwise(T.df)
  temp <- dplyr::do(T.df, subset(trday, TradingDate >= .$begT  &  TradingDate <= .$endT))
  colnames(temp) <- "date"
  return(temp$date)
}

#' Count trading days by passing vector inputs.
#'
#' @param begTvec begT vector.
#' @param endTvec endT vector.
#' @return A vector which each row represents the lag of days between begT and endT.
#' @export
trday.count.vec <- function(begTvec, endTvec){
  if(length(begTvec)!=length(endTvec)) stop("The lengths of begTvec and endTvec do not match.")
  mmin <- min(min(begTvec), min(endTvec))
  mmax <- max(max(begTvec), max(endTvec))
  T.df <- data.frame("begT" = begTvec, "endT" = endTvec)
  T.df2 <- T.df
  ind <- T.df$begT > T.df$endT
  T.df2$begT[ind] = T.df$endT[ind]
  T.df2$endT[ind] = T.df$begT[ind]

  # get the market trading days
  begTT <- max(mmin,as.Date("1990-12-19"))
  begTT <- QUtility::rdate2int(begTT)
  endTT <- QUtility::rdate2int(mmax)
  qr <- paste("select TradingDate from QT_TradingDay where SecuMarket=83 and IfTradingDay=1 and TradingDate between ",begTT,"and",endTT)
  trday <- QDataGet::queryAndClose.dbi(QDataGet::db.local(),qr)
  trday$TradingDate <- QUtility::intdate2r(trday$TradingDate)

  #
  T.df2 <- dplyr::rowwise(T.df2)
  temp <- dplyr::do(T.df2, as.data.frame(nrow(subset(trday, TradingDate >= .$begT  &  TradingDate <= .$endT))))
  colnames(temp) <- "diff"
  temp[ind,] <- temp[ind,]*(-1)
  return(temp$diff)
}


# ----- JYDB connection -----

#' build key for key data frame
#'
#' @param kkey The column name.
#' @param decode The decoding number if the values have to be interpreted by the CT_SystemConst sheet.
#' @param isSE NA if the variable is not in the sub-sheet. Otherwise, look up the SE sub-sheet and fill in the TypeCode of the variable.
#' @return A single row in the data frame format.
#' @details Rbind the keys to one single data frame and apply it in the the EE_getETSfromJY function.
#' @export
buildkey <- function(kkey,decode,isSE){
  re <- data.frame(kkey = kkey)
  re$decode <- ifelse(missing(decode),NA,decode)
  re$isSE <- ifelse(missing(isSE),NA,isSE)
  return(re)
}

#' Get ETS object from JY database
#'
#' @param SheetName The sheet name string of the data.
#' @param key.df The key data frame.
#' @param extra.condition The extra qr string could be add to the SQL.
#' @param stock.column The column name which indicating the stockID in the sheet
#' @param stock.decode The column name in decode sheet which is used to decode the stockID.
#' @param date.column The column name string of the date in the data.
#' @return A dataframe with date, stockID and vars.
#' @export
#' @examples
#' # Fetching the ETS of investors' activities.
#' key.df <- rbind(buildkey("SerialNb"))
#' tmpdat <- EE_getETSfromJY(SheetName = "LC_InvestorRa", key.df)
#' # Fetching the ETS of shares transfering.
#' key.df <- rbind(buildkey("TranShareType",1040),
#'                 buildkey("TranMode",1202),
#'                 buildkey("IfSuspended"),
#'                 buildkey("TransfererEcoNature",1096,1),
#'                 buildkey("ReceiverEcoNature",1096,2))
#' tmpdat <- EE_getETSfromJY(SheetName = "LC_ShareTransfer", key.df)
EE_getETSfromJY <- function(SheetName, key.df,
                            extra.condition = NULL,
                            stock.column = "InnerCode",
                            stock.decode = "InnerCode",
                            date.column = "InfoPublDate"){
  nkey <- nrow(key.df)
  varqr <- " "
  sheetqr <- " "
  decodeqr <- " "
  namevec <- c()
  for( i in 1:nkey){
    if(is.na(key.df$decode[i])){
      varqr <- paste0(varqr,","," target.",key.df$kkey[i]," ","var",i)
      namevec <- c(namevec, as.character(key.df$kkey[i]))
    }else{
      if(is.na(key.df$isSE[i])){
        varqr <- paste0(varqr,","," target.",key.df$kkey[i]," ","var",i)
        varqr <- paste0(varqr,","," decode",i,".MS"," ","var_",i)
        sheetqr <- paste0(sheetqr,","," JYDB.dbo.CT_SystemConst decode",i)
        decodeqr <- paste0(decodeqr," ","and target.",key.df$kkey[i]," = decode",i,".DM")
        decodeqr <- paste0(decodeqr," ","and decode",i,".LB=",key.df$decode[i])
      }else{
        varqr <- paste0(varqr,","," targetse",i,".Code var",i)
        varqr <- paste0(varqr,","," decode",i,".MS"," ","var_",i)
        sheetqr <- paste0(sheetqr,","," JYDB.dbo.CT_SystemConst decode",i)
        sheetqr <- paste0(sheetqr,","," JYDB.dbo.",SheetName,"_SE targetse",i)
        decodeqr <- paste0(decodeqr," ","and target.ID=targetse",i,".ID")
        decodeqr <- paste0(decodeqr," ","and targetse",i,".TypeCode=",key.df$isSE[i])
        decodeqr <- paste0(decodeqr," ","and decode",i,".LB=",key.df$decode[i])
        decodeqr <- paste0(decodeqr," ","and targetse",i,".Code=decode",i,".DM")
      }
      namevec <- c(namevec, as.character(key.df$kkey[i]), paste0(key.df$kkey[i],"_decode"))
    }
  }
  qr <- paste0(
    "select convert(varchar(8),target.",date.column,",112) date,
    'EQ'+s.SecuCode stockID",varqr,"
    from JYDB.dbo.",SheetName," target,
    JYDB.dbo.SecuMain s", sheetqr,"
    where target.",stock.column,"=s.",stock.decode,"
    and s.SecuCategory in (1,2)",decodeqr
  )
  if(!is.null(extra.condition)){
    qr <- paste(qr, extra.condition, sep = " and ")
  }
  temp <- QDataGet::queryAndClose.odbc(QDataGet::db.jy(), qr)
  if(!is.null(nrow(temp))){
    temp$date <- QUtility::intdate2r(temp$date)
    colnames(temp) <- c("date","stockID", namevec)
  }
  return(temp)
}

# ----- Event Effect Research -----

#' plot event histogram
#'
#' @export
EE_distribute <- function(ETS,TSErr, bin = c("year","yearmonth","month","yearquarter","quarter")){
  if(missing(ETS) & !missing(TSErr)){
    ETS <- subset(TSErr, No == 0, select = c("date","stockID"))
  }
  bin <- match.arg(bin)
  if( bin == "year"){
    ETS$year <- lubridate::year(ETS$date)
    fig <- ggplot2::ggplot()+
      ggplot2::geom_bar(data = ETS, ggplot2::aes(year))
  }else if( bin == "yearmonth"){
    ETS$yearmonth <- rdate2int(ETS$date)
    ETS$yearmonth <- substr(ETS$yearmonth,1,6)
    fig <- ggplot2::ggplot()+
      ggplot2::geom_bar(data = ETS, ggplot2::aes(yearmonth)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  }else if( bin == "month"){
    ETS$month <- lubridate::month(ETS$date,label = TRUE)
    fig <- ggplot2::ggplot()+
      ggplot2::geom_bar(data = ETS, ggplot2::aes(month))
  }else if( bin == "yearquarter"){
    ETS$quarter <- paste0("Q",lubridate::quarter(ETS$date))
    ETS$year <- lubridate::year(ETS$date)
    ETS$yearquarter <- paste0(ETS$year, ETS$quarter)
    fig <- ggplot2::ggplot()+
      ggplot2::geom_bar(data = ETS, ggplot2::aes(yearquarter))+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  }else if( bin == "quarter"){
    ETS$quarter <- paste0("Q",lubridate::quarter(ETS$date))
    fig <- ggplot2::ggplot()+
      ggplot2::geom_bar(data = ETS, ggplot2::aes(quarter))
  }
  fig <- fig + ggplot2::ggtitle("Event Distribution")
  return(fig)
}

#' Expand ETS object into TS object with index. Multi-row version.
#'
#' @param ets A event TS object which includes the event date and the corresponding stock.
#' @param win1 Integer. The time window of days before the event date.
#' @param win2 Integer. The time window of days after the event date.
#' @return EETS(Expanded event TS).
#' @description Basic function in the event research system. In ETS, the date in ETS represent the first tradable date after the info released date. In EETS, the No 0 represent the first tradable date after the info released.
#' @export
EE_expandETS <- function(ets, win1, win2){
  # arguments checking
  QUtility::check.colnames(ets, c("date","stockID"))
  if( win1 < 0 | win2 < 0){
    stop("The window of days must not smaller than 0.")
  }
  # organization
  len <- win1+1+win2
  stockID_column <- rep(ets$stockID, each = len)
  date0 <- trday.nearest(ets$date, dir = 1)
  begT <- trday.nearby(date0, by = -win1)
  endT <- trday.nearby(date0, by = win2)
  date_column <- trday.get.vec(begT, endT)
  if(length(date_column) != (len * nrow(ets))) stop("bug!") # double checking
  No_ <- (-win1):win2
  No <- rep(No_, nrow(ets))
  # output : return EETS
  result <- data.frame("No" = No, "date" = date_column, "stockID" = stockID_column)
  return(result)
}

#' Plug in ETS and return a TS object with Err and index.
#'
#' @param ETS A event TS object which includes the event date and the corresponding stock.
#' @param db The name string of local database err sheet which containing the columns of "date", "stockID", "err".
#' @param win1 Integer. The time window of days before the event date.
#' @param win2 Integer. The time window of days after the event date.
#' @param cleansing Logical values. If true, the result will be fill NA and winsorized before out put.
#' @return A TS object with Err and index.
#' @export
#' @examples
#' date <- as.Date(c("2014-07-01","2014-07-08"))
#' stockID <- c("EQ000001","EQ000002")
#' ETS <- data.frame(date, stockID)
#' TSErr <- EE_getTSErr(ETS)
#' EE_plot(TSErr)
EE_getTSErr <- function(ETS, err_database = c("EE_CroxSecReg","EE_CroxSecGroup","pct_chg","pct_chg_bmk"), win1 = 10, win2 = 20) {
  # argument checking
  check.colnames(ETS, c('date','stockID'))
  err_database <- match.arg(err_database)
  EETS <- EE_expandETS(ETS, win1 = win1, win2 = win2)
  # get Err
  if(err_database == "pct_chg"){
    EETS$date <- trday.nearby(EETS$date, by = -1)
    result <- getTSR(EETS, dure = lubridate::days(1))
    result$date <- trday.nearby(result$date, by = 1)
    result <- result[,c("No","date","stockID","periodrtn")]
    result <- renameCol(result, "periodrtn", "err")
  }else if(err_database == "pct_chg_bmk"){
    if(is.null(bmk)){bmk = "EI000001"}
    EETS$date <- trday.nearby(EETS$date, by = -1)
    result <- getTSR(EETS, dure = lubridate::days(1))
    result$date <- trday.nearby(result$date, by = 1)
    bmk_data <- data.frame("date" = result$date, "stockID" = bmk)
    bmk_data <- TS.getTech_ts(bmk_data, funchar = "StockZf3()", varname = "bmk_pct")
    if(nrow(bmk_data) != nrow(result)) stop("bug!") # double check
    result$err <- result$periodrtn - bmk_data$bmk_pct/100
    result <- result[,c("No","date","stockID","err")]
  }else{
    EETS$date <- rdate2int(EETS$date)
    con <- db.local()
    DBI::dbWriteTable(conn = con, value = EETS, name = 'mazi_tmp', overwrite = TRUE, append = FALSE, row.names = FALSE)
    qr <- paste (
      "select a.*, b.err
      from mazi_tmp a
      left join ", err_database, " b
      on a.stockID = b.stockID
      and a.date = b.date"
    )
    result <- DBI::dbGetQuery(con, qr)
    DBI::dbDisconnect(conn = con)
    result$date <- intdate2r(result$date)
  }
  # output
  check.colnames(result,c("No","date","stockID","err"))
  return(result)
}

#' Plug in TSErr object and return the mean summary table
#'
#' @param TSErr The TSErr object
#' @param withSD Logical. Whether to compute sd.
#' @param withACC Logical. Whether to compute accumulated err.
#' @return Dataframe.
#' @export
EE_table <- function(TSErr, withSD = TRUE, withACC = FALSE){

  # TSErr$err[is.na(TSErr$err)] <- 0
  tmpdat <- dplyr::group_by(TSErr, No)
  if(withSD){
    result <- dplyr::summarise(tmpdat, mean = mean(err, na.rm = TRUE), sd = sd(err, na.rm = TRUE))
    colnames(result) <- c("No","err","sd")
  }else{
    result <- dplyr::summarise(tmpdat, mean = mean(err, na.rm = TRUE))
    colnames(result) <- c("No","err")
  }
  if(withACC){
    result$acc_err <- cumprod(result$err + 1)
  }
  return(result)
}

#' conduct day 0 statistics analysis
#'
#' @param ets ets.
#' @return A list containing suspend_ratio, winning_raio and stat_table.
#' @export
#' @examples
#' ets <- ets.employeeplan()
#' EE_day0(ets)
EE_day0 <- function(ets){
  # db
  data <- TS.getTech(ets, variables = c("pct_chg","pre_close","open","high","low","volume")) # correct matching
  # pre_close always > 0
  data$jump_open <- (data$open - data$pre_close)/data$pre_close
  data$vibration <- (data$high -data$low)/data$pre_close
  nrow1 <- nrow(data)

  data <- subset(data, volume > 0)
  nrow2 <- nrow(data)

  # suspend ratio
  suspend_ratio <- (nrow1 - nrow2)/nrow1

  # basic stat
  data_subset <- data[,c("pct_chg","jump_open","vibration")]
  stat_table <- data.frame("mean" = sapply(data_subset, mean),
                           "sd" = sapply(data_subset, sd),
                           "median" = sapply(data_subset, median),
                           "mad" = sapply(data_subset, mad))

  # winning ratio
  winning_ratio <- mean(data_subset$pct_chg > 0, na.rm = TRUE)

  # output
  result <- list(suspend_ratio, winning_ratio, round(stat_table, 5))
  names(result) <- c("suspend_ratio","winning_ratio","stat_table")
  return(result)
}

#' split TSerr
#'
#' @param TSErr
#' @return TSErrs, TSErr with sector
#' @export
EE_splitTSErr <- function(TSErr, group_mode = c("year","sector","EQ"),
                          sectorAttr = NULL){

  ets <- subset(TSErr, No == 0)
  group_mode <- match.arg(group_mode)
  if(group_mode == "year"){
    ets$sector <- lubridate::year(ets$date)
  }else if(group_mode == "sector"){
    ets <- getSectorID(ets, sectorAttr = sectorAttr)
  }else if(group_mode == "EQ"){
    ets$sector <- "ZHUBAN"
    ets$sector[substr(ets$stockID,3,5) == "002"] <- "002"
    ets$sector[substr(ets$stockID,3,5) == "300"] <- "300"
  }

  window_len <- max(TSErr$No) - min(TSErr$No) + 1
  sector <- rep(ets$sector, each = window_len)
  if(nrow(TSErr) != length(sector)) stop("bugs!")
  TSErr$sector <- sector

  return(TSErr)
}

#' Split plot by sector
#'
#' @param TSErrs TSErr with sector column.
#' @return plot.
#' @export
EE_splitplot <- function(TSErrs){

  # check colnames
  if(!all(c("No","date","stockID","err","sector") %in% colnames(TSErrs))) stop("colnames do not match.")
  # loop starts
  sector_list <- unique(TSErrs$sector)
  for(i in 1:length(sector_list)){
    sector_ <- sector_list[i]
    TSErr_ <- subset(TSErrs, sector == sector_)
    table_ <- EE_table(TSErr_, withSD = FALSE, withACC = TRUE)
    table_$sector <- sector_
    if(i == 1L){
      plot_data <- table_
    }else{
      plot_data <- rbind(plot_data, table_)
    }
  }
  plot_data$sector <- as.character(plot_data$sector)
  fig <- ggplot2::ggplot() +
    ggplot2::geom_path(data = plot_data, ggplot2::aes(x = No, y=acc_err, colour = sector), size = 1) +
    ggplot2::geom_vline(xintercept = -1, color = 'red', linetype = 2)+
    ggplot2::ylab("Accumulated Abnormal Return") + ggplot2::xlab("Date Series")
  return(fig)
}

#' Plug in TSErr object and return the summary plot
#'
#' @param TSErr The TSErr object which must containing No and err columns.
#' @return Plot
#' @export
#' @examples
#' date <- as.Date(c("2014-07-01","2014-07-08"))
#' stockID <- c("EQ000001","EQ000002")
#' ETS <- data.frame(date, stockID)
#' TSErr <- EE_getTSErr(ETS)
#' EE_plot(TSErr)
EE_plot <- function(TSErr){

  suppressPackageStartupMessages(require(ggplot2))
  # p0
  TSErr0 <- na.omit(TSErr)
  TSErr0 <- dplyr::group_by(TSErr0, No)
  TSErr0 <- dplyr::mutate(TSErr0, err = robustHD:::winsorize(err, const = 3))
  p0 <- ggplot() +
    geom_boxplot(data = TSErr0, aes(x= No, group = No, y =err))+
    geom_vline(xintercept = -0.5, color = 'red', linetype = 2)+
    ylab("Daily Abnormal Return")+
    xlab("Date Series")

  # p1
  rtn_table <- EE_table(TSErr, withSD = TRUE, withACC = TRUE)
  p1 <- ggplot()+
    geom_bar(data = rtn_table, aes(x = No, y=err), stat = 'identity')+
    geom_vline(xintercept = -0.5, color = 'red', linetype = 2)+
    ylab("Daily Abnormal Return")+
    xlab("Date Series")

  # p2
  p2 <-  ggplot()+
    geom_ribbon(data = rtn_table, aes(x = No, ymin=acc_err-sd, ymax=acc_err+sd), fill="grey70")+
    geom_path(data = rtn_table, aes(x = No, y=acc_err), size = 1) +
    geom_vline(xintercept = -1, color = 'red', linetype = 2)+
    ylab("Accumulated Abnormal Return")+
    xlab("Date Series")+
    theme(axis.title.x = element_blank())

  # p3
  TSErrs <- EE_splitTSErr(TSErr, group_mode = "year")
  p3 <- EE_splitplot(TSErrs)

  # output
  result <- multiplot(plotlist = list(p0,p1,p2,p3), ncol=2)
  return(result)
}


#' Wrap up ETS analyzing functions
#'
#' @param ets
#' @param db The database of studying objects. Whether to study residuals or daily_pct_chg.
#' @param win1 The time window(days) before the events happened.
#' @param win2 The time window(days) after the events happened.
#' @return list. Containing stat table, day0 analyzing, daily perfomance and plots.
#' @export
#' @examples
#' ets <- ets.employeeplan()
#' EE_wrap_analyzer(ets)
EE_wrap_analyzer <- function(ets, err_database = c("EE_CroxSecReg","EE_CroxSecGroup","pct_chg","pct_chg_bmk"), win1 = 10, win2 = 20){
  err_database <- match.arg(err_database)
  tserr <- EE_getTSErr(ets, err_database = err_database, win1 = win1, win2 = win2)
  # daily table
  rtn_table <- EE_table(tserr, withACC = TRUE)
  # stat table
  overall_stat_table <- data.frame("winning_rate" = mean(rtn_table$err > 0, na.rm = TRUE),
                           "rtn" = tail(rtn_table$acc_err, 1),
                           "sd" = sd(rtn_table$err, na.rm = TRUE))
  # EE_plot
  fig <- EE_plot(tserr)
  # EE_day0
  day_0_table <- EE_day0(ets)
  # output
  result_list <- list(overall_stat_table, day_0_table, rtn_table, fig)
  names(result_list) <- c("Overall Stat", "Day 0 Performance", "Daily Performance", "Plot")
  return(result_list)
}

# ----- Backtesting -----

#' back test function for ets
#'
#' @param ets the ets object must contain the columns date, stockID and date_end.
#' @param wgt_limit the biggest wgt of each stock.
#' @description the function is modified by port.backtest. This revised version could provide different date_end for each stock. The weight is set to eq for each trday. The date in ets is the buying date and the pct_chg in that date is not obtained. The date_end is the selling date and the pct_chg is included.
#' @return Same result type with port.backtest
#' @export
#'
EE.port_backtest <- function(ets, wgt_limit = 0.1, output_type = c("port","rtn"),fee.buy=0, fee.sell=0){

  check.colnames(ets, c("date","stockID","date_end"))
  output_type <- match.arg(output_type)
  # assuming in the first tradable date the pct_chg could not be obtained.
  # the pct_chg in the date_end is included, wgt changes in the next trday

  # date validation :
  # the latest date_end is set to (sys.date - 1)
  TD <- Sys.Date()-1
  TD <- trday.nearest(TD, -1)
  ets <- subset(ets, date < TD)
  ets$date_end[ets$date_end >= TD] <- TD

  # holdingEndT
  holdingEndT <- max(c(ets$date, ets$date_end))

  # datelist
  datelist_1 <- unique(ets$date)
  datelist_2 <- trday.nearby(unique(ets$date_end), by = 1L)
  datelist <- sort(unique(c(datelist_1, datelist_2)))
  datelist <- subset(datelist, datelist < holdingEndT)

  #
  for( ii in 1:length(datelist)){
    date_ <- datelist[[ii]]
    ets_ <- subset(ets, date <= date_ & date_ <= date_end)
    if(nrow(ets_) == 0){
      pt_ <- data.frame("date" = date_, "stockID" = NA)
    }else{
      pt_ <- data.frame("date" = date_, "stockID" = ets_$stockID)
    }
    if(ii == 1L){
      pt <- pt_
    }else{
      pt <- rbind(pt, pt_)
    }
  }

  # solving problems that same stocks have overlaps holding windows
  pt <- unique(pt[,c("date","stockID")]) # NA not removed
  # compute wgt
  pt <- dplyr::group_by(pt, date)
  pt <- dplyr::mutate(pt, wgt = min(1/n(), wgt_limit))

  if(output_type == "port"){
    return(pt)
  }

  # wide format
  pt_wide <- reshape2::dcast(data = pt, formula = date ~ stockID, value.var = 'wgt', fill = 0)
  pt_wide$'NA' <- NULL
  weights <- xts::xts(pt_wide[,-1],pt_wide[,1])
  # colnames(weights) <- colnames(pt_wide)[-1]
  weights <- weights[,sort(colnames(weights))]

  # get R
  # datelist <- unique(pt_wide$date)
  R.df <- data.frame()
  cat("Looping to get the quote data in function 'port.backtest' ....\n")
  pb <- txtProgressBar(style = 3)
  for(ii in 1:length(datelist)){
    date_ <- datelist[ii]
    pt_ <- subset(pt, date == date_)
    # to do ...
    # if nrow == 0
    stocks <- pt_$stockID
    begT <- date_
    endT <- as.Date(ifelse(ii==length(datelist), holdingEndT, datelist[ii+1]), "1970-01-01") # ???
    qt <- getQuote(stocks = stocks, begT = begT, endT = endT, variables = c("pct_chg"),tableName = "QT_DailyQuote2")
    qt <- renameCol(qt, "pct_chg", "rtn")
    if(ii==1L){
      R.df <- qt
    } else {
      R.df <- rbind(R.df, qt)
    }
    setTxtProgressBar(pb, ii/length(datelist))
  }
  close(pb)
  # solving problems that same stocks have overlaps holding windows
  R.df <- unique(R.df) # NA not removed
  #
  R.df <- reshape2::dcast(R.df, date~stockID,value.var='rtn',fill=0)
  R <- xts::xts(R.df[,-1],R.df[,1])
  # colnames(R) <- colnames(R.df)[-1]
  R <- R[,sort(colnames(R))]

  # double checking colnames
  if(!all(colnames(R) == colnames(weights))) stop("weights and R, colnames not matching.")

  # computations
  re_tmp <- QUtility:::Return.backtesting(R = R,weights = weights,fee.buy = fee.buy,fee.sell = fee.sell,output=c("rtn","turnover"))
  re <- re_tmp[["rtn"]]
  attr(re,"turnover") <- re_tmp[["turnover"]]

  # output
  attr(re,"fee") <- c(fee_buy=fee.buy, fee_sell=fee.sell)

  result <- list("port" = pt, "rtn" = re)
  return(result)
}


# ----- ETS FRESH -----

#' YJYZ
#'
#' @export
ets.yjyz <- function(is1q=TRUE){
  PeriodMark <- ifelse(is1q,2,3)
  result <- queryAndClose.dbi(db.local(),
                              query = paste0(
                             'select stockID, InfoPublDate as date
                             from LC_PerformanceGrowth
                             where src="for"
                             and ForcastType=4
                             and PeriodMark=',PeriodMark))
  result$date <- intdate2r(result$date)

  # with in 985
  result <- is_component(TS = result, sectorID = "EI000985")
  result <- subset(result, is_comp == 1)

  result <- result[,c("date","stockID")]
  # double check date : passed
  return(result)
}

#' YJYZ REFINE
#'
#' @export
ets.yjyz_refine <- function(freq = c("y","q")){
  freq = match.arg(freq)

  # get mdata
  mdata <- queryAndClose.dbi(db.local(), query = '
                             select * from LC_PerformanceGrowth
                             where PeriodMark=2')
  mdata$EndDate <- intdate2r(mdata$EndDate)
  mdata$InfoPublDate <- intdate2r(mdata$InfoPublDate)

  # arrange mdata
  mdata <- dplyr::arrange(mdata, EndDate, stockID, InfoPublDate)

  # make mdata2
  mdata2 <- mdata
  colnames(mdata2) <- paste0("L.",colnames(mdata2))
  mdata2 <- dplyr::rename(mdata2, stockID = L.stockID)

  # merge
  mdata$L.EndDate <- rptDate.offset(mdata$EndDate, by = -1, freq = freq)
  result <- merge.x(x = mdata, y = mdata2, by = c("L.EndDate", "stockID"), mult = "last")
  # (double check old data correctness : passed)

  result <- subset(result, ForcastType == 4 & src == "for")
  result <- subset(result, NP_YOY > L.NP_YOY)
  result <- dplyr::rename(result, date = InfoPublDate)

  # with in 985
  result <- is_component(TS = result, sectorID = "EI000985")
  result <- subset(result, is_comp == 1)

  # output
  result <- result[,c("date", "stockID")]
  return(result)
}

#' get ETS of stocks unfreezing
#'
#' @return ETS object.
#' @export
ets.unfreeze <- function(withPercent = FALSE, research_mode = FALSE){
    mdata <- EE_getETSfromJY(date.column = "StartDateForFloating",
                             SheetName = "LC_SharesFloatingSchedule",
                             key.df = data.frame("kkey"=c("Proportion1","SourceType"),
                                                 "decode"=c(NA,1022),
                                                 "isSE"=c(NA,NA)))
    mdata_sub_1 <- subset(mdata, SourceType == 24 & Proportion1 > 11.39)
    mdata_sub_2 <- subset(mdata, SourceType == 82 & Proportion1 > 83.69)
    mdata_sub <- rbind(mdata_sub_1, mdata_sub_2)
    mdata_sub <- dplyr::arrange(mdata_sub, date, stockID)

    if(withPercent){
      result <- subset(mdata_sub, select = c("date","stockID","Proportion1"))
    }else{
      result <- subset(mdata_sub, select = c("date","stockID"))
    }
    if(research_mode){
      result <- subset(result, date < Sys.Date() - 10)
    }
    return(result)
}

#' get ETS of 002 stocks
#'
#' @return ETS object.
#' @export
ets.002 <- function(research_mode = FALSE){

  mdata <- queryAndClose.odbc(db.quant(),"select * from LC_PerformanceForecast
                              where ForecastObject=13")
  mdata <- subset(mdata, substr(stockID,1,5) == "EQ002")
  mdata$InfoPublDate <- intdate2r(mdata$InfoPublDate)
  mdata$EndDate <- intdate2r(mdata$EndDate)

  # arrange mdata
  mdata <- dplyr::arrange(mdata, EndDate, stockID, InfoPublDate)
  mdata <- dplyr::rename(mdata, date = InfoPublDate)
  mdata <- is_component(mdata, sectorID = "EI000985")
  mdata <- subset(mdata, is_comp == 1)

  old_mdata <- gf.old_NP_YOY(mdata[,c("date","stockID")], src = "for", clear_result = FALSE)
  colnames(old_mdata) <- paste0("L.",colnames(old_mdata))
  old_mdata <- dplyr::rename(old_mdata, date = L.date)
  old_mdata <- dplyr::rename(old_mdata, stockID = L.stockID)

  mdata <- merge.x(mdata, old_mdata, by = c("date","stockID"))
  mdata <- subset(mdata, (EGrowthRateFloor + EGrowthRateCeiling)/2 > L.factorscore*1.1)
  mdata <- subset(mdata, ForcastType == 4)

  # double check
  mdata$L.InfoPublDate <- intdate2r(mdata$L.InfoPublDate)
  mdata$L.EndDate <- intdate2r(mdata$L.EndDate)
  mdata <- subset(mdata, date >= L.InfoPublDate & EndDate>=L.EndDate)

  result <- mdata[,c("EndDate","stockID")]
  result <- dplyr::rename(result, rptDate = EndDate)

  # find reserved date
  result <- rptDate.rsvDate(result)

  if(research_mode){
    result <- subset(result, date < Sys.Date() - 10 & date > as.Date("2007-01-01"))
  }
  result <- result[!duplicated(result),]
  result <- na.omit(result)
  return(result)
}


#' get ETS of leader trading stocks.
#'
#' @return ETS object.
#' @export
ets.leader_trade <- function(direction = c("buy","sell")){

  direction <- match.arg(direction)
  mdata <- queryAndClose.dbi(db.local(), "select * from EE_LeaderStockAlter")
  shareholder_type_list <- sort(unique(mdata$shareholder_type))
  if(length(shareholder_type_list) != 3) stop("The structure of database has changed.")
  direction_list <- sort(unique(mdata$direction))
  if(length(direction_list) != 2) stop("The structure of database has changed.")
  if(direction == "buy"){
    ind = 2
  }else if(direction == "sell"){
    ind = 1
  }
  mdata <- subset(mdata, shareholder_type == shareholder_type_list[1] & direction == direction_list[ind])

  # to do ...
  # the duration between change_end_date and announce_date

  # format organizing
  result <- mdata[,c("announcement_date","stockID"), drop = FALSE]
  colnames(result) <- c("date","stockID")
  result$date <- intdate2r(result$date)
  result <- dplyr::arrange(result, date, stockID)
  result <- unique(result)
  return(result)
}

#' get ETS of employee plan
#'
#' @return ETS object
#' @export
ets.employeeplan <- function(){
  mdata <- queryAndClose.odbc(db.jy(), "select * from LC_ESOP")
  mdata$stockID <- stockID2stockID(mdata$InnerCode, from = "jy", to = "local")
  mdata$IniInfoPublDate <- as.Date(mdata$IniInfoPublDate, tz = "Asia/Taipei")
  mdata <- dplyr::arrange(mdata, IniInfoPublDate, stockID)
  result <- mdata[,c("IniInfoPublDate", "stockID")]
  colnames(result) <- c("date","stockID")
  result <- unique(result)
  # to do ...
  # remove events that happen before IPO
  result$IPO_date <- trday.IPO(result$stockID)
  result <- subset(result, date >= IPO_date, select = c("date","stockID"))
  return(result)
}

#' get ETS of st strategy
#'
#' @return ETS object
#' @export
ets.st <- function(){

  # if(missing(begT)) begT <- as.Date("2010-01-04")
  # if(missing(endT))
  endT <- Sys.Date() - 1
  endT_year <- lubridate::year(endT)

  ### Wind Database
  require(WindR)
  WindR::w.start(showmenu = FALSE)
  wind_data <- WindR::w.wset('carryoutspecialtreatment','startdate=2009-01-01',enddate=Sys.Date()-1)
  wind_data <- wind_data$Data

  sec_type_list <- sort(unique(wind_data$sec_type))
  if(length(sec_type_list) != 5) stop("The structure of database has changed.")
  wind_data <- subset(wind_data, sec_type == sec_type_list[1])

  reason_list <- sort(unique(wind_data$reason))
  if(length(reason_list) != 11) stop("The structure of database has changed.")
  wind_data <- subset(wind_data, reason %in% reason_list[5:8])

  wind_data$stockID <- stockID2stockID(wind_data$wind_code, from = "wind", to = "local")
  wind_data$date <- WindR::w.asDateTime(wind_data$implementation_date, asdate = TRUE)

  st_pool <- wind_data[,c("date","stockID")]
  st_pool <- dplyr::arrange(st_pool, date, stockID)
  st_pool <- getrptDate_newest(st_pool, freq = "y", mult = "last")
  st_pool <- st_pool[,c("rptDate","stockID")]
  ets <- data.frame()

  ######## LOOPING STARTS
  while(1){

    st_pool$rptDate <- rptDate.offset(st_pool$rptDate, by = 1, freq = "y")

    if(all(lubridate::year(st_pool$rptDate) > endT_year)) break

    ### get forecastType infomation
    forecast_info <- rptDate.forecastInfo(st_pool)
    st_pool$EndDate <- st_pool$rptDate
    st_pool <- merge.x(st_pool, forecast_info, by = c("EndDate","stockID"))

    ### get is_st
    st_pool$date <- st_pool$InfoPublDate
    st_info <- is_st(st_pool[,c("date","stockID")])
    st_pool$is_st <- st_info$is_st

    # to do ...
    # if st_pool EMPTY
    ets_ <- subset(st_pool, ForcastType == 3 & is_st == TRUE, select = c("InfoPublDate", "stockID", "EndDate"))
    if(nrow(ets_)>0){
      ets <- rbind(ets, ets_)
    }
    st_pool <- subset(st_pool, is_st != FALSE, select = c("rptDate","stockID"))
    if(nrow(st_pool) == 0) break
  }
  # LOOP ENDS

  ### get rsv Date
  ets <- dplyr::arrange(ets, InfoPublDate, stockID)
  colnames(ets) <- c("date","stockID","rptDate")
  rsvDate <- rptDate.rsvDate(ets)
  if(nrow(rsvDate) != nrow(ets)) stop("bug in getting rsvDate")
  ets$rsvDate <- rsvDate$date

  ets <- na.omit(ets)
  # output
  return(ets)
}


# ----- ETS factor function -----

#' return default event set
#'
#' @return vector
#' @export
EE_defaultEvents <- function(){
  event <- c("ets.employeeplan","ets.unfroz","ets.leaderbuy","ets.EQ002_forecast")
  foretell <- c(0,1,0,1)
  re <- data.frame(event, foretell)
  return(re)
}

#' plug in ts object and return with ets object.
#'
#' @param tsobj A ts object.
#' @param EventSet a vector to specify the events. If null, all the events in DefaultEventSet will be applied to use.
#' @return A data frame with date, stockID, event and the lag of days.
#' @export
getETS <- function(tsobj, EventSet = NULL, ee_win = 30, naomit = TRUE){

  ts <- tsobj[,c("date","stockID")]
  ts$date <- trday.nearby(ts$date, 1)
  datelist <- unique(ts$date)
  # load all
  con <- QDataGet::db.local()
  EE_pool <- DBI::dbReadTable(con,'EE_pool')
  DBI::dbDisconnect(con)
  EE_pool$date <- QUtility::intdate2r(EE_pool$date)
  if(!is.null(EventSet)){
    EE_pool <- subset(EE_pool, event %in% EventSet)
  }
  finalre <- list()

  for(i in 1:length(datelist)){
    tmpts <- subset(ts, date == datelist[i])
    startdate <- QDataGet::trday.nearby(datelist[i], by = -ee_win)
    enddate <- QDataGet::trday.nearby(datelist[i], by = ee_win)
    # normal case
    tmppool1 <- subset(EE_pool, date >= startdate & date <= datelist[i] & foretell == 0)
    # special case : unfroz
    tmppool2 <- subset(EE_pool, date >= startdate & date <= enddate & foretell == 1)
    # bind
    tmppool <- rbind(tmppool1,tmppool2)
    if(nrow(tmppool) == 0) next
    tmptmp <- rep(datelist[i], nrow(tmppool)) ### WARNINGS
    tmppool$diff <- trday.count.vec(begTvec =  tmppool$date, endTvec = tmptmp)
    tmppool$date <- datelist[i]
    rownames(tmppool) <- NULL
    tmppool$diff[tmppool$diff > 0] <- tmppool$diff[tmppool$diff > 0] - 1
    tmppool$diff[tmppool$diff < 0] <- tmppool$diff[tmppool$diff < 0] + 1
    tmppool <- dplyr::arrange(tmppool, date, stockID, diff)
    finalre[[i]] <- tmppool
  }
  finalre <- data.table::rbindlist(finalre)
  finalre$date <- trday.nearby(finalre$date, -1)
  # output
  finalre2 <- merge.x(tsobj, finalre, by = c("date","stockID"), mult = "all")
  if(naomit) {finalre2 <- na.omit(finalre2)}
  return(finalre2)
}

#' plug in ts object and return with ets score.
#'
#' @param tsobj A tsobj.
#' @param EventSet a vector to specify the events. If null, all the events in DefaultEventSet will be applied to use.
#' @return ts object with event score.
#' @export
getETSscore <- function(tsobj, EventSet = NULL, ee_win = 30, rollwin = 20,
                        sub_mode = c("mean","sum"), mode = c("mean","sum"),
                        withdetails = TRUE){
  # pre
  sub_mode <- match.arg(sub_mode)
  mode <- match.arg(mode)
  # build ee_score_sum df
  con <- QDataGet::db.local()
  EE_score <- DBI::dbReadTable(con,'EE_score')
  DBI::dbDisconnect(con)
  if(!is.null(EventSet)){
    EE_score <- subset(EE_score, event %in% EventSet)
  }
  eventlist <- unique(EE_score$event)
  EE_score_sum <- list()
  for( i in 1:length(eventlist)){
    subdat <- subset(EE_score, event == eventlist[i], select = c("No","err","event"))
    errsum <- zoo::rollsum(subdat$err, k = rollwin, align = "left")
    subdat <- subdat[1:length(errsum),]
    subdat$err <- errsum
    EE_score_sum[[i]] <- subdat
    gc()
  }
  EE_score_sum <- data.table::rbindlist(EE_score_sum)
  ####
  ts <- tsobj[,c("date","stockID")]
  ets <- getETS(ts, EventSet = EventSet, ee_win = ee_win, naomit = TRUE)
  ets <- QUtility::renameCol(ets, "diff", "No")
  ####
  re <- merge.x(ets, EE_score_sum, by = c("event","No"))
  re <- dplyr::arrange(re, date, stockID)
  re$err <- fillna(re$err, "zero")
  # sum up eventscore and merge
  re_ <- dplyr::group_by(re, date, stockID, event)
  if(sub_mode == "mean"){
    re_ <- dplyr::summarise(re_, eventscore = mean(err))
  }else if(sub_mode == "sum"){
    re_ <- dplyr::summarise(re_, eventscore = sum(err))
  }
  re_ <- reshape2::dcast(re_, formula = date + stockID ~ event)
  if(mode == "mean"){
    re_$totalscore <- colMeans(re_[,3:ncol(re_)], na.rm = TRUE)
  }else if(mode == "sum"){
    re_$totalscore <- colSums(re_[,3:ncol(re_)], na.rm = TRUE)
  }
  # output
  re2 <- merge.x(tsobj, re_)
  re2$totalscore <- fillna(re2$totalscore, "zero")
  if(!withdetails){
    re2 <- subset(re2, select = c("date","stockID","totalscore"))
  }
  return(re2)
}

# ----- LCDB.build & update -----

#' lcdb.build.EE_CroxSecReg
#'
#' @export
lcdb.build.EE_CroxSecReg <- function(begT,endT,factorLists){
  if(missing(begT)){
    begT <- as.Date("2005-01-04")
  }
  if(missing(endT)){
    endT <- Sys.Date()-1
  }
  if(missing(factorLists)){
    factorLists <- list(fl_cap(log = FALSE, bc_lambda = "auto", var = "mkt_cap"))
  }
  RebDates <- getRebDates(begT,endT,rebFreq = "day")
  RebDates <- as.data.frame(RebDates)
  RebDates$year <- lubridate::year(RebDates$RebDates)
  yearlist <- unique(RebDates$year)
  # loop starts
  for(i in 1:length(yearlist)){
    year_ <- yearlist[i]
    cat(year_,'\n')
    RebDates_ <- subset(RebDates, year == year_)
    RebDates_ <- trday.nearby(RebDates_$RebDates, -1)
    TS_ <- getTS(RebDates_, indexID = "EI000985")

    # reg
    reg_result <- reg.TS(TS = TS_, FactorLists = factorLists,
                         dure = lubridate::days(1),
                         sectorAttr = defaultSectorAttr(),
                         regType = "glm")
    result <- reg_result$res

    # organize format
    result$date <- trday.nearby(result$date, 1)
    result$date <- rdate2int(result$date)
    result <- renameCol(result, "res", "err")

    # write into database
    con <- db.local()
    if(i == 1){
      RSQLite::dbWriteTable(con,'EE_CroxSecReg',result,overwrite=T,append=F,row.names=F)
    }else{
      RSQLite::dbWriteTable(con,'EE_CroxSecReg',result,overwrite=F,append=T,row.names=F)
    }
    RSQLite::dbDisconnect(con)
    gc()
    result <- NULL
  }
  return("Done!")
}

#' lcdb.update.EE_CroxSecReg
#'
#' @export
lcdb.update.EE_CroxSecReg <- function(begT, endT, factorLists){

  con <- db.local()
  qr <- paste0("select max(date) from EE_CroxSecReg")
  begT_lcdb <- dbGetQuery(con, qr)[[1]]
  dbDisconnect(con)
  begT_lcdb <- intdate2r(begT_lcdb)

  # get begT, the max date in local database
  if(missing(begT)){
    begT <- begT_lcdb
  }else{
    begT <- trday.nearest(begT, by = -1)
    if(begT <= begT_lcdb){
      begT_ <- rdate2int(begT)
      con <- db.local()
      qr <- paste0("delete from EE_CroxSecReg
                   where date >= ", begT_)
      RSQLite::dbSendQuery(con,qr)
      RSQLite::dbDisconnect(con)
      begT <- trday.nearby(begT,by = -1)
    }else if(begT > begT_lcdb){
      begT <- begT_lcdb
    }
  }

  # endT, the max date should exist in the local database
  if(missing(endT)){
    endT <- Sys.Date() - 1
    endT <- trday.nearest(endT, dir = -1)
  }

  #
  if(begT >= endT){
    return("It's already up-to-date.")
  }else{
    if(missing(factorLists)){
      factorLists <- list(fl_cap(bc_lambda = "auto"))
    }
    # begT <- trday.nearby(begT, by = 1)
    # RebDates <- getRebDates(begT, endT, rebFreq = "day")
    # RebDates <- trday.nearby(RebDates, by = -1)
    endT <- trday.nearby(endT, by = -1)
    RebDates <- getRebDates(begT,endT,rebFreq = "day")
    TS <- getTS(RebDates, indexID = "EI000985")
    reg_result <- reg.TS(TS = TS_, dure = lubridate::days(1),
                        factorLists = factorLists,
                        regType = "glm", sectorAttr = defaultSectorAttr())
    result <- reg_result$res

    # organize formats
    result$date <- trday.nearby(result$date, by = 1)
    result$date <- rdate2int(result$date)
    result <- renameCol(result, "res", "err")

    # write into lcdb
    con <- db.local()
    RSQLite::dbWriteTable(con,'EE_CroxSecReg',result,overwrite=F,append=T,row.names=F)
    RSQLite::dbDisconnect(con)
    return("Done!")
  }
}


#' lcdb.build.EE_CroxSecGroup
#'
#' @export
lcdb.build.EE_CroxSecGroup <- function(begT,endT){
  if(missing(begT)){
    begT <- as.Date("2005-01-04")
  }
  if(missing(endT)){
    endT <- Sys.Date()-1
  }
  RebDates <- getRebDates(begT,endT,rebFreq = "day")
  RebDates <- as.data.frame(RebDates)
  RebDates$year <- lubridate::year(RebDates$RebDates)
  yearlist <- unique(RebDates$year)
  # loop starts
  for(i in 1:length(yearlist)){
    year_ <- yearlist[i]
    cat(year_,'\n')
    RebDates_ <- subset(RebDates, year == year_)
    RebDates_ <- trday.nearby(RebDates_$RebDates, -1)
    TS_ <- getTS(RebDates_, indexID = "EI000985")
    TSR_ <- getTSR(TS_, dure = lubridate::days(1))
    # get TSFR_
    TSRS_ <- getSectorID(TSR_, drop = FALSE, fillNA = TRUE, sectorAttr = defaultSectorAttr(type = "ind_fct"), ungroup = 10)
    TSRS_ <- dplyr::group_by(TSRS_, date, sector)
    TSRS_ <- dplyr::mutate(TSRS_, periodrtn = periodrtn - mean(periodrtn, na.rm = TRUE))
    #
    result <- as.data.frame(TSRS_[,c("date","stockID","periodrtn")])
    result$date <- trday.nearby(result$date, by = 1)
    result$date <- rdate2int(result$date)
    result <- renameCol(result, "periodrtn", "err")

    # write into database
    con <- db.local()
    if(i == 1){
      RSQLite::dbWriteTable(con,'EE_CroxSecGroup',result,overwrite=T,append=F,row.names=F)
    }else{
      RSQLite::dbWriteTable(con,'EE_CroxSecGroup',result,overwrite=F,append=T,row.names=F)
    }
    RSQLite::dbDisconnect(con)
    gc()
    result <- NULL
  }
  return("Done!")
}

#' lcdb.update.EE_CroxSecGroup
#'
#' @export
lcdb.update.EE_CroxSecGroup <- function(){

  con <- db.local()
  qr <- paste0("select max(date) from EE_CroxSecGroup")
  begT_lcdb <- dbGetQuery(con, qr)[[1]]
  dbDisconnect(con)
  begT_lcdb <- intdate2r(begT_lcdb)

  if(missing(begT)){
    begT <- begT_lcdb
  }else{
    begT <- trday.nearest(begT, by = -1)
    if(begT <= begT_lcdb){
      begT_ <- rdate2int(begT)
      con <- db.local()
      qr <- paste0("delete from EE_CroxSecGroup
                   where date >= ", begT_)
      RSQLite::dbSendQuery(con,qr)
      RSQLite::dbDisconnect(con)
      begT <- trday.nearby(begT,by = -1)
    }else if(begT > begT_lcdb){
      begT <- begT_lcdb
    }
  }

  # endT, the max date should exist in the local database
  if(missing(endT)){
    endT <- Sys.Date() - 1
    endT <- trday.nearest(endT, dir = -1)
  }

  if(begT >= endT){
    return("It's already up-to-date.")
  }else{
    # begT <- trday.nearby(begT, by = 1)
    # RebDates <- getRebDates(begT, endT, rebFreq = "day")
    # RebDates <- trday.nearby(RebDates, by = -1)
    endT <- trday.nearby(endT, by = -1)
    RebDates <- getRebDates(begT,endT,rebFreq = "day")
    TS <- getTS(RebDates, indexID = "EI000985")
    TSR <- getTSR(TS, dure = lubridate::days(1))

    # get TSFR_
    TSRS <- getSectorID(TSR, drop = FALSE, fillNA = TRUE, sectorAttr = defaultSectorAttr(type = "ind_fct"), ungroup = 10)
    TSRS <- dplyr::group_by(TSRS, date, sector)
    TSRS <- dplyr::mutate(TSRS, periodrtn = periodrtn - mean(periodrtn, na.rm = TRUE))
    #
    result <- as.data.frame(TSRS[,c("date","stockID","periodrtn")])
    result$date <- trday.nearby(result$date, by = 1)
    result$date <- rdate2int(result$date)
    result <- renameCol(result, "periodrtn", "err")

    # write into lcdb
    con <- db.local()
    RSQLite::dbWriteTable(con,'EE_CroxSecGroup',result,overwrite=F,append=T,row.names=F)
    RSQLite::dbDisconnect(con)
    return("Done!")
  }
}

#' lcdb.build.EE_LeaderStockAlter
#'
#' @examples
#' lcdb.build.EE_LeaderStockAlter()
#' @export
lcdb.build.EE_LeaderStockAlter <- function(){

  require(WindR)
  result <- data.frame()
  startdate <- paste0(2005:2015, "-01-01")
  enddate <- paste0(2005:2015,"-12-31")

  for(i in 1:length(startdate)){
    w_wset_data <- w.wset('majorholderdealrecord',startdate=startdate[i],enddate=enddate[i],'sectorid=a001010100000000;type=announcedate')
    data_ <- w_wset_data$Data
    if(w_wset_data$ErrorCode != 0) return(data_)
    if(nrow(data_) == 0) next
    data_ <- subset(data_, select = -CODE)
    data_ <- dplyr::rename(data_, stockID = wind_code)
    data_$stockID <- stockID2stockID(data_$stockID, from = "wind", to = "local")
    data_$announcement_date <- rdate2int(w.asDateTime(data_$announcement_date, asdate = T))
    data_$change_start_date <- rdate2int(w.asDateTime(data_$change_start_date, asdate = T))
    data_$change_end_date <- rdate2int(w.asDateTime(data_$change_end_date, asdate = T))
    result = rbind(result,data_)
  }
  result <- dplyr::arrange(result, announcement_date, stockID)
  con <- QDataGet::db.local()
  RSQLite::dbWriteTable(con,'EE_LeaderStockAlter',result,overwrite=T,append=F,row.names=F)
  RSQLite::dbDisconnect(con)
  return('Done!')
}

#' lcdb.update.EE_LeaderStockAlter
#'
#' @examples
#' lcdb.update.EE_LeaderStockAlter()
#' @export
lcdb.update.EE_LeaderStockAlter <- function(){
  require(WindR)

  # begT max date in the database
  con <- db.local()
  begT <- RSQLite::dbGetQuery(con, "select max(announcement_date) from EE_LeaderStockAlter")[[1]]
  RSQLite::dbDisconnect(con)
  begT <- intdate2r(begT)

  # endT
  endT <- Sys.Date() - 1

  # main codes
  if(begT >= endT){
    return("Done!")
  }else{
    # begT < endT
    begT = begT + 1
    result <- data.frame()
    # check year laps
    begT_year <- lubridate::year(begT)
    endT_year <- lubridate::year(endT)
    while(begT_year < endT_year){
      tmp_endT <- paste0(begT_year, "-12-31")
      # get data
      # begT to tmp_endT
      w_wset_data <- w.wset('majorholderdealrecord',startdate=begT,enddate=tmp_endT,
                            'sectorid=a001010100000000;type=announcedate')
      data_ <- w_wset_data$Data
      if(w_wset_data$ErrorCode != 0) return(data_)
      if(nrow(data_) > 0){
        data_ <- subset(data_, select = -CODE)
        data_ <- dplyr::rename(data_, stockID = wind_code)
        data_$stockID <- stockID2stockID(data_$stockID, from = "wind", to = "local")
        data_$announcement_date <- rdate2int(w.asDateTime(data_$announcement_date, asdate = T))
        data_$change_start_date <- rdate2int(w.asDateTime(data_$change_start_date, asdate = T))
        data_$change_end_date <- rdate2int(w.asDateTime(data_$change_end_date, asdate = T))
        result = rbind(result,data_)
      }
      # update begT
      begT <- paste0(begT_year+1, "-01-01")
      begT_year = begT_year + 1
    }
    # get data
    # begT to endT
    w_wset_data <- w.wset('majorholderdealrecord',startdate=begT,enddate=endT,
                          'sectorid=a001010100000000;type=announcedate')
    data_ <- w_wset_data$Data
    if(w_wset_data$ErrorCode != 0) return(data_)
    if(nrow(data_) > 0){
      data_ <- subset(data_, select = -CODE)
      data_ <- dplyr::rename(data_, stockID = wind_code)
      data_$stockID <- stockID2stockID(data_$stockID, from = "wind", to = "local")
      data_$announcement_date <- rdate2int(w.asDateTime(data_$announcement_date, asdate = T))
      data_$change_start_date <- rdate2int(w.asDateTime(data_$change_start_date, asdate = T))
      data_$change_end_date <- rdate2int(w.asDateTime(data_$change_end_date, asdate = T))
      result = rbind(result,data_)
    }
    # check format : date : int
    if(nrow(result) > 0){
      result <- dplyr::arrange(result, announcement_date, stockID)
      con <- QDataGet::db.local()
      RSQLite::dbWriteTable(con,'EE_LeaderStockAlter',result,overwrite=F,append=T,row.names=F)
      RSQLite::dbDisconnect(con)
    }
    # done
    return("Done!")
  }
}

#' lcdb.build.EE_score
#'
#' @export
lcdb.build.EE_score <- function(){
  re <- list()
  DefaultEventSet <- EE_defaultEvents()
  for(i in 1:nrow(DefaultEventSet)){
    ets <- eval(call(as.character(DefaultEventSet$event[i])))
    ets <- subset(ets, date <= Sys.Date())
    tserr <- EE_getTSErr(ets, win1 = 60, win2 = 60)
    re[[i]] <- EE_table(tserr)
    re[[i]]$event <- DefaultEventSet$event[i]
  }
  finalre <- data.table::rbindlist(re)
  con <- QDataGet::db.local()
  RSQLite::dbWriteTable(con,'EE_score',finalre,overwrite=T,append=F,row.names=F)
  RSQLite::dbDisconnect(con)
  return("Done!")
}

#' lcdb.build.EE_pool
#'
#' @export
lcdb.build.EE_pool <- function(){
  re <- list()
  DefaultEventSet <- EE_defaultEvents()
  for(i in 1:nrow(DefaultEventSet)){
    re[[i]] <- eval(call(as.character(DefaultEventSet$event[i])))
    re[[i]]$event <- DefaultEventSet$event[i]
    re[[i]]$foretell <- DefaultEventSet$foretell[i]
  }
  finalre <- data.table::rbindlist(re)
  finalre$date <- QUtility::rdate2int(finalre$date)
  finalre <- dplyr::arrange(finalre, date, stockID, event)
  con <- QDataGet::db.local()
  RSQLite::dbWriteTable(con,'EE_pool',finalre,overwrite=T,append=F,row.names=F)
  RSQLite::dbDisconnect(con)
  return("Done!")
}


# ----- Daily Report Part -----

#' rpt.unfroz_show
#'
#' @export
rpt.unfreeze_show <- function(ob_win=10){

  ets0 <- ets.unfreeze(withPercent = TRUE)
  colnames(ets0) <- c("unfreeze_date", "stockID", "proportion")
  ets0$begT <- trday.nearby(ets0$unfreeze_date, -ob_win)

  # for the new stocks that are just included in the table, do not consider rtn
  TD <- trday.nearest(Sys.Date(), 1)
  ets0 <- subset(ets0, TD >= begT & TD <= unfreeze_date )
  ets0$endT <- trday.nearby(TD, -1) # the last trday with existing trading data

  # getPeriodrtn note : begT same as endT, periodrtn 0
  # begT should be one day before endT, get endT rtn
  rtn_data <- getPeriodrtn(stockID = ets0$stockID, begT = trday.nearby(ets0$begT,-1), endT = ets0$endT)
  # do it later # rtn_data$begT <- trday.nearby(rtn_data$begT,1)
  # make it understandable on the table, and match the data in ets0
  rtn_data <- renameCol(rtn_data, "periodrtn", "periodrtn_stock")

  # get index rtn
  rtn_data$indexID <- stockID2indexID(stockID = rtn_data$stockID)$indexID
  rtn_data$periodrtn_index <- 0
  for( i in 1:nrow(rtn_data)){
    if(rtn_data$begT[i] == rtn_data$endT[i]) next
    close_data <- getIndexQuote(stocks = rtn_data$indexID[i], begT = rtn_data$begT[i], endT = rtn_data$endT[i], variables = "close", datasrc = "jy")
    pct_chg <- (close_data$close[nrow(close_data)] - close_data$close[1])/close_data$close[1]
    rtn_data$periodrtn_index[i] <- pct_chg
  }
  # adjust rtn_data begT back
  rtn_data$begT <- trday.nearby(rtn_data$begT, 1L)

  # merge
  result <- merge(ets0, rtn_data, by = c("stockID","begT","endT"))

  # format and output
  result$endT <- NULL
  result$periodrtn_stock[is.na(result$periodrtn_stock)] <- 0
  # result$periodrtn_stock <- round(result$periodrtn_stock, 4)
  # result$proportion <- round(result$proportion, 2)
  result <- dplyr::arrange(result, begT, stockID)
  return(result)
}

#' return daily emotion report
#'
#' @param begT
#' @param endT
#' @return data frame
#' @export
rpt.dailyEmotion <- function(begT, endT){
  # pool
  datelist <- getRebDates(begT, endT, rebFreq = "day")
  stockpool <- getIndexComp("EI000985", endT = datelist) # vec
  total_num <- nrow(stockpool)
  stockpool <- is_suspend(TS = stockpool)
  stockpool <- TS.getTech_ts(stockpool, funchar = c("IsST_()","StockGoMarketDays()"),varname = c("st","days"))

  # loop
  stockpoolrecord <- data.frame()
  finalre <- data.frame()
  for(i in 1:length(datelist)){

    TD_ <- datelist[i]
    TD_ts <- rdate2ts(TD_)
    TD_0 <- trday.nearby(TD_, by = -60)
    TD_0_ts <- rdate2ts(TD_0)
    stockpool_ <- subset(stockpool, date == TD_)
    qr0 <- c("StockHigh(","StockLow(")
    qr0 <- paste0(qr0,TD_0_ts,",")
    qr1 <- c("StockIsZt(","StockIsZt2(","StockIsDt(","StockIsDt2(","StockHigh4(","StockLow4(",qr0)
    qr2 <- paste0(qr1,TD_ts,")")
    qr2 <- c(qr2,"Close()","MA(Close(),5)","MA(Close(),10)","MA(Close(),20)","MA(Close(),60)","MA(Close(),120)","MA(Close(),250)")
    stockpool_ <- TS.getTech_ts(stockpool_, funchar = qr2, varname = c("zt","yzzt","dt","yzdt","high","low","sixtyhigh","sixtylow","close","ma5","ma10","ma20","ma60","ma120","ma250"))

    re <- stockpool_
    stockpoolrecord <- rbind(stockpoolrecord, stockpool_)
    bottom_re <- subset(re, st == 0 & sus == FALSE & days > 120 & yzzt == 0 & yzdt == 0)
    bottom_num <- nrow(bottom_re)

    # zt & dt
    dat_zt <- subset(bottom_re, zt == 1)
    zt_num <- nrow(dat_zt)

    dat_dt <- subset(bottom_re, dt == 1)
    dt_num <- nrow(dat_dt)

    # 60 days best
    dat_sixtyhigh <- subset(bottom_re, high == sixtyhigh)
    sixtyhigh_num <- nrow(dat_sixtyhigh)

    dat_sixtylow <- subset(bottom_re, low == sixtylow)
    sixtylow_num <- nrow(dat_sixtylow)

    # MA arranging
    dat_dtpl <- subset(bottom_re, ma5>ma10 & ma10>ma20 & ma20>ma60 & ma60>ma120 & ma120>ma250)
    dtpl_num <- nrow(dat_dtpl)

    dat_ktpl <- subset(bottom_re, ma5<ma10 & ma10<ma20 & ma20<ma60 & ma60<ma120 & ma120<ma250)
    ktpl_num <- nrow(dat_ktpl)

    # MA arranging2
    dat_ma5 <- subset(bottom_re, close > ma5)
    ma5_num <- nrow(dat_ma5)

    dat_ma20 <- subset(bottom_re, close > ma20)
    ma20_num <- nrow(dat_ma20)

    # output
    finalre_ <- data.frame("date" = TD_, "obs" = bottom_num, "zt" = zt_num, "dt" = dt_num,
                           "sixtyhigh" = sixtyhigh_num, "sixtylow" = sixtylow_num,
                           "dtpl" = dtpl_num, "ktpl" = ktpl_num,
                           "aboveMA5" = ma5_num, "aboveMA20" = ma20_num)
    finalre <- rbind(finalre, finalre_)
  }
  return(finalre)
}




#' rpt.EQ002_show
#'
#' @export
rpt.002_show <- function(begT=as.Date("2013-01-04"),
                         endT=Sys.Date()-1,
                         ob_win = 20, wgtmax = 0.05){
  datelist <- getRebDates(begT,endT,rebFreq = "day")
  ets <- ets.002()
  ets <- dplyr::rename(ets, date_end = date)
  ets$date <- trday.nearby(ets$date_end, by = -ob_win)

  ets <- subset(ets, date_end > begT)
  ets <- subset(ets, date < endT)

  result_list <- EE.port_backtest(ets, wgt_limit = wgtmax, output_type = "rtn")
  return(result_list)
}

#' rpt.st_show
#'
#' @export
rpt.st_show <- function(){
  ets <- ets.st()
  ets <- dplyr::rename(ets, date_end = rsvDate)
  ets <- ets[,c("date","stockID","date_end")]
  result_list <- EE.port_backtest(ets, wgt_limit = 0.05, output_type = "rtn")
  return(result_list)
}

#' RSRS timing
#'
#' @param indexID
#' @param begT
#' @param endT
#' @param param_N The length of time series used to fit beta.
#' @param param_M The length of data used to smooth and adjust beta.
#' @param param_S The epsilon used to control buy and sell.
#' @return A list, containing rtn and finalre.
#' @export
rsrs_timing <- function(indexID = "EI000300",
                        begT = as.Date("2002-01-05"),
                        endT = Sys.Date() - 1,
                        param_N = 18,
                        param_M = 600,
                        param_S = 0.8){
  # index
  datelist <- getRebDates(begT,endT, rebFreq = "day")
  TS <- data.frame("date" = datelist, "stockID" = indexID)
  # high/low price ts
  dat <- TS.getTech_ts(TS, funchar = c("high()","low()"), varname = c("high","low"))
  # fit and get beta
  re <- data.frame()
  for( i in (param_N):length(datelist)){
    TD_ <- datelist[i]
    dat_ <- dat[(i-param_N+1):i,]
    fit <- lm(dat_$high~dat_$low)
    beta_ <- fit$coefficients[2]
    attributes(beta_) <- NULL
    re_ <- data.frame("date" = TD_, "beta" = beta_)
    re <- rbind(re, re_)
  }
  # smooth beta get Z score
  datelist <- re$date
  re_adj <- data.frame()
  for( i in (param_M):length(datelist)){
    TD_ <- datelist[i]
    dat_ <- re[(i-param_M+1):i,]
    temp_ <- scale(dat_$beta)
    beta_adj_ <- tail(temp_,1)
    attributes(beta_adj_) <- NULL
    re_adj_ <- data.frame("date" = TD_, "beta_adj" = beta_adj_)
    re_adj <- rbind(re_adj, re_adj_)
  }# output re_adj
  # backtest
  datelist <- re_adj$date
  TS <- data.frame("date" = datelist, "stockID" = indexID)
  TS <- merge(TS, re_adj, by = "date")
  finalre <- TS
  finalre <- TS.getTech_ts(finalre,funchar = c("StockZf3()","CMa_v(20)"), varname = c("pct_chg","MA"))
  finalre$pct_chg <- finalre$pct_chg/100
  finalre$hold <- 0
  finalre$rtn <- 0
  finalre$NV <- 1
  for( i in 4:length(datelist)){
    if(finalre$hold[i-1] == 1){
      if(finalre$beta_adj[i] < -param_S){
        finalre$hold[i] <- 0
      }else{
        finalre$hold[i] <- 1
      }
    }else if(finalre$hold[i-1] == 0){
      if(finalre$beta_adj[i] > param_S){
        if( finalre$MA[i-1]>finalre$MA[i-2] & finalre$MA[i-1]>finalre$MA[i-3]){
          finalre$hold[i] <- 1
        }
      }
    }
    if(finalre$hold[i-1] == 1){
      finalre$NV[i] <- finalre$NV[i-1] * (1 + finalre$pct_chg[i])
      finalre$rtn[i] <- finalre$pct_chg[i]
    }else{
      finalre$NV[i] <- finalre$NV[i-1]
    }
  }
  rtn <- xts::as.xts(x = finalre$rtn, order.by = finalre$date)
  relist <- list("finalre" = finalre, "rtn" = rtn)
  return(relist)
}
