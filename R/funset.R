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

#' test sensitivity of the parameters
#'
#' @param funcqr The querry as character strings. Replace the param with the name param_1. And plug in the values into later arguments.
#' @param param_1 The values to test, could be vector or list.
#' @param param_2 The values to test, could be vector or list.
#' @param withPlot Logical value. whether plot the matrix.
#' @return Result matrix.
#' @description The function right now could only support 2 params varing. To write the funcqr querry, replace the parameters with the name param_1 and param_2, and plug the vector or list that you would like to test into the latter arguments param_1 and param_2. The result of funcqr could only be one single value.
#' @export
#' @examples
#' paramsensi("sum(1:param_1)", param_1 = 1:20)
#' paramsensi("param_1*param_2", param_1 = 1:10, param_2 = 1:9)
paramsensi <- function(funcqr, param_1, param_2, withPlot = TRUE){
  # table part
  param_1_raw <- param_1
  if(missing(param_2)){
    re <- matrix(nrow = length(param_1_raw))
    for( i in 1:length(param_1_raw)){
      if(is.list(param_1_raw)){
        param_1 <- param_1_raw[[i]]
      }else{
        param_1 <- param_1_raw[i]
      }
      re[i,] <- eval(parse(text = funcqr))
    }
  }else{
    param_2_raw <- param_2
    re <- matrix(nrow = length(param_1_raw), ncol = length(param_2_raw))
    for( i in 1:length(param_1_raw)){
      for(j in 1:length(param_2_raw)){
        if(is.list(param_1_raw)){
          param_1 <- param_1_raw[[i]]
        }else{
          param_1 <- param_1_raw[i]
        }
        if(is.list(param_2_raw)){
          param_2 <- param_2_raw[[j]]
        }else{
          param_2 <- param_2_raw[j]
        }
        re[i,j] <- eval(parse(text = funcqr))
      }
    }
  }
  # plot part
  if(withPlot){
    # rename labels
    if(is.list(param_1_raw)){
      row.names(re) <- paste0("par_1_list_",1:length(param_1_raw))
    }else{
      row.names(re) <- paste0("par_1:",param_1_raw)
    }
    if(!missing(param_2)){
      if(is.list(param_2_raw)){
        colnames(re) <- paste0("par_2_list_",1:length(param_2_raw))
      }else{
        colnames(re) <- paste0("par_2:",param_2_raw)
      }
    }else{
      colnames(re) <- " "
    }
    corrplot::corrplot(re,method = "color",
                       is.corr = FALSE, outline = TRUE, addCoef.col = "black",
                       tl.col = "black", cl.pos = "n", mar = c(1,0.2,2,0.2),
                       tl.srt = 45, tl.cex = 0.8, title = "ParamSensiMatrix")
  }
  # output
  return(re)
}

#' turning ets to rtn series
#'
#' @param etsfunc ETS function strings, supporting multiple functions. The length of the vector must match ob_win.
#' @param funcparlist A list. Could be missing. Otherwise, the length of the list must match etsfunc. Each element of the list should be a list or NULL.
#' @param begT begT
#' @param endT endT. If NULL, then using latest trading day.
#' @param univ stock universe. If NULL, considering all stocks. Default NULL.
#' @param ob_win The holding days window for each ETS strategies.
#' @param wgtmax The max weight limit for each stock.
#' @return A list, rtn and port.
#' @export
#' @description A function used to turning ets to strategies and roughly calculating returns.
#' @examples
#' list1 <- strategy_rtn_rough(etsfunc = "ets.EQ002_forecast", begT = as.Date("2013-01-04"), ob_win = 20)
#' ggplot.WealthIndex(list1$rtn)
#' list2 <- strategy_rtn_rough(etsfunc = c("ets.EQ002_forecast","ets.employeeplan"), funcparlist = list(list(ahead_win = 15),NULL) , begT = as.Date("2013-01-04"), ob_win = c(15,20))
#' ggplot.WealthIndex(list2$rtn)
strategy_rtn_rough <- function(etsfunc, funcparlist,
                               begT=as.Date("2013-01-04"),
                               endT=Sys.Date()-1,
                               univ = NULL,
                               ob_win = 20,
                               wgtmax = 0.05){
  # stocklist
  stocklist <- queryAndClose.dbi(db.local(), "select * from SecuMain")
  stocklist <- subset(stocklist, SecuCategory == 1)
  stocklist <- sort(unique(stocklist$ID))
  # funcparlist
  if(missing(funcparlist)){
    funcparlist <- vector("list",length = length(etsfunc))
  }
  if(length(etsfunc) != length(funcparlist)) stop("length of funcparlist must match etsfunc.")
  for( i in 1:length(funcparlist)){
    if(is.null(funcparlist[[i]])){
      funcparlist[[i]] <- list()
    }
  }
  # input check
  if(length(etsfunc) != length(ob_win)) stop("length of ob_win must match etsfunc.")
  # date
  datelist <- getRebDates(begT,endT,rebFreq = "day")
  # universe
  if(!is.null(univ)){
    TS_raw <- getTS(RebDates = datelist, univ)
  }else{
    TS_raw <- getTS(RebDates = datelist, stocks = stocklist)
  }
  # pool
  ets <- data.frame()
  if(length(etsfunc) > 0){
    for(i in 1:length(etsfunc)){
      ets_ <- do.call(what = etsfunc[i], args = funcparlist[[i]])
      ets_$enddate <- trday.nearby(ets_$date, by = ob_win[i]) # USE ETS DATE SYSTEM
      ets <- rbind(ets, ets_)
    }
    # univ
    ets <- ets[,c("date","enddate","stockID")]
    ets <- merge.x(TS_raw, ets, by = c("date","stockID"))
    ets <- na.omit(ets)
    ets <- ets[,c("date","enddate","stockID")]
  }
  # loop
  port <- list()
  rtn <- vector("numeric",length = length(datelist))
  for( i in 1:length(datelist)){
    TD <- datelist[i]
    TS_ <- subset(ets, (date <= TD & TD <= enddate & date < enddate) |
                    (enddate <= TD & TD <= date & enddate < date), select = stockID) # avoid jump open, delete date
    if(nrow(TS_)==0) next
    #
    TS_$date <- datelist[i]
    TSR_ <- TS.getTech(TS_,variables = "pct_chg")
    TSR_$wgt <- 1/nrow(TS_)
    TSR_$wgt[TSR_$wgt > wgtmax] <- wgtmax
    TSR_$pct_chg <- fillna(TSR_$pct_chg, "zero")
    port[[i]] <- TSR_
    rtn[i] <- sum(TSR_$pct_chg * TSR_$wgt)
  }
  # rtn
  rtn <- fillna(rtn,"zero")
  re <- xts::as.xts(rtn, order.by = datelist)
  # output
  port <- data.table::rbindlist(port)
  relist <- list("rtn" = re, "port" = port)
  return(relist)
}

# ----- Internal using functions -----

#' Fill in the NA.
#'
#' @param vec A vector.
#' @param method The method to fill the NAs, could be median, mean, zero.
#' @return A vector.
#' @export
fillna <- function(vec, method = "mean", trim = NA){
  match.arg(method, c("mean","median","zero"))
  if(method == "mean"){
    if(is.na(trim)){
      vec[is.na(vec)] = mean(vec, na.rm = TRUE)
    }else{
      vec[is.na(vec)] = mean(vec, na.rm = TRUE, trim = trim)
    }
  }else if( method == "median"){
    vec[is.na(vec)] = median(vec, na.rm = TRUE)
  }else if( method == "zero"){
    vec[is.na(vec)] = 0
  }
  return(vec)
}

#' Transform daily TSF to montly TSF.
#'
#' @param ts The TS object.
#' @param db The database containing the daily TSF data.
#' @param window An interger indicating the time window to summarize. If NA, the time window will be the time interval between the rebalance dates.
#' @return A TSF object.
#' @export
getmonthFac <- function(ts, db, window = NA){
  QUtility::check.colnames(ts, c("date","stockID"))
  if(is.na(window)){
    v <- unique(ts$date)
    stocklist <- unique(ts$stockID)
    v0 <- QDataGet::trday.nearby(v[1], by = -20)
    v <- c(v0, v)
    ind <- findInterval(db$date, v, left.open = TRUE)
    db2 <- cbind(db, ind)
    db2 <- subset(db2, ind != 0)
    db2 <- subset(db2, stockID %in% stocklist)
    db2 <- dplyr::group_by(.data = db2, stockID, ind)
    db2 <- dplyr::summarise(db2, newfac = sum(factorscore))
    ind <- findInterval(ts$date, v, left.open = TRUE)
    ts2 <- cbind(ts, ind)
    re <- merge(ts2, db2, by = c("stockID","ind"), all.x = TRUE)
    re2 <- data.frame("date" = re$date, "stockID" = as.character(re$stockID), "factorscore" = re$newfac)
    return(re2)
  }else{
    stocklist <- unique(ts$stockID)
    db <- subset(db, stockID %in% stocklist)
    v1 <- unique(ts$date)
    v2 <- QDataGet::trday.nearby(v1, by = -window)
    ind1 <- findInterval(db$date, v1, left.open = TRUE)
    ind2 <- findInterval(db$date, v2, left.open = FALSE)
    db2 <- cbind(db, ind1, ind2)
    db2 <- dplyr::group_by(db2, stockID, ind1)
    db2 <- dplyr::filter(db2, ind2 >= ind1 + 1)
    db2 <- dplyr::summarise(db2, newfac = sum(factorscore))
    ind1 <- findInterval(ts$date, v1, left.open = TRUE)
    ts2 <- cbind(ts, ind1)
    re <- merge(ts2,db2, by = c("stockID","ind1"), all.x = TRUE)
    re2 <- data.frame("date" = re$date, "stockID" = as.character(re$stockID), "factorscore" = re$newfac)
    return(re2)
  }
}

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


# ----- JY database -----

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
  print(p0)

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

# ----- ETS FRESH -----

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


ets.002_new <- function(research_mode = FALSE){

  mdata <- queryAndClose.odbc(db.quant(),"select * from LC_PerformanceForecast")
  # mdata <- queryAndClose.dbi(db.local(), "select * from LC_PerformanceGrowth
  #                            where PeriodMark = 2 ")

  mdata <- subset(mdata, substr(stockID,1,5) == "EQ002")
  mdata$InfoPublDate <- intdate2r(mdata$InfoPublDate)
  mdata$EndDate <- intdate2r(mdata$EndDate)

  # arrange mdata
  mdata <- dplyr::arrange(mdata, EndDate, stockID, InfoPublDate)

  # find eq002 that meets the requirements
  mdata2 <- mdata
  colnames(mdata2) <- paste0("L.", colnames(mdata2))
  mdata2 <- dplyr::rename(mdata2, stockID = L.stockID)

  # merge
  mdata$L.EndDate <- rptDate.offset(mdata$EndDate, by = -1, freq = "q")

  merged_data <- merge.x(mdata, mdata2, by = c("stockID", "L.EndDate"), mult = "last")

  # bigger NP
  # merged_data <- subset(merged_data, NP_YOY > L.NP_YOY*1.1 & ForcastType == 4)
  merged_data <- subset(merged_data, EGrowthRateCeiling > L.EGrowthRateCeiling & EGrowthRateFloor > L.EGrowthRateFloor & ForcastType == 4 & ForecastObject == 10)
  merged_data$rptDate <- rptDate.offset(merged_data$EndDate, by = 1, freq = "q")
  result <- merged_data[,c("rptDate","stockID")]

  # find reserved date
  result <- rptTS.getFin_ts(result, funchar ='"ReservedDate",report(128002,RDate),
                            "FirstChangedDate",report(128003,RDate),
                            "SecondChangedDate",report(128004,RDate),
                            "ThirdChangedDate",report(128005,RDate)')
  result$PredictedDate <- result$ReservedDate
  ind_ <- result$FirstChangedDate > 0
  result$PredictedDate[ind_] <- result$FirstChangedDate[ind_]
  ind_ <- result$SecondChangedDate > 0
  result$PredictedDate[ind_] <- result$SecondChangedDate[ind_]
  ind_ <- result$ThirdChangedDate > 0
  result$PredictedDate[ind_] <- result$ThirdChangedDate[ind_]
  result <- result[, c("PredictedDate","stockID")]
  result <- dplyr::rename(result, date = PredictedDate)
  result$date <- intdate2r(result$date)
  result$date <- trday.nearest(result$date, dir = 1)
  if(research_mode){
    result <- subset(result, date < Sys.Date() - 10)
  }
  result <- result[!duplicated(result),]
  return(result)
}

# ----- ETS CORE -----

#' get ETS of leader buying stocks.
#'
#' @return ETS object.
#' @export
ets.leaderbuy <- function(datasrc = c("local")){
  con <- QDataGet::db.local()
  qr <- "select * from EE_LeaderStockAlter"
  tmpdat <- DBI::dbGetQuery(con,qr)
  DBI::dbDisconnect(con)
  tmpdat <- subsetCol(tmpdat, "shareholder_type" ,3)
  tmpdat <- subsetCol(tmpdat, "direction", 5)
  tmpdat2 <- subset(tmpdat, select = c("announcement_date","stockID"))
  colnames(tmpdat2) <- c("date","stockID")
  tmpdat2$date <- QUtility::intdate2r(tmpdat2$date)
  tmpdat2 <- dplyr::arrange(tmpdat2, date, stockID)
  return(tmpdat2)
}


#' get ETS of EQ002 forecast strategy.
#'
#' @return ETS object.
#' @export
ets.EQ002_forecast <- function(ahead_win = 20, withlatest = TRUE){

  con <- db.local()
  tmpdat <- dbGetQuery(con, "select * from EE_ForecastAndReport")
  dbDisconnect(con)

  tmpdat <- subset(tmpdat, substr(stockID,1,5) == "EQ002")
  ind <- (tmpdat$EGrowthRateFloor > tmpdat$L.EGrowthRateFloor) & (tmpdat$EGrowthRateCeiling > tmpdat$L.EGrowthRateCeiling)
  ind[is.na(ind)] <- FALSE
  tmpdat$flag <- 0
  tmpdat$flag[ind] <- tmpdat$flag[ind] + 1

  tmpdat_ <- subset(tmpdat, flag == 1 & ForcastType == 4 & ForecastObject == 10)
  tmpdat_ <- dplyr::arrange(tmpdat_, stockID, enddate, date)
  tmpdat_ <- tmpdat_[!duplicated(tmpdat_[,c("stockID","enddate")]),]

  re <- tmpdat_[,c("stockID","FirstReservedDate")]
  colnames(re) <- c("stockID","date")
  tmpdat_$FirstChangeDate <- fillna(tmpdat_$FirstChangeDate, "zero")
  tmpdat_$SecondChangeDate <- fillna(tmpdat_$SecondChangeDate, "zero")
  tmpdat_$ThirdChangeDate <- fillna(tmpdat_$ThirdChangeDate, "zero")
  re$date[tmpdat_$FirstChangeDate > 0] <- tmpdat_$FirstChangeDate[tmpdat_$FirstChangeDate > 0]
  re$date[tmpdat_$SecondChangeDate > 0] <- tmpdat_$SecondChangeDate[tmpdat_$SecondChangeDate > 0]
  re$date[tmpdat_$ThirdChangeDate > 0] <- tmpdat_$ThirdChangeDate[tmpdat_$ThirdChangeDate > 0]

  if(withlatest){
    re <- subset(re, date > 20070000)
  }else{
    re <- subset(re, date > 20070000 & date < 20170000)
  }

  re$date <- intdate2r(re$date)
  re <- re[!duplicated(re),]
  re$date <- trday.nearby(re$date, -ahead_win)
  re <- re[,c("date","stockID")]
  re <- dplyr::arrange(re, date, stockID)
  return(re)
}


# ----- ETS extra -----

#' get ETS of employee plan
#'
#' @return ETS object
#' @export
ets.employeeplan <- function(){
  con <- QDataGet::db.local()
  qr <- "select * from EE_EmployeePlan"
  tmpdat <- DBI::dbGetQuery(con,qr)
  # as.data.frame(table(tmpdat$fund_source))
  mark <- summary(tmpdat$shares_ratio)[[3]]
  tmpdat2 <- subset(tmpdat, shares_ratio >= mark ,select = c("preplan_date","stockID"))
  colnames(tmpdat2) <- c("date","stockID")
  tmpdat2$date <- QUtility::intdate2r(tmpdat2$date)
  return(tmpdat2)
}

#' get ETS of forecast report
#'
#' @return ETS object
#' @export
ets.forecast <- function(season = c("all",'1','2','3','4'), pool=c("002","300","zhuban","all")){
  con <- db.local()
  season <- match.arg(season)
  pool <- match.arg(pool)
  qr <- paste("select stockID, enddate, date, ForcastType, EGrowthRateFloor, EProfitFloor, ActualDate
              from LC_ForecastAndReport")
  res <- dbGetQuery(con, qr)
  res <- subset(res, ForcastType == 4)
  if(season == '1'){
    res <- subset(res, substr(enddate, 5,8) == "0331")
  }else if( season == '2'){
    res <- subset(res, substr(enddate, 5,8) == "0630")
  }else if( season == '3'){
    res <- subset(res, substr(enddate, 5,8) == "0930")
  }else if( season == '4'){
    res <- subset(res, substr(enddate, 5,8) == "1231")
  }
  res <- na.omit(res)
  if( pool == "002"){
    res <- subset(res, substr(stockID, 1, 5) == "EQ002")
  }else if( pool == "300"){
    res <- subset(res, substr(stockID, 1, 5) == "EQ300")
  }else if( pool == "zhuban"){
    res <- subset(res, substr(stockID, 1, 5) != "EQ002")
    res <- subset(res, substr(stockID, 1, 5) != "EQ300")
  }
  datelist <- sort(unique(res$enddate))
  finalres <- list()
  s1 <- 0
  s2 <- 0
  for( i in 1:length(datelist)){
    tmpres <- subset(res, enddate == datelist[i])
    if(nrow(tmpres) < 20) next
    tmpres_ <- subset(tmpres, EGrowthRateFloor >= s1 & EProfitFloor >= s2)
    finalres[[i]] <- tmpres_[,c("date","stockID")]
    colnames(finalres[[i]]) <- c("date","stockID")
    s1 <- quantile(tmpres$EGrowthRateFloor, 0.25)
    s2 <- quantile(tmpres$EProfitFloor, 0.25)
  }
  finalres2 <- data.table::rbindlist(finalres)
  finalres2$date <- QUtility::intdate2r(finalres2$date)
  finalres2 <- na.omit(finalres2)
  return(finalres2)
}


#' get ETS of stocks with low forecasting
#'
#' @return ETS object.
#' @export
ets.low_F_NP <- function(){
  begT <- as.Date("2005-01-31")
  endT <- Sys.Date()-1
  RebDates <- RFactorModel::getRebDates(begT,endT)
  ts <- RFactorModel::getTS(RebDates,"EI000985")
  tsf <- QFactorGet::gf.F_NP_chg(ts,span='w4')
  tmp <- na.omit(tsf)
  tmp <- tmp[tmp$factorscore<(-1),]
  ts2 <- subset(tmp,select=c("date","stockID"))
  return(ts2)
}

#' get ETS of leader selling stocks.
#'
#' @return ETS object.
#' @export
ets.leadersell <- function(){
  con <- QDataGet::db.local()
  qr <- "select * from EE_LeaderStockAlter"
  tmpdat <- DBI::dbGetQuery(con,qr)
  DBI::dbDisconnect(con)
  tmpdat <- subsetCol(tmpdat, "shareholder_type" ,3)
  tmpdat <- subsetCol(tmpdat, "direction", 4)
  tmpdat2 <- subset(tmpdat, select = c("announcement_date","stockID"))
  colnames(tmpdat2) <- c("date","stockID")
  tmpdat2$date <- QUtility::intdate2r(tmpdat2$date)
  tmpdat2 <- dplyr::arrange(tmpdat2, date, stockID)
  return(tmpdat2)
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
    factorLists = buildFactorLists(
      buildFactorList(factorFun = "gf.ln_mkt_cap",
                      factorRefine = setrefinePar(refinePar = refinePar_default("old_robust", sectorAttr = NULL), outlier_method = "none"))
    )
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
    TSRS_ <- getSectorID(TSR_, drop = FALSE, fillNA = TRUE, sectorAttr = defaultSectorAttr(type = "ind_fct"))
    TSRS_ <- TSSregroup(TSRS_)
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
      factorLists = buildFactorLists(
        buildFactorList(factorFun = "gf.ln_mkt_cap",
                        factorRefine = setrefinePar(refinePar = refinePar_default("old_robust", sectorAttr = NULL), outlier_method = "none"))
      )
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
    TSRS <- getSectorID(TSR, drop = FALSE, fillNA = TRUE, sectorAttr = defaultSectorAttr(type = "ind_fct"))
    TSRS <- TSSregroup(TSRS)
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

#' lcdb.build.EE_employeeplan
#'
#' @export
#' @examples
#' lcdb.build.EE_employeeplan()
lcdb.build.EE_employeeplan <- function(){
  require(WindR)
  w.start(showmenu = FALSE)
  result <- data.frame()
  begT <- as.Date("2014-01-01")
  endT <- Sys.Date() - 1
  w_wset_data<-w.wset('esop',startdate=begT,enddate=endT)
  if(w_wset_data$ErrorCode == 0){
    result <- w_wset_data$Data
    result <- subset(result, select = -CODE)
    result <- renameCol(result, "wind_code", "stockID")
    result$stockID <- stockID2stockID(result$stockID, from = "wind", to = "local")
    result$preplan_date <- rdate2int(w.asDateTime(result$preplan_date, asdate = T))
    result$fellow_smtganncedate <- rdate2int(w.asDateTime(result$fellow_smtganncedate, asdate = T))
    result <- result[!is.na(result$stockID),]
    result <- dplyr::arrange(result, preplan_date, stockID)
    con <- db.local()
    RSQLite::dbWriteTable(con,'EE_EmployeePlan',result,overwrite=T,append=F,row.names=F)
    RSQLite::dbDisconnect(con)
    return("Done!")
  }else{
    return(w_wset_data)
  }
}

#' lcdb.update.EE_employeeplan
#'
#' @export
#' @examples
#' library(WindR)
#' lcdb.update.EE_employeeplan()
lcdb.update.EE_employeeplan <- function(){
  # begT
  con <- db.local()
  begT <- RSQLite::dbGetQuery(con, "select max(preplan_date) from EE_EmployeePlan")
  RSQLite::dbDisconnect(con)
  begT <- intdate2r(begT)

  # endT
  endT <- Sys.Date() - 1

  # main codes
  if(begT >= endT){
    return("Done!")
  }else{
    begT <- begT + 1
    require(WindR)
    w.start(showmenu = F)
    w_wset_data<-w.wset('esop',startdate=begT,enddate=endT)
    if(w_wset_data$ErrorCode != 0) return(tmp)
    result <- w_wset_data$Data
    if(nrow(result) == 0){
      return("Done!")
    }else{
      # res <- tmp
      result <- subset(result, select = -CODE)
      result <- renameCol(result, "wind_code", "stockID")
      result$stockID <- stockID2stockID(result$stockID, from = "wind", to = "local")
      result$preplan_date <- rdate2int(w.asDateTime(result$preplan_date, asdate = T))
      result$fellow_smtganncedate <- rdate2int(w.asDateTime(result$fellow_smtganncedate, asdate = T))
      result <- result[!is.na(result$stockID),]
      result <- dplyr::arrange(result, preplan_date, stockID)
      con <- db.local()
      RSQLite::dbWriteTable(con,'EE_EmployeePlan',result,overwrite=F,append=T,row.names=F)
      RSQLite::dbDisconnect(con)
      return("Done!")
    }
  }
}

#' lcdb.build.EE_ForecastAndReport
#'
#' @export
lcdb.build.EE_ForecastAndReport <- function(){
  # RPTSQ
  sq1 <- seq(2000,lubridate::year(Sys.Date())+1)
  sq2 <- c('0331','0630','0930','1231')
  rptsq <- c()
  for(i in sq1){
    rptsq <- c(rptsq, paste0(i,sq2))
  }
  rptsq <- as.integer(rptsq)

  # step 0
  TD_ <- rdate2int(Sys.Date())
  CUT_ <- rptsq[rptsq >= TD_][1]
  rptsq <- rptsq[rptsq <= CUT_]
  fct0 <- data.frame("enddate" = rptsq)

  # stocklist
  con <- db.local()
  stocklist <- RSQLite::dbGetQuery(con, "select * from SecuMain")
  dbDisconnect(con)
  stocklist <- subset(stocklist, SecuCategory == 1)
  stocklist <- sort(unique(stocklist$ID))

  # step 1
  tsInclude()
  tsConnect()
  funchar = "infoarray(128)"
  tmpfile <- stockID2stockID(stocklist,from="local",to="ts")
  tmpcsv <- tempfile(fileext=".csv")
  tmpcsv2 <- stringr::str_replace_all(tmpcsv,'\\\\',"\\\\\\\\")
  write.csv(tmpfile,tmpcsv,row.names=FALSE,quote=FALSE)
  qrstr <- paste0('oV:=BackUpSystemParameters();
                  rdo2 importfile(ftcsv(),"","',tmpcsv2,'",stockframe);
                  factorexp:=&"',funchar,'";
                  result:=array();
                  for i:=0 to length(stockframe)-1 do
                  begin
                  SetSysParam(pn_stock(),stockframe[i]["x"]);
                  factorvalue:=eval(factorexp);
                  result[i]:=factorvalue;
                  end;
                  RestoreSystemParameters(oV);
                  return result;
                  ')
  fct1 <- tsRemoteExecute(qrstr)
  for( i in 1:length(fct1)){
    fct1[[i]] <- plyr::ldply(fct1[[i]], unlist)
    if(nrow(fct1[[i]]) > 0){
      fct1[[i]]$stockID <- stocklist[i]
    }
  }
  fct1 <- data.table::rbindlist(fct1)
  colnames(fct1) <- c("enddate",
                      "FirstReservedDate","FirstChangeDate","SecondChangeDate",
                      "ThirdChangeDate","ActualDate",
                      "FirstReservePublDate","FirstChangePublDate","SecondChangePublDate",
                      "ThirdChangePublDate","ActualPublDate",
                      "stockID")
  # step 2
  qr <- paste("select t.*, 'EQ'+s.SecuCode stockID,
              convert(varchar(8),t.InfoPublDate,112) date,
              convert(varchar(8),t.EndDate,112) enddate
              from dbo.LC_PerformanceForecast t,
              dbo.SecuMain s
              where t.CompanyCode = s.CompanyCode
              and s.SecuCategory in (1,2)")
  fct2 <- queryAndClose.odbc(db.jy(), qr)
  fct2 <- dplyr::select(fct2, -ID, -InfoPublDate, -EndDate, -CompanyCode)
  fct2 <- subset(fct2, substr(stockID,1,3) != "EQ9")
  fct2 <- subset(fct2, substr(stockID,1,3) != "EQ8")
  fct2 <- subset(fct2, substr(stockID,1,3) != "EQX")
  # step 3
  fct3 <- subset(fct2, select = c("stockID","date","enddate","ForcastType","EGrowthRateFloor","EGrowthRateCeiling"))
  fct3 <- dplyr::arrange(fct3, enddate, stockID, desc(date))
  fct3 <- fct3[!duplicated(fct3[,c("stockID","enddate")]),]
  colnames(fct3) <- c("stockID","L.date","enddate","L.ForcastType","L.EGrowthRateFloor","L.EGrowthRateCeiling")
  ind <- match(fct3$enddate, rptsq)
  ind <- ind + 1
  fct3$enddate <- rptsq[ind]
  fct3 <- fct3[!is.na(fct3$enddate),]
  # merging
  res <- merge(fct0, fct1, by = c("enddate"), all = TRUE)
  res <- merge(res, fct2, by = c("stockID","enddate"), all = TRUE)
  res <- merge(res, fct3, by = c("stockID","enddate"), all = TRUE)
  res <- res[!is.na(res$stockID),]
  # organize
  col1 <- c("stockID","enddate","date")
  col2 <- setdiff(colnames(res), col1)
  res <- res[,c(col1,col2)]
  res <- dplyr::arrange(res, stockID, enddate, date)
  # fixation
  dupind <- (1:nrow(res))[duplicated(res[,c("stockID","enddate")])]
  dupind2 <- dupind[res[dupind,c("date")] > res[dupind-1, c("date")]]
  res[dupind2,c("L.date","L.ForcastType","L.EGrowthRateFloor","L.EGrowthRateCeiling")] = res[dupind2-1, c("date","ForcastType","EGrowthRateFloor","EGrowthRateCeiling")]
  # output
  con <- QDataGet::db.local()
  RSQLite::dbWriteTable(con,'EE_ForecastAndReport',res,overwrite=T,append=F,row.names=F)
  RSQLite::dbDisconnect(con)
  return("Done!")
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

#' ST strategy initiate
#'
#' @param begT
#' @param endT
#' @param wgt_limit Default 0.1, the maximum weight for one individual stock.
#' @return A list, containing rtn and port.
#' @export
#' @examples
#' st_strat_relist <- strategy_st_init(begT = as.Date("2010-01-04"), endT = as.Date("2015-04-23"))
strategy_st_init <- function(begT,endT,wgt_limit = 0.1){
  ######## input
  if(missing(begT)){begT <- as.Date("2010-01-04")}
  if(missing(endT)){endT <- Sys.Date()-1}
  ######## prep
  # rpt sq
  sq1 <- seq(2000, 2017)
  sq2 <- c('0331','0630','0930','1231')
  resq <- c()
  for(i in sq1){resq <- c(resq, paste0(i,sq2))}
  resq <- as.integer(resq)
  # db
  tmpdat <- queryAndClose.dbi(db.local(), "select * from EE_ForecastAndReport")
  # rough filter to speed up
  tmpdat <- subset(tmpdat, lubridate::year(begT) - 1 <= as.integer(substr(enddate,1,4)))

  # IsST
  TS_ <- tmpdat[,c("enddate","stockID")]
  colnames(TS_) <- c("date","stockID")
  TS_$date <- intdate2r(TS_$date)
  tmpre <- TS.getTech_ts(TS_,"IsST_()",varname = "flag")
  tmpdat$flag <- tmpre$flag
  # db2
  require(WindR)
  WindR::w.start(showmenu = FALSE)
  w_wset_data <- WindR::w.wset('carryoutspecialtreatment','startdate=2009-01-01',enddate=Sys.Date()-1)
  raw_dat <- w_wset_data$Data
  raw_dat$implementation_date <- WindR::w.asDateTime(raw_dat$implementation_date, asdate = TRUE)
  raw_dat <- subset(raw_dat, sec_type == sort(unique(raw_dat$sec_type))[1])
  reason_case <- sort(unique(raw_dat$reason))
  qualified_st <- subset(raw_dat, reason %in% reason_case[5:8])
  qualified_stockID <- stockID2stockID(qualified_st$wind_code, from = "wind", to = "local")
  qualified_stockID <- unique(qualified_stockID)
  # subset
  tmpdat2 <- subset(tmpdat, flag == 1 & ForcastType == 3 & substr(enddate,5,8) == "1231")
  tmpdat3 <- subset(tmpdat2, stockID %in% qualified_stockID)
  # endT column
  tmpdat3$endT <- tmpdat3$FirstReservedDate
  ind1 <- tmpdat3$FirstChangeDate > 0
  ind1[is.na(ind1)] <- FALSE
  tmpdat3$endT[ind1] <- tmpdat3$FirstChangeDate[ind1]
  ind2 <- tmpdat3$SecondChangeDate > 0
  ind2[is.na(ind2)] <- FALSE
  tmpdat3$endT[ind2] <- tmpdat3$SecondChangeDate[ind2]
  ind3 <- tmpdat3$ThirdChangeDate > 0
  ind3[is.na(ind3)] <- FALSE
  tmpdat3$endT[ind3] <- tmpdat3$ThirdChangeDate[ind3]
  # fix duplicated case
  for(i in 1:nrow(tmpdat3)){
    stockID_ <- tmpdat3$stockID[i]
    enddate_ <- tmpdat3$enddate[i]
    ind_ <- match(enddate_, resq)
    enddate_ <- resq[ind_ - 1]
    re_ <- subset(tmpdat, stockID == stockID_ & enddate == enddate_)
    lasttomatch_ <- re_$ActualDate[1]
    if(is.na(lasttomatch_)) next
    if(tmpdat3$date[i] == lasttomatch_){
      tmpdat3$endT[i] <- NA
    }
  }
  tmpdat3 <- tmpdat3[,c("stockID","enddate","date","endT")]
  tmpdat3 <- na.omit(tmpdat3)
  tmpdat3 <- tmpdat3[!duplicated(tmpdat3[,c("stockID","enddate")]),]
  ######### back test
  holding_case <- tmpdat3[,c("date","endT","stockID")]
  holding_case <- na.omit(holding_case)
  colnames(holding_case) <- c("begT","endT","stockID")
  holding_case$begT <- intdate2r(holding_case$begT)
  holding_case$endT <- intdate2r(holding_case$endT)

  datelist <- getRebDates(begT, endT, rebFreq = "day")
  rtn <- data.frame()
  port <- vector("list", length = length(datelist))
  for(i in 1:length(datelist)){
    TD_ <- datelist[i]
    pool_ <- subset(holding_case, begT <= TD_ & endT >= TD_, select = "stockID")
    if(nrow(pool_) == 0){
      rtn_ <- data.frame("date" = TD_, "rtn" = 0)
      rtn <- rbind(rtn, rtn_)
      next
    }
    pool_$date <- TD_
    pool_ <- pool_[,c("date","stockID")]
    pool_ <- TS.getTech_ts(pool_,"IsST_()",varname = "flag")
    pool_ <- subset(pool_, flag > 0)
    if(nrow(pool_) == 0){
      rtn_ <- data.frame("date" = TD_, "rtn" = 0)
      rtn <- rbind(rtn, rtn_)
      next
    }
    pool_ <- is_priceLimit(pool_, lim = c(-4.9,4.9), priceType = "open")
    if(sum(pool_$overlim) > 0){
      ind_ <- (1:nrow(pool_))[pool_$overlim]
      if(i == 1){
        ind2_ <- rep(FALSE, length(ind_)) # WARININGS
      }else if(is.null(port[[i-1]])){
        ind2_ <- rep(FALSE, length(ind_)) # WARNINGS
      }else{
        ind2_ <- pool_$stockID[ind_] %in% port[[i-1]]$stockID
      }
      if(sum(!ind2_) > 0){
        ind3_ <- ind_[!ind2_]
        pool_$overlim[ind3_] <- NA
      }
    }
    pool_ <- na.omit(pool_)
    if(nrow(pool_) == 0){
      rtn_ <- data.frame("date" = TD_, "rtn" = 0)
      rtn <- rbind(rtn, rtn_)
      next
    }
    row.names(pool_) <- NULL
    pool_$wgt <- 1/nrow(pool_)
    pool_$wgt[pool_$wgt > wgt_limit] <- wgt_limit
    port[[i]] <- pool_
    TSR_ <- TS.getTech(pool_, variables = "pct_chg")
    TSR_$pct_chg <- fillna(TSR_$pct_chg, method = "zero")
    rtn_ <- data.frame("date" = TD_, "rtn" = sum(TSR_$pct_chg * TSR_$wgt))
    rtn <- rbind(rtn, rtn_)
  }
  port2 <- data.table::rbindlist(port)
  st_strat_relist <- list("rtn" = rtn, "port" = port2)
  # output
  return(st_strat_relist)
}


#' ST strategy update
#'
#' @description update the st dataset to the latest date(sys.date - 1).
#' @param wgt_limit Default 0.1, the maximum weight for one individual stock.
#' @return A list, containing rtn and port.
#' @export
#' @examples
#' st_strat_relist <- strategy_st_init(begT = as.Date("2010-01-04"), endT = as.Date("2015-04-23"), save_result = TRUE)
#' st_strat_relist <- strategy_st_upd(st_strat_relist)
strategy_st_upd <- function(st_strat_relist,wgt_limit = 0.1){
  old_rtn <- st_strat_relist$rtn
  old_port2 <- st_strat_relist$port
  # extract date
  begT <- tail(old_port2$date,1)
  endT <- Sys.Date()-1
  endT <- trday.nearest(endT)
  if(begT >= endT) {return("Done!")}
  begT <- trday.nearby(begT,1)
  new_relist <- strategy_st_init(begT, endT, wgt_limit = wgt_limit)
  new_rtn <- new_relist$rtn
  new_port2 <- new_relist$port
  # combine
  rtn <- rbind(old_rtn,new_rtn)
  port2 <- rbind(old_port2, new_port2)
  st_strat_relist <- list("rtn" = rtn, "port" = port2)
  # output
  return(st_strat_relist)
}



#' rpt.unfroz_show
#'
#' @export
rpt.unfroz_show <- function(ob_win=10){

  ets0 <- ets.unfroz(withP = TRUE)
  colnames(ets0) <- c("unfroz_date", "stockID", "proportion")
  ets0 <- subset(ets0, unfroz_date >= trday.nearby(Sys.Date(),-1))

  TD <- Sys.Date()
  ets0$begT <- trday.nearby(ets0$unfroz_date, -ob_win)
  ets0 <- subset(ets0, begT <= Sys.Date())
  ets0$endT <- trday.nearby(TD,-1)
  temp_ <- getPeriodrtn(stockID = ets0$stockID, begT = trday.nearby(ets0$begT,-1), endT = ets0$endT)
  temp_$begT <- trday.nearby(temp_$begT,1)
  temp_ <- renameCol(temp_, "periodrtn", "periodrtn_stock")
  temp_$indexID <- stockID2indexID(stockID = temp_$stockID)$indexID
  temp_$periodrtn_index <- 0
  for( i in 1:nrow(temp_)){
    if(temp_$begT[i] > temp_$endT[i]) next
    rtn <- getIndexQuote(stocks = temp_$indexID[i], begT = temp_$begT[i], endT = temp_$endT[i], variables = "pct_chg", datasrc = "jy")
    temp_$periodrtn_index[i] <- sum(rtn$pct_chg)
  }
  re <- merge(ets0, temp_, by = c("stockID","begT","endT"))
  re <- dplyr::select(re, -endT)
  re$periodrtn_stock <- fillna(re$periodrtn_stock, "zero")
  re$periodrtn_stock <- round(re$periodrtn_stock, 4)
  re$proportion <- round(re$proportion, 2)
  re <- dplyr::arrange(re, begT)
  return(re)
}


#' rpt.EQ002_show
#'
#' @export
rpt.EQ002_show <- function(begT=as.Date("2013-01-04"),
                           endT=Sys.Date()-1,
                           ob_win = 20, wgtmax = 0.05){
  datelist <- getRebDates(begT,endT,rebFreq = "day")
  ets <- ets.EQ002_forecast(withlatest = TRUE, ahead_win = ob_win)
  ets$enddate <- trday.nearby(ets$date, ob_win)
  port <- list()
  rtn <- data.frame()
  for( i in 1:length(datelist)){
    TD <- datelist[i]
    TS_ <- subset(ets, date <= TD & enddate >= TD, select = stockID)
    if(nrow(TS_)==0) next
    TS_$date <- datelist[i]
    TSR_ <- TS.getTech(TS_,variables = "pct_chg")
    TSR_$wgt <- 1/nrow(TS_)
    TSR_$wgt[TSR_$wgt > wgtmax] <- wgtmax
    TSR_$pct_chg <- fillna(TSR_$pct_chg, "zero")
    port[[i]] <- TSR_
    rtn_ <- sum(TSR_$pct_chg * TSR_$wgt)
    rtndf_ <- data.frame("date" = TD, "rtn" = rtn_)
    rtn <- rbind(rtn, rtndf_)
  }
  # rtn
  rtn$rtn <- fillna(rtn$rtn,"zero")
  re <- xts::as.xts(rtn$rtn, order.by = rtn$date)
  # output
  port <- data.table::rbindlist(port)
  relist <- list("rtn" = re, "port" = port)
  return(relist)
}


#' return daily emotion report
#'
#' @param begT
#' @param endT
#' @return data frame
#' @export
rpt.dailyemotion <- function(begT, endT){
  ######## Pool
  datelist <- getRebDates(begT, endT, rebFreq = "day")
  stockpool <- getIndexComp("EI000985", endT = datelist) # vec
  total_num <- nrow(stockpool)
  stockpool <- is_suspend(TS = stockpool)
  stockpool <- TS.getTech_ts(stockpool, funchar = c("IsST_()","StockGoMarketDays()"),varname = c("st","days"))

  ######## LOOP
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
    qr2 <- c(qr2,"IsDtpl(5,30,60,120)","IsKtpl(5,30,60,120)")
    stockpool_ <- TS.getTech_ts(stockpool_, funchar = qr2, varname = c("zt","yzzt","dt","yzdt","high","low","sixtyhigh","sixtylow","dtpl","ktpl"))

    re <- stockpool_
    stockpoolrecord <- rbind(stockpoolrecord, stockpool_)
    bottom_re <- subset(re, st == 0 & sus == FALSE & days > 120 & yzzt == 0 & yzdt == 0)
    bottom_num <- nrow(bottom_re)

    ######## zt & dt
    dat_zt <- subset(bottom_re, zt == 1)
    zt_num <- nrow(dat_zt)

    dat_dt <- subset(bottom_re, dt == 1)
    dt_num <- nrow(dat_dt)

    ####### 60 days best
    dat_sixtyhigh <- subset(bottom_re, high == sixtyhigh)
    sixtyhigh_num <- nrow(dat_sixtyhigh)

    dat_sixtylow <- subset(bottom_re, low == sixtylow)
    sixtylow_num <- nrow(dat_sixtylow)

    ######## MA arranging
    dat_dtpl <- subset(bottom_re, dtpl == 1)
    dtpl_num <- nrow(dat_dtpl)

    dat_ktpl <- subset(bottom_re, ktpl == 1)
    ktpl_num <- nrow(dat_ktpl)

    ######## output
    finalre_ <- data.frame("date" = TD_, "obs" = bottom_num, "zt" = zt_num, "dt" = dt_num,
                           "sixtyhigh" = sixtyhigh_num, "sixtylow" = sixtylow_num, "dtpl" = dtpl_num, "ktpl" = ktpl_num)
    finalre <- rbind(finalre, finalre_)
  }
  return(finalre)
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
  rtn <- as.xts(x = finalre$rtn, order.by = finalre$date)
  relist <- list("finalre" = finalre, "rtn" = rtn)
  return(relist)
}


# ----- Tempararoy Wasted -----

#' plug in ets and return frequency of certain period.
#'
#' @param ets A ets object.
#' @param win1 The days window before the event to calculate the frequency.
#' @return etsobj
etstick <- function(ets, win1 = 20){
  begT <- min(ets$date)
  endT <- max(ets$date)
  date <- QDataGet::trday.get(begT, endT)
  fillindate <- data.frame(date, stockID = "None")
  tmp <- rbind(ets, fillindate)
  tmp <- dplyr::arrange(tmp, date, stockID)
  tmp <- reshape2::dcast(tmp, formula = date~stockID)
  extra <- tmp[1:(win1-1),]
  extra[,-1] <- 0
  extra[,1] <- tmp[1,1]-(win1-1):1
  tmp <- rbind(extra,tmp)
  tmp <- xts::xts(tmp[,-1], order.by = tmp[,1])
  re <- zoo::rollsum(tmp, win1, align = "right")
  date <- zoo::index(re)
  re2 <- as.data.frame(re)
  re2 <- cbind(date, re2)
  re3 <- reshape2::melt(re2, id = 'date')
  colnames(re3) <- c("date","stockID","ticks")
  re4 <- merge(ets, re3, by=c("date","stockID"))
  re4 <- dplyr::arrange(re4, date, stockID)
  re4 <- re4[!duplicated(re4),]
  return(re4)
}


#' plug in ets and return accumulated value of certain varialbe.
#'
#' @param etsobj An etsobj.
#' @param win1 The days window before the event to calculate the accumulated value.
#' @return etsobj
etstack <- function(etsobj, win1 = 20){
  begT <- min(etsobj$date)
  endT <- max(etsobj$date)
  date <- QDataGet::trday.get(begT,endT)
  colnames(etsobj) <- c("date","stockID","var")
  fillindate <- data.frame(date, stockID = "None", var = 0)
  tmp <- rbind(etsobj, fillindate)
  tmp <- dplyr::arrange(tmp, date, stockID)
  tmp <- reshape2::dcast(tmp, formula = date~stockID, fun.aggregate = sum, value.var = "var")
  extra <- tmp[1:(win1-1),]
  extra[,-1] <- 0
  extra[,1] <- tmp[1,1]-(win1-1):1
  tmp <- rbind(extra,tmp)
  tmp[is.na(tmp)] <- 0
  tmp <- xts::xts(tmp[,-1], order.by = tmp[,1])
  re <- zoo::rollsum(tmp, win1, align = "right")
  date <- zoo::index(re)
  re2 <- as.data.frame(re)
  re2 <- cbind(date, re2)
  re3 <- reshape2::melt(re2, id = 'date')
  colnames(re3) <- c("date","stockID","tacks")
  re4 <- merge(etsobj[,c("date","stockID")], re3, by=c("date","stockID"))
  re4 <- dplyr::arrange(re4, date, stockID)
  re4 <- re4[!duplicated(re4),]
  return(re4)
}


#' adjusted subset function for Chinese string.
#'
#' @param tmpdat The data frame.
#' @param colchar The column names.
#' @param subsetcode The code for the subset that is going to be filtered.
#' @return data frame subset.
subsetCol <- function(tmpdat, colchar, subsetcode){
  tmpdat <- renameCol(tmpdat, colchar, "char")
  tmpdat <- merge(tmpdat, EE_CT, by = "char", all.x = TRUE)
  tmpdat <- subset(tmpdat, code == subsetcode)
  tmpdat <- tmpdat[,setdiff(colnames(tmpdat), c("char","code"))]
  return(tmpdat)
}


#' get ETS of leader selling stocks a lot within a few times.
#'
#' @return ETS object.
ets.leadersell_largesell <- function(){
  con <- QDataGet::db.local()
  qr <- "select * from EE_LeaderStockAlter"
  tmpdat <- dbGetQuery(con,qr)
  dbDisconnect(con)
  tmpdat <- subsetCol(tmpdat, "shareholder_type" ,3)
  tmpdat <- subsetCol(tmpdat, "direction", 4)

  tmpdat2 <- subset(tmpdat, select = c("announcement_date","stockID"))
  colnames(tmpdat2) <- c("date","stockID")
  tmpdat2$date <- QUtility::intdate2r(tmpdat2$date)
  tmpdat2 <- dplyr::arrange(tmpdat2, date, stockID)
  tmpdat3 <- etstick(tmpdat2, 20)

  tmpdat2_v <- subset(tmpdat, select = c("announcement_date","stockID","value_change"))
  colnames(tmpdat2_v) <- c("date","stockID","var")
  tmpdat2_v$date <- QUtility::intdate2r(tmpdat2_v$date)
  tmpdat2_v <- dplyr::arrange(tmpdat2_v, date, stockID)
  tmpdat3_v <- etstack(tmpdat2_v, 20)

  re1 <- subset(tmpdat3, ticks <= 10)
  re2 <- subset(tmpdat3_v, tacks >= 1000)
  re <- merge(re1, re2, by = c("date","stockID"))

  re <- subset(re, select = c("date","stockID"))
  return(re)
}


#' get ETS of leader buying stocks a lot within a few times.
#'
#' @return ETS object.
ets.leaderbuy_largebuy <- function(){

  con <- QDataGet::db.local()
  tmpdat <- DBI::dbReadTable(con,'EE_LeaderStockAlter')
  DBI::dbDisconnect(con)
  tmpdat <- subsetCol(tmpdat, "shareholder_type", 3)
  tmpdat <- subsetCol(tmpdat, "direction", 5)

  tmpdat2 <- subset(tmpdat, select = c("announcement_date","stockID"))
  colnames(tmpdat2) <- c("date","stockID")
  tmpdat2$date <- QUtility::intdate2r(tmpdat2$date)
  tmpdat2 <- dplyr::arrange(tmpdat2, date, stockID)
  tmpdat3 <- etstick(tmpdat2, 20)

  tmpdat2_v <- subset(tmpdat, select = c("announcement_date","stockID","change_proportion_floating"))
  colnames(tmpdat2_v) <- c("date","stockID","var")
  tmpdat2_v$date <- QUtility::intdate2r(tmpdat2_v$date)
  tmpdat2_v <- dplyr::arrange(tmpdat2_v, date, stockID)
  tmpdat3_v <- etstack(tmpdat2_v, 20)

  re1 <- subset(tmpdat3, ticks <= 3)
  re2 <- subset(tmpdat3_v, tacks > 0.125)
  re <- merge(re1, re2, by = c("date","stockID"))
  re <- subset(re, select = c("date","stockID"))
  return(re)
}


#' ets.belowExpectation
#'
#' @return ETS object.
ets.belowExpectation <- function(){

  # RPTDATE SEQ
  sq1 <- seq(2000, 2017)
  sq2 <- c('0331','0630','0930','1231')
  rptsq <- c()
  for(i in sq1){
    rptsq <- c(rptsq, paste0(i,sq2))
  }
  rptsq <- as.integer(rptsq)

  # DATA SET 0, FORECAST
  con <- db.local()
  qr <- paste("select * from LC_ForecastAndReport")
  tmpdat <- dbGetQuery(con,qr)
  dbDisconnect(con)
  ind <- !is.na(tmpdat$EGrowthRateFloor) & !is.na(tmpdat$EGrowthRateCeiling)
  tmpdat0 <- tmpdat[ind,]

  # DATA SET 1, LAST FORECAST
  tmpdat1 <- subset(tmpdat0, select=c("stockID","enddate","date","EGrowthRateFloor","EGrowthRateCeiling"))
  colnames(tmpdat1) <- c("stockID","enddate","date","L.EGrowthRateFloor","L.EGrowthRateCeiling")
  tmpdat1 <- dplyr::arrange(tmpdat1, stockID, enddate, date)
  tmpdat1 <- tmpdat1[!duplicated(tmpdat1,fromLast = T),]
  tmpdat1 <- dplyr::select(tmpdat1, -date)
  ind <- match(tmpdat1$enddate, rptsq)
  ind <- ind + 1
  tmpdat1$enddate <- rptsq[ind]

  # merge
  finalre <- merge(tmpdat0, tmpdat1, by = c("stockID","enddate"), all.x = T)
  finalre <- dplyr::arrange(finalre, stockID, enddate)

  # compute flags
  # flag1
  finalre$flag1 <- 0
  ind1 <- finalre$EGrowthRateCeiling < finalre$L.EGrowthRateCeiling
  ind1[is.na(ind1)] <- FALSE
  ind2 <- finalre$EGrowthRateFloor < finalre$L.EGrowthRateFloor
  ind2[is.na(ind2)] <- FALSE
  finalre$flag1[ind1] <- finalre$flag1[ind1] + 1
  finalre$flag1[ind2] <- finalre$flag1[ind2] + 1

  finalrere <- subset(finalre, flag1 >= 2 & ForcastType == 4, select = c("date","stockID"))
  finalrere <- na.omit(finalrere)
  finalrere$date <- intdate2r(finalrere$date)
  return(finalrere)
}

