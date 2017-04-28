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

#' get month MA for TS
#'
#' @param ts TS.
#' @param N
#' @return dataframe.
#' @export
#' @examples
#' ts <- ets.employeeplan()
#' N = 9
#' test <- MonthMA(ts,N)
MonthMA <- function(ts, N){
  # tmp funcs
  TS.getTech_ts2 <- function(TS,funchar,varname=funchar, Rate=1, RateDay=0){
    check.TS(TS)

    tmpfile <- TS
    tmpfile$stockID <- stockID2stockID(tmpfile$stockID,from="local",to="ts")
    tmpfile$date <- as.character(tmpfile$date)

    tmpcsv <- tempfile(fileext=".csv")
    tmpcsv2 <- stringr::str_replace_all(tmpcsv,'\\\\',"\\\\\\\\")
    write.csv(tmpfile,tmpcsv,row.names=FALSE,quote=FALSE)

    subqr <- ""
    for( i in 1:length(funchar)){
      subqr <- paste0(subqr, '  factorexp[',i-1,'] := &"',funchar[i],'";  ')
    }
    subqr2 <- ""
    if(!is.null(Rate)){
      subqr2 <- paste0(subqr2, ' SetSysParam(pn_rate(), ',Rate,'); ')
    }
    if(!is.null(RateDay)){
      subqr2 <- paste0(subqr2, ' SetSysParam(pn_rateday(), ',RateDay,'); ')
    }
    len.funchar <- length(funchar)-1

    qrstr <- paste0('oV:=BackUpSystemParameters();
                    SetSysParam(pn_cycle(),cy_month());
                    ',subqr2,'
                    rdo2 importfile(ftcsv(),"","',tmpcsv2,'",timestockframe);
                    factorexp := array();
                    ',subqr,'
                    result:=array();
                    for i:=0 to length(timestockframe)-1 do
                    begin
                    SetSysParam(pn_stock(),timestockframe[i]["stockID"]);
                    SetSysParam(pn_date(),strtodate(timestockframe[i]["date"]));
                    for j:= 0 to ',len.funchar,' do
                    begin
                    factorvalue:=eval(factorexp[j]);
                    result[i][j]:=factorvalue;
                    end;
                    end;
                    RestoreSystemParameters(oV);
                    return result;')
    tsRequire()
    fct <- tsRemoteExecute(qrstr)
    fct <- plyr::ldply(fct, unlist)
    colnames(fct) <- varname
    result <- cbind(TS,fct)
    return(result)
  }
  # begin
  ts_raw <- ts
  ts$date2 <- trday.offset(datelist = ts$date, by = months(-1))
  ts$lag <- lubridate::month(ts$date) - lubridate::month(ts$date2)
  # adjust over adjustment
  ind_ <- ts$lag != 1 & ts$lag != -11
  ind_ <- (1:nrow(ts))[ind_]
  for( i in ind_){
    while(ts$lag[i] == 2 | ts$lag[i] == -10){
      ts$date2[i] <- trday.nearby(ts$date2[i], by = 2)
      ts$lag[i] <- lubridate::month(ts$date[i]) - lubridate::month(ts$date2[i])
    }
  }
  # double check
  ts$lag <- lubridate::month(ts$date) - lubridate::month(ts$date2)
  ind_ <- ts$lag != 1 & ts$lag != -11
  if(sum(ind_) != 0) stop("bugs in date adjustment")
  # get monthly ma
  ts <- renameCol(ts, "date", "raw_date")
  ts <- renameCol(ts, "date2","date")
  funchar <- paste0("ma(close(),",N-1,")")
  ts <- TS.getTech_ts2(TS = ts, funchar = funchar, varname = "MApart")
  # get price
  ts <- renameCol(ts, "date", "adj_date")
  ts <- renameCol(ts, "raw_date","date")
  ts <- TS.getTech_ts(ts, funchar = "close()", varname = "close")
  # recalculate
  ts$MA <- (ts$close + ts$MApart*(N-1))/N
  # output
  re <- ts[,c("date","stockID","MA")]
  return(re)
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
  if( method == "mean"){
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

#' Expand ETS object into TS object with index. Multi-row version.
#'
#' @param ets A event TS object which includes the event date and the corresponding stock.
#' @param win1 Integer. The time window of days before the event date.
#' @param win2 Integer. The time window of days after the event date.
#' @return A TS object with index.
#' @export
EE_ExpandETS <- function(ets, win1, win2){
  QUtility::check.colnames(ets, c("date","stockID"))
  len <- win1+1+win2
  stockID.col <- rep(ets$stockID, each = len)
  date.col <- QDataGet::trday.nearest(ets$date, dir = 1)
  begT.col <- QDataGet::trday.nearby(date.col, by = -win1)
  endT.col <- QDataGet::trday.nearby(date.col, by = win2)
  # T.df <- data.frame("begT" = begT.col, "endT" = endT.col)
  date.col <- trday.get.vec(begT.col, endT.col)
  if(win1 > 0){
    b <- c((-win1:-1),0:win2)
    No <- rep(b, nrow(ets))
  }else{
    b <- 0:win2
    No <- rep(b, nrow(ets))
  }
  res <- data.frame("No" = No, "date" = date.col, "stockID" = stockID.col)
  return(res)
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
#' TSErr <- EE_GetTSErr(ETS)
#' EE_Plot(TSErr)
EE_GetTSErr <- function(ETS, db = c("EE_CroxSecReg","pct_chg","pct_chg_bmk"), win1 = 20, win2 = 60, bmk = NULL) {
  QUtility::check.colnames(ETS, c('date','stockID'))
  db <- match.arg(db)
  TargetTS <- EE_ExpandETS(ETS, win1 = win1, win2 = win2)
  if(db == "pct_chg"){
    finalres <- getTSR(TargetTS)
    finalres <- finalres[,c("No","date","stockID","periodrtn")]
    finalres <- renameCol(finalres, "periodrtn", "err")
  }else if(db == "pct_chg_bmk"){
    if(is.null(bmk)){bmk = "EI000001"}
    finalres <- getTSR(TargetTS)
    TS_bmk <- data.frame("date" = finalres$date, "stockID" = bmk)
    TS_bmk <- TS.getTech_ts(TS_bmk,"StockZf3()")
    finalres$err <- finalres$periodrtn - TS_bmk$`StockZf3()`/100
    finalres <- finalres[,c("No","date","stockID","err")]
  }else{
    TargetTS$date <- QUtility::rdate2int(TargetTS$date)
    con <- QDataGet::db.local()
    DBI::dbWriteTable(conn = con, value = TargetTS, name = 'mazi_tmp', overwrite = TRUE, append = FALSE, row.names = FALSE)
    qr <- paste (
      "select a.*, b.err
      from mazi_tmp a
      left join ", db, " b
      on a.stockID = b.stockID
      and a.date = b.date"
    )
    finalres <- DBI::dbGetQuery(con, qr)
    DBI::dbDisconnect(conn = con)
    finalres$date <- QUtility::intdate2r(finalres$date)
  }
  # output
  QUtility::check.colnames(finalres,c("No","date","stockID","err"))
  return(finalres)
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
#' TSErr <- EE_GetTSErr(ETS)
#' EE_Plot(TSErr)
EE_Plot <- function(TSErr, bmk = NULL){
  TSErr$err <- fillna(TSErr$err, method = "zero")
  TSErr0 <- dplyr::group_by(TSErr, No)
  TSErr0 <- dplyr::mutate(TSErr0, err = robustHD:::winsorize(err, const = 3))
  p0 <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = TSErr0, ggplot2::aes(x= No, group = No, y =err))+
    ggplot2::geom_vline(xintercept = -0.5, color = 'red', linetype = 2)+
    ggplot2::ylab("Abnormal Return")+ggplot2::xlab("Date Series")
  print(p0)
  cat("Press ENTER to continue")
  line <- readline()
  if(!is.null(bmk)){
    tmpbmk <- EE_table(bmk, withSTD = TRUE)
    tmpdat <- EE_table(TSErr, withSTD = TRUE)
    tmpdat$err <- tmpdat$err - tmpbmk$err
    tmpvec <- cumprod(tmpdat$err+1)
  }else{
    tmpdat <- EE_table(TSErr, withSTD = TRUE)
    tmpvec <- cumprod(tmpdat$err+1)
  }
  TSErr1 <- data.frame('No'=tmpdat$No, 'err' = tmpvec, 'std' = tmpdat$std)
  p1 <-  ggplot2::ggplot()+
    ggplot2::geom_ribbon(data = TSErr1, ggplot2::aes(x = No, ymin=err-std, ymax=err+std), fill="grey70")+
    ggplot2::geom_path(data = TSErr1, ggplot2::aes(x = No, y=err), size = 1) +
    ggplot2::geom_vline(xintercept = -1, color = 'red', linetype = 2)+
    ggplot2::ylab("Accumulated Abnormal Return")+ggplot2::xlab("Date Series")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
  TSErr2 <- tmpdat
  p2 <- ggplot2::ggplot()+
    ggplot2::geom_bar(data = TSErr2, ggplot2::aes(x = No, y=err), stat = 'identity')+
    ggplot2::geom_vline(xintercept = -0.5, color = 'red', linetype = 2)+
    ggplot2::ylab("Daily Abnormal Return")+ggplot2::xlab("Date Series")
  re <- QUtility::multiplot(plotlist = list(p1,p2), ncol=1)
  return(re)
}

#' Plug in TSErr object and return the mean summary table
#'
#' @param TSErr The TSErr object
#' @param withSTD Logical. Whether to include STD in the return.
#' @return Dataframe.
#' @export
EE_table <- function(TSErr, withSTD = TRUE, withACC = FALSE){
  TSErr$err <- fillna(TSErr$err, method = "zero")
  tmpdat <- dplyr::group_by(TSErr, No)
  if(withSTD){
    tmpdat <- dplyr::summarise(tmpdat, mean = mean(err), std = sqrt(var(err)))
    colnames(tmpdat) <- c("No","err","std")
  }else{
    tmpdat <- dplyr::summarise(tmpdat, mean = mean(err))
    colnames(tmpdat) <- c("No","err")
  }
  if(withACC){
    tmpdat$acc_err <- cumprod(tmpdat$err + 1)
  }
  return(tmpdat)
}

#' Split and return the plots of each year
#'
#' @param TSErr The TSErr object.
#' @param everyyear Logical value. Whether to return the plot of details in each year.
#' @param breakwindow Logical value. Whether to keep the event window complete or forcely cut the object by year. Default is false.
#' @return A list containing plots and data.
#' @export
EE_splityear <- function(TSErr, everyyear = FALSE, bmk = NULL){
   if(!is.null(bmk)){
     bmk <- dplyr::arrange(bmk, date, stockID)
      coreETS_0 <- subset(bmk, No == 0, select = c("date", "stockID"))
      tmpyy_0 <- lubridate::year(coreETS_0$date)
      yy_0 <- rep(tmpyy_0, each = nrow(bmk)/nrow(coreETS_0))
      yy_0 <- as.factor(yy_0)
      bmk$yy <- yy_0
   }
  TSErr <- dplyr::arrange(TSErr, date, stockID)

  coreETS <- subset(TSErr, No == 0, select = c("date", "stockID"))
  tmpyy <- lubridate::year(coreETS$date)
  yy <- rep(tmpyy, each = nrow(TSErr)/nrow(coreETS))
  yy <- as.factor(yy)
  TSErr$yy <- yy

  yylist <- unique(TSErr$yy)
  tmplist <- list()
  if(!is.null(bmk)){
    if( !all(unique(yy) %in% unique(yy_0)) ){
      stop("bmk time sequence is not complete")
    }
  }
  for(i in 1:length(yylist)){
    tmp <- subset(TSErr, yy == yylist[i])
    tmpdat <- EE_table(tmp, withSTD = FALSE)
    if(!is.null(bmk)){
      tmpbmk <- subset(bmk, yy == yylist[i])
      tmpdatbmk <- EE_table(tmpbmk, withSTD = FALSE)
      tmpdat$err <- tmpdat$err - tmpdatbmk$err
    }
    tmpvec <- cumprod(tmpdat$err+1)
    tmplist[[i]] <- data.frame('No'=tmpdat$No, 'err' = tmpvec)
    tmplist[[i]]$year <- levels(yy)[i]
  }
  TSErr1 <- data.table::rbindlist(tmplist)
  fig <- ggplot2::ggplot() +
    ggplot2::geom_path(data = TSErr1, ggplot2::aes(x = No, y=err, colour = year), size = 1) +
    ggplot2::geom_vline(xintercept = -1, color = 'red', linetype = 2)+
    ggplot2::ylab("Accumulated Abnormal Return") + ggplot2::xlab("Date Series")
  reslist <- list(fig)
  if(everyyear == TRUE){
    for( i in 1:length(tmplist)){
      tmp.fig <- ggplot2::ggplot()+
        ggplot2::geom_path(data = tmplist[[i]], ggplot2::aes(x = No, y=err), size = 1) +
        ggplot2::geom_vline(xintercept = -1, color = 'red', linetype = 2)+
        ggplot2::ylab("Accumulated Abnormal Return")+ggplot2::xlab("Date Series")+ggplot2::ggtitle(yylist[i])
      reslist[[i+1]] <- tmp.fig
    }
    names(reslist) <- c("all years",levels(yy))
  }else{
    names(reslist) <- c("all years")
  }
  reslist$data <- TSErr1
  return(reslist)
}

#' conduct day 0 statistics analysis
#'
#' @param ets ets.
#' @return A list.
#' @export
#' @examples
#' ets <- ets.employeeplan()
#' EE_Day0(ets)
EE_Day0 <- function(ets){
  # db
  re <- TS.getTech(ets, variables = c("pct_chg","pre_close","open","high","low"))
  re$jump_open <- (re$open - re$pre_close)/re$pre_close
  re$vibration <- (re$high -re$low)/re$pre_close
  nrow1 <- nrow(re)
  re <- na.omit(re)
  nrow2 <- nrow(re)
  # suspend ratio
  suspend_ratio <- (nrow1 - nrow2)/nrow1
  # basic stat
  re_sub <- re[,c("pct_chg","jump_open","vibration")]
  output_table <- data.frame("mean" = sapply(re_sub, mean), "std" = sqrt(sapply(re_sub, var)),
                             "median" = sapply(re_sub, median), "mad" = sapply(re_sub, mad))
  # winning ratio
  winning_ratio <- sum(re_sub$pct_chg > 0)/nrow(re_sub)
  # output
  re <- list(suspend_ratio, winning_ratio, output_table)
  names(re) <- c("suspend_ratio","winning_ratio","table")
  return(re)
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
#' EE_Analyzer(ets)
EE_Analyzer <- function(ets, db = c("EE_CroxSecReg","pct_chg"), win1 = 20, win2 = 30){
  db <- match.arg(db)
  tserr <- EE_GetTSErr(ets, db = db, win1 = win1, win2 = win2)
  # daily table
  daily_table <- EE_table(tserr, withACC = TRUE)
  # stat table
  stat_table <- data.frame("winning_rate" = mean(daily_table$err > 0), "rtn" = tail(daily_table$acc_err, 1), "std" = sqrt(var(daily_table$err)))
  # ee_plot
  fig <- EE_Plot(tserr)
  # ee_day0
  day_0_table <- EE_Day0(ets)
  # output
  relist <- list(stat_table, day_0_table, daily_table, fig)
  names(relist) <- c("Stat", "Day 0 Performance", "Daily Performance", "Plot")
  return(relist)
}


#' split ets into group and conduct analysis
#'
#' @param ets ETS.
#' @param group_mode One single character string. Could be "year", "sector", "simple_sector", "EQ"(002,300,etc.), "size".
#' @param customized_level A vector indicating how to group ets. If this argument is supplied, the group_mode will be neglect.
#' @param db The database of studying objects. Whether to study residuals or daily_pct_chg.
#' @param win1 The time window(days) before the events happened.
#' @param win2 The time window(days) after the events happened.
#' @param withplot Logical value.
#' @param minimum_group_num The minimum number for each sub group.
#' @return list.
#' @export
#' @examples
#' ets <- ets.employeeplan()
#' EE_SplitAnalyzer(ets,"year")
#' EE_SplitAnalyzer(ets,"sector", minimum_group_num = 10)
#' EE_SplitAnalyzer(ets,"simple_sector")
#' EE_SplitAnalyzer(ets,"EQ")
#' EE_SplitAnalyzer(ets,"size")
EE_SplitAnalyzer <- function(ets, group_mode, customized_level,
                             db = c("EE_CroxSecReg","pct_chg"), win1 = 20, win2 = 30,
                             withplot = TRUE, minimum_group_num = 0){
  # input & args
  db <- match.arg(db)
  # split part
  if(missing(customized_level)){
    if(group_mode == "year"){
      ets$group <- lubridate::year(ets$date)
    }else if(group_mode == "sector"){
      ets <- getSectorID(ets)
    }else if(group_mode == "simple_sector"){
      ets <- getSectorID(ets, sectorAttr = list("std" = 336, "level" = 1))
    }else if(group_mode == "EQ"){
      ets$group <- "ZHUBAN"
      ets$group[substr(ets$stockID,3,5) == "002"] <- "002"
      ets$group[substr(ets$stockID,3,5) == "300"] <- "300"
    }else if(group_mode == "size"){
      ets <- gf.ln_mkt_cap(ets)
      ets$group <- "small"
      ets$group[ets$factorscore > median(ets$factorscore, na.rm = TRUE)] <- "large"
      ets <- ets[,c("date","stockID","group")]
    }
  }else{
    if(length(customized_level) != nrow(ets)) stop("The length of level must match ets.")
    ets$group <- customized_level
  }
  re1 <- ets
  colnames(re1) <- c("date","stockID","group")
  # core part
  loopindex <- sort(unique(re1$group))
  re2 <- data.frame()
  stat_df <- data.frame()
  for( i in 1:length(loopindex)){
    # basic process
    dat_ <- subset(re1, group == loopindex[i])
    ets_ <- dat_[,c("date","stockID")]
    if(nrow(ets_) <= minimum_group_num) next
    tserr_ <- EE_GetTSErr(ets_, db = db, win1 = win1, win2 = win2)
    re_ <- EE_table(tserr_)
    re_$acc_err <- cumprod(re_$err + 1)
    re_$group <- loopindex[i]
    re2 <- rbind(re2,re_)
    # stat
    df_ <- data.frame("acc_rtn" = tail(re_$acc_err,1),"std" = sqrt(var(re_$err)),"obs" = nrow(ets_), row.names = loopindex[i])
    stat_df <- rbind(stat_df, df_)
  }
  # plot
  if(withplot){
    re2$group <- as.character(re2$group)
    fig <- ggplot2::ggplot() +
      ggplot2::geom_path(data = re2, ggplot2::aes(x = No, y= acc_err, colour = group), size = 1) +
      ggplot2::geom_vline(xintercept = -1, color = 'red', linetype = 2)+
      ggplot2::ylab("Accumulated Abnormal Return") + ggplot2::xlab("Day Series")
  }
  # output part - list
  relist <- list(stat_df, fig)
  names(relist) <- c("Stat","Plot")
  return(relist)
}

# ----- ETS factor function -----

#' return default event set
#'
#' @return vector
#' @export
EE_DefaultEventSet <- function(){
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
    tmptmp <- rep(datelist[i], nrow(tmppool))
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

# ----- ETS CORE -----

#' get ETS of stocks unfrozing
#'
#' @return ETS object.
#' @export
ets.unfroz <- function(withP = FALSE){
  df_jy <- EE_getETSfromJY(date.column = "StartDateForFloating", SheetName = "LC_SharesFloatingSchedule", key.df = data.frame("kkey"=c("Proportion1","SourceType"),"decode"=c(NA,NA),"isSE"=c(NA,NA)))
  df_jy <- subset(df_jy, Proportion1>5 & SourceType %in% c(24,25))
  if(withP){
    ETS <- subset(df_jy, select = c("date","stockID","Proportion1"))
  }else{
    ETS <- subset(df_jy, select = c("date","stockID"))
  }
  return(ETS)
}

#' get ETS of leader buying stocks.
#'
#' @return ETS object.
#' @export
ets.leaderbuy <- function(){
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
    re <- subset(re, date > 20080000)
  }else{
    re <- subset(re, date > 20080000 & date < 20170000)
  }
  re$date <- intdate2r(re$date)
  re <- re[!duplicated(re),]
  re$date <- trday.nearby(re$date, -ahead_win)
  re <- re[,c("date","stockID")]
  re <- dplyr::arrange(re, date, stockID)
  return(re)
}

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


# ----- ETS extra -----


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
      buildFactorList(factorFun = "gf.ln_mkt_cap", factorStd = "norm", factorNA = "na"))
  }
  RebDates <- getRebDates(begT,endT,rebFreq = "day")
  monthind <- cut.Date2(RebDates,"month")
  monthlist <- unique(monthind)
  loopind <- data.frame(RebDates, monthind)
  for( ii in 1:(length(monthlist))){
    cat(rdate2int(as.Date(monthlist[ii])),"\n")
    loopind_ <- subset(loopind, monthind == monthlist[ii])
    RebDates_ <- loopind_$RebDates
    TS_ <- getTS(RebDates_, indexID = "EI000985")
    res_list <- suppressWarnings(reg.TS(TS = TS_,FactorLists=factorLists,dure = lubridate::days(1)))
    finalre <- res_list$res
    finalre <- renameCol(finalre, "res", "err")
    finalre$date <- trday.nearby(finalre$date, by = 1)
    finalre$date <- rdate2int(finalre$date)
    con <- QDataGet::db.local()
    if(ii == 1){
      RSQLite::dbWriteTable(con,'EE_CroxSecReg',finalre,overwrite=T,append=F,row.names=F)
    }else{
      RSQLite::dbWriteTable(con,'EE_CroxSecReg',finalre,overwrite=F,append=T,row.names=F)
    }
    RSQLite::dbDisconnect(con)
    gc()
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

  if(missing(begT)){
    begT <- begT_lcdb
  }else{
    begT <- trday.nearest(begT)
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

  if(missing(endT)){
    endT <- Sys.Date() - 1
    endT <- trday.nearest(endT)
  }

  if(begT >= endT){
    return("It's already up-to-date.")
  }else{
    if(missing(factorLists)){
      factorLists = buildFactorLists(
        buildFactorList(factorFun = "gf.ln_mkt_cap", factorStd = "norm", factorNA = "na"))
    }
    begT <- trday.nearby(begT, by = 1)
    RebDates <- getRebDates(begT,endT,rebFreq = "day")
    monthind <- cut.Date2(RebDates,"month")
    monthlist <- unique(monthind)
    loopind <- data.frame(RebDates, monthind)
    for( ii in 1:(length(monthlist))){
      cat(rdate2int(as.Date(monthlist[ii])),"\n")
      loopind_ <- subset(loopind, monthind == monthlist[ii])
      RebDates_ <- loopind_$RebDates
      TS_ <- getTS(RebDates_, indexID = "EI000985")
      res_list <- suppressWarnings(reg.TS(TS = TS_, dure = lubridate::days(1), factorLists = factorLists,
                                          regType = "glm", glm_wgt = "sqrtFV"))
      finalre <- res_list$res
      finalre <- renameCol(finalre, "res", "err")
      finalre$date <- trday.nearby(finalre$date, by = 1)
      finalre$date <- rdate2int(finalre$date)
      con <- QDataGet::db.local()
      RSQLite::dbWriteTable(con,'EE_CroxSecReg',finalre,overwrite=F,append=T,row.names=F)
      RSQLite::dbDisconnect(con)
      gc()
    }
    return("Done!")
  }
}


#' lcdb.build.EE_LeaderStockAlter
#'
#' @examples
#' library(WindR)
#' lcdb.build.EE_LeaderStockAlter()
#' @export
lcdb.build.EE_LeaderStockAlter <- function(){
  WindR::w.start(showmenu = FALSE)
  res <- data.frame()
  s.date <- c()
  e.date <- c()
  for(ii in 1990:2015){
    s.date <- c(s.date, paste(ii,"-01-01", sep=""))
    e.date <- c(e.date, paste(ii,"-12-31", sep=""))
  }
  len <- length(s.date)
  for(ii in 1:len){
    w_wset_data <- WindR::w.wset('majorholderdealrecord',startdate=s.date[ii],enddate=e.date[ii],'sectorid=a001010100000000;type=announcedate')
    tmp <- w_wset_data$Data
    if(w_wset_data$ErrorCode != 0) return(tmp)
    if(nrow(tmp) == 0) next
    tmp <- tmp[, setdiff(colnames(tmp),"CODE")]
    tmp <- QUtility::renameCol(tmp,"wind_code","stockID")
    tmp$stockID <- QDataGet::stockID2stockID(tmp$stockID, from = "wind", to = "local")
    tmp$announcement_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$announcement_date, asdate = T))
    tmp$change_start_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_start_date, asdate = T))
    tmp$change_end_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_end_date, asdate = T))
    res = rbind(res,tmp)
  }
  res <- dplyr::arrange(res, announcement_date, stockID)
  con <- QDataGet::db.local()
  RSQLite::dbWriteTable(con,'EE_LeaderStockAlter',res,overwrite=T,append=F,row.names=F)
  RSQLite::dbDisconnect(con)
  return('Done!')
}

#' lcdb.update.EE_LeaderStockAlter
#'
#' @examples
#' library(WindR)
#' lcdb.update.EE_LeaderStockAlter()
#' @export
lcdb.update.EE_LeaderStockAlter <- function(){
  con <- QDataGet::db.local()
  re <- DBI::dbReadTable(con,'EE_LeaderStockAlter')
  begT <- QUtility::intdate2r(max(re$announcement_date))
  begT <- begT+1
  endT <- Sys.Date()
  oldyy <- lubridate::year(begT)
  newyy <- lubridate::year(endT)
  while(newyy > oldyy){
    WindR::w.start(showmenu = F)
    endT.tmp <- as.Date(paste(oldyy,"-12-31",sep=""))
    w_wset_data<- WindR::w.wset('majorholderdealrecord',startdate=begT,enddate=endT.tmp,'sectorid=a001010100000000;type=announcedate')
    tmp <- w_wset_data$Data
    if(w_wset_data$ErrorCode != 0) return(tmp)
    if(nrow(tmp) != 0) {
      tmp <- tmp[, setdiff(colnames(tmp),"CODE")]
      tmp <- QUtility::renameCol(tmp,"wind_code","stockID")
      tmp$stockID <- QDataGet::stockID2stockID(tmp$stockID, from = "wind", to = "local")
      tmp$announcement_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$announcement_date, asdate = T))
      tmp$change_start_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_start_date, asdate = T))
      tmp$change_end_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_end_date, asdate = T))
      tmp <- dplyr::arrange(tmp, announcement_date, stockID)
      DBI::dbWriteTable(con, "EE_LeaderStockAlter", tmp, overwrite=F, append=T, row.names=F)
      DBI::dbDisconnect(con)
    }
    begT = as.Date(paste(oldyy+1,"-01-01",sep=""))
    oldyy <- lubridate::year(begT)
  }
  if(begT >= endT){
    return('Done!')
  }else{
    WindR::w.start(showmenu = F)
    w_wset_data<- WindR::w.wset('majorholderdealrecord',startdate=begT,enddate=endT,'sectorid=a001010100000000;type=announcedate')
    tmp <- w_wset_data$Data
    if(w_wset_data$ErrorCode != 0) return(tmp)
    if(nrow(tmp) == 0) {
      return('Done!')
    }else{
      tmp <- tmp[, setdiff(colnames(tmp),"CODE")]
      tmp <- QUtility::renameCol(tmp,"wind_code","stockID")
      tmp$stockID <- QDataGet::stockID2stockID(tmp$stockID, from = "wind", to = "local")
      tmp$announcement_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$announcement_date, asdate = T))
      tmp$change_start_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_start_date, asdate = T))
      tmp$change_end_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_end_date, asdate = T))
      tmp <- dplyr::arrange(tmp, announcement_date, stockID)
      DBI::dbWriteTable(con, "EE_LeaderStockAlter", tmp, overwrite=F, append=T, row.names=F)
      DBI::dbDisconnect(con)
      return('Done!')
    }
  }
}

#' lcdb.build.EE_employeeplan
#'
#' @export
#' @examples
#' library(WindR)
#' lcdb.build.EE_employeeplan()
lcdb.build.EE_employeeplan <- function(){
  WindR::w.start(showmenu = FALSE)
  res <- data.frame()
  s.date <- as.Date("2014-01-01")
  e.date <- Sys.Date()
  w_wset_data<-w.wset('esop',startdate=s.date,enddate=e.date)
  if(w_wset_data$ErrorCode == 0){
    res <- w_wset_data$Data
    res <- res[,setdiff(colnames(res), "CODE")]
    res <- QUtility::renameCol(res, "wind_code", "stockID")
    res$stockID <- QDataGet::stockID2stockID(res$stockID, from = "wind", to = "local")
    res$preplan_date <- QUtility::rdate2int(WindR::w.asDateTime(res$preplan_date, asdate = T))
    res$fellow_smtganncedate <- QUtility::rdate2int(WindR::w.asDateTime(res$fellow_smtganncedate, asdate = T))
    res <- res[!is.na(res$stockID),]
    res <- dplyr::arrange(res, preplan_date, stockID)
    con <- QDataGet::db.local()
    RSQLite::dbWriteTable(con,'EE_EmployeePlan',res,overwrite=T,append=F,row.names=F)
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
  con <- QDataGet::db.local()
  re <- DBI::dbReadTable(con,'EE_EmployeePlan')
  begT <- QUtility::intdate2r(max(re$preplan_date))
  begT <- begT+1
  endT <- Sys.Date()
  if(begT >= endT){
    return("Done!")
  }else{
    WindR::w.start(showmenu = F)
    w_wset_data<-w.wset('esop',startdate=begT,enddate=endT)
    tmp <- w_wset_data$Data
    if(w_wset_data$ErrorCode != 0) return(tmp)
    if(nrow(tmp) == 0){
      return("Done!")
    }else{
      res <- tmp
      res <- res[,setdiff(colnames(res), "CODE")]
      res <- QUtility::renameCol(res, "wind_code", "stockID")
      res$stockID <- QDataGet::stockID2stockID(res$stockID, from = "wind", to = "local")
      res$preplan_date <- QUtility::rdate2int(WindR::w.asDateTime(res$preplan_date, asdate = T))
      res$fellow_smtganncedate <- QUtility::rdate2int(WindR::w.asDateTime(res$fellow_smtganncedate, asdate = T))
      res <- res[!is.na(res$stockID),]
      res <- dplyr::arrange(res, preplan_date, stockID)
      con <- QDataGet::db.local()
      RSQLite::dbWriteTable(con,'EE_EmployeePlan',res,overwrite=F,append=T,row.names=F)
      RSQLite::dbDisconnect(con)
      return("Done!")
    }
  }
}

#' lcdb.build.EE_score
#'
#' @export
lcdb.build.EE_score <- function(){
  re <- list()
  DefaultEventSet <- EE_DefaultEventSet()
  for(i in 1:nrow(DefaultEventSet)){
    ets <- eval(call(as.character(DefaultEventSet$event[i])))
    ets <- subset(ets, date <= Sys.Date())
    tserr <- EE_GetTSErr(ets, win1 = 60, win2 = 60)
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
  DefaultEventSet <- EE_DefaultEventSet()
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
  stocklist <- dbGetQuery(con, "select * from SecuMain")
  dbDisconnect(con)
  stocklist <- subset(stocklist, SecuCategory == 1)
  stocklist <- sort(unique(stocklist$ID))
  # step 1
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

# ----- Daily Report Part -----

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
  rtn <- c()
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
  tmpdat <- QUtility::renameCol(tmpdat, colchar, "char")
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

