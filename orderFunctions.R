require("jsonlite")
require("httr")

SideParameters <- function(x){
  errorMsg <- 'Side must be one of "buy" or "sell"'
  if(length(x) > 1) stop(errorMsg)
  x <- trimws(x,"both")
  x <- tolower(x)
  key <- switch(x,
                "buy" = 1,
                "sell" = 2)
  if(length(key) != 1) stop(errorMsg)
  return(key)
}

# position effect parameter conversation @ Tablo 6.6
PositionEffectParameters <- function(x){
  errorMsg <- 'PositionEffect must be one of "none", "default", "open" or "close"'
  if(length(x) > 1) stop(errorMsg)
  x <- trimws(x,"both")
  x <- tolower(x)
  key <- switch(x,
                "none" = 0,
                "default" = 1,
                "open" = 2,
                "close" = 3)
  if(length(key) != 1) stop(errorMsg)
  return(key)
}

# order type parameter conversation (only first 3) @ Tablo 3.8
OrderTypeParameters <- function(x){
  errorMsg <- 'OrderType must be one of "none", "mkt" or "lmt"'
  if(length(x) > 1) stop(errorMsg)
  x <- trimws(x,"both")
  x <- tolower(x)
  key <- switch(x,
                "none" = 0,
                "mkt" = 1,
                "lmt" = 2)
  if(length(key) != 1) stop(errorMsg)
  return(key)
}

# time in force parameter conversation (only first 5) @ Tablo 3.9
TimeInForceTypeParameters <- function(x){
  errorMsg <- 'TimeInForce must be one of "none", "session", "day", "goodtillcancel", "immediateorcancel" or "fillorkill"'
  if(length(x) > 1) stop(errorMsg)
  x <- trimws(x,"both")
  x <- tolower(x)
  key <- switch(x,
                "none" = -1,
                "session" = 8,
                "day" = 0,
                "goodtillcancel" = 1,
                "immediateorcancel" = 3, # piyasa fiyatlı satışta kesinlikle kullanılacak TODO.
                "fillorkill" = 4)
  if(length(key) != 1) stop(errorMsg)
  return(key)
}

# transaction type parameter conversation @ Tablo 3.10
TransactionTypeParameters <- function(x){
  errorMsg <- 'TransactionType must be one of "none", "normal", "shortdefault", "shortdaily", "virman", "credit" or "cloceshort"'
  if(length(x) > 1) stop(errorMsg)
  x <- trimws(x,"both")
  x <- tolower(x)
  key <- switch(x,
                "none" = 0,
                "normal" = 1,
                "shortdefault" = 2,
                "shortdaily" = 3,
                "virman" = 4,
                "credit" = 5,
                "closeshort" = 6)
  if(length(key) != 1) stop(errorMsg)
  return(key)
}

parameters <- function(x,argument){
  switch(argument,
         "Side" = SideParameters(x),
         "PositionEffect" = PositionEffectParameters(x),
         "OrderType" = OrderTypeParameters(x),
         "TimeInForce" = TimeInForceTypeParameters(x),
         "TransactionType" = TransactionTypeParameters(x)
  )
}

argumentList <<- c("MsgType","CustomerNo","Username","Password","AccountID","sourceID","OutputType","ClientIP","ClOrdID","Side","MarketCode",
                   "Symbol","ExchangeID","PositionEffect","OrderType","LimitPrice","StopPrice","Quantity","TimeInForce","ExpireDate","TransactionType",
                   "ParentRef", "OrderID", "OrderID2", "PricePrev", "LeavesQty")

serverAddr <<- "https://tb.matriksdata.com/0202/Integration.aspx?"

addOrder <- function(Username, Password, AccountID, CustomerNo, ClOrdID, Symbol,
                     OrderType = c("none", "mkt", "lmt"), Side = c("buy", "sell"), LimitPrice = 0, Quantity, PositionEffect = c("none", "default", "open", "close"),
                     TimeInForce = c("none", "session", "day", "goodtillcancel", "immediateorcancel", "fillorkill"), 
                     TransactionType = c("none", "normal", "shortdefault", "shortdaily", "virman", "credit", "closeshort"), ...){
  argumentList <- argumentList
  serverAddr <- serverAddr
  MsgType <- "D"
  ExchangeID <- 4
  Side <- parameters(Side,"Side")
  PositionEffect <- parameters(PositionEffect,"PositionEffect")
  OrderType <- parameters(OrderType,"OrderType")
  TimeInForce <- parameters(TimeInForce,"TimeInForce")
  TransactionType <- parameters(TransactionType,"TransactionType")
  tmp <- mget(ls()[ls() %in% argumentList])
  
  x <- GET(serverAddr, query=tmp)
  list(query = x$url, response = fromJSON(content(x,as="text")))
}

replaceOrder <- function(Username, Password, AccountID, CustomerNo, ClOrdID, OrderID, OrderID2, Symbol,
                         OrderType = c("none", "mkt", "lmt"), Side = c("buy", "sell"), LimitPrice, Quantity, PricePrev, LeavesQty, 
                         PositionEffect = c("none", "default", "open", "close"),
                         TimeInForce = c("none", "session", "day", "goodtillcancel", "immediateorcancel", "fillorkill"), 
                         TransactionType = c("none", "normal", "shortdefault", "shortdaily", "virman", "credit", "closeshort"), ...){
  argumentList <- argumentList
  serverAddr <- serverAddr
  MsgType <- "G"
  ExchangeID <- 4
  Side = c("buy", "sell")
  PositionEffect <- parameters(PositionEffect,"PositionEffect")
  OrderType <- parameters(OrderType,"OrderType")
  TimeInForce <- parameters(TimeInForce,"TimeInForce")
  TransactionType <- parameters(TransactionType,"TransactionType")
  tmp <- mget(ls()[ls() %in% argumentList])
  
  x <- GET(serverAddr,query=tmp)
  list(query = x$url, response = fromJSON(content(x,as="text")))
}

cancelOrder <- function(Username, Password, AccountID, CustomerNo, ClOrdID, OrderID, OrderID2, Symbol,
                        OrderType = c("none", "mkt", "lmt"), Side = c("buy", "sell"), PositionEffect = c("none", "default", "open", "close"),
                        TimeInForce = c("none", "session", "day", "goodtillcancel", "immediateorcancel", "fillorkill"), 
                        TransactionType = c("none", "normal", "shortdefault", "shortdaily", "virman", "credit", "closeshort"), ...){
  argumentList <- argumentList
  serverAddr <- serverAddr
  MsgType <- "F"
  ExchangeID <- 4
  Side = c("buy", "sell")
  PositionEffect <- parameters(PositionEffect,"PositionEffect")
  OrderType <- parameters(OrderType,"OrderType")
  TimeInForce <- parameters(TimeInForce,"TimeInForce")
  TransactionType <- parameters(TransactionType,"TransactionType")
  tmp <- mget(ls()[ls() %in% argumentList])
  
  x <- GET(serverAddr,query=tmp)
  list(query = x$url, response = fromJSON(content(x,as="text")))
}
# # 
# # New account
# CustomerNo <- "91902"
# Username <- "123456"
# Password <- "aC4fQ9"
# 
# # positionList(Username = Username, Password = Password,AccountID = 0,CustomerNo = CustomerNo, ExchangeID = 4)
# 
# # 
# # 
# # CustomerNo <- 176081
# # Username <- "123456"
# # Password <- "2016omd"
# AccountID <- 0
# ClOrdID <- 1
# Side <- "buy"
# Symbol <- "GARAN"
# # LimitPrice <- 0.30
# Quantity <- -1
# PositionEffect <- "open"
# OrderType <- "mkt"
# TimeInForce <- "immediateorcancel"
# TransactionType <- "normal"
# 
# # 
# a1 <- addOrder(Username,Password,AccountID,CustomerNo,ClOrdID,Symbol,OrderType,Side,,Quantity,PositionEffect,TimeInForce,TransactionType)
# # # c1 <- cancelOrder(Username,Password,AccountID,CustomerNo,ClOrdID,OrderID = "3929264",OrderID2="",Symbol,OrderType,Side,PositionEffect,TimeInForce,TransactionType)
# # # # GET("https://tb.matriksdata.com/9999/Integration.aspx?", query = list(b=1,tmp="kamil"))
# # # replaceOrder(Username,Password,AccountID,CustomerNo,ClOrdID,OrderID = "MTX203845",OrderID2 = "",Symbol,OrderType,Side,LimitPrice = 7.11,
# # #              Quantity = 7,PricePrev = 7.09,LeavesQty = 5,PositionEffect,TimeInForce,TransactionType)
