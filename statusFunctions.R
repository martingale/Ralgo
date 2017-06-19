require(httr)
require(jsonlite)

orderList <- function(Username, Password, AccountID, CustomerNo, ExchangeID){
  queryList <- list(MsgType = "AF", Username = Username, Password = Password, AccountID = AccountID, CustomerNo = CustomerNo,ExchangeID = ExchangeID, Filter = "A")
  responseRaw <- GET("https://tb.matriksdata.com/0202/Integration.aspx?", query = queryList)
  responseText <- content(responseRaw,"text")
  list(parsed = fromJSON(responseText), raw = minify(responseText), queryText = responseRaw$url)
}

# tmp <- orderList(Username, Password, AccountID, CustomerNo, ExchangeID)

# tmp$Item[tmp$Item$OrderStatus==2,]
# tmp$Item[tmp$Item$OrderStatus==0,]

positionList <- function(Username, Password, AccountID, CustomerNo, ExchangeID){
  queryList <- list(MsgType = "AN", Username = Username, Password = Password, AccountID = AccountID, CustomerNo = CustomerNo,ExchangeID = ExchangeID)
  responseRaw <- GET("https://tb.matriksdata.com/0202/Integration.aspx?", query = queryList)
  responseText <- content(responseRaw,"text") 
  list(parsed = fromJSON(responseText), raw = minify(responseText), queryText = responseRaw$url)
}

# tmp1 <- positionList(Username, Password, AccountID, CustomerNo, ExchangeID)

# pos <- (tmp1$Item)
# pos[pos$Symbol=="IHLAS",]
