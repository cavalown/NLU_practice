

######## jieba basic ########
# 先安裝jieba套件，並匯入
install.packages("jiebaR")
library(jiebaR)
# 建立一個分詞器
seg_worker = worker() 
# 分詞
segment("這是我的第一段文本",seg_worker)
seg_worker

# 顯示jieba預設詞庫的位址
show_dictpath()
# 查看目錄
dir(show_dictpath())


######## 網頁rss擷取製作文字雲 ########
### 製作文字雲
# 安裝套件
install.packages("devtools")
install.packages("tidyRSS")
install.packages("XML")
install.packages("RCurl")
install.packages("plyr")
install.packages("wordcloud")
install.packages("wordcloud2")
# 匯入套件
library(tidyRSS)
library(XML)
library(RCurl)
library(jiebaR)
library(stringr)
library(plyr)
library(wordcloud)
library(wordcloud2)

#選定一個RSS的URL（以公視新聞為例）
rss_url <- 'https://about.pts.org.tw/rss/XML/newsfeed.xml'
rss <- tidyRSS::tidyfeed(feed = rss_url) 

# request
ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.122 Safari/537.36"
myHttpHeader <- c(
  "User-Agent"=ua,
  "accept"="image/webp,image/apng,image/*,*/*;q=0.8",
  "accept-Language"="zh-TW,zh;q=0.9,en-US;q=0.8,en;q=0.7",
  "accept-encoding"="gzip, deflate, br",
  "Connection"="keep-alive",
  "cache-control"="no-cache",
  "Accept-Charset"="UTF8,utf-8;q=0.7,*;q=0.7"
   )
curl_handle <- getCurlHandle()
curlSetOpt(.opts = list(myHttpHeader), cookiejar="cookies.txt", useragent = ua, followlocation = TRUE, curl=curl_handle, verbose = TRUE)

# 用xpath爬蟲
data <- list()
i<-1
for( link in rss$item_link ){
  print( paste(i, link, sep=","))
  html_doc <- htmlParse(getURL(link, curl = curl_handle ),encoding = "utf-8")
  article_item <- xpathSApply(html_doc, '/html/body/section/div/div/div[1]/div/div[2]/div[1]/div[4]', xmlValue)
  article_item <- gsub("\\s+", "", article_item)
  article_item <- gsub(" $", "", article_item)
  article_item <- paste(article_item, collapse = " ")
  data[i] <- article_item
  i <- i+1
  #t <- sample(2:5,1)  #可以為爬蟲設定休息時間，比較不易被鎖IP
  #Sys.sleep(t)
  }
data <- unlist(data) #把分行的資料接起來
View(data)

# 分詞不帶stop_word
cutter = worker()
seg_words <- cutter <= data

# 分詞帶stop_word
cutter = worker(stop_word = "/Users/huangyiling/my_dicts_folder/my_jieba_dict/stop_words.utf8",
                user = "/Users/huangyiling/my_dicts_folder/my_jieba_dict/user.dict.utf8")
seg_words <- cutter <= data


# 用R計算字詞出現頻率
table_words <- count(seg_words)

# 在R用SQL計算字詞出現頻率
install.packages("sqldf")
library(sqldf)
seg_words1 <- as.data.frame(x = seg_words)
table_words <- sqldf("SELECT seg_words,count(*) FROM seg_words1 group by seg_words")

# 繪製文字雲wordcloud
wordcloud(table_words[,1], table_words[,2], random.order=F,family = "PingFang HK") 
wordcloud(table_words[,1], table_words[,2], random.order=F,ordered.colors = T, colors = rainbow(length(row.names(table_words))) ,family = "PingFang HK")

# 繪製文字雲wordcloud2
wordcloud2(table_words, size = 1, shape = "circle", fontFamily = "PingFang HK", color = "random-light", backgroundColor = "grey") #demoFreq是该包自带的数据集



