# TaiwanGovDrunkenCarAccidentToll
內政部開放資料 [歷年酒駕肇事受傷及死亡件數、人數統計表]

## 內政資料開放平臺
[按我開啟網頁](https://data.moi.gov.tw/)

## API 來源網址
[按我開啟網頁](https://data.gov.tw/dataset/9018)

## 安裝套件
install.packages("jsonlite")

install.packages("curl")

install.packages("ggplot2")


## 範例圖片
![範例圖片](https://github.com/telunyang/TaiwanGovDrunkenCarAccidentToll/blob/master/plot.png)

## 資料來源
```
{
  "success": true,
  "result": {
    "resource_id": "A01010000C-000462-015",
    "limit": 2000,
    "total": 11,
    "fields": [
      {
        "type": "text",
        "id": "year"
      },
      {
        "type": "text",
        "id": "A1-count"
      },
      {
        "type": "text",
        "id": "A2-count"
      },
      {
        "type": "text",
        "id": "dead"
      },
      {
        "type": "text",
        "id": "A1-hurt"
      },
      {
        "type": "text",
        "id": "A2-hurt"
      }
    ],
    "records": [
      {
        "year": "時間別",
        "A1-count": "A1件數",
        "A2-count": "A2件數",
        "dead": "死亡人數",
        "A1-hurt": "A1受傷人數",
        "A2-hurt": "A2受傷人數"
      },
      {
        "year": "97年",
        "A1-count": "474",
        "A2-count": "9105",
        "dead": "500",
        "A1-hurt": "211",
        "A2-hurt": "11695"
      },
      {
        "year": "98年",
        "A1-count": "386",
        "A2-count": "9410",
        "dead": "397",
        "A1-hurt": "157",
        "A2-hurt": "11997"
      },
      {
        "year": "99年",
        "A1-count": "399",
        "A2-count": "10599",
        "dead": "419",
        "A1-hurt": "154",
        "A2-hurt": "13366"
      },
      {
        "year": "100年",
        "A1-count": "412",
        "A2-count": "11261",
        "dead": "439",
        "A1-hurt": "137",
        "A2-hurt": "14144"
      },
      {
        "year": "101年",
        "A1-count": "370",
        "A2-count": "9745",
        "dead": "376",
        "A1-hurt": "118",
        "A2-hurt": "12075"
      },
      {
        "year": "102年",
        "A1-count": "234",
        "A2-count": "7877",
        "dead": "245",
        "A1-hurt": "107",
        "A2-hurt": "9691"
      },
      {
        "year": "103年",
        "A1-count": "160",
        "A2-count": "7353",
        "dead": "169",
        "A1-hurt": "79",
        "A2-hurt": "9056"
      },
      {
        "year": "104年",
        "A1-count": "137",
        "A2-count": "6521",
        "dead": "142",
        "A1-hurt": "59",
        "A2-hurt": "8061"
      },
      {
        "year": "105年",
        "A1-count": "95",
        "A2-count": "5600",
        "dead": "102",
        "A1-hurt": "54",
        "A2-hurt": "6939"
      },
      {
        "year": "106年",
        "A1-count": "85",
        "A2-count": "4954",
        "dead": "87",
        "A1-hurt": "58",
        "A2-hurt": "6102"
      }
    ]
  }
}
```

## 原始碼
```
# 匯入 json 的套件
library(jsonlite)

# 匯入圖形套件
library(ggplot2)

# json 來源網址，來自內政部
url_json <- "https://od.moi.gov.tw/api/v1/rest/datastore/A01010000C-000462-015"

# 將 json 轉成 dataframe
df <- fromJSON(url_json)

# 去除標題列
df <- df$result$records
df <- df[-1,]

# 將人數從 json 的文字格式，轉成數值，才不會在作 fit regression 的時候出錯
df$dead <- as.numeric(df$dead)

# 因 ggplot 的標題排序會影響到我的設定，所以另行排序
df$year <- factor(df$year, levels = unique(df$year))

# 列數
x_vector <- c(1:nrow(df))

# 建立模型
lr_model <- lm(dead ~ x_vector, df)

# 截距
intc <- coef(lr_model)[1]

# 斜率
slp <- coef(lr_model)[2]

# 製作圖表
chart <- ggplot(data = df, aes(x = df$year, y = df$dead, label = df$dead))
chart <- chart + geom_point()
chart <- chart + geom_text(hjust = 0, nudge_x = 0.05)
chart <- chart + geom_abline(intercept = intc, slope = slp, linetype = "dashed", color = "red", size = 1)
chart <- chart + geom_smooth(method = "lm", se = FALSE)
chart <- chart + ggtitle("內政部開放資料【歷年酒駕肇事受傷及死亡件數、人數統計表】")
chart <- chart + labs(x = "年份", y = "死亡人數")
chart <- chart + theme(text = element_text(family = ""), 
                       axis.text.x = element_text(size = 10, angle = 0), 
                       axis.text.y = element_text(size = 10, angle = 0), 
                       axis.title = element_text(size = 16))

# 儲存圖片
ggsave("plot.png", width = 7, height = 7)
```