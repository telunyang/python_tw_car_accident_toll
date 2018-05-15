# TaiwanGovDrunkenCarAccidentToll
內政部開放資料 -- 酒駕傷亡人數統計表

## 內政資料開放平臺
[按我開啟網頁](https://data.moi.gov.tw/)

## 範例圖片
![範例圖片](https://github.com/telunyang/TaiwanGovDrunkenCarAccidentToll/blob/master/plot.png){:target="_blank"}

## 原始碼
># 匯入 json 的套件
>library(jsonlite)
>
># 匯入圖形套件
>library(ggplot2)
>
># json 來源網址，來自內政部
>url_json <- "https://od.moi.gov.tw/api/v1/rest/datastore/A01010000C-000462-015"
>
># 將 json 轉成 dataframe
>df <- fromJSON(url_json)
>
># 去除標題列
>df <- df$result$records
>df <- df[-1,]
>
># 將人數從 json 的文字格式，轉成數值，才不會在作 fit regression 的時候出錯
>df$dead <- as.numeric(df$dead)
>
># 因 ggplot 的標題排序會影響到我的設定，所以另行排序
>df$year <- factor(df$year, levels = unique(df$year))
>
># 列數
>x_vector <- c(1:nrow(df))
>
># 建立模型
>lr_model <- lm(dead ~ x_vector, df)
>
># 截距
>intc <- coef(lr_model)[1]
>
># 斜率
>slp <- coef(lr_model)[2]
>
># 製作圖表
>chart <- ggplot(data = df, aes(x = df$year, y = df$dead, label = df$dead))
>
>chart <- chart + geom_point()
>
>chart <- chart + geom_text(hjust = 0, nudge_x = 0.05)
>
>chart <- chart + geom_abline(intercept = intc, slope = slp, linetype = "dashed", color = "red", size = 1)
>
>chart <- chart + geom_smooth(method = "lm", se = FALSE)
>
>chart <- chart + ggtitle("內政部開放資料【歷年酒駕肇事交通事故統計表】")
>
>chart <- chart + labs(x = "年份", y = "死亡人數")
>
>chart <- chart + theme(text = element_text(family = ""), 
>
>                       axis.text.x = element_text(size = 10, angle = 0), 
>
>                       axis.text.y = element_text(size = 10, angle = 0), 
>
>                       axis.title = element_text(size = 16))
>
>
># 儲存圖片
>ggsave("plot.png", width = 7, height = 7)
