# 匯入 json 的套件
library(jsonlite)

# 匯入圖形套件
library(ggplot2)
library(ggpubr)
 
# json 來源網址，來自內政部
url_json <- "https://od.moi.gov.tw/api/v1/rest/datastore/A01010000C-000462-015"

# 將 json 轉成 dataframe
df <- fromJSON(url_json)

# 去除標題列
df <- df$result$records
df <- df[-1,]

df$`A1-count`

# 列數
x_vector <- c(1:nrow(df))

# 因 ggplot 的標題排序會影響到我的設定，所以另行排序
df$year <- factor(df$year, levels = unique(df$year))





# 將人數從 json 的文字格式，轉成數值，才不會在作 fit regression 的時候出錯
df$`A1-count` <- as.numeric(df$`A1-count`)

# 建立模型
lr_model <- lm(`A1-count` ~ x_vector, df)

# 截距
intc <- coef(lr_model)[1]

# 斜率
slp <- coef(lr_model)[2]

# 圖表(A1件數)
chart_A1_count <- ggplot(data = df, aes(x = df$year, y = df$`A1-count`, label = df$`A1-count`))
chart_A1_count <- chart_A1_count + geom_point()
chart_A1_count <- chart_A1_count + geom_text(hjust = 0, nudge_x = 0.05)
chart_A1_count <- chart_A1_count + geom_abline(intercept = intc, slope = slp, linetype = "dashed", color = "red", size = 1)
chart_A1_count <- chart_A1_count + geom_smooth(method = "lm", se = FALSE)
chart_A1_count <- chart_A1_count + ggtitle("內政部開放資料【歷年酒駕肇事交通事故統計表】")
chart_A1_count <- chart_A1_count + labs(x = "年份", y = "A1件數")
chart_A1_count <- chart_A1_count + theme(text = element_text(family = ""), 
                                         axis.text.x = element_text(size = 10, angle = 0), 
                                         axis.text.y = element_text(size = 10, angle = 0), 
                                         axis.title = element_text(size = 16))





# 將人數從 json 的文字格式，轉成數值，才不會在作 fit regression 的時候出錯
df$`A2-count` <- as.numeric(df$`A2-count`)

# 建立模型
lr_model <- lm(`A2-count` ~ x_vector, df)

# 截距
intc <- coef(lr_model)[1]

# 斜率
slp <- coef(lr_model)[2]

# 圖表(A2件數)
chart_A2_count <- ggplot(data = df, aes(x = df$year, y = df$`A2-count`, label = df$`A2-count`))
chart_A2_count <- chart_A2_count + geom_point()
chart_A2_count <- chart_A2_count + geom_text(hjust = 0, nudge_x = 0.05)
chart_A2_count <- chart_A2_count + geom_abline(intercept = intc, slope = slp, linetype = "dashed", color = "red", size = 1)
chart_A2_count <- chart_A2_count + geom_smooth(method = "lm", se = FALSE)
chart_A2_count <- chart_A2_count + ggtitle("內政部開放資料【歷年酒駕肇事交通事故統計表】")
chart_A2_count <- chart_A2_count + labs(x = "年份", y = "A2件數")
chart_A2_count <- chart_A2_count + theme(text = element_text(family = ""), 
                                         axis.text.x = element_text(size = 10, angle = 0), 
                                         axis.text.y = element_text(size = 10, angle = 0), 
                                         axis.title = element_text(size = 16))





# 將人數從 json 的文字格式，轉成數值，才不會在作 fit regression 的時候出錯
df$`A1-hurt` <- as.numeric(df$`A1-hurt`)

# 建立模型
lr_model <- lm(`A1-hurt` ~ x_vector, df)

# 截距
intc <- coef(lr_model)[1]

# 斜率
slp <- coef(lr_model)[2]

# 圖表(A1受傷人數)
chart_A1_hurt <- ggplot(data = df, aes(x = df$year, y = df$`A1-hurt`, label = df$`A1-hurt`))
chart_A1_hurt <- chart_A1_hurt + geom_point()
chart_A1_hurt <- chart_A1_hurt + geom_text(hjust = 0, nudge_x = 0.05)
chart_A1_hurt <- chart_A1_hurt + geom_abline(intercept = intc, slope = slp, linetype = "dashed", color = "red", size = 1)
chart_A1_hurt <- chart_A1_hurt + geom_smooth(method = "lm", se = FALSE)
chart_A1_hurt <- chart_A1_hurt + ggtitle("內政部開放資料【歷年酒駕肇事交通事故統計表】")
chart_A1_hurt <- chart_A1_hurt + labs(x = "年份", y = "A1受傷人數")
chart_A1_hurt <- chart_A1_hurt + theme(text = element_text(family = ""), 
                                      axis.text.x = element_text(size = 10, angle = 0), 
                                      axis.text.y = element_text(size = 10, angle = 0), 
                                      axis.title = element_text(size = 16))





# 將人數從 json 的文字格式，轉成數值，才不會在作 fit regression 的時候出錯
df$`A2-hurt` <- as.numeric(df$`A2-hurt`)

# 建立模型
lr_model <- lm(`A2-hurt` ~ x_vector, df)

# 截距
intc <- coef(lr_model)[1]

# 斜率
slp <- coef(lr_model)[2]

# 圖表(A2受傷人數)
chart_A2_hurt <- ggplot(data = df, aes(x = df$year, y = df$`A2-hurt`, label = df$`A2-hurt`))
chart_A2_hurt <- chart_A2_hurt + geom_point()
chart_A2_hurt <- chart_A2_hurt + geom_text(hjust = 0, nudge_x = 0.05)
chart_A2_hurt <- chart_A2_hurt + geom_abline(intercept = intc, slope = slp, linetype = "dashed", color = "red", size = 1)
chart_A2_hurt <- chart_A2_hurt + geom_smooth(method = "lm", se = FALSE)
chart_A2_hurt <- chart_A2_hurt + ggtitle("內政部開放資料【歷年酒駕肇事交通事故統計表】")
chart_A2_hurt <- chart_A2_hurt + labs(x = "年份", y = "A2受傷人數")
chart_A2_hurt <- chart_A2_hurt + theme(text = element_text(family = ""), 
                                      axis.text.x = element_text(size = 10, angle = 0), 
                                      axis.text.y = element_text(size = 10, angle = 0), 
                                      axis.title = element_text(size = 16))





# 將人數從 json 的文字格式，轉成數值，才不會在作 fit regression 的時候出錯
df$dead <- as.numeric(df$dead)

# 建立模型
lr_model <- lm(dead ~ x_vector, df)

# 截距
intc <- coef(lr_model)[1]

# 斜率
slp <- coef(lr_model)[2]

# 圖表(死亡人數)
chart_dead <- ggplot(data = df, aes(x = df$year, y = df$dead, label = df$dead))
chart_dead <- chart_dead + geom_point()
chart_dead <- chart_dead + geom_text(hjust = 0, nudge_x = 0.05)
chart_dead <- chart_dead + geom_abline(intercept = intc, slope = slp, linetype = "dashed", color = "red", size = 1)
chart_dead <- chart_dead + geom_smooth(method = "lm", se = FALSE)
chart_dead <- chart_dead + ggtitle("內政部開放資料【歷年酒駕肇事交通事故統計表】")
chart_dead <- chart_dead + labs(x = "年份", y = "死亡人數")
chart_dead <- chart_dead + theme(text = element_text(family = ""), 
                                 axis.text.x = element_text(size = 10, angle = 0), 
                                 axis.text.y = element_text(size = 10, angle = 0), 
                                 axis.title = element_text(size = 16))



ggarrange(chart_A1_count, chart_A2_count, chart_A1_hurt, chart_A2_hurt, chart_dead, ncol = 2, nrow = 3)

# 儲存圖片
ggsave("plot.png", width = 14, height = 12)