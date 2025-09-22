# =========================
# Stock Market Analysis with First-Signal Plot
# =========================

# 載入套件
library(quantmod)
library(dplyr)
library(zoo)
library(ggplot2)
library(TTR)
library(xts)

# -------------------------
# 1️⃣ 抓取股價資料
# -------------------------
getSymbols("2330.TW", from="2024-09-01", to="2025-09-01")
tsmc <- `2330.TW`

# -------------------------
# 2️⃣ 整理資料
# -------------------------
tsmc_df <- data.frame(
  date = index(tsmc),
  close = as.numeric(Cl(tsmc)),
  volume = as.numeric(Vo(tsmc))
) %>%
  mutate(
    daily_return = (close - dplyr::lag(close)) / dplyr::lag(close),
    MA5 = zoo::rollmean(close, 5, fill = NA, align="right"),
    MA20 = zoo::rollmean(close, 20, fill = NA, align="right")
  ) %>%
  mutate(
    signal = case_when(
      MA5 > MA20 ~ "Buy",
      MA5 < MA20 ~ "Sell",
      TRUE ~ NA_character_
    ),
    # 只標記首次觸發的買賣訊號
    buy_signal_first = signal == "Buy" & (lag(signal) != "Buy" | is.na(lag(signal))),
    sell_signal_first = signal == "Sell" & (lag(signal) != "Sell" | is.na(lag(signal)))
  )

# -------------------------
# 3️⃣ 計算 MACD 和 RSI
# -------------------------
close_clean <- na.locf(tsmc_df$close)

macd_vals <- MACD(close_clean, nFast=12, nSlow=26, nSig=9)
rsi_vals <- RSI(close_clean, n=14)

tsmc_df$MACD <- c(rep(NA, nrow(tsmc_df)-nrow(macd_vals)), macd_vals[, "macd"])
tsmc_df$SignalLine <- c(rep(NA, nrow(tsmc_df)-nrow(macd_vals)), macd_vals[, "signal"])
tsmc_df$RSI <- c(rep(NA, nrow(tsmc_df)-length(rsi_vals)), rsi_vals)

# -------------------------
# 4️⃣ 計算累積報酬率
# -------------------------
daily_ret <- tsmc_df$daily_return
daily_ret[is.na(daily_ret)] <- 0
cum_xts <- cumprod(1 + daily_ret)
tsmc_df$cum_return <- cum_xts

# -------------------------
# 5️⃣ 計算買賣訊號次數 (首次觸發)
# -------------------------
buy_signals <- sum(tsmc_df$buy_signal_first, na.rm = TRUE)
sell_signals <- sum(tsmc_df$sell_signal_first, na.rm = TRUE)

# -------------------------
# 6️⃣ 生成 summary 面板
# -------------------------
summary_panel <- data.frame(
  Indicator = c(
    "平均日報酬率",
    "日報酬率標準差 (波動率)",
    "累積報酬率",
    "MA5>MA20 信號次數 (買入)",
    "MA5<MA20 信號次數 (賣出)",
    "RSI 平均值",
    "MACD 平均值"
  ),
  Value = c(
    round(mean(daily_ret),5),
    round(sd(daily_ret),5),
    round(tail(cum_xts,1)-1,5),
    buy_signals,
    sell_signals,
    round(mean(tsmc_df$RSI, na.rm=TRUE),2),
    round(mean(tsmc_df$MACD, na.rm=TRUE),5)
  )
)

print(summary_panel)

# -------------------------
# 7️⃣ 繪製股價圖 + MA + 首次訊號
# -------------------------
ggplot(tsmc_df, aes(x=date, y=close)) +
  geom_line(color="blue", size=1, na.rm=TRUE) +
  geom_line(aes(y=MA5), color="green", linetype="dashed", na.rm=TRUE) +
  geom_line(aes(y=MA20), color="red", linetype="dashed", na.rm=TRUE) +
  geom_point(data=tsmc_df %>% filter(buy_signal_first),
             aes(y=close), color="darkgreen", size=2, shape=24) +
  geom_point(data=tsmc_df %>% filter(sell_signal_first),
             aes(y=close), color="darkred", size=2, shape=25) +
  labs(title="TSMC Price with MA Crossover First Signals",
       x="Date", y="Close Price") +
  theme_minimal()
