##########################################################################
##########Mô phỏng dữ liệu chuỗi thời gian và ứng dụng mô hình############
##########ARIMA/SARIMA trong giảng dạy thống kê y sinh tại Đại############
##########học Y Dược Hải Phòng ###########################################
##########Phùng Chí Thiện, Nguyễn Hải Tuấn################################
##########Trung tâm Mô phỏng lâm sàng - Trường ĐHYH Hải Phòng#############
##########Viện Vệ sinh dịch tễ Trung ương#################################
##########################################################################
# Tải và chỉ định các thư viện cần thiết
# install.packages("readr") để cài thư viện readr
# R.version.string # [1] "R version 4.2.2 (2022-10-31 ucrt)"
library(readr) # read_csv
library(graphics) 
library(lubridate) # Ngày tháng
library(tsibble)
# install.packages("tsibble")
library(forecast) # autoplot()
library(ggplot2) # forecast's dependency
library(DT) # install.packages("DT") # Xem bảng dưới dạng html
library(dplyr) # Biên tập dữ liệu
library(tseries) # time series for adf test
library(forecast) # auto.arima()
#install.packages("moments")
library(moments)
socathang <- read_csv("https://raw.githubusercontent.com/nghait/time_series/refs/heads/main/socathang_simulated.csv")
# Mô tả bảng số liệu:
# Bảng số liệu gồm có hai biến gồm biến ngày "date" 
# và biến số lượng ca bệnh "cases" nhập viện do sốt khu vực miền Bắc
## Kiểm tra cấu trúc của bản số liệu
dim(socathang)
str(socathang)
## Kiểm tra danh sách các biến số của bảng số liệu
names(socathang)
colnames(socathang)
## Kiểm tra 10 bản ghi đầu tiên (head) và 10 bản ghi cuối cùng (tail)
head(socathang, 10) # kiểm tra 10 bản ghi đầu tiên của bảng số liệu
tail(socathang, 6) # kiểm tra 10 bản ghi cuối của bảng số liệu
## Kiểm tra đặc trung thống kê và  phân bố sơ bộ
# Tính toán các chỉ số
summary(socathang$cases)

stats_summary <- data.frame(
  Mean = mean(socathang$cases),
  Median = median(socathang$cases),
  Std_Dev = sd(socathang$cases),
  Min = min(socathang$cases),
  Max = max(socathang$cases),
  Skewness = skewness(socathang$cases), # Độ lệch
  Kurtosis = kurtosis(socathang$cases)  # Độ nhọn
)
print(stats_summary)

# Kiểm định Shapiro-Wilk (nếu n < 5000) giả thuyết dữ liệu phân phối chuẩn
shapiro.test(socathang$cases)

par(mfrow=c(1,2))
# Vẽ biểu đồ Histogram kèm đường cong phân phối chuẩn để minh họa
hist(socathang$cases, breaks = 20, prob = TRUE, 
     main = "Phân phối của số ca bệnh", xlab = "Số ca", col = "lightblue")
lines(density(socathang$cases), col = "red", lwd = 2)

boxplot(socathang$cases) # Phát hiện có một giá trị ngoại lệ


# Thiết lập khung hình 1 hàng 2 cột
par(mfrow=c(1,2), mar=c(5, 4, 4, 2) + 0.1)

# 1. Vẽ Histogram bên trái
hist(socathang$cases, 
     breaks = 20, 
     prob = TRUE, 
     main = "A", 
     xlab = "Số ca bệnh", 
     ylab = "Mật độ", 
     col = "lightblue", 
     border = "white")
# Thêm đường mật độ thực tế (màu đỏ)
lines(density(socathang$cases), col = "red", lwd = 2)
# Thêm đường phân phối chuẩn lý thuyết (màu xanh đậm, đứt nét) để đối chiếu
x <- seq(min(socathang$cases), max(socathang$cases), length = 100)
y <- dnorm(x, mean = mean(socathang$cases), sd = sd(socathang$cases))
lines(x, y, col = "darkblue", lwd = 2, lty = 2)

# 2. Vẽ Boxplot bên phải
boxplot(socathang$cases, 
        main = "B", 
        xlab = "Chuỗi số liệu", 
        ylab = "Số ca bệnh", 
        col = "lightblue", 
        border = "darkblue",
        outcol = "red",      # Màu cho giá trị ngoại lệ
        outpch = 19)         # Kiểu chấm cho giá trị ngoại lệ

boxplot(socathang$cases) # Phát hiện có một giá trị ngoại lệ

# Thêm tiêu đề chung cho cả khung hình (tùy chọn)
# mtext("Phân tích phân phối số trườn g hợp nhập ện", side = 3, line = -2, outer = TRUE, font = 2, cex = 1.2)

# Trả lại thiết lập đồ họa mặc định
par(mfrow=c(1,1)

library(knitr)
kable(stats_summary, caption = "Bảng tóm tắt đặc trưng thống kê của chuỗi số ca bệnh", digits = 2)

## 10 giá trị nhỏ nhất của biến cases
head(sort(socathang$cases), 10)
## 10 giá trị lớn nhất của biến cases
tail(sort(socathang$cases), 10)
## Xem toàn bảng số liệu
View(socathang) # datatable(socathang) # Xem bảng bằng thư viện DT
## Chuyển định dạng ngày tháng
socathang$date <- as.Date(socathang$date)
socathang_ts <- ts(socathang$cases, start = c(2008, 1), frequency = 12)
autoplot(socathang_ts) +
  labs(x = "Thời gian", y = "Số ca")
# title = "Số ca nhập viện do sốt khu vực miền Bắc (2008 - 2017)",
# edit(socathang) # 26273
# socathang$cases[socathang$cases %in% c("")] <- c(26273, 36593)
# socathang[socathang$cases=="", c("date", "cases")]
# sum(is.na(socathang$cases))
# colnames(socathang)

# 1. Phân rã chuỗi thời gian (sử dụng mô hình cộng - additive)
socathang_decomp <- decompose(socathang_ts, type = "additive")
# Vẽ với nhãn tiếng Việt
autoplot(socathang_decomp) +
  labs(title = "Phân rã chuỗi thời gian số ca nhập viện",
       x = "Năm", y = "Giá trị") +
  theme_minimal()
# 2. Vẽ biểu đồ phân rã mặc định
plot(socathang_decomp)

#######################
## Biên tập dữ liệu
#######################

# 1. Tính giá trị trung bình, loại bỏ giá trị 1807 và làm tròn (số nguyên)
mean_value <- round(mean(socathang$cases[socathang$cases != 1807], na.rm = TRUE))

# 2. Thay thế giá trị 1807 bằng giá trị trung bình vừa tính
socathang$cases[socathang$cases == 1807] <- mean_value

# Biên tập và thay thế giá trị
# mean_value <- mean(socathang$cases[!socathang$cases %in% c(3, 4, 5, 54)], na.rm = TRUE)
# socathang$cases[socathang$cases %in% c(3, 4, 5, 54)] <- mean_value

# Và thay thế giá trị ngoại lệ 118760 bằng 11876 do thừa số 0
# socathang$cases[socathang$cases == 118760] <- 11876

summary(socathang$cases)

dev.off()
# Vẽ lại biểu đồ
# Chuyển thành chuỗi thời gian
socathang_ts <- ts(socathang$cases, start = c(2008, 1), frequency = 12)
# Vẽ biểu đồ chuỗi thời gian
autoplot(socathang_ts) +
  labs(title = "Số ca nhập viện do sốt khu vực miền Bắc (2008–2017)",
       x = "Tháng", y = "Số ca")

summary(socathang_ts)
# window() trích xuất một phần từ đối tượng chuỗi thời gian ts
# window(data, start = c(2008, 1), end = c(2010, 12))
# Trích xuất 15 tháng cuối cùng
# start = c(2016, 10) vì từ tháng 10/2016 đến tháng 12/2017 là đúng 15 tháng
socathang_15m <- window(socathang_ts, start = c(2016, 10))
socathang_15m <- window(socathang_ts, start = c(2016, 10), end = c(2017, 12))
# Vẽ biểu đồ
autoplot(socathang_15m) +
  labs(title = "15 tháng cuối cùng: Số ca nhập viện (Miền Bắc)",
       x = "Tháng", y = "Số ca")


boxplot(socathang$cases) # Không còn giá trị ngoại lệ

# Kiểm định tính dừng

adf.test(socathang_ts) # Không cần lấy sai phân vì chuỗi đã dừng
# Vẽ biểu đồ ACF và PACF trên cùng một hàng

par(mfrow=c(1,2))
# Xác định thủ công p, d, q bằng biểu đồ ACF và PACF
acf(socathang_ts, main="Biểu đồ ACF") # --> q = 2
pacf(socathang_ts, main="Biểu đồ PACF") # --> p = 0

# Chuyển lại đơn vị Lag x theo ngày
par(mfrow=c(1,2))
acf(as.numeric(socathang_ts), main="ACF") # Lag mặc định là 30
pacf(as.numeric(socathang_ts), main="PACF") # Lag mặc định là 30

dev.off()

model_arima <- arima(socathang_ts, order = c(0, 0, 2))
summary(model_arima)
BIC(model_arima)
checkresiduals(model_arima) # Không bác bỏ giả thuyết nhiễu trắng

model_arima <- arima(socathang_ts, order = c(1, 0, 2))
summary(model_arima)
BIC(model_arima)
checkresiduals(model_arima) # Không bác bỏ giả thuyết nhiễu trắng

# > checkresiduals(model_arima)
# 
# Ljung-Box test
# 
# data:  Residuals from ARIMA(2,0,5) with non-zero mean
# Q* = 19.721, df = 16, p-value = 0.233
# 
# Model df: 7.   Total lags used: 23
## ==> Phần dư là nhiễu trắng (không bác bỏ giả thuyết Ho)

# Dự báo 3 tháng tới
forecast_nhapviem <- forecast(model_arima, h = 3)

# Vẽ biểu đồ dự báo 3 tháng tiếp theo
plot(forecast_nhapviem, main = "Dự báo tổng ca nhập viện trong 3 tháng tiếp theo", 
     fcol = "red",
     flty = 1,
     xlab = "Thời gian", ylab = "Tổng ca nhiễm")



# Biểu điễn giá trị dự báo từ mô hình
# Lấy giá trị khớp từ mô hình
# Hàm fitted() trích xuất các giá trị mà mô hình dự đoán cho dữ liệu quá khứ
fitted_values <- fitted(model_arima)
# Vẽ đường giá trị khớp đè lên biểu đồ
# Ta dùng màu khác (ví dụ màu đỏ hoặc xanh) và nét đứt (lty = 2) để dễ phân biệt
lines(fitted_values, col = "red", lty = 1, lwd = 1)

# Thêm chú thích để biểu đồ rõ ràng hơn
legend("topright", 
       legend = c("Dữ liệu gốc", "Dự báo"), 
       col = c("black",  "red"), 
       lty = c(1, 1),
       bty = "n")

########################################
# Tìm mô hình tự động
model_auto <- auto.arima(socathang_ts) # Tìm kiếm nhanh
summary(model_auto) ## Máy gợi ý mô hình ARIMA(0,0,2)(0,0,1)[12] 
# summary(model_arima)
model_best <- auto.arima(socathang_ts, stepwise = FALSE, approximation = FALSE)
summary(model_best) ## Máy gợi ý mô hình SARIMA hay ARIMA(0,1,4)(1,0,0)[12]

checkresiduals(model_auto)
checkresiduals(model_best) # Kiểm định giả thuyết nhiễu trắng của mô hình SARIMA chu kỳ 12 tháng

# Sử dụng mô hình để dự báo 3 tháng tới
forecast_nhapviem_sarima <- forecast(model_best, h = 12)
# Vẽ biểu đồ dự báo 3 tháng tiếp theo
plot(forecast_nhapviem_sarima, main = "Dự báo số trường hợp nhập viện trong 12 tháng tiếp theo", 
     xlab = "Thời gian", ylab = "Tổng ca nhiễm")

# Sử dụng autoplot từ gói forecast nhưng giới hạn trục X
autoplot(forecast_nhapviem_sarima, include = 12) +
  labs(title = "Cận cảnh dự báo 3 tháng tới",
       x = "Thời gian",
       y = "Tổng ca nhiễm") +
  theme_minimal()

# Sử dụng mô hình để dự báo 6 tháng tới
forecast_nhapviem_sarima <- forecast(model_best, h = 24)
# Vẽ biểu đồ dự báo 3 tháng tiếp theo
plot(forecast_nhapviem_sarima, main = "Dự báo  số trường hợp nhập viện trong 24 tháng tiếp theo", 
     xlab = "Thời gian", ylab = "Tổng ca nhiễm")

# Vẽ biểu đồ với đường dự báo màu đỏ
plot(forecast_nhapviem_sarima, 
     main = "Dự báo số trường hợp nhập viện trong 24 tháng tiếp theo", 
     xlab = "Thời gian", 
     ylab = "Tổng ca nhiễm",
     lwd = 2,            # Đường dữ liệu gốc dày hơn
     # shadecols = c("gray80", "gray90"), # Màu vùng tin cậy 80% và 95%
     fcol = "red") # Đổi màu đường dự báo thành đỏ


fitted_values <- fitted(model_best)

# Ta dùng màu khác (ví dụ màu đỏ hoặc xanh) và nét đứt (lty = 2) để dễ phân biệt
lines(fitted_values, col = "red", lty = 1, lwd = 2)

# Thêm chú thích để biểu đồ rõ ràng hơn
legend("topleft", 
       legend = c("Dữ liệu gốc", "Dự báo"), 
       col = c("black",  "red"), 
       lty = c(1, 1),
       lwd = c(2, 2),
       bty = "n")
