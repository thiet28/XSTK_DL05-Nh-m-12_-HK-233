
library(readr)

data <- read_csv("my_data.csv");
qtt_data <- data[, c("number_of_pixels", "core_speed_value","max_power_value", "memory_value", "memory_bandwidth_value", "memory_speed_value", "release_date")]

# Tìm giá trị trung bình
avg = sapply(qtt_data, mean)

# Tìm phương sai
s2 = sapply(qtt_data, var)

# Tìm 5 thông số lần lượt, bao gồm
# Min, Tứ phân vị 0.25, Trung vị, Tứ phân vị 0.75, Max
fivenum = sapply(qtt_data, fivenum)

# Tạo bảng chứa các giá trị đặc trưng 
# và làm tròn đến 2 chữ số thập phân
fivenum = t(fivenum)
t = split(fivenum, rep(1:ncol(fivenum), each = nrow(fivenum)))
names(t) = c("Min", "Quar1", "Quar2", "Quar3", "max")
table = round(data.frame(avg, s2, t), 2)
show(table)

summary(data$release_price)

```{r}
#Hàm tự định nghĩa để vẽ biểu đồ Histogram
plotHist <- function(sample, name, num_of_breaks){
  sample_x_axis = floor(seq(floor(min(sample)), 
    max(sample), length.out = num_of_breaks+1))
  hist(sample, sample_x_axis, freq = TRUE, xlab = name,
    labels = TRUE, xaxt="n", col = "azure")
  axis(1, at = sample_x_axis)
}
plotHist(data$core_speed_value, "Core Speed [MHz]", 20)
plotHist(data$max_power_value, "Max Power [Watts]",20)
plotHist(data$memory_bandwidth_value, "Memory_Bandwidth [GB/sec]", 30)
plotHist(data$memory_value, "Memory Value [MB]", 20)

#Tạo hàm tự định nghĩa để vẽ biểu đồ boxplot 
library(ggplot2)
my_boxplot <- function(dt, dt_colm, name){
  ggplot(dt, aes(x = dt_colm)) +
    labs(
      title = name)+
    geom_boxplot() +
    coord_flip() +
    theme_grey(base_size = 22)
}

my_boxplot(data, data$core_speed_value, "Core Speed [MHz]")
my_boxplot(data, data$max_power_value, "Max Power [Watts]")
my_boxplot(data, data$release_date, "Release Date")
my_boxplot(data, data$memory_value, "Memory Value [MB]")

library(GGally)
#Tạo bảng thống kê 2 cặp thông số
qtt_data <- data[, c("number_of_pixels", "core_speed_value","max_power_value", "memory_value", "memory_bandwidth_value", "memory_speed_value", "release_date", "release_price")]
ggpairs(qtt_data)
