dat<-read.csv(All_GPUs.csv)
head(dat)

dat<-dat[,c("Name", "Best_Resolution", "Core_Speed", "Max_Power", "Memory", "Memory_Bandwidth", "Memory_Speed", "Manufacturer", "Release_Date", "Release_Price")]
View(dat)

library(tidyr)
library(janitor)
# Định dạng lại tên biến
dat2 <- clean_names(dat)
head(dat2)
# Xóa hàng và cột trống
dat3 <- remove_empty(dat2, which = c("rows","cols"),quiet=FALSE)
head(dat3)

# Đổi tên best_resolution thành number_of_pixels 
names(dat3)[ names (dat3) == "best_resolution"] <- "number_of_pixels"
# Thực hiện phép nhân số pixel hai chiều ngang và dọc
dat3$number_of_pixels <- sapply (strsplit(dat3$number_of_pixels,"x"), function(x) as.numeric (x [1])*as.numeric (x[2]))

# Loại bỏ giá trị khuyết biểu diễn bởi "-"
dat3$core_speed[grepl("-", dat3$core_speed)] <- ""
# Chia thành 2 cột core_speed_value và core_speed_unit
dat3 <- separate(dat3,col = core_speed, into = c("core_speed_value",
                                                 "core_speed_unit"),sep=" ", fill="right")
# Đổi giá trị thành dạng số
dat3$core_speed_value <- as.numeric(dat3$core_speed_value)
# Thực hiện tương tự cho các cột còn lại
dat3 <- separate(dat3, col = max_power, into = c("max_power_value", 
                                                 "max_power_unit"), sep=" ", fill = "right")
dat3$max_power_value <- as.numeric(dat3$max_power_value)

dat3 <- separate(dat3, col = memory, into = c("memory_value", 
                                              "memory_unit"), sep=" ", fill = "right", extra = "drop")
dat3$memory_value <- as.numeric(dat3$memory_value)

dat3 <- separate(dat3, col = memory_bandwidth, 
                 into = c("memory_bandwidth_value", "memory_bandwidth_unit"), 
                 sep = "(?<=\\d)(?=[A-Za-z])", fill = "right")
dat3$memory_bandwidth_value <- as.numeric(dat3$memory_bandwidth_value)

dat3 <- separate(dat3, col = memory_speed , 
                 into = c("memory_speed_value", "memory_speed_unit"), 
                 sep =" ", fill = "right")
dat3$memory_speed_value <- as.numeric (dat3$memory_speed_value)

table(dat3$core_speed_unit)
table(dat3$max_power_unit) 
table(dat3$memory_unit) 
table(dat3$memory_bandwidth_unit)
table(dat3$memory_speed_unit)

dat3$memory_bandwidth_value <- ifelse(dat3$memory_bandwidth_unit == "MB/sec", dat3$memory_bandwidth_value/1024, dat3$memory_bandwidth_value)
dat3$memory_bandwidth_unit <- ifelse(dat3$memory_bandwidth_unit == "MB/sec", "GB/sec", dat3$memory_bandwidth_unit)

# Từ định dạng chuỗi -> định dạng ngày tháng
dat3$release_date <- as.Date(sub("^\\s*\\n", "", dat3$release_date),
                             format ="%d-%b-%Y")
# Tính giá trị của cột theo đơn vị "năm"
dat3$release_date <- as.numeric(format(dat3$release_date, "%Y")) +
  as.numeric(format(dat3$release_date, "%m")) / 12

dat3$release_price <- gsub("\\$", "", dat3$release_price)
dat3$release_price <- as.numeric(dat3$release_price)

apply(is.na(dat3), MARGIN = 2, FUN = mean)

# Tạo Data frame "temp" từ dat3 trừ release_price
temp <- dat3[, !(names(dat3) %in% "release_price")]
# Loại bỏ dữ liệu khuyết
temp <- na.omit(temp)
# Gộp 2 data frame lại
dat3 <- dat3[rownames(temp), ]
