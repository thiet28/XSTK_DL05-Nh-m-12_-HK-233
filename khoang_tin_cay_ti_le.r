# Đọc dữ liệu đã được xử lý và lấy mẫu của giá trị memory_value
df <- read.csv("my_data.csv")
mem <- df$memory_value

# Lấy ra các mẫu có giá trị memory_value >= 4096 MB
mem_larger <- mem[mem >= 4096]

# Tính kích thước mẫu và tỉ lệ các GPU có memory_value từ 4096 MB trở lên
size_total <- length(mem)  
size_larger <- length(mem_larger)

f <- size_larger / size_total

# Tính giá trị z_score ứng với alpha / 2
confidence <- 0.95
alpha <- 1 - confidence

z_score <- qnorm(1 - alpha / 2)

# Tính khoảng tinh cậy và trình bày kết quả
SE <- sqrt(f * (1 - f) / size_total)
epsilon <- z_score * SE

lower_bound <- f - epsilon
upper_bound <- f + epsilon
cat("The 95% confidence interval of p is (", lower_bound, ", ", upper_bound, ")", sep = "")



# ------Z test --------#

prop.test(
  x = size_larger,
  n = size_total,
  alternative = "two.sided",
  conf.level = confidence
)

