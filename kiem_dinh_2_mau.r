df <- read.csv("my_data.csv")

# Trích xuất giá trị memory_bandwidth_value hai mẫu con dựa theo Manufacturer
# "Nvidia" và "AMD"
nvidia <- df$memory_bandwidth_value[df$manufacturer == "Nvidia"]
amd <- df$memory_bandwidth_value[df$manufacturer == "AMD"]

# Tính giá trị trung bình và phương sai của hai mẫu
size_nvidia <- length(nvidia)
mean_nvidia <- mean(nvidia)
sd_nvidia <- sd(nvidia)

size_amd <- length(amd)
mean_amd <- mean(amd)
sd_amd <- sd(amd)

# Giá trị z_score ứng với alpha = 0.05
alpha <- 0.05
z_score <- qnorm(1- alpha)

# Tính giá trị quan sát Z_qs
SE <- sqrt(sd_nvidia^2 / size_nvidia + sd_amd^2 / size_amd)
Z_qs <- (mean_nvidia - mean_amd) / SE
print(paste("Z_qs = ", Z_qs))

# ------------ Z test ----------- $

library(BSDA)
z.test(
  x = nvidia,
  y = amd,
  mu = 0,
  sigma.x = sd_nvidia,
  sigma.y = sd_amd,
  alternative = "greater",
  conf.level = 1 - alpha
)
