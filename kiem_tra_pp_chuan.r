

df <- read.csv("my_data.csv")
nvidia <- df$memory_bandwidth_value[df$manufacturer == "Nvidia"]
amd <- df$memory_bandwidth_value[df$manufacturer == "AMD"]

library (ggpubr)
ggqqplot(nvidia, ylab = " Memory Bandwidth [GB/sec]", xlab = "Nvidia")
ggqqplot(amd, ylab = " Memory Bandwidth [GB/sec]", xlab = "AMD")

#-----Shapiro-----#

shapiro.test(nvidia)
shapiro.test(amd)
