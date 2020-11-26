# AIRSEOUL <== 대기분석 이거 쓸
AIR <- read.csv('./data/airseoul.csv', header = T)[,-1]
AIR$SGG <- factor(AIR$SGG)
AIR$week <- as.Date(AIR$week)

# COVID19
C19 = read.csv("./data/C19/C19seoul.csv")[,-1]

# MERGE <== c19+대기 분석 이걸 쓸
C19AIR20_day = read.csv("./data/C19/C19AIR20_day.csv")
C19AIR20_week = read.csv("./data/C19/C19AIR20_week.csv")

# death MERGE
