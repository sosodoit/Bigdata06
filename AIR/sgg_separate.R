#******************************************************************************#
#                                 data load
#******************************************************************************#
AIR <- read.csv('./data/airseoul.csv', header = T)[,-1]
AIR$SGG <- factor(AIR$SGG)
AIR$week <- as.Date(AIR$week)
#******************************************************************************#
#                                 구별 데이터 분리
#******************************************************************************#
df <- AIR
tr = df[df$week < '2020-01-06',]
te = df[df$week >= '2020-01-06',]
kind_ssg <-as.character(unique(AIR$SGG))
name_ssg <- c("week","강남구","강남대로","강동구","강변북로","강북구","강서구","공항대로"
              ,"관악구","광진구","구로구","금천구","노원구","도봉구","도산대로"
              ,"동대문구", "동작구","동작대로","마포구","서대문구","서초구","성동구"
              ,"성북구","송파구","신촌로","양천구","영등포구","영등포로","용산구"
              ,"은평구","정릉로","종로","종로구", "중구","중랑구","천호대로"
              , "청계천로","한강대로","홍릉로","화랑로")
#******************************************************************************#
# train
sgg1 = tr[tr$SGG==kind_ssg[1],]
sgg2 = tr[tr$SGG==kind_ssg[2],]
sgg3 = tr[tr$SGG==kind_ssg[3],]
sgg4 = tr[tr$SGG==kind_ssg[4],]
sgg5 = tr[tr$SGG==kind_ssg[5],]
sgg6 = tr[tr$SGG==kind_ssg[6],]
sgg7 = tr[tr$SGG==kind_ssg[7],]
sgg8 = tr[tr$SGG==kind_ssg[8],]
sgg9 = tr[tr$SGG==kind_ssg[9],]
sgg10 = tr[tr$SGG==kind_ssg[10],]

sgg11 = tr[tr$SGG==kind_ssg[11],]
sgg12 = tr[tr$SGG==kind_ssg[12],]
sgg13 = tr[tr$SGG==kind_ssg[13],]
sgg14 = tr[tr$SGG==kind_ssg[14],]
sgg15 = tr[tr$SGG==kind_ssg[15],]
sgg16 = tr[tr$SGG==kind_ssg[16],]
sgg17 = tr[tr$SGG==kind_ssg[17],]
sgg18 = tr[tr$SGG==kind_ssg[18],]
sgg19 = tr[tr$SGG==kind_ssg[19],]
sgg20 = tr[tr$SGG==kind_ssg[20],]

sgg21 = tr[tr$SGG==kind_ssg[21],]
sgg22 = tr[tr$SGG==kind_ssg[22],]
sgg23 = tr[tr$SGG==kind_ssg[23],]
sgg24 = tr[tr$SGG==kind_ssg[24],]
sgg25 = tr[tr$SGG==kind_ssg[25],]
sgg26 = tr[tr$SGG==kind_ssg[26],]
sgg27 = tr[tr$SGG==kind_ssg[27],]
sgg28 = tr[tr$SGG==kind_ssg[28],]
sgg29 = tr[tr$SGG==kind_ssg[29],]
sgg30 = tr[tr$SGG==kind_ssg[30],]

sgg30 = tr[tr$SGG==kind_ssg[30],]
sgg31 = tr[tr$SGG==kind_ssg[31],]
sgg32 = tr[tr$SGG==kind_ssg[32],]
sgg33 = tr[tr$SGG==kind_ssg[33],]
sgg34 = tr[tr$SGG==kind_ssg[34],]
sgg35 = tr[tr$SGG==kind_ssg[35],]
sgg36 = tr[tr$SGG==kind_ssg[36],]
sgg37 = tr[tr$SGG==kind_ssg[37],]
sgg38 = tr[tr$SGG==kind_ssg[38],]
sgg39 = tr[tr$SGG==kind_ssg[39],]
#******************************************************************************#
# test
sgg1.te = te[te$SGG==kind_ssg[1],]
sgg2.te = te[te$SGG==kind_ssg[2],]
sgg3.te = te[te$SGG==kind_ssg[3],]
sgg4.te = te[te$SGG==kind_ssg[4],]
sgg5.te = te[te$SGG==kind_ssg[5],]
sgg6.te = te[te$SGG==kind_ssg[6],]
sgg7.te = te[te$SGG==kind_ssg[7],]
sgg8.te = te[te$SGG==kind_ssg[8],]
sgg9.te = te[te$SGG==kind_ssg[9],]
sgg10.te = te[te$SGG==kind_ssg[10],]

sgg11.te = te[te$SGG==kind_ssg[11],]
sgg12.te = te[te$SGG==kind_ssg[12],]
sgg13.te = te[te$SGG==kind_ssg[13],]
sgg14.te = te[te$SGG==kind_ssg[14],]
sgg15.te = te[te$SGG==kind_ssg[15],]
sgg16.te = te[te$SGG==kind_ssg[16],]
sgg17.te = te[te$SGG==kind_ssg[17],]
sgg18.te = te[te$SGG==kind_ssg[18],]
sgg19.te = te[te$SGG==kind_ssg[19],]
sgg20.te = te[te$SGG==kind_ssg[20],]

sgg21.te = te[te$SGG==kind_ssg[21],]
sgg22.te = te[te$SGG==kind_ssg[22],]
sgg23.te = te[te$SGG==kind_ssg[23],]
sgg24.te = te[te$SGG==kind_ssg[24],]
sgg25.te = te[te$SGG==kind_ssg[25],]
sgg26.te = te[te$SGG==kind_ssg[26],]
sgg27.te = te[te$SGG==kind_ssg[27],]
sgg28.te = te[te$SGG==kind_ssg[28],]
sgg29.te = te[te$SGG==kind_ssg[29],]
sgg30.te = te[te$SGG==kind_ssg[30],]

sgg30.te = te[te$SGG==kind_ssg[30],]
sgg31.te = te[te$SGG==kind_ssg[31],]
sgg32.te = te[te$SGG==kind_ssg[32],]
sgg33.te = te[te$SGG==kind_ssg[33],]
sgg34.te = te[te$SGG==kind_ssg[34],]
sgg35.te = te[te$SGG==kind_ssg[35],]
sgg36.te = te[te$SGG==kind_ssg[36],]
sgg37.te = te[te$SGG==kind_ssg[37],]
sgg38.te = te[te$SGG==kind_ssg[38],]
sgg39.te = te[te$SGG==kind_ssg[39],]
