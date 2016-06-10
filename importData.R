# import Google Flu Trends data
gft <- read.csv("./Data/Google Trend.csv")
gft$Date <- as.Date(gft$Date, "%y-%m-%d")

# import Canada data
can <- read.csv("./Data/Canada.csv")
can$Date <- as.Date(can$Date, "%d-%b-%y")
can <- can[,1:17]
can <- merge(can, 
             subset(gft, select = c("Date", "Canada", "Normalized.Values.for.Canada")), 
             by = "Date", all = TRUE)
names(can)[18:19] <- c("gft", "gft.normalized.values")
can$reg <- "CAN"

# import provincial data
ab <- read.csv("./Data/Alberta.csv")
ab$Date <- as.Date(ab$Date, "%d-%b-%y")
ab <- ab[,1:17]
ab <- merge(ab, subset(gft, select = c("Date", "Alberta")),
            by = "Date", all = TRUE)
names(ab)[18] <- "gft"
ab$reg <- "AB"
ab$gft.normalized.values <- 0

bc <- read.csv("./Data/BC.csv")
bc$Date <- as.Date(bc$Date, "%d-%b-%y")
bc <- bc[,1:17]
bc <- merge(bc, subset(gft, select = c("Date", "British.Columbia")),
            by = "Date", all = TRUE)
names(bc)[18] <- "gft"
bc$reg <- "BC"
bc$gft.normalized.values <- 0

on <- read.csv("./Data/Ontario.csv")
on$Date <- as.Date(on$Date, "%d-%b-%y")
on <- on[,1:17]
on <- merge(on, subset(gft, select = c("Date", "Ontario")),
            by = "Date", all = TRUE)
names(on)[18] <- "gft"
on$reg <- "ON"
on$gft.normalized.values <- 0

qc <- read.csv("./Data/Quebec.csv")
qc$Date <- as.Date(qc$Date, "%d-%b-%y")
qc <- qc[,1:17]
qc <- merge(qc, subset(gft, select = c("Date", "Quebec")),
            by = "Date", all = TRUE)
names(qc)[18] <- "gft"
qc$reg <- "QC"
qc$gft.normalized.values <- 0

all <- rbind(can, ab, bc, on, qc)
all$reg <- factor(all$reg, levels = c("CAN", "AB", "BC", "ON", "QC"))
all <- subset(all, !is.na(Date))

# get the min and max date
minDate <- min(all$Date[!is.na(all$FluPos) & !is.na(all$gft)])
maxDate <- max(all$Date[!is.na(all$FluPos) & !is.na(all$gft)])
all$yr <- as.numeric(format(all$Date, '%Y'))
all$mth <- as.numeric(format(all$Date, '%m'))
all$wk <- as.numeric(format(all$Date + 3, '%U'))

can <- subset(all, reg == "CAN" & Date >= minDate & Date <= maxDate)
on <- subset(all, reg == "ON" & Date >= minDate & Date <= maxDate)
bc <- subset(all, reg == "BC" & Date >= minDate & Date <= maxDate)
ab <- subset(all, reg == "AB" & Date >= minDate & Date <= maxDate)
qc <- subset(all, reg == "QC" & Date >= minDate & Date <= maxDate)