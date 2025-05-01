# Load the dataset
library(foreign)
dat <- suppressWarnings(read.spss("v2.5 paper.sav", to.data.frame=TRUE))

# Trim whitespace from columns with text so that they are readable
strCol <- sapply(dat, class)=="character"
strCol <- names(strCol[strCol])
for (x in strCol) dat[,x] <- trimws(dat[,x])

# Convert SPSS dates to R format
convert_date <- function(x) as.Date(x/86400, origin = "1582-10-14")
dat$dadm <- convert_date(dat$Ημερομηνίαεισόδου)
dat$dCOVID <- convert_date(dat$ΗμερομηνιαCOVID)
dat$ddeath <- convert_date(dat$Ημερομηνίαθανάτου)

# COVID-19 mentioned in the sequence of events leading to death?
dat$ment_I <- grepl("COVI", toupper(dat$Α)) | grepl("COVI", toupper(dat$Β)) | grepl("COVI", toupper(dat$Γ))
# COVID-19 mentioned in the conditions contributing to death?
dat$ment_II <- grepl("COVI", toupper(dat$Συνετέλεσαν))
# COVID-19 mentioned *anywhere* in the death certificate?
dat$ment <- dat$ment_I | dat$ment_II

# Classification based on the death certificate:
dat$classDC <- as.integer(dat$ment)
dat$classDC[dat$ment_I] <- 2
dat$classDC <- factor(dat$classDC, levels=2:0, labels=c("attributed", "related", "unrelated"))

# Table 1 of the letter
tb1 <- with(dat, table(classDC, Επικριση))


