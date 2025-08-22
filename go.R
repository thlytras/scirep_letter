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

# COVID-19 mentioned in the sequence of events leading to death? (part I of the death certificate)
dat$ment_I <- grepl("COVI", toupper(dat$Α)) | grepl("COVI", toupper(dat$Β)) | grepl("COVI", toupper(dat$Γ))
# COVID-19 mentioned in the conditions contributing to death? (part II of the death certificate)
dat$ment_II <- grepl("COVI", toupper(dat$Συνετέλεσαν))
# COVID-19 mentioned *anywhere* in the death certificate?
dat$ment <- dat$ment_I | dat$ment_II

# Classification based on the death certificate:
dat$classDC <- as.integer(dat$ment)
dat$classDC[dat$ment_I] <- 2
dat$classDC <- factor(dat$classDC, levels=2:0, labels=c("attributed", "related", "unrelated"))

# Table 1 of the letter
tb1 <- with(dat, table(classDC, Επικριση))

# Subset of patients categorized as "unrelated" by the authors, but with COVID mentioned in part I of the death certificate (N=127):
sub <- subset(dat, Επικριση=="not related" & ment_I)
sub$remdesivir <- sub$Ρεμδεσιβίρη=="5-day treatment"
sub$oxy <- sub$Οξυγόνο!="none"
sub$dyspnea_hypoxia <- sub$ΔΥΣΠΝΟΙΑ=="yes" | sub$ΥΠΟΞΥΓΟΝΑΙΜΙΑ=="yes"
sub$COVIDadm <- sub$Αιτίαεισόδου=="COVID"
sub$anyCond <- with(sub, remdesivir | oxy | dyspnea_hypoxia | COVIDadm)

sub_howmany <- with(sub, c(sum(remdesivir), sum(oxy), sum(dyspnea_hypoxia), sum(COVIDadm), sum(anyCond)))

# Same, but for the 53 patients that only mention COVID in part II of the death certificate:
sub2 <- subset(dat, Επικριση=="not related" & ment_II)
sub2$remdesivir <- sub2$Ρεμδεσιβίρη=="5-day treatment"
sub2$oxy <- sub2$Οξυγόνο!="none"
sub2$dyspnea_hypoxia <- sub2$ΔΥΣΠΝΟΙΑ=="yes" | sub2$ΥΠΟΞΥΓΟΝΑΙΜΙΑ=="yes"
sub2$COVIDadm <- sub2$Αιτίαεισόδου=="COVID"
sub2$anyCond <- with(sub2, remdesivir | oxy | dyspnea_hypoxia | COVIDadm)

sub2_howmany <- with(sub2, c(sum(remdesivir, na.rm=TRUE), sum(oxy, na.rm=TRUE), sum(dyspnea_hypoxia), sum(COVIDadm), sum(anyCond)))


