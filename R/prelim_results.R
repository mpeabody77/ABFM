preliminary_PRIMARY<-function() {

  DATA<-read.csv("./Scoring/PER/PER_Overall.CSV", skip=1)
  ID<-read.csv("./Scoring/PER/PER_Overall.CSV", skip=1)
  ID<-subset(ID, select=c(NAME))
  Unanswered<-read.csv("./Scoring/PER/PER_Unanswered.CSV", skip=1)
  DATA<-subset(DATA, select = -c(STATUS,IN.MSQ,IN.ZSTD,OUT.MSQ,OUT.ZSTD,DISPLACE,PTMA,PTMA.E,
                                 WEIGHT,OBSMATCH,EXPMATCH,PVALUE,PTMA,RMSR,WMLE))

  #SCORING RUN
  DATA$N.Correct <- DATA$SCORE
  DATA$N.Incorrect<-DATA$COUNT - DATA$SCORE
  DATA$N.Unanswered<-DATA$COUNT - Unanswered$COUNT
  DATA$Pct.Correct <- (DATA$SCORE/DATA$COUNT)*100
  DATA$Scaled.Score.Detailed <-  (218.47*DATA$MEASURE) + 250.86
  library(plyr)
  DATA$Scaled.Score <- round_any(((218.47*DATA$MEASURE) + 250.86),10, floor)
  DATA$PF.Prelim.ss<-ifelse(DATA$Scaled.Score<=400, "FAIL",
                            ifelse(DATA$Scaled.Score >= 450, "PASS", "GRAY"))
  DATA$PF.Prelim.pct<-ifelse(DATA$Pct.Correct >= 62, "PASS",
                             ifelse(DATA$Pct.Correct <= 50, "FAIL", "GRAY"))
  DATA$PF.Agree <- ifelse(DATA$PF.Prelim.ss == DATA$PF.Prelim.pct, 1, 0)
  DATA$TOO.MANY.MISSING <- ifelse(DATA$N.Unanswered >=3, "HOLD", "OK")
  DATA$RESULT <- ifelse(DATA$PF.Agree == 1, DATA$PF.Prelim.ss, "HOLD")
  DATA$RESULT <- ifelse(DATA$TOO.MANY.MISSING =="HOLD", "HOLD", DATA$RESULT)

  DATA<-subset(DATA, select = -c(ENTRY, MEASURE, COUNT, SCORE, MODLSE))

  library(tidyr)
  ID<-ID %>%
    separate(NAME, into = c("ID.Code",
                            "Gender",
                            "Ethnicity",
                            "Degree",
                            "Med.School",
                            "Cert.Recert",
                            "IMG",
                            "Total.Exams",
                            "Total.Fails",
                            "Consec.Fails",
                            "ACGME",
                            "Fellowship",
                            "Cert.Year",
                            "Form",
                            "Last.Name",
                            "First.Name",
                            "State",
                            "Zip",
                            "Test.Center",
                            "Exam.Date"), sep = c(7, 12, 14, 17, 28, 30, 32, 34, 37, 40, 52,
                                                  57, 68, 70, 100, 130, 133, 139, 145))

  ID<-as.data.frame(apply(ID,2,function(x)gsub('\\s+', '',x)))
  DATA2<-merge(ID, DATA, by="row.names")
  DATA3<-subset(DATA2, select=-c(Row.names, NAME))

  write.csv(DATA3, "Scoring/Preliminary_Results.csv", row.names=FALSE)

}




