preliminary_PRIMARY<-function() {
  #packages to load (dependencies)
  library(tools)
  library(plyr)
  library(reshape2)
  library(rmarkdown)



  #NEED TO GET A CSV FILE FOR DEMOGRAPHICS WHERE COL1=ENTRY
  DATA<-read.csv("./Scoring/PER/PER_Overall.CSV")
  ID<-read.csv("./Scoring/ID.csv")
  ID$Repeater<-ifelse(ID$Consecutive.Fails>0, "REPEAT", "FIRST")


  #DROP UNUSED VARIABLES
  DATA<-subset(DATA, select = -c(STATUS,IN.MSQ,IN.ZSTD,OUT.MSQ,OUT.ZSTD,DISPLACE,PTMA,PTMA.E,
                                 WEIGHT,OBSMATCH,EXPMATCH,PVALUE,PTMA,RMSR,WMLE,NAME))

  #SCORING RUN
  DATA$Scaled_Score_Detailed <-  (218.47*DATA$MEASURE) + 250.86
  DATA$Scaled_Score <- round_any(((218.47*DATA$MEASURE) + 250.86),10, floor)
  DATA$Scaled_Score_SE<-((218.47*(DATA$MEASURE))+250.86 -
                            ((218.47*((DATA$MEASURE)-(DATA$MODLSE)))+250.86))
  DATA$pct_correct<-(DATA$SCORE/DATA$COUNT)*100
  DATA$CIH<-DATA$Scaled_Score_Detailed + DATA$Scaled_Score_SE
  DATA$CIL<-DATA$Scaled_Score_Detailed - DATA$Scaled_Score_SE
  DATA$Scaled_Score<-ifelse(DATA$Scaled_Score < 200,200, DATA$Scaled_Score)
  DATA$Scaled_Score<-ifelse(DATA$Scaled_Score > 800,800, DATA$Scaled_Score)
  DATA$CIH<-ifelse(DATA$CIH > 1000, 1000, DATA$CIH)
  DATA$CIH<-ifelse(DATA$Scaled_Score==200 & DATA$CIH < 220, 220, DATA$CIH)
  DATA$CIL<-ifelse(DATA$CIH < 0, 0, DATA$CIL)
  DATA$CIL<-ifelse(DATA$Scaled_Score==800 & DATA$CIL >780, 780, DATA$CIL)


  #CALCULATE PASS/FAIL
  DATA$PF_Overall<-ifelse(DATA$Scaled_Score.Overall>=380, "PASS", "FAIL")
  DATA$Unanswered<-DATA$COUNT.Overall - DATA$COUNT.Unanswered

  return (write.csv(DATA, "Scoring/Preliminary_Results.csv", row.names=FALSE))




}



