library(readr)
library(magrittr)
vol <- scan("FULL_TXT/Vol1-ToSplit.txt", sep="\n", what="character")
meta <- read_csv("TOCMetaData.csv")

grep("\f\\(13)\\)", ignore.case=TRUE, vol1)

metarows <- nrow(meta)
for (x in 1:metarows) {
  if (x != metarows){
    #cat(paste("printing row #",x, sep=""))
    page1 <- print(paste("#\\(",meta$Page[x],"\\)", sep=""))
    page2 <- print(paste("#\\(",meta$Page[x + 1],"\\)", sep=""))
    pageno <- grep(page1, ignore.case=TRUE, vol)
    pageno2 <- grep(page2, ignore.case=TRUE, vol)
    lines <- print(vol[pageno:pageno2])
    filename <- print(paste("ETO",meta$ETO[x],meta$accused,".txt", sep=""))
    fileConn<-file(paste("IndivCases/", filename))
    writeLines(lines, fileConn)
    close(fileConn)
  } else {
    page1 <- print(paste("#\\(",meta$Page[metarows],"\\)", sep=""))
    enddoc <- length(vol)
    pageno <- grep(page1, ignore.case=TRUE, vol)
    lines <- print(vol[pageno:enddoc])
    filename <- print(paste("ETO",meta$ETO[metarows],meta$accused,".txt", sep=""))
    fileConn<-file(paste("IndivCases/", filename))
    writeLines(lines, fileConn)
    close(fileConn)
  }
}


make.files <- function(files, input.dir) {
  for (x in 1:length(files)) {
    text <- scan(files[x], sep="\n", what="character")
    text.lower <- tolower(text)
    volpos.v <- grep("(^Vol.) [\\XVlILNU]", ignore.case = TRUE, text)
    for (i in 1:length(volpos.v)){
      if (i != length(volpos.v)){
        start <- volpos.v[i]
        end <- volpos.v[i+1]
        volume.lines.l <- text[start:end]
        filename <- print(paste(text[start],".txt"))
        fileConn<-file(paste("IndivVolumes/", filename))
        writeLines(volume.lines.l, fileConn)
        close(fileConn)
      } else {
        start <- tail(volpos.v, n=1)
        end <- length(text)
        vol.title <- text[[start]]
        lastlines <- text[start:end]
        filename <- print(paste(text[start],".txt"))
        fileConn<-file(paste("IndivVolumes/", filename))
        writeLines(volume.lines.l, fileConn)
        close(fileConn)
      }
    } 
  }
}
#check
nrow(meta)
length(list.files("IndivCases/"))
