require(readr)
require(magrittr)






create.individual.docs <- function (filepath, volumeno) {
  meta <- read_csv("TOCMetaData.csv")
  volumenumber <- paste("vol", volumeno, sep="")
  metasub <- filter(meta, meta$volume == volumenumber)
  vol <- scan(filepath, sep="\n", what="character")
  metarows <- nrow(metasub)
  for (x in 1:metarows) {
    if (x != metarows){
      #cat(paste("printing row #",x, sep=""))
      page1 <- print(paste("#\\(",metasub$Page[x],"\\)", sep=""))
      page2 <- print(paste("#\\(",metasub$Page[x + 1],"\\)", sep=""))
      pageno <- grep(page1, ignore.case=TRUE, vol)
      pageno2 <- grep(page2, ignore.case=TRUE, vol)
      lines <- print(vol[pageno:pageno2])
      filename <- print(paste("ETO",metasub$ETO[x],metasub$accused,".txt", sep=""))
      fileConn<-file(paste("IndivCases/", filename))
      writeLines(lines, fileConn)
      close(fileConn)
    } else {
      page1 <- print(paste("#\\(",metasub$Page[metarows],"\\)", sep=""))
      enddoc <- length(vol)
      pageno <- grep(page1, ignore.case=TRUE, vol)
      lines <- print(vol[pageno:enddoc])
      filename <- print(paste("ETO",metasub$ETO[metarows],metasub$accused,".txt", sep=""))
      fileConn<-file(paste("IndivCases/", filename))
      writeLines(lines, fileConn)
      close(fileConn)
    }
  }
}



#check
checkfiles <- function() {
  print(nrow(meta))
  print(length(list.files("IndivCases/")))
}
  