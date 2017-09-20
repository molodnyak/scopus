#Загрузка Authors, Authors with affiliations, EID
library(xlsx)
scopus <- read.xlsx("//fs5/Аналитический центр/44-Публикации (Молодняк)/2016/N/АвторПубликацияEID.xlsx", 1)
nrow(scopus)

#Разделение авторов с аффиляциями
s <- strsplit(as.character(scopus$Authors.with.affiliations), '; ')
s <- data.frame(AuthorsAffil=unlist(s), 
                Authors=rep(scopus$Authors, sapply(s, FUN=length)), 
                EID=rep(scopus$EID, sapply(s, FUN=length)), stringsAsFactors = FALSE)
#Отделение автора от аффиляции
s$Author <- gsub("^([^,]*,[^,]*),.*$", "\\1", s$AuthorsAffil)

#Нахождение всех авторов ВШЭ
s$HSE[
  grepl("HSE", s$AuthorsAffil) == TRUE | 
    grepl("Higher School of Economics", s$AuthorsAffil) == TRUE | 
    grepl("High School of Economics", s$AuthorsAffil) == TRUE | 
    grepl("Higher, School of Economics", s$AuthorsAffil) == TRUE | 
    grepl("Higher Schools of Economics", s$AuthorsAffil) == TRUE | 
    grepl("Higher School of Economies", s$AuthorsAffil) == TRUE | 
    grepl("Higher School of Economic", s$AuthorsAffil) == TRUE | 
    grepl("High. School of Economic", s$AuthorsAffil) == TRUE | 
    grepl("High School of Economic", s$AuthorsAffil) == TRUE | 
    grepl("Higher School of Economy", s$AuthorsAffil) == TRUE | 
    grepl("Higher School of Economia", s$AuthorsAffil) == TRUE | 
    grepl("Higher School for Economics", s$AuthorsAffil) == TRUE | 
    grepl("High Economic School", s$AuthorsAffil) == TRUE | 
    grepl("Moscow Economics National Research University", s$AuthorsAffil) == TRUE | 
    grepl("Mjasnickaja", s$AuthorsAffil) == TRUE | 
    grepl("Myasnitskaya", s$AuthorsAffil) == TRUE | 
    grepl("Inst. of Electronics and Mathematics", s$AuthorsAffil) == TRUE | 
    grepl("Institute of Electronics and Mathematics", s$AuthorsAffil) == TRUE | 
    grepl("Institute for Electronics and Mathematics", s$AuthorsAffil) == TRUE ] <- "HSE"

#Поиск ID авторов
Encoding(s$Author) <- "UTF-8"
y <- s[!is.na(s$HSE),]

x <- strsplit(as.character(y$Author), ', ')
x <- data.frame(y$AuthorsAffil, y$EID, y$Author, do.call(rbind, x), stringsAsFactors = FALSE)
names(x) <- c("AuthorsAffil", "EID", "Author", "authlast", "authfirst")
x <- unique(x)

x$authlast <- chartr("a??ceiinooSsu", "abceiinooSsu", x$authlast)

x$authlast <- gsub("’", "*", x$authlast)
x$authlast <- gsub("'", "*", x$authlast)
x$authlast <- gsub(" ", "*", x$authlast)
x$authfirst <- gsub(" ", "*", x$authfirst)
x$url <- paste0("authlast(", x$authlast, ")%20and%20authfirst(", x$authfirst, ")")
x <- x[order(x$Author),]
row.names(x) <- 1:nrow(x)
nrow(x)
x[,"scopus.id"] <- NA
x[,"scopus.id2"] <- NA
x[,"scopus.id3"] <- NA
x[,"scopus.id4"] <- NA
x[,"scopus.id5"] <- NA
x[,"givname"] <- NA
x[,"givname2"] <- NA
x[,"givname3"] <- NA
x[,"givname4"] <- NA
x[,"givname5"] <- NA
x[,"initials"] <- NA
x[,"initials2"] <- NA
x[,"initials3"] <- NA
x[,"initials4"] <- NA
x[,"initials5"] <- NA
x[,"affid"] <- NA
x[,"affid2"] <- NA
x[,"affid3"] <- NA
x[,"affid4"] <- NA
x[,"affid5"] <- NA
x[,"doccount"] <- NA
x[,"doccount2"] <- NA
x[,"doccount3"] <- NA
x[,"doccount4"] <- NA
x[,"doccount5"] <- NA

baseurl <- "https://api.elsevier.com/content/search/author?query="
affil <- "%20and%20affil(National%20Research%20University%20Higher%20School%20of%20Economics)"
apiKey <- "YOUR_API_KEY" # работает только с компьютеров НИУ ВШЭ
httpAccept <- "%20application%2Fjson"

library(jsonlite)
library(curl)

for (i in 1:5424){
  tryCatch({
req <- fromJSON(paste0(baseurl, x$url[i], affil, "&apiKey=", apiKey, "&httpAccept=", httpAccept))
if(req[1][[1]][[1]] == "0") {stop("Отсутствует Scopus ID автора ! - ", x$Author[i])} 
else { 
  if(req[1][[1]][[1]] != "0") {

    x$scopus.id[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[1]
    x$scopus.id2[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[2]
    x$scopus.id3[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[3]
    x$scopus.id4[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[4]
    x$scopus.id5[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[5]

    x$givname[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[1], req$`search-results`$entry$`preferred-name`$`given-name`[1], sep = ", ")
    x$givname2[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[2], req$`search-results`$entry$`preferred-name`$`given-name`[2], sep = ", ")
    x$givname3[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[3], req$`search-results`$entry$`preferred-name`$`given-name`[3], sep = ", ")
    x$givname4[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[4], req$`search-results`$entry$`preferred-name`$`given-name`[4], sep = ", ")
    x$givname5[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[5], req$`search-results`$entry$`preferred-name`$`given-name`[5], sep = ", ")

    x$initials[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[1], req$`search-results`$entry$`preferred-name`$initials[1], sep = ", ")
    x$initials2[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[2], req$`search-results`$entry$`preferred-name`$initials[2], sep = ", ")
    x$initials3[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[3], req$`search-results`$entry$`preferred-name`$initials[3], sep = ", ")
    x$initials4[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[4], req$`search-results`$entry$`preferred-name`$initials[4], sep = ", ")
    x$initials5[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[5], req$`search-results`$entry$`preferred-name`$initials[5], sep = ", ")
    
    x$affid[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[1]
    x$affid2[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[2]
    x$affid3[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[3]
    x$affid4[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[4]
    x$affid5[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[5]
    
    x$doccount[i] <- req$`search-results`$entry$`document-count`[1]
    x$doccount2[i] <- req$`search-results`$entry$`document-count`[2]
    x$doccount3[i] <- req$`search-results`$entry$`document-count`[3]
    x$doccount4[i] <- req$`search-results`$entry$`document-count`[4]
    x$doccount5[i] <- req$`search-results`$entry$`document-count`[5]
  }
}
  message("Загружается данные автора ", paste(x$Author[i], i, "из 5424", sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

save(x, file="scopus/scopusidpub.R")

-------------------------------------------------
#Повторный запрос по ненайденным авторам (фамилия и 1 буква инициалов)

x$url[is.na(x$scopus.id)] <- paste0("authlast(", x$authlast[is.na(x$scopus.id)], ")%20and%20authfirst(", paste0(unlist(lapply(strsplit(x$authfirst[is.na(x$scopus.id)], "[.]"), '[[', 1)), "."), ")")

n <- as.numeric(row.names(x[is.na(x$scopus.id),]))
for (i in n){
  tryCatch({
    req <- fromJSON(paste0(baseurl, x$url[i], affil, "&apiKey=", apiKey, "&httpAccept=", httpAccept))
    if(req[1][[1]][[1]] == "0") {stop("Отсутствует Scopus ID автора ! - ", x$Author[i])} 
    else { 
      if(req[1][[1]][[1]] != "0") {
        
        x$scopus.id[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[1]
        x$scopus.id2[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[2]
        x$scopus.id3[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[3]
        x$scopus.id4[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[4]
        x$scopus.id5[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[5]
        
        x$givname[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[1], req$`search-results`$entry$`preferred-name`$`given-name`[1], sep = ", ")
        x$givname2[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[2], req$`search-results`$entry$`preferred-name`$`given-name`[2], sep = ", ")
        x$givname3[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[3], req$`search-results`$entry$`preferred-name`$`given-name`[3], sep = ", ")
        x$givname4[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[4], req$`search-results`$entry$`preferred-name`$`given-name`[4], sep = ", ")
        x$givname5[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[5], req$`search-results`$entry$`preferred-name`$`given-name`[5], sep = ", ")
        
        x$initials[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[1], req$`search-results`$entry$`preferred-name`$initials[1], sep = ", ")
        x$initials2[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[2], req$`search-results`$entry$`preferred-name`$initials[2], sep = ", ")
        x$initials3[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[3], req$`search-results`$entry$`preferred-name`$initials[3], sep = ", ")
        x$initials4[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[4], req$`search-results`$entry$`preferred-name`$initials[4], sep = ", ")
        x$initials5[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[5], req$`search-results`$entry$`preferred-name`$initials[5], sep = ", ")
        
        x$affid[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[1]
        x$affid2[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[2]
        x$affid3[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[3]
        x$affid4[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[4]
        x$affid5[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[5]
        
        x$doccount[i] <- req$`search-results`$entry$`document-count`[1]
        x$doccount2[i] <- req$`search-results`$entry$`document-count`[2]
        x$doccount3[i] <- req$`search-results`$entry$`document-count`[3]
        x$doccount4[i] <- req$`search-results`$entry$`document-count`[4]
        x$doccount5[i] <- req$`search-results`$entry$`document-count`[5]
      }
    }
    message("Загружается данные автора ", paste(x$Author[i], i, "из 5424", sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
  
--------------------------------------------------------
#Повторный запрос по ненайденным авторам (только фамилия)
x$url[is.na(x$scopus.id)] <- paste0("authlast(", x$authlast[is.na(x$scopus.id)], ")")

n <- as.numeric(row.names(x[is.na(x$scopus.id),]))
for (i in n){
  tryCatch({
    req <- fromJSON(paste0(baseurl, x$url[i], affil, "&apiKey=", apiKey, "&httpAccept=", httpAccept))
    if(req[1][[1]][[1]] == "0") {stop("Отсутствует Scopus ID автора ! - ", x$Author[i])} 
    else { 
      if(req[1][[1]][[1]] != "0") {
        
        x$scopus.id[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[1]
        x$scopus.id2[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[2]
        x$scopus.id3[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[3]
        x$scopus.id4[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[4]
        x$scopus.id5[i] <- gsub("AUTHOR_ID:", "", req$`search-results`$entry$`dc:identifier`)[5]
        
        x$givname[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[1], req$`search-results`$entry$`preferred-name`$`given-name`[1], sep = ", ")
        x$givname2[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[2], req$`search-results`$entry$`preferred-name`$`given-name`[2], sep = ", ")
        x$givname3[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[3], req$`search-results`$entry$`preferred-name`$`given-name`[3], sep = ", ")
        x$givname4[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[4], req$`search-results`$entry$`preferred-name`$`given-name`[4], sep = ", ")
        x$givname5[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[5], req$`search-results`$entry$`preferred-name`$`given-name`[5], sep = ", ")
        
        x$initials[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[1], req$`search-results`$entry$`preferred-name`$initials[1], sep = ", ")
        x$initials2[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[2], req$`search-results`$entry$`preferred-name`$initials[2], sep = ", ")
        x$initials3[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[3], req$`search-results`$entry$`preferred-name`$initials[3], sep = ", ")
        x$initials4[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[4], req$`search-results`$entry$`preferred-name`$initials[4], sep = ", ")
        x$initials5[i] <- paste(req$`search-results`$entry$`preferred-name`$surname[5], req$`search-results`$entry$`preferred-name`$initials[5], sep = ", ")
        
        x$affid[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[1]
        x$affid2[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[2]
        x$affid3[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[3]
        x$affid4[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[4]
        x$affid5[i] <- req$`search-results`$entry$`affiliation-current`$`affiliation-id`[5]
        
        x$doccount[i] <- req$`search-results`$entry$`document-count`[1]
        x$doccount2[i] <- req$`search-results`$entry$`document-count`[2]
        x$doccount3[i] <- req$`search-results`$entry$`document-count`[3]
        x$doccount4[i] <- req$`search-results`$entry$`document-count`[4]
        x$doccount5[i] <- req$`search-results`$entry$`document-count`[5]
      }
    }
    message("Загружается данные автора ", paste(x$Author[i], i, "из 5424", sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

-------------------------------------------------------
#Запрос по полученным ID авторов
idurl <- "https://api.elsevier.com/content/author/author_id/"
for (i in 1:2255){
  tryCatch({
req <- fromJSON(paste0(idurl, x$scopus.id[1], "?apiKey=", apiKey, "&httpAccept=", httpAccept))
if(req[1][[1]][[1]] == "0") stop("Отсутствует Scopus ID автора ! ---", x$Author[i])
x$scopus.id[i] <- gsub("AUTHOR_ID:", "", req[1][[1]]$entry[4][[1]])[1]

message("Загружаются данные по ID автора ", x$Author[i])
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}    

-------------------------------------------------------
#Запрос ORCID по проверенным ID
library(xlsx)
library(reshape)

orcid <- read.xlsx("//fs5/Аналитический центр/44-Публикации (Молодняк)/2016/scopusorcid.xlsx", 1)
orcid <- na.omit(reshape(orcid, varying = 2:5, v.names = "ID", direction = "long"))
orcid[] <- lapply(orcid, as.character)
orcid$time <- NULL
orcid$id <- NULL
orcid <- orcid[order(orcid$Author), ]
row.names(orcid) <- 1:nrow(orcid)
n <- as.numeric(row.names(orcid))

baseurl <- "https://api.elsevier.com/content/author/author_id/"
apiKey <- "YOUR_API_KEY" # работает только с компьютеров НИУ ВШЭ
httpAccept <- "%20application%2Fjson"

orcid[,"current"] <- NA
orcid[,"create"] <- NA
orcid[,"ORCID"] <- NA

for (i in n){
  tryCatch({
    req <- fromJSON(paste0(baseurl, orcid$ID[i], "?apiKey=", apiKey, "&httpAccept=", httpAccept))
    if(is.null(req$`author-retrieval-response`$coredata$orcid) == TRUE) 
    {
      orcid$current[i] <- req$`author-retrieval-response`$`affiliation-current`[[2]]
      orcid$create[i] <- paste(req$`author-retrieval-response`$`author-profile`$`date-created`[[1]], req$`author-retrieval-response`$`author-profile`$`date-created`[[2]], req$`author-retrieval-response`$`author-profile`$`date-created`[[3]], sep = ".")
      orcid[,"ORCID"][i] <- NA
    }
 
    else { 
      if(is.null(req$`author-retrieval-response`$coredata$orcid) == FALSE) {    
        
        orcid$current[i] <- req$`author-retrieval-response`$`affiliation-current`[[2]]
        orcid$create[i] <- paste(req$`author-retrieval-response`$`author-profile`$`date-created`[[1]], req$`author-retrieval-response`$`author-profile`$`date-created`[[2]], req$`author-retrieval-response`$`author-profile`$`date-created`[[3]], sep = ".")
        orcid$ORCID[i] <- req$`author-retrieval-response`$coredata$orcid
      }
    }
    message("Загружается ORCID и данные по ID автора ", paste(orcid$Author[i], i, "из ", nrow(orcid), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

save(orcid, file="scopus/orcid.R")
write.xlsx(orcid, "//fs5/Аналитический центр/44-Публикации (Молодняк)/2016/IDOrcid.xlsx")
