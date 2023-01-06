#' @export
#' @title API Anahtarini Tanitma
#' @description TCMB/EVDS'den alinan API anahtarini tanimlamaya yarayan fonksiyondur.
#' Anahtar olmadan herhangi bir veri cekme islemi gerceklestirilemez.
#' Yardim icin yardim() fonksiyonu calistirilabilir.
#' @param api_anahtar API anahtari.
#' @examples
#' \dontrun{anahtar(api_anahtar = "api_anahtar")}
anahtar <- function(api_anahtar){

  Sys.setenv(myKey = api_anahtar)
  anahtarim <<- Sys.getenv("myKey")

}

#' @export
#' @title API Anahtarinin Nasil Alinacagi Konusunda Yardim
#' @description Adim adim API anahtarinin nasil alinacagini gosteren fonksiyondur.
#' Aciklamada bulunan url:
#' \url{https://evds2.tcmb.gov.tr/index.php?/evds/login}
yardim <- function(){
  print("https://evds2.tcmb.gov.tr/index.php?/evds/login adresine gidin.", quote = F)
  print("Uye ekraninda giris yaptiktan sonra Kullanici ismine tiklayip gelen menudeki Profil secenegine tiklayin.", quote = F)
  print("Gelen ekrandan API Anahtari dugmesine tiklayarak ihtiyaciniz olan degere ulasabilirsiniz.", quote = F)
  print("Paketi cagirdiktan sonra anahtar() fonksiyonu ile anahtarinizi tanitin.", quote = F)
}

#' @export
#' @title Gozlem, Formul ve Frekans Parametrelerini Tanima
#' @description Veriyi/verileri cekmeden once filtrelemeyi saglarlar.
#' Kullanimi zorunlu degildir.
#' Belirtilmedigi zaman sistem varsayilani getirecektir.
#' Bu fonksiyon sadece tanitim amaclidir.
#' veriler() fonksiyonunda kullanilacaktir.
parametreler <- function(){

  gozlem <<- data.frame(
    "Tip" = c("Ortalama",
              "En Dusuk",
              "En Yuksek",
              "Baslangic",
              "Bitis",
              "Kumulatif"),
    "Kisa_Kod" = c("avg",
                   "min",
                   "max",
                   "first",
                   "last",
                   "sum")
  )

  formul <<- data.frame(
    "Tip" = c("Duzey",
              "Yuzde Degisim",
              "Fark",
              "Yillik Yuzde Degisim",
              "Yillik Fark",
              "Bir Onceki Yilin Sonuna Gore Yuzde Degisim",
              "Bir Onceki Yilin Sonuna Gore Fark",
              "Hareketli Ortalama",
              "Hareketli Toplam"),
    "Kisa_Kod" = c("0",
                   "1",
                   "2",
                   "3",
                   "4",
                   "5",
                   "6",
                   "7",
                   "8")
  )

  frekans <<- data.frame(
    "Tip" = c("Gunluk",
              "Isgunu",
              "Haftalik",
              "Ayda 2 Kez",
              "Aylik",
              "3 Aylik",
              "6 Aylik",
              "Yillik"),
    "Kisa_Kod" = c("1",
                   "2",
                   "3",
                   "4",
                   "5",
                   "6",
                   "7",
                   "8")
  )

}

#' @export
#' @title Kategorileri Tanima
#' @description Piyasa Verileri, Kurlar, Faiz Istatistikleri gibi kategorileri icermektedir.
#' Ilgilenilen kategori/kategoriler belirlendikten sonra verigruplari() fonksiyonunda kullanilacaktir.
#' @importFrom dplyr select rename %>%
#' @importFrom jsonlite fromJSON
kategoriler <- function(){

  if(exists("anahtarim")){
    url <- paste0("https://evds2.tcmb.gov.tr/service/evds/categories/key=",anahtarim,"&type=json")
    kategori <<- fromJSON(url) %>%
      as.data.frame() %>%
      select(CATEGORY_ID,TOPIC_TITLE_TR) %>%
      rename("Kategori_Kodu" = "CATEGORY_ID", "Konu_Basligi"="TOPIC_TITLE_TR")
  } else {

    stop("Lutfen TCMB'den API anahtarinizi alin. Detayli bilgi icin yardim() fonksiyonunu calistirin.")

  }

}

#' @export
#' @title Veri Gruplarini Tanima
#' @description Doviz Kurlari, Efektif Kurlar gibi veri gruplarini icermektedir.
#' Ilgilenilen veri grubu/gruplari secildikten sonra seriler() fonksiyonunda kullanilacaktir.
#' @param kategoriKodu Istenen kategorinin/kategorilerin kodu. Deger girilecekse hepsi parametresi FALSE birakilmalidir. Birden fazla secilmek istenirse c("","",...) seklinde girilmelidir.
#' @param hepsi Varsayilani FALSE'tur. Eger tum veri gruplari istenirse TRUE yapilmalidir. Bu durumda kategoriKodu parametresine deger girilmemelidir.
#' @examples
#' \dontrun{verigruplari(kategoriKodu = 2)}
#' \dontrun{verigruplari(kategoriKodu = c(2,14))}
#' \dontrun{verigruplari(hepsi = TRUE)}
#' @importFrom dplyr select rename filter %>%
#' @importFrom jsonlite fromJSON
verigruplari <- function(kategoriKodu = NULL, hepsi = FALSE){

  if(exists("anahtarim")){
    url <- paste0("https://evds2.tcmb.gov.tr/service/evds/datagroups/key=",anahtarim,"&mode=0&type=json")
    verigrubu <<- fromJSON(url) %>%
      as.data.frame() %>%
      filter(if(hepsi == FALSE & !is.null(kategoriKodu)) CATEGORY_ID %in% kategoriKodu else CATEGORY_ID == CATEGORY_ID)
  } else {

    stop("Lutfen TCMB'den API anahtarinizi alin. Detayli bilgi icin yardim() fonksiyonunu calistirin.")

  }

}

#' @export
#' @title Serileri Tanima
#' @description ABD Dolari (Doviz Alis), ABD Dolari (Doviz Satis) gibi serileri icermektedir.
#' Ilgilenilen seri/seriler secildikten sonra veriler() fonksiyonunda kullanilacaktir.
#' @param verigrupKodu Istenen veri grubunun kodu.
#' @examples
#' \dontrun{seriler(verigrupKodu = "bie_dkdovytl")}
#' @importFrom dplyr select rename %>%
#' @importFrom jsonlite fromJSON
seriler <- function(verigrupKodu){

  if(exists("anahtarim")){
    url <- paste0("https://evds2.tcmb.gov.tr/service/evds/serieList/key=",anahtarim,"&type=json&code=",verigrupKodu)
    seri <<- fromJSON(url) %>%
      as.data.frame() %>%
      select(SERIE_CODE,DATAGROUP_CODE,SERIE_NAME,FREQUENCY_STR,DEFAULT_AGG_METHOD_STR,DEFAULT_AGG_METHOD,START_DATE,END_DATE)
  } else {

    stop("Lutfen TCMB'den API anahtarinizi alin. Detayli bilgi icin yardim() fonksiyonunu calistirin.")

  }

}

#' @export
#' @title Verileri Cekme
#' @description Istenen verinin/verilerin seri kodunun/kodlarinin tanimlanacagi fonksiyondur.
#' @param seriKodu Veri/verilerin seri kodu.
#' Birden fazla kod girmek icin c("","",...) seklinde yazilmalidir.
#' @param baslangicTarihi gg-aa-yyyy formatinda girilmelidir.
#' @param bitisTarihi Opsiyonel.
#' gg-aa-yyyy formatinda girilmelidir.
#' Girilmezse son tarihi dikkate alacaktir.
#' @param gozlemParametresi Opsiyonel.
#' Detay icin parametreler() fonksiyonu calistirilabilir.
#' Birden fazla seri kodu istenirse gozlem parametreleri her biri icin c("","",...) seklinde tanitilmalidir.
#' Birden fazla seri kodu girilmis ve tek bir parametre yazilmis ise digerleri de ayni sekilde gelecektir.
#' Girilmedigi zaman sistem ilgili seri icin orijinal gozlemin parametresini uygulayacaktir.
#' @param formulParametresi Opsiyonel.
#' Detay icin parametreler() fonksiyonu calistirilabilir.
#' Birden fazla seri kodu istenirse formul parametreleri her biri icin c("","",...) seklinde tanitilmalidir.
#' Birden fazla seri kodu girilmis ve tek bir parametre yazilmis ise digerleri de ayni sekilde gelecektir.
#' Girilmedigi zaman sistem ilgili seri icin duzey parametresini uygulayacaktir.
#' @param sayisalFrekans Opsiyonel.
#' Detay icin parametreler() fonksiyonu calistirilabilir.
#' Birden fazla seri kodunda tek bir ortak deger olmalidir.
#' Girilmedigi zaman sistem serilerin ortak frekansini alacaktir.
#' @examples
#' \dontrun{veriler(seriKodu = "TP.DK.USD.A.YTL", baslangicTarihi = "01-01-2020")}
#' \dontrun{veriler(seriKodu = "TP.DK.USD.A.YTL", baslangicTarihi = "01-01-2020", bitisTarihi = "01-03-2021", gozlemParametresi = "avg", formulParametresi = 1, sayisalFrekans = 5)}
#' \dontrun{veriler(seriKodu = c("TP.DK.USD.A.YTL","TP.DK.USD.S.YTL"), baslangicTarihi = "01-01-2020", bitisTarihi = "01-03-2021", gozlemParametresi = "avg", formulParametresi = 1, sayisalFrekans = 5)}
#' \dontrun{veriler(seriKodu = c("TP.DK.USD.A.YTL","TP.DK.USD.S.YTL"), baslangicTarihi = "01-01-2010", bitisTarihi = "01-12-2020", gozlemParametresi = c("avg","max"), formulParametresi = 1, sayisalFrekans = 8)}
#' @importFrom dplyr select select_if rename_with everything %>%
#' @importFrom jsonlite fromJSON
veriler <- function(seriKodu, baslangicTarihi, bitisTarihi = "01-01-3000", gozlemParametresi = NULL, formulParametresi = NULL, sayisalFrekans = NULL){

  if(exists("anahtarim")){

    if(length(seriKodu) > 1){

      seriesVec <- paste(seriKodu, collapse = "-")

      if(length(gozlemParametresi) == 1){
        aggVec <- paste(rep(gozlemParametresi,length(seriKodu)), collapse = "-")
      }
      if(length(formulParametresi) == 1){
        formulasVec <- paste(rep(formulParametresi,length(seriKodu)), collapse = "-")
      }

      if(length(gozlemParametresi) > 1){
        aggVec <- paste(gozlemParametresi, collapse = "-")
      }
      if(length(formulParametresi) > 1){
        formulasVec <- paste(formulParametresi, collapse = "-")
      }

      url <- paste0("https://evds2.tcmb.gov.tr/service/evds/series=",seriesVec,
                    "&startDate=",baslangicTarihi,
                    "&endDate=",bitisTarihi,
                    "&type=json&key=",anahtarim,
                    ifelse(is.null(gozlemParametresi),"&aggregationTypes",paste0("&aggregationTypes=",aggVec)),
                    ifelse(is.null(formulParametresi),"&formulas",paste0("&formulas=",formulasVec)),
                    ifelse(is.null(sayisalFrekans),"&frequency",paste0("&frequency=", sayisalFrekans)))

    } else if(length(seriKodu) == 1){

      url <- paste0("https://evds2.tcmb.gov.tr/service/evds/series=",seriKodu,
                    "&startDate=",baslangicTarihi,
                    "&endDate=",bitisTarihi,
                    "&type=json&key=",anahtarim,
                    ifelse(is.null(gozlemParametresi),"&aggregationTypes",paste0("&aggregationTypes=",gozlemParametresi)),
                    ifelse(is.null(formulParametresi),"&formulas",paste0("&formulas=",formulParametresi)),
                    ifelse(is.null(sayisalFrekans),"&frequency",paste0("&frequency=", sayisalFrekans)))

    }

  } else {

    stop("Lutfen TCMB'den API anahtarinizi alin. Detayli bilgi icin yardim() fonksiyonunu calistirin.")

  }

  veri <<- fromJSON(url) %>%
    as.data.frame() %>%
    select(-totalCount,-`items.UNIXTIME`) %>%
    rename_with(~gsub("items.", "", .x), everything()) %>%
    select_if(function(x) any(sum(is.na(x)) != nrow(.))) %>%
    rename_with(~gsub("\\..*", "", .), everything())

  #Oh, you've found a bug in my code?
  #Please, tell me more -_-

}

#' @export
#' @title Verileri Kaydetme
#' @description Verilerin .csv, .xlsx veya .txt formatlarinda kaydedilmesini saglayan fonksiyondur.
#' @param format Opsiyonel. Varsayilan formati .csv olarak girilmistir.
#' @param dosyaYolu Opsiyonel. Degistirilmezse varsayilan dosya dizinini alir.
#' Dosya dizinini ogrenmek icin getwd() fonksiyonu kullanilabilir.
#' @examples
#' \dontrun{aktar(format = ".xlsx", dosyaYolu = "C:/Users/...")}
#' @importFrom openxlsx write.xlsx
aktar <- function(format = NULL, dosyaYolu = NULL){

  if(exists("veri")){

    if(is.null(dosyaYolu)){
      dosyaYolu <- getwd()
    } else {
      dosyaYolu <- dosyaYolu
    }

    if(is.null(format)){
      write.csv(veri, paste0(dosyaYolu,"/veri_",format(Sys.time(),"%Y%m%d%H%M"),".csv"), row.names = F)
    } else if(format == ".xlsx"){
      write.xlsx(veri, paste0(dosyaYolu,"/veri_",format(Sys.time(),"%Y%m%d%H%M"),".xlsx"), row.names = F)
    } else if(format == ".txt"){
      write.table(veri, paste0(dosyaYolu,"/veri_",format(Sys.time(),"%Y%m%d%H%M"),".txt"), row.names = F, sep = "\t")
    }

  } else {

    stop("'veri' isimli veri cercevesi bulunmamaktadir.")

  }

}

#' @export
#' @title Veri Gorsellestirme
#' @description Tek bir seriyi basit bir duzeyde ve interaktif bir sekilde gorsellestirmeyi saglayan fonksiyondur.
#' Gorsel uzerinde yakinlastirma, kaydetme gibi islemler yapilabilir.
#' @param seriKodu Gorsellestirilmek istenen seri.
#' NA (kayip) degerler kaldirilir.
#' @param sayisalFrekans Gorsellestirilmek istenen serinin sayisal formatta frekansi.
#' Veri ile ayni frekansta olmalidir.
#' @examples
#' \dontrun{gorsellestir(seriKodu = veri$TP_DK_USD_A_YTL, sayisalFrekans = 5)}
#' @importFrom dplyr %>%
#' @importFrom zoo as.Date
#' @importFrom lubridate dmy ymd
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal scale_x_continuous
#' @importFrom plotly ggplotly
gorsellestir <- function(seriKodu, sayisalFrekans){

  if(exists("veri")){

    dt <- veri

    if(sayisalFrekans == 1 | sayisalFrekans == 2 | sayisalFrekans == 4){
      dt$Tarih <- dmy(dt$Tarih)
    } else if(sayisalFrekans == 5){
      dt$Tarih <- ymd(paste(dt$Tarih,1,sep = "-"))
    } else if(sayisalFrekans == 6){
      dt$Tarih <- ifelse(grepl("Q1",dt$Tarih),
                         ymd(paste(substr(dt$Tarih,1,4),3,1,sep = "-")),
                         ifelse(grepl("Q2",dt$Tarih),
                                ymd(paste(substr(dt$Tarih,1,4),6,1,sep = "-")),
                                ifelse(grepl("Q3",dt$Tarih),
                                       ymd(paste(substr(dt$Tarih,1,4),9,1,sep = "-")),
                                       ymd(paste(substr(dt$Tarih,1,4),12,1,sep = "-")))))
      dt$Tarih <- as.Date(dt$Tarih)
    } else if(sayisalFrekans == 7){
      dt$Tarih <- ifelse(grepl("S1", dt$Tarih),
                         as.Date(paste(substr(dt$Tarih,1,4),6,1,sep = "-")),
                         as.Date(paste(substr(dt$Tarih,1,4),12,1,sep = "-")))
      dt$Tarih <- as.Date(dt$Tarih)
    } else if(sayisalFrekans == 8){
      dt$Tarih <- as.numeric(dt$Tarih)
    } else if(sayisalFrekans == 3){
      dt$Tarih <- dmy(dt$Tarih)
      dt$YEARWEEK <- NULL
    }

    dt[[2]] <- as.numeric(dt[[2]])
    names(dt)[2] <- "Seri"
    dt <- dt %>% na.omit()

    if(sayisalFrekans != 8){
      g <- ggplot(data = dt, mapping = aes(x = Tarih, y = Seri)) +
        geom_line() +
        theme_minimal()
    } else {
      g <- ggplot(data = dt, mapping = aes(x = Tarih, y = Seri)) +
        geom_line() +
        theme_minimal() +
        scale_x_continuous(breaks = c(seq(min(dt$Tarih),max(dt$Tarih),1)))
    }

    ggplotly(g)

  } else {

    stop("'veri' isimli veri cercevesi bulunmamaktadir.")

  }

}
