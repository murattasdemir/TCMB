#' @export
#' @title API Anahtarını Tanıtma
#' @description TCMB/EVDS'den alınan API anahtarını tanımlamaya yarayan fonksiyondur.
#' Anahtar olmadan herhangi bir veri çekme işlemi gerçekleştirilemez.
#' Yardim için `yardim()` fonksiyonu çalıştırılabilir.
#' @param api_anahtar API anahtarı.
#' @examples
#' \dontrun{
#' anahtar(api_anahtar = "api_anahtar")
#' }
anahtar <- function(api_anahtar){

  Sys.setenv(myKey = api_anahtar)
  anahtarim <- Sys.getenv("myKey")
  assign('anahtarim', anahtarim, envir = .GlobalEnv)
}

#' @export
#' @title API Anahtarının Nasıl Alınacağı Konusunda Yardım
#' @description Adım adım API anahtarının nasıl alınacağını gösteren fonksiyondur.
#' Açıklamada bulunan url:
#' \url{https://evds2.tcmb.gov.tr/index.php?/evds/login}
yardim <- function(){
  print("https://evds2.tcmb.gov.tr/index.php?/evds/login adresine gidin.", quote = F)
  print("Üye ekranında giriş yaptıktan sonra Kullanıcı ismine tıklayıp gelen menüdeki Profil seçeneğine tıklayın.", quote = F)
  print("Gelen ekrandan API Anahtarı dügmesine tıklayarak ihtiyacınız olan değere ulaşabilirsiniz.", quote = F)
  print("Paketi çağırdıktan sonra anahtar() fonksiyonu ile anahtarınızı tanıtın.", quote = F)
}

#' @export
#' @title Gözlem, Formül ve Frekans Parametrelerini Tanıma
#' @description Veriyi/verileri çekmeden önce filtrelemeyi sağlarlar.
#' Kullanımı zorunlu değildir.
#' Belirtilmediği zaman sistem varsayılanı getirecektir.
#' Bu fonksiyon sadece tanıtım amaçlıdır.
#' `veriler()` fonksiyonunda kullanılacaktır.
#' Bu fonksiyon üç adet veri çerçevesi üretir: `gozlem`, `formul`, `frekans`.
parametreler <- function(){

  gozlem <- data.frame(
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

  formul <- data.frame(
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

  frekans <- data.frame(
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
  #Assign to global variables
  assign('gozlem', gozlem, envir = .GlobalEnv)
  assign('frekans', frekans, envir = .GlobalEnv)
  assign('formul', formul, envir = .GlobalEnv)
}

#' @export
#' @title Kategorileri Tanıma
#' @description Piyasa Verileri, Kurlar, Faiz İstatistikleri gibi kategorileri içermektedir.
#' İlgilenilen kategori/kategoriler belirlendikten sonra `verigruplari()` fonksiyonunda kullanılacaktır.
#' @importFrom dplyr select rename %>%
#' @importFrom jsonlite fromJSON
kategoriler <- function(){

  if(exists("anahtarim")){
    url <- paste0("https://evds2.tcmb.gov.tr/service/evds/categories/key=",anahtarim,"&type=json")
    kategori <- fromJSON(url) %>%
      as.data.frame() %>%
      select(CATEGORY_ID,TOPIC_TITLE_TR) %>%
      rename("Kategori_Kodu" = "CATEGORY_ID", "Konu_Basligi"="TOPIC_TITLE_TR")
    assign('kategoriler', kategori, envir = .GlobalEnv)
  } else {

    stop("Lutfen TCMB'den API anahtarinizi alin. Detayli bilgi icin yardim() fonksiyonunu calistirin.")

  }

}

#' @export
#' @title Veri Gruplarını Tanıma
#' @description Döviz Kurları, Dış Ticaret gibi veri gruplarını içermektedir.
#' İlgilenilen veri grubu/grupları seçildikten sonra `seriler()` fonksiyonunda kullanılacaktır.
#' @param kategoriKodu İstenen kategorinin/kategorilerin kodu. Değer girilecekse hepsi parametresi FALSE bırakılmalıdır. Birden fazla seçilmek istenirse `c("", "", ...)` şeklinde girilmelidir.
#' @param hepsi Varsayılanı FALSE'tur. Eğer tüm veri grupları istenirse TRUE yapılmalıdır. Bu durumda kategoriKodu parametresine değer girilmemelidir.
#' @examples
#' \dontrun{
#' verigruplari(kategoriKodu = 2)
#' verigruplari(kategoriKodu = c(2,14))
#' verigruplari(hepsi = TRUE)
#' }
#' @importFrom dplyr select rename filter %>%
#' @importFrom jsonlite fromJSON
verigruplari <- function(kategoriKodu = NULL, hepsi = FALSE){

  if(exists("anahtarim")){
    url <- paste0("https://evds2.tcmb.gov.tr/service/evds/datagroups/key=",anahtarim,"&mode=0&type=json")
    verigrubu <- fromJSON(url) %>%
      as.data.frame() %>%
      filter(if(hepsi == FALSE & !is.null(kategoriKodu)) CATEGORY_ID %in% kategoriKodu else CATEGORY_ID == CATEGORY_ID)
    assign('verigruplari', verigrubu, envir = .GlobalEnv)
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
#' \dontrun{
#' seriler(verigrupKodu = "bie_dkdovytl")
#' }
#' @importFrom dplyr select rename %>%
#' @importFrom jsonlite fromJSON
seriler <- function(verigrupKodu){

  if(exists("anahtarim")){
    url <- paste0("https://evds2.tcmb.gov.tr/service/evds/serieList/key=",anahtarim,"&type=json&code=",verigrupKodu)
    seri <- fromJSON(url) %>%
      as.data.frame() %>%
      select(SERIE_CODE,DATAGROUP_CODE,SERIE_NAME,FREQUENCY_STR,DEFAULT_AGG_METHOD_STR,DEFAULT_AGG_METHOD,START_DATE,END_DATE)
    assign('seriler', seri, envir = .GlobalEnv)
  } else {

    stop("Lutfen TCMB'den API anahtarinizi alin. Detayli bilgi icin yardim() fonksiyonunu calistirin.")

  }

}

#' @export
#' @title Verileri Çekme
#' @description İstenen verinin/verilerin seri kodunun/kodlarının tanımlanacağı fonksiyondur.
#' @param seriKodu Veri/verilerin seri kodu.
#' Birden fazla kod girmek için `c("", "", ...)` seklinde yazılmalıdır.
#' @param baslangicTarihi **gg-aa-yyyy** formatinda girilmelidir.
#' @param bitisTarihi Opsiyonel.
#' **gg-aa-yyyy** formatında girilmelidir.
#' Girilmezse son tarihi dikkate alacaktır.
#' @param gozlemParametresi Opsiyonel.
#' Detay için `parametreler()` fonksiyonu çalıştırılabilir.
#' Birden fazla seri kodu istenirse gözlem parametreleri her biri için `c("", "", ...)` seklinde tanıtılmalıdır.
#' Birden fazla seri kodu girilmiş ve tek bir parametre yazılmış ise diğerleri de ayni sekilde gelecektir.
#' Girilmediği zaman sistem ilgili seri için orijinal gözlemin parametresini uygulayacaktır.
#' @param formulParametresi Opsiyonel.
#' Detay icin `parametreler()` fonksiyonu çalıştırılabilir.
#' Birden fazla seri kodu istenirse formül parametreleri her biri için `c("", "", ...)` seklinde tanıtılmalıdır.
#' Birden fazla seri kodu girilmiş ve tek bir parametre yazılmış ise diğerleri de aynı şekilde gelecektir.
#' Girilmediği zaman sistem ilgili seri için düzey parametresini uygulayacaktır.
#' @param sayisalFrekans Opsiyonel.
#' Detay için `parametreler()` fonksiyonu çalıştırılabilir.
#' Birden fazla seri kodunda tek bir ortak değer olmalıdır.
#' Girilmediği zaman sistem serilerin ortak frekansını alacaktır.
#' @examples
#' \dontrun{
#' veriler(seriKodu = "TP.DK.USD.A.YTL", baslangicTarihi = "01-01-2020")
#' veriler(seriKodu = "TP.DK.USD.A.YTL", baslangicTarihi = "01-01-2020",
#' bitisTarihi = "01-03-2021", gozlemParametresi = "avg", formulParametresi = 1,
#' sayisalFrekans = 5)
#' veriler(seriKodu = c("TP.DK.USD.A.YTL","TP.DK.USD.S.YTL"),
#' baslangicTarihi = "01-01-2020", bitisTarihi = "01-03-2021", gozlemParametresi = "avg",
#'  formulParametresi = 1, sayisalFrekans = 5)
#' veriler(seriKodu = c("TP.DK.USD.A.YTL","TP.DK.USD.S.YTL"),
#' baslangicTarihi = "01-01-2010", bitisTarihi = "01-12-2020",
#' gozlemParametresi = c("avg","max"), formulParametresi = 1, sayisalFrekans = 8)
#' }
#' @importFrom dplyr select select_if rename_with everything %>%
#' @importFrom stats na.omit
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

  veri <- fromJSON(url) %>%
    as.data.frame() %>%
    select(-totalCount,-`items.UNIXTIME`) %>%
    rename_with(~gsub("items.", "", .x), everything()) %>%
    select_if(function(x) any(sum(is.na(x)) != nrow(.))) %>%
    rename_with(~gsub("\\..*", "", .), everything())
  return(veri)

  #Oh, you've found a bug in my code?
  #Please, tell me more -_-
}

#' @export
#' @title `veriler()` için wrapper
#' @description Bir veya daha fazla veriyi indirip analize hazır hale getirir.
#' `veriler()` fonksiyonu için bir wrapper görevi görür.
#' @param series Seri/serilerin kodu.
#' Birden fazla kod girmek için `c("", "", ...)` şeklinde yazılmalıdır.
#' @param freq Opsiyonel.
#' Detay icin `parametreler()` fonksiyonu çalıştırılabilir.
#' Birden fazla seri kodunda tek bir ortak değer olmalıdır.
#' Girilmediği zaman sistem serilerin ortak frekansını alacaktır.
#' @param obs Opsiyonel.
#' Detay için `parametreler()` fonksiyonu çalıştırılabilir.
#' Birden fazla seri kodu istenirse gözlem parametreleri her biri için `c("", "", ...)` şeklinde tanıtılmalıdır.
#' Birden fazla seri kodu girilmiş ve tek bir parametre yazılmış ise diğerleri de aynı şekilde gelecektir.
#' Girilmediği zaman sistem ilgili seri için orijinal gözlemin parametresini uygulayacaktır.
#' @param formula Opsiyonel.
#' Detay için `parametreler()` fonksiyonu çalıştırılabilir.
#' Birden fazla seri kodu istenirse formül parametreleri her biri için `c("", "", ...)` seklinde tanıtılmalıdır.
#' Birden fazla seri kodu girilmiş ve tek bir parametre yazılmış ise diğerleri de ayni sekilde gelecektir.
#' Girilmediği zaman sistem ilgili seri için düzey parametresini uygulayacaktır.
#' @param start **gg-aa-yyyy** formatında girilmelidir.
#' @param end Opsiyonel.
#' **gg-aa-yyyy** formatında girilmelidir.
#' Girilmezse son tarihi dikkate alacaktır.
#' @examples
#' \dontrun{get_tcmb(seriler = c(), freq = 5, obs = "avg", formula = 0,
#' start = "1-1-2009", end = "12-5-2022")}
#' \dontrun{get_tcmb(seriler = c(), freq = 5, obs = "avg", formula = 0,
#' start = "1-1-2009", end = "last")}
#' @importFrom dplyr select mutate mutate_at vars everything  %>%
#' @importFrom lubridate yq
get_tcmb <- function(series,
                     freq = NULL,
                     obs = NULL,
                     formula = NULL,
                     start,
                     end = "01-01-3000"){
    #Get series
    df <- veriler(seriKodu = series, baslangicTarihi = start, bitisTarihi = end, gozlemParametresi = obs, formulParametresi = formula, sayisalFrekans = freq)
  # Convert Tarih to Date class
  if(freq ==8){
    dff <- df %>% mutate(Tarih = as.Date(Tarih, format = "%Y"))
  } else if(freq == 6){
    dff <- df %>% mutate(Tarih = yq(Tarih))
  } else if(freq == 5){
    dff <- df %>% mutate(Tarih = as.Date(paste(Tarih,1,sep="-"),"%Y-%m-%d"))
  } else if(freq %in% c(1,2)){
    dff <- df %>% mutate(Tarih = as.Date(Tarih,"%Y-%m-%d"))
  } else if(!(freq %in% c(1,2,5,6,8))){
    dff <- df
    warning("freq value must be one of 1,2,5,6 and 8. For other values Tarih must be converted to Date or numeric class by hand")
  }
  #Convert all other columns to numeric
  dff <- dff %>%
    mutate_at(vars(-("Tarih")),as.numeric)
  return(dff)
}

#' @export
#' @title Veri Görselleştirme
#' @description Tek bir seriyi basit bir düzeyde ve interaktif bir sekilde görselleştirmeyi sağlayan fonksiyondur.
#' Görsel üzerinde yakınlaştırma, kaydetme gibi işlemler yapılabilir.
#' @param veri Veri çerçevesi
#' @param seriKodu Görselleştirilmek istenen seri.
#' NA (kayıp) değerler kaldırılır.
#' @param sayisalFrekans Görselleştirilmek istenen serinin sayısal formatta frekansı.
#' Veri ile ayni frekansta olmalıdır.
#' @examples
#' \dontrun{gorsellestir(seriKodu = veri$TP_DK_USD_A_YTL, sayisalFrekans = 5)}
#' @importFrom dplyr %>%
#' @importFrom zoo as.Date
#' @importFrom lubridate dmy ymd
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal scale_x_continuous
#' @importFrom plotly ggplotly
gorsellestir <- function(veri, seriKodu, sayisalFrekans){

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
}
