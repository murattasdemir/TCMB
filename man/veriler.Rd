% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{veriler}
\alias{veriler}
\title{Verileri Çekme}
\usage{
veriler(
  seriKodu,
  baslangicTarihi,
  bitisTarihi = "01-01-3000",
  gozlemParametresi = NULL,
  formulParametresi = NULL,
  sayisalFrekans = NULL
)
}
\arguments{
\item{seriKodu}{Veri/verilerin seri kodu.
Birden fazla kod girmek için \code{c("", "", ...)} seklinde yazılmalıdır.}

\item{baslangicTarihi}{\strong{gg-aa-yyyy} formatinda girilmelidir.}

\item{bitisTarihi}{Opsiyonel.
\strong{gg-aa-yyyy} formatında girilmelidir.
Girilmezse son tarihi dikkate alacaktır.}

\item{gozlemParametresi}{Opsiyonel.
Detay için \code{parametreler()} fonksiyonu çalıştırılabilir.
Birden fazla seri kodu istenirse gözlem parametreleri her biri için \code{c("", "", ...)} seklinde tanıtılmalıdır.
Birden fazla seri kodu girilmiş ve tek bir parametre yazılmış ise diğerleri de ayni sekilde gelecektir.
Girilmediği zaman sistem ilgili seri için orijinal gözlemin parametresini uygulayacaktır.}

\item{formulParametresi}{Opsiyonel.
Detay icin \code{parametreler()} fonksiyonu çalıştırılabilir.
Birden fazla seri kodu istenirse formül parametreleri her biri için \code{c("", "", ...)} seklinde tanıtılmalıdır.
Birden fazla seri kodu girilmiş ve tek bir parametre yazılmış ise diğerleri de aynı şekilde gelecektir.
Girilmediği zaman sistem ilgili seri için düzey parametresini uygulayacaktır.}

\item{sayisalFrekans}{Opsiyonel.
Detay için \code{parametreler()} fonksiyonu çalıştırılabilir.
Birden fazla seri kodunda tek bir ortak değer olmalıdır.
Girilmediği zaman sistem serilerin ortak frekansını alacaktır.}
}
\description{
İstenen verinin/verilerin seri kodunun/kodlarının tanımlanacağı fonksiyondur.
}
\examples{
\dontrun{
veriler(seriKodu = "TP.DK.USD.A.YTL", baslangicTarihi = "01-01-2020")
veriler(seriKodu = "TP.DK.USD.A.YTL", baslangicTarihi = "01-01-2020",
bitisTarihi = "01-03-2021", gozlemParametresi = "avg", formulParametresi = 1,
sayisalFrekans = 5)
veriler(seriKodu = c("TP.DK.USD.A.YTL","TP.DK.USD.S.YTL"),
baslangicTarihi = "01-01-2020", bitisTarihi = "01-03-2021", gozlemParametresi = "avg",
 formulParametresi = 1, sayisalFrekans = 5)
veriler(seriKodu = c("TP.DK.USD.A.YTL","TP.DK.USD.S.YTL"),
baslangicTarihi = "01-01-2010", bitisTarihi = "01-12-2020",
gozlemParametresi = c("avg","max"), formulParametresi = 1, sayisalFrekans = 8)
}
}
