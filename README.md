
TCMB
====

TCMB paketi EVDS’den verileri cekmenizi, bu verileri farkli format
secenekleri ile kaydetmenizi ve tek seri uzerinden interaktif bir
sekilde gorsellestirmenizi saglar.

Paket Yukleme
-------------

Paketi asagidaki kod ile yukleyebilirsiniz.

``` r
devtools::install_github("rpydaneogrendim/TCMB")
library(TCMB)
```

Fonksiyonlar ve Ornek
---------------------

``` r
#Yardim alin:
yardim()

#API anahtarinizi tanitin:
anahtar()

#Parametreleri gorun:
parametreler()

#Kategorileri gorun:
kategoriler()

#Veri Gruplarini gorun:
verigruplari()

#Serileri gorun:
seriler()

#Verileri cekin:
veriler()

#Verileri kaydedin:
aktar()

#Veriyi gorsellestirin:
gorsellestir()
```

ABD Dolar Kuru (Alis) verilerinin 2021 icin aylik bazda ortalama
degerlerini gosteren kodlar:

``` r
#Anahtari tanitin:
anahtar(api_anahtar = "api_anahtar")

#Kategorisini alin:
kategoriler() #2 -> KURLAR

#Veri Gruplarini alin:
verigruplari(kategoriKodu = 2)

#Serileri alin:
seriler(verigrupKodu = "bie_dkdovytl") #bie_dkdovytl -> Kurlar-Döviz Kurları

#Verileri alin:
veriler(seriKodu = "TP.DK.USD.A.YTL", baslangicTarihi = "01-01-2021", bitisTarihi = "15-03-2021", gozlemParametresi = "avg", formulParametresi = 0, sayisalFrekans = 5)

#     Tarih    TP_DK_USD_A_YTL
# 1 2021-1           7.393975
# 2 2021-2 7.0723650000000005
# 3 2021-3  7.466490909090909
```
