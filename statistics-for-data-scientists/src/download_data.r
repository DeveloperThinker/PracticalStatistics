library(RCurl)
PSDS_PATH <- file.path('~', 'r_projects/statistics-for-data-scientists')

download_from_google_drive <- function(id, fname, path)
{
  url <- sprintf("https://drive.google.com/uc?export=download&id=%s", id)
  data <- getBinaryURL(url, followlocation = TRUE, ssl.verifypeer = FALSE)
  dest <- file.path(path, 'data', fname)
  writeBin(data, dest, useBytes = TRUE)
}

## Импортировать данные штатов
download_from_google_drive(id="0B98qpkK5EJembFc5RmVKVVJPdGc", fname='state.csv', path=PSDS_PATH)

## Данные авикомпаний
download_from_google_drive(id="0B98qpkK5EJemcmZYX2VhMHBXelE", fname='dfw_airline.csv', path=PSDS_PATH)

## Импортировать биржевые данные
download_from_google_drive('0B98qpkK5EJemV2htZWdhVFRMNlU', fname='sp500_px.csv', path=PSDS_PATH)
download_from_google_drive('0B98qpkK5EJemY0U0N1N6a21lUzA', fname='sp500_sym.csv', path=PSDS_PATH)

## Импортировать данные о налогах на недвижимость KC housing tax
download_from_google_drive('0B98qpkK5EJemck5VWkszN3F3RGM', fname='kc_tax.csv', path=PSDS_PATH)

## Импортировать данные о ссудах кредитного куба Lending club
download_from_google_drive('0B98qpkK5EJemRXpfa2lONlFRSms', fname='lc_loans.csv', path=PSDS_PATH)

## Импортировать выборку из 200 записей из данных о ссудах кредитного куба Lending club 
download_from_google_drive('0B98qpkK5EJemd0JnQUtjb051dTA', fname='loan200.csv', path=PSDS_PATH)

## Импортировать выборку из 3000 записей из данных о ссудах кредитного куба Lending club 
download_from_google_drive('0B98qpkK5EJemQXYtYmJUVkdsN1U', fname='loan3000.csv', path=PSDS_PATH)


## Импортировать полный набор записей данных о ссудах кредитного клуба Lending club 
download_from_google_drive('0B98qpkK5EJemZzdoQ2I3SWlBYzg', fname='loan_data.csv', path=PSDS_PATH)

## Импортировать данные о доходах для ссуд loans_income
download_from_google_drive('0B98qpkK5EJemRXVld0NSbWhYNVU', fname='loans_income.csv', path=PSDS_PATH)

## Импортировать данные времен сессий session_times
download_from_google_drive('0B98qpkK5EJemOC0xMHBTTEowYzg', fname='web_page_data.csv', path=PSDS_PATH)

## Импортировать данные четырех сессий four_sessions
download_from_google_drive('0B98qpkK5EJemOFdZM1JsaEF0Mnc', fname='four_sessions.csv', path=PSDS_PATH)

## Импортировать данные о проценте нажатий click_rate
download_from_google_drive('0B98qpkK5EJemVHB0ZzdtUG9SeTg', fname='click_rates.csv', path=PSDS_PATH)

## Импортировать данные imanishi
download_from_google_drive('0B98qpkK5EJemZTJnUDd5Ri1vRDA', fname='imanishi_data.csv', path=PSDS_PATH)

## Импортировать данные легочных заболеваний lung_deasease
download_from_google_drive('0B98qpkK5EJemb25YYUFJZnZVSnM', fname='LungDisease.csv', path=PSDS_PATH)

## Импортировать уровень округов ZHVI от Zillow 
download_from_google_drive('0B98qpkK5EJemWGRWOEhYN1RabVk', fname='County_Zhvi_AllHomes.csv', path=PSDS_PATH)

## Импортировать данных продажи домов в округе Кинг house_sale
download_from_google_drive('0B98qpkK5EJemVTRRN0dLakxwTmM', fname='house_sales.csv', path=PSDS_PATH)

