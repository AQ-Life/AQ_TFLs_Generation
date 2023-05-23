

require(rsconnect)

rsconnect::setAccountInfo(name='aqlife', 
                          token='BAF86A3C3389979468BFF085DCB4FEF5', 
                          secret='iJShShPnv7YaEiPCSJ3IDmcbA/CnDjWHUUSjq52x')

options(encoding = "UTF-8")
rsconnect::deployApp('D:/360Downloads/R/project/TFLs_Generation/AQ_TFLs_Generation')

