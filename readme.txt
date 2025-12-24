This is the R project folder for the project entitled, "Telecoupled Cascading Effects of a Regional Disaster on International Land Use and Biodiversity", by Nicholas Manning, Mateus Batistella, Ramon F.B. da Silva, Kenneth A. Frank, Geraldo B. Martha Jr., David L. Ortega, Michele Remer, Joris Van Zeghbroeck, Andrés Viña, & Jianguo Liu. For further information, please see the corresponding research (submitted to Nature Sustainability 12/23/25).

##########################################################################
Please structure your folders in the following format to run the code:
Code
Data_Derived
Data_Source
Figures
##########################################################################

Initial file contents to run the code:
Code
- 00_GetShapefiles.R
- 1_Data_Import_Clean.R
- 2_Figure2_LinePlots.R
- 2_Figure3_USMW_DroughtDiff_FigureSI1_PriceDiff.R
- 2_Figure4_CerradoMuniDiffs.R
- 2_Figure5_ExportValueUSBR.R
- 3_FigureS2_MapB_Conversion.R
- 3_FigureS3_ExchangeRate.R
- 3_TableS2_Stats_MapB.R 
- x_extra_InsetMaps

Data_Derived
## Empty Folder to start

Data_Source
## br_st_abbv.csv 
-- A CSV of the abbreviations for the different states
## UNComtrade_USBR_HS1201_20072018.csv
-- A CSV of Soybean Trade data from UNComtrade, available at https://comtradeplus.un.org/
## CEPEA_Parana_BR_60kg_1997_2021_daily_raw.csv
-- A CSV of daily soybean prices from the port of Parana, available from https://www.cepea.org.br/br/indicador/soja.aspx
## SOURCE_transonly_col8_mapbiomas_municip.csv
-- A CSV of Land Transition data from MapBiomas Collection 8, uploaded here. Additional Information at: https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2023/08/ATBD-Collection-8-v1.docx.pdf
## FAOSTAT_BrUS_2000_2020_ExportQuantity.csv 
-- Soybean export quantity data from FAOSTAT, available at https://www.fao.org/faostat/en/#data/TM
## Terrabrasilis_CerradoDeforestation.csv
-- A CSV of Cerrado deforestation data from Brazil's INPE (National Institute for Space Research) - PRODES, available from https://terrabrasilis.dpi.inpe.br/app/dashboard/deforestation/biomes/cerrado/increments
## USyielddata.csv
-- USDA Yield Data per county - available from USDA NASS QuickStat: https://quickstats.nass.usda.gov/
## soymaize_2010_2022_SIDRA_PAM/soy_Brazil_2010_2022t 
-- Soybean Spatial Data from SIDRA-PAM: https://sidra.ibge.gov.br/pesquisa/pam/tabelas
## soymaize_2010_2022_SIDRA_PAMs/muni_codes_cerr.Rdata 
-- codes for municipalities in the Cerrado from 00_GetShapefiles.R.R
## ExchangeRate_1USDtoBRR_20002017.csv
-- from https://www.investing.com/currencies/usd-brl-historical-data 
## UNComtrade_USBR_Exports_20072019_sheet.xlsx, sheet = "RelevantExportData"
-- Excel workbook with source data changed into a more digestible format
## FAOSTAT_BRtoChina_ExportQuantity.csv
-- FAOSTAT data of 

Figures 
## Empty Folder to start


###########################
Required for Each Script:
###########################

00_GetShapefiles.R
-- No additional data required, just to have the file structure set up as above ^


1_Data_Import_Clean.R:
## ../Data_Source/br_st_abbv.csv 
-- A CSV of the abbreviations for the different states
## ../Data_Source/UNComtrade_USBR_HS1201_20072018.csv
-- A CSV of Soybean Trade data from UNComtrade, available at https://comtradeplus.un.org/
## ../Data_Source/CEPEA_Parana_BR_60kg_1997_2021_daily_raw.csv
-- A CSV of daily soybean prices from the port of Parana, available from https://www.cepea.org.br/br/indicador/soja.aspx
## SOURCE_transonly_col8_mapbiomas_municip.csv
-- A CSV of Land Transition data from MapBiomas Collection 8, uploaded here. Additional Information at: https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2023/08/ATBD-Collection-8-v1.docx.pdf


2_Figure2_LinePlots.R:
## ../Data_Derived/prod_price_area_yield_exports.RData 
-- from 1_Data_Import_Clean.R
## ../Data_Source/FAOSTAT_BrUS_2000_2020_ExportQuantity.csv" 
-- Soybean export quantity data from FAOSTAT, available at https://www.fao.org/faostat/en/#data/TM
## ../Data_Derived/land_trans_tosoy_df.RData 
-- cleaned MapBiomas data, from 00_GetShapefiles.R
## ../Data_Derived/land_trans_toclasses_df.RData 
-- cleaned MapBiomas data, from 00_GetShapefiles.R
## ../Data_Source/Terrabrasilis_CerradoDeforestation.csv
-- A CSV of Cerrado deforestation data from Brazil's INPE (National Institute for Space Research) - PRODES, available from https://terrabrasilis.dpi.inpe.br/app/dashboard/deforestation/biomes/cerrado/increments


2_Figure3_USMW_DroughtDiff_FigureSI1_PriceDiff.R:
## ../Data_Source/USyielddata.csv
-- USDA Yield Data per county - available from USDA NASS QuickStat: https://quickstats.nass.usda.gov/


2_Figure4_CerradoMuniDiffs.R:
## ../Data_Source/soymaize_2010_2022_SIDRA_PAM/soy_Brazil_2010_2022t 
-- Soybean Spatial Data from SIDRA-PAM: https://sidra.ibge.gov.br/pesquisa/pam/tabelas
## ../Data_Source/soymaize_2010_2022_andres/muni_codes_cerr.Rdata 
-- codes for municipalities in the Cerrado from 00_GetShapefiles.R.R


2_Figure5_ExportValueUSBR.R:
## ../Data_Source/UNComtrade_USBR_Exports_20072019_sheet.xlsx, sheet = "RelevantExportData"
## ../Data_Source/UNComtrade_USBR_HS1201_20072018.csv
## ../Data_Source/FAOSTAT_BrUS_2000_2020_ExportQuantity.csv
## ../Data_Source/FAOSTAT_BRtoChina_ExportQuantity.csv
## ../Data_Source/FAOSTAT_BrUS_2000_2020_ExportQuantity.csv


3_FigureS2_MapB_Conversion.R:
## "mapb_col8_clean_long.Rdata" from 1_Data_Import_Clean.R
## "muni_codes_cerr.Rdata" from 00_GetShapefiles.R


3_TableS2_Stats_MapB.R: 
## "mapb_col8_clean_long.Rdata" from 1_Data_Import_Clean.R
## "muni_codes_cerr.Rdata" from 00_GetShapefiles.R


3_FigureS3_ExchangeRate.R
## ../Data_Source/ExchangeRate_1USDtoBRR_20002017.csv
-- from https://www.investing.com/currencies/usd-brl-historical-data 
