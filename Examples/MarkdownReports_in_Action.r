######################################################################################################
## MarkdownReports in Action
######################################################################################################
# source("/Users/abelvertesy/MarkdownReports.wiki/Reports/MarkdownReports_in_Action.r")



# Functions ------------------------
try (source ('/Users/abelvertesy/TheCorvinas/R/CodeAndRoll.R'),silent= F)
# library(stringr)

# Setup ------------------------
setup_MarkdownReports(OutDir = "/Users/abelvertesy/MarkdownReports/Examples/MarkdownReports_in_Action/", fname = "MarkdownReports_in_Action.r")
create_set_OutDir("/Users/abelvertesy/Google_Drive/X_react_Data/Abelz/X_react_2016")

OutDirOrig = OutDir

# Your data ------------------------

# Parameters ------------------------

cell_IDs = c( 'd1_01_gf', 'd1_02_gf', 'd1_03_gf', 'd1_04_gf', 'd1_05_gf', 'd1_06_gf', 'd1_07_af', 'd1_08_af', 'd1_27_gf', 'd1_28_gf', 'd1_29_gf', 'd1_31_af', 'd2_12_gf', 'd2_13_gf', 'd2_14_gf', 'd2_15_gf', 'd2_16_gf', 'd2_17_gf', 'd2_18_af', 'd2_19_af', 'd2_32_gf', 'd2_33_gf', 'd2_34_gf', 'd2_35_gf', 'd2_36_af', 'd2_37_af', 'd2_52_gf', 'd2_53_gf', 'd2_54_gf', 'd2_55_gf', 'd2_56_gf', 'd2_57_gf', 'd2_58_gf', 'd3_20_gm', 'd3_21_gm', 'd3_22_gm', 'd3_23_gm', 'd3_24_gm', 'd3_25_gm', 'd3_26_am', 'd3_27_am', 'd3_38_gm', 'd3_39_gm', 'd3_40_gm', 'd3_59_am', 'd3_60_am', 'd3_61_am', 'd3_62_am', 'd4_01_gf', 'd4_02_gf', 'd4_03_gf', 'd4_04_gf', 'd4_05_gf', 'd4_06_gf', 'd4_07_gf', 'd4_08_gf', 'd4_09_gf', 'd4_10_gf', 'd4_11_af', 'd4_12_af', 'd4_13_af', 'd4_31_gf', 'd4_32_gf', 'd4_33_gf', 'd4_34_gf', 'd4_35_gf', 'd4_36_gf', 'd4_37_gf', 'd4_38_gf', 'd4_39_af', 'd4_40_af', 'd4_41_af', 'd4_42_af', 'd4_43_af', 'd4_44_af', 'd4_45_af', 'd4_46_af', 'd4_47_af', 'd4_48_af', 'd4_49_af', 'd4_50_af', 'd5_14_gf', 'd5_15_gf', 'd5_16_gf', 'd5_17_gf', 'd5_18_gf', 'd5_19_gf', 'd5_20_gf', 'd5_21_gf', 'd5_22_gf', 'd5_23_gf', 'd5_24_af', 'd5_25_af', 'd5_26_af', 'd5_63_gf', 'd5_64_gf', 'd5_65_gf', 'd5_66_gf', 'd5_67_gf', 'd5_68_gf', 'd5_69_gf', 'd5_70_gf', 'd5_71_gf', 'd5_72_gf', 'd5_73_af', 'd5_74_af', 'd5_75_af', 'd5_76_af')
NR_of_SNP_Containing_Genes_log2 = c( 11.3, 11.2, 11.2, 11.1, 11, 11, 10.9, 10.8, 10.8, 10.8, 10.6, 10.5, 10.5, 10.5, 10.4, 10.4, 10.4, 10.4, 10.3, 10.3, 10.3, 10.3, 10.3, 10.3, 10.2, 10.2, 10.2, 10.2, 10.1, 10.1, 10.1, 10.1, 10.1, 10, 10, 10, 9.99, 9.98, 9.98, 9.97, 9.96, 9.91, 9.91, 9.89, 9.87, 9.86, 9.85, 9.85, 9.8, 9.78, 9.78, 9.76, 9.76, 9.76, 9.76, 9.75, 9.71, 9.67, 9.67, 9.63, 9.61, 9.58, 9.57, 9.53, 9.5, 9.49, 9.45, 9.45, 9.43, 9.38, 9.37, 9.36, 9.36, 9.34, 9.28, 9.28, 9.27, 9.26, 9.2, 9.18, 9.16, 9.13, 9.04, 8.96, 8.77, 8.66, 8.3, 7.83, 7.69, 7.11, 6.92, 6.63, 6.36, 6.19, 5.73, 5.36, 5.21, 5.21, 5.09, 5.04, 4.81, 4, 3.7, 3.58, 3.58, 3, 2.81, 1 )
NR_of_reads_per_cell_log2 = c( 17.8, 16.9, 16.7, 16.6, 16.6, 16.4, 16.4, 16.3, 16.3, 16.3, 16.1, 16.1, 16, 15.8, 15.8, 15.8, 15.7, 15.7, 15.6, 15.6, 15.5, 15.5, 15.4, 15.4, 15.3, 15.3, 15.2, 15.2, 15.2, 15.2, 15.2, 15.1, 15.1, 15, 15, 15, 14.9, 14.9, 14.9, 14.8, 14.8, 14.7, 14.7, 14.7, 14.6, 14.6, 14.6, 14.5, 14.4, 14.3, 14.3, 14.3, 14.2, 14.1, 14.1, 14, 14, 13.9, 13.8, 13.8, 13.7, 13.7, 13.7, 13.6, 13.6, 13.6, 13.6, 13.5, 13.5, 13.5, 13.5, 13.4, 13.4, 13.4, 13.4, 13.3, 13.3, 13.2, 13.2, 13.2, 13.2, 13.1, 12.8, 12.8, 12.7, 12.2, 12.1, 12.1, 11.8, 11.8, 11.3, 11.2, 10.5, 10.3, 9.81, 9.8, 9.72, 8.91, 8.89, 8.39, 8.38, 8.33, 8.04, 8.01, 7.64, 6.92, 6.74, 5.32 )

NR_of_reads_per_cell_log2

log2(minSNP)
log2(minReadCount)

bicolore = 2+(NR_of_reads_per_cell_log2 > log2(minReadCount) | NR_of_SNP_Containing_Genes_log2 > log2(minSNP))



Single_Cell_RNAseq_libraries = cbind (
	"NR_of_SNP_Containing_Genes_log2" = NR_of_SNP_Containing_Genes_log2,
	"NR_of_reads_per_cell_log2" = NR_of_reads_per_cell_log2)

wplot(SingleCells, col = bicolore)

inline_vec.num(iround(NR_of_SNP_Containing_Genes_log2))

wbarplot (NR_of_SNP_Containing_Genes_log2, sub = "We exclude samples having less than 200 SNPs", hline = log2(minSNP), mdlink = T)


