## This identify all ROHs without any length streshold, so the results still need further processing ## 



#!/bin/bash
#$ -P fair_share
#$ -cwd
#$ -l idle=1
#$ -l vf=8G
#$ -V
#$ -e All_site.err
#$ -o All_site.out

# go to each pop folder
population="AK BC CO EC EU GU WC"
for pop in $population
do
mkdir ${pop}
cd ${pop}/
# make vcf into bcftools format 
bcftools view ../../${pop}_all_sites_filtered_vcf.gz > ${pop}_all_site.vcf

bgzip ${pop}_all_site.vcf
tabix ${pop}_all_site.vcf.gz
rm ${pop}_all_site.vcf

# create the target file
bcftools query -f'%CHROM\t%POS\t%REF,%ALT\n' ${pop}_all_site.vcf.gz | bgzip -c > als.tsv.gz && tabix -s1 -b2 -e2 als.tsv.gz
# run roh
indiv=$(cat /prj/porcini/ROH_world/Bcftools/NO_CLONE/${pop}/Indiv_list.txt)
bcftools roh -T als.tsv.gz \
      -s $indiv \
      -e- \
      -G30 \
      -o results ${pop}_all_site.vcf.gz

# filter the results with RG
cat results | grep "RG" > ${pop}_Results.csv

cd ../
done 
