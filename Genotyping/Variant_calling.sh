####~~~~~~~~~~~~~~~~~~~~~~~~~~~ Variant calling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# Using GATK

### It is a 3 step process, the first being "Haplotype caller"
#1) Haplotype caller

population="AK BC EC EU CO GU WC"

for pop in $population 
do 
mkdir /prj/porcini/ROH_world/Variant_Calling/${pop}
cd /prj/porcini/ROH_world/Variant_Calling/${pop}
ref=$(cat /prj/porcini/ROH_world/MAPPING/reference_paths.txt | grep ${pop} | awk -F " " '{print $2}') 
#index ref
/vol/biotools/bin/gatk CreateSequenceDictionary -R ${ref}
samtools faidx ${ref}
Basename_samples=$(cat /prj/porcini/ROH_world/MAPPING/${pop}/Indiv_list.txt)
for name in $Basename_samples
	do	

    mkdir ${name}_variantcalling
    cd ${name}_variantcalling
    cat > ${name}_HaplotypeCaller.sh << EOF
#!/bin/bash
#$ -P fair_share
#$ -cwd
#$ -l idle=1
#$ -l vf=10G
#$ -V
#$ -e ${name}_haplo.err
#$ -o ${name}_haplo.out

/vol/biotools/bin/gatk --java-options "-Xmx10g" HaplotypeCaller -ERC BP_RESOLUTION \
-R ${ref} \
-I "/prj/porcini/ROH_world/MAPPING/${pop}/${name}_mapping/${name}.sorted.duplicates.bam.gz" \
-O /prj/porcini/ROH_world/Variant_Calling/${pop}/${name}_variantcalling/${name}_gatk.vcf.gz \
--do-not-run-physical-phasing true
EOF
    qsub ${name}_HaplotypeCaller.sh
    cd /prj/porcini/ROH_world/Variant_Calling/${pop}
done


#############################################################################################

# 2) The second step is called DB import, and is done with the following script: (one per lineage, here I present the one for AK)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#!/bin/bash
#$ -P fair_share
#$ -cwd
#$ -l idle=1
#$ -l vf=5G
#$ -V
#$ -e DB_AK.err
#$ -o DB_AK.out

# ImportDB 

/vol/biotools/bin/gatk --java-options "-Xmx4g -Xms4g" GenomicsDBImport \
-V /prj/porcini/ROH_world/Variant_Calling/AK/15-08-09_variantcalling/15-08-09_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/15-08-10_variantcalling/15-08-10_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/15-08-12_variantcalling/15-08-12_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/552NVP_variantcalling/552NVP_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/554NVP_variantcalling/554NVP_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/KZOM-213_variantcalling/KZOM-213_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/KZOM-214_variantcalling/KZOM-214_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/N2_variantcalling/N2_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/N8_variantcalling/N8_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/N9_variantcalling/N9_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/WTU-21375_variantcalling/WTU-21375_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/WTU-24032_variantcalling/WTU-24032_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/WTU-24390_variantcalling/WTU-24390_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/WTU-24718_variantcalling/WTU-24718_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/WTU-24735_variantcalling/WTU-24735_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/WTU-27121_variantcalling/WTU-27121_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/WTU-27134_variantcalling/WTU-27134_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/WTU-30041_variantcalling/WTU-30041_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/WTU-32261_variantcalling/WTU-32261_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/WTU-68299_variantcalling/WTU-68299_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/YSU-05349_variantcalling/YSU-05349_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/YSU-05404_variantcalling/YSU-05404_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/YSU-07119_variantcalling/YSU-07119_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/YSU-08902_variantcalling/YSU-08902_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/YSU-09148_variantcalling/YSU-09148_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/YSU-09856_variantcalling/YSU-09856_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/YSU-09901_variantcalling/YSU-09901_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/YSU-09911_variantcalling/YSU-09911_gatk.vcf.gz \
-V /prj/porcini/ROH_world/Variant_Calling/AK/YSU-09929_variantcalling/YSU-09929_gatk.vcf.gz \
--genomicsdb-workspace-path /prj/porcini/ROH_world/Variant_Calling/AK/DB \
      -L /prj/porcini/ROH_world/RAW_DATA/Boletus_edulis_reference_genomes_23march23/YSU-09856/Scaffold.list
      --tmp-dir /prj/porcini/ROH_world/Variant_Calling/AK

#############################################################################################################

# 3) the last step is the GenotypeGVCF, here is the script for the AK lineage
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#!/bin/bash
#$ -P fair_share
#$ -cwd
#$ -l idle=1
#$ -l vf=5G
#$ -V
#$ -e GVCF.err
#$ -o GVCF.out

 
 
/vol/biotools/bin/gatk --java-options "-Xmx4g" GenotypeGVCFs \
   -R /prj/porcini/ROH_world/RAW_DATA/Boletus_edulis_reference_genomes_23march23/YSU-09856/YSU-09856.scaffolds.fa \
   -V gendb:///prj/porcini/ROH_world/Variant_Calling/AK/DB \
   -O AK_all_site.vcf.gz \
   --include-non-variant-sites true ### Using all site vcf here, for more acurate FROh calling. Maybe not necessary, but better for the most concervative approach possible


##############################################################################################################














