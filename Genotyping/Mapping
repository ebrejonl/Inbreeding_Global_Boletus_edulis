#####~~~~~~~~~~~~~~~~~~~~~~~~ Mapping ~~~~~~~~~~~~~~~~~~~~~~~~~~~######
# The end goal is to create one vcf per lineage, where each lineage is mapped on its specific reference genome
# For more information about the lineages and the reference assemblies, check Tremble et al, (2023) New Phytologist
# The raw data is available at the BioProject accession #PRJNA1010140

# This script is greedy in ressources and produces huge files so it is best run on a computing cluster
population="AK BC EC EU CO GU WC"
for pop in $population 
do 
# Get the reference path with 
    cd /prj/porcini/ROH_world/MAPPING/${pop}
    ref=$(cat /prj/porcini/ROH_world/MAPPING/ref_bwa1.txt | grep ${pop} | awk -F " " '{print $2}') 
    Basename_samples=$(cat Indiv_list.txt)
	for name in $Basename_samples
		do	
    		DIR_samples="/prj/porcini/ROH_world/RAW_DATA/Bedulis_publicationQuality_fastq_23march23/"
    		fastq_R1=${DIR_samples}${name}.1.trimmed.fastq.gz
    		fastq_R2=${DIR_samples}${name}.2.trimmed.fastq.gz
   			mkdir ${name}_mapping
   			cd ${name}_mapping
  			cat > ${name}_mapping.sh << EOF
#!/bin/bash
#$ -P fair_share
#$ -cwd
#$ -l idle=1
#$ -l vf=8G
#$ -V
#$ -e ${name}_map.err
#$ -o ${name}_map.out
    
/vol/biotools/bin/bwa mem -t 10 -M -R "@RG\tID:${name}_1\tSM:${name}" ${ref} ${fastq_R1} ${fastq_R2} | samtools view -bS -1 -h > ${name}.bam.gz    
echo "mapping done"

# SORTING STEP
samtools sort -l 6 -o ${name}.sorted.bam.gz -O bam -@ 20 ${name}.bam.gz
echo "sorting done"

# MARK DUPLICATES
/prj/porcini/miniconda/envs/MAPPING/bin/picard MarkDuplicates \
-I ${name}.sorted.bam.gz \
-M metrics_duplicates.txt \
-O ${name}.sorted.duplicates.bam.gz \
-COMPRESSION_LEVEL 5
echo "mark duplicates done"

# index
samtools index ${name}.sorted.duplicates.bam.gz
echo "index done"
EOF
    qsub ${name}_mapping.sh
    echo "done for ${name}"
    cd  /prj/porcini/ROH_world/MAPPING/${pop}
    done
echo "done for ${pop}"
done
