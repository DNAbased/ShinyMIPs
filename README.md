# ShinyMIPs
A shiny user interface for the MIPgen pipeline by Evan Boyle / shendurelab (https://github.com/shendurelab/MIPGEN)

Requirements:
- the helpers.R script from Dean Attali's Advanced Shiny repository in your working directory (https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R)
- MIPgen's extract_coding_gene_exons.sh in the scripts/ folder (https://github.com/shendurelab/MIPGEN/blob/master/tools/extract_coding_gene_exons.sh)
- a refgene file in the refs/ folder (e.g. http://hgdownload.cse.ucsc.edu/goldenpath/hg19/database/refGene.txt.gz)
- MIPgen's extract_gene_exons_plus_utrs.sh in the scripts/ folder (https://github.com/shendurelab/MIPGEN/blob/master/tools/extract_gene_exons_plus_utrs.sh)
- the MIPgen script in the scripts/ folder (https://github.com/shendurelab/MIPGEN)
- a reference genome (FASTA format) in the refs/ folder (e.g. ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/technical/reference/phase2_reference_assembly_sequence/hs37d5.fa.gz)
- a SNP file in the refs/ folder (e.g. ftp://ftp.ncbi.nlm.nih.gov/snp/organisms/human_9606/VCF/common_all_20180418.vcf.gz)
- MIPgen's generate_ucsc_track.py in the scripts/folder (https://github.com/shendurelab/MIPGEN/blob/master/tools/generate_ucsc_track.py) with respective modifications to allow more color choices

Optional requirements (only for the additional tools):
- twoBitToFa script in the scripts/ folder (http://hannonlab.cshl.edu/fastx_toolkit/)
- 2bit reference genome (e.g.https://hgdownload-test.gi.ucsc.edu/goldenPath/hg19/bigZips/hg19.2bit)
- fasta_formatter (http://hannonlab.cshl.edu/fastx_toolkit/)

For further information on reference files etc. see MIPgen.
