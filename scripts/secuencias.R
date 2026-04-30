library(Biostrings)
globinas<-readAAStringSet("Datos_crudos/globinas.fasta")
VIH<-readDNAStringSet("Datos_crudos/NC_001802_HIV-1.fasta")
View(VIH)
rev(VIH) #Para observar la secuencia
reverse(VIH) #Se usa para colocar el reverso de la secuencia
reverseComplement(VIH) #Te saca el reverso complementario de la secuencia
translate(VIH) #Traduce de ADN o ARN a proteína
alphabetFrequency(VIH) #Calcula la frecuencia de todos los nucleotidos
letterFrequency(VIH,c("T","A")) #Calcula la frecuencia de nucleotidos especificos
dinucleotideFrequency(VIH)# Calcula la frecuencia de las combinaciones de dos nucleotidos
letterFrequencyInSlidingView(VIH,"T")
consensusMatrix(VIH)#Crea una matriz de consenso 
#Ejercicio 1- Creacion y manipulación de secuencias
#Crea un objeto DNAString con la secuencia “AGTCGTAGC”.
ejer1_sec<-DNAString("AGTCGTAGC")
#Encuentra el complemento inverso de la secuencia.
reverseComplement(ejer1_sec)
#Cuenta las ocurrencias del nucleótido “A”.
letterFrequency(ejer1_sec,"A")
#Extrae la subsecuencia de la posición 3 a la 7.
subseq(ejer1_sec,start = 3,end = 7)
#Ejercicio 2 - Coincidencia de patrones
#Crea un objeto DNAString con la secuencia “AGTCAGCTAG”.
ejer2_sec<-DNAString("AGTCAGCTAG")
#Encuentra todas las coincidencias exactas del patrón “AGC”.
matchPattern("AGC",ejer2_sec)
#Realiza coincidencias aproximadas del patrón “AGC” permitiendo 1 desajuste
matchPattern("AGC",ejer2_sec,max.mismatch = 1)
#Ejercicio 3 - Alineación de secuencias
#Realiza una alineación global entre las secuencias “ACGT” y “AGCT”.
alig1<-pwaling::pairwiseAlignment(DNAString("ACGT"),DNAString("AGCT"),type="global")
#Realiza una alineación local entre las secuencias “ACGT” y “CG”.
alig2<-pairwiseAlignment(DNAString("ACGT"),DNAString("CG"), type="local")
#Escribe el resultado de la alineación en un archivo.
