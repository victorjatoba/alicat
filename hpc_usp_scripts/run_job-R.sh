#!/bin/bash -v
#PBS -N BuildData200m
#PBS -l nodes=1:ppn=10
##PBS -o stdout_R01
##PBS -e stderr_R01
#PBS -l walltime=2000:00:00
#PBS -l cput=40000:00:00
#PBS -q verylong

date

### cd to directory where the job was submitted:
cd $PBS_O_WORKDIR

### determine the number of allocated processors:
NPROCS=`wc -l < $PBS_NODEFILE`

echo "----------------"
echo "PBS job running on: `hostname`"
echo "in directory:       `pwd`"
echo "nodes: $NPROCS"
echo "nodefile:"
cat $PBS_NODEFILE
echo "----------------"
#MPIRUNSL54L=`which mpirun`
#echo "usando o mpirun $MPIRUNSL54L"

ulimit -s unlimited
module load R/3.0.2-intel 

#Roda o calculo R

time R --no-restore --slave --file=./../experiment/storing-enem-cat-results.R 1> ./../data/200m-2012-enem-responses.txt 2> ./../data/200m-error.txt

date
