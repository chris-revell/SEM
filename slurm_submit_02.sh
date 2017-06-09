#!/bin/bash
#!
#! Example SLURM job script for Darwin (Sandy Bridge, ConnectX3)
#! Last updated: Sat Apr 18 13:05:53 BST 2015
#!

#!#############################################################
#!#### Modify the options in this section as appropriate ######
#!#############################################################

#! sbatch directives begin here ###############################
#! Name of the job:
#SBATCH -J interface_test
#! Which project should be charged:
#SBATCH -A CHALUT-SL3
#! How many whole nodes should be allocated?
#SBATCH --nodes=1
#! How many (MPI) tasks will there be in total? (<= nodes*16)
#SBATCH --ntasks=16
#! How much wallclock time will be required?
#SBATCH --time=09:00:00
#! What types of email messages do you wish to receive?
#SBATCH --mail-type=ALL
#! Uncomment this to prevent the job from being requeued (e.g. if
#! interrupted by node failure or system downtime):
#SBATCH --no-requeue

#! Do not change:
#SBATCH -p sandybridge

#! Number of nodes and tasks per node allocated by SLURM (do not change):
#numnodes=$SLURM_JOB_NUM_NODES
#numtasks=$SLURM_NTASKS
#mpi_tasks_per_node=$(echo "$SLURM_TASKS_PER_NODE" | sed -e  's/^\([0-9][0-9]*\).*$/\1/')
#! ############################################################
#! Modify the settings below to specify the application's environment, location
#! and launch method:

#! Optionally modify the environment seen by the application
#! (note that SLURM reproduces the environment at submission irrespective of ~/.bashrc):
. /etc/profile.d/modules.sh                # Leave this line (enables the module command)
module purge                               # Removes all modules still loaded
module load default-impi                   # REQUIRED - loads the basic environment

#! Insert additional module load commands after this line if needed:

#! Full path to application executable:
application="/home/cr395/interface_test/SEM/ScEM_master"

#! Run options for the application:
#options=""

#! Work directory (i.e. where the job will run):
workdir="/home/cr395/interface_test/SEM"  # The value of SLURM_SUBMIT_DIR sets workdir to the directory
                             # in which sbatch is run.

#! Are you using OpenMP (NB this is unrelated to OpenMPI)? If so increase this
#! safe value to no more than 16:
#export OMP_NUM_THREADS=1

#! Number of MPI tasks to be started by the application per node and in total (do not change):
#np=$[${numnodes}*${mpi_tasks_per_node}]

#! The following variables define a sensible pinning strategy for Intel MPI tasks -
#! this should be suitable for both pure MPI and hybrid MPI/OpenMP jobs:
#export I_MPI_PIN_DOMAIN=omp:compact # Domains are $OMP_NUM_THREADS cores in size
#export I_MPI_PIN_ORDER=scatter # Adjacent domains have minimal sharing of caches/sockets
#! Notes:
#! 1. These variables influence Intel MPI only.
#! 2. Domains are non-overlapping sets of cores which map 1-1 to MPI tasks.
#! 3. I_MPI_PIN_PROCESSOR_LIST is ignored if I_MPI_PIN_DOMAIN is set.
#! 4. If MPI tasks perform better when sharing caches/sockets, try I_MPI_PIN_ORDER=compact.


#! Uncomment one choice for CMD below (add mpirun/mpiexec options if necessary):

#! Choose this for a MPI code (possibly using OpenMP) using Intel MPI.
#CMD="mpirun -ppn $mpi_tasks_per_node -np $np $application $options"

#! Choose this for a pure shared-memory OpenMP parallel program on a single node:
#! (OMP_NUM_THREADS threads will be created):
#CMD="$application $options"

#! Choose this for a MPI code (possibly using OpenMP) using OpenMPI:
#CMD="mpirun -npernode $mpi_tasks_per_node -np $np $application $options"


###############################################################
### You should not have to change anything below this line ####
###############################################################

cd $workdir
echo -e "Changed directory to `pwd`.\n"

JOBID=$SLURM_JOB_ID

echo -e "JobID: $JOBID\n======"
echo "Time: `date`"
echo "Running on master node: `hostname`"
echo "Current directory: `pwd`"

if [ "$SLURM_JOB_NODELIST" ]; then
        #! Create a machine file:
        export NODEFILE=`generate_pbs_nodefile`
        cat $NODEFILE | uniq > machine.file.$JOBID
        echo -e "\nNodes allocated:\n================"
        echo `cat machine.file.$JOBID | sed -e 's/\..*$//g'`
fi

#echo -e "\nnumtasks=$numtasks, numnodes=$numnodes, mpi_tasks_per_node=$mpi_tasks_per_node (OMP_NUM_THREADS=$OMP_NUM_THREADS)"

#echo -e "\nExecuting command:\n==================\n$CMD\n"

#eval $CMD

for i in 1.00; do
  for j in 0.50 1.00 1.50 2.00 2.50 3.00; do
    for k in 0.01 0.05 0.10 0.15 0.20; do
      for l in 0.10 0.20 0.30 0.40; do
        for m in 1 2 3 4; do
          ./ScEM_master $i $j $k $l $m
        done
      done
      wait
      for l in 0.50 0.60 0.70 0.80; do
        for m in 1 2 3 4; do
          ./ScEM_master $i $j $k $l $m
        done
      done
      wait
      for l in 0.90 1.00 2.00; do
        for m in 1 2 3 4; do
          ./ScEM_master $i $j $k $l $m
        done
      done
      wait
    done
  done
done
wait
