#!/bin/bash
##
## process_runner.sh <concurrent> <total procs>
##
## Example script given the maximum number of processes to
## run concurrently, c, and the total number of processes, n
## runs at most c, processes in the background until all n
## Have been run.
##
## Author Donald 'Paddy' McCarthy Dec. 17 2007
##
# how many processes to run in parallel
concurrent=$1
# total processes to run
maxprocs=$2

printf "\n## STARTING %i Processes, %i at a time\n\n" \
  $maxprocs $concurrent


# main loop wait time between checking background procs.
tick=1


# dummy processes sleep for a random time
function runproc {
  local -i n=$1
  local -i j
  (( j = 5+$RANDOM*10/32767 ))
  #(date; printf "#>>>JOB %i : Sleeping for %i\n" $n $j)
  printf "OUT JOB ,%03i, Sleep for ,%2i,  , @,%s\n" $n $j "`date`"
  sleep $j
  returned=$?
  printf "IN  JOB ,%03i, Slept for ,%2i, returned ,%i, @,%s\n" \
      $n $j $returned "`date`"
  #(date; printf "#<<<JOB %i : Slept for %i\n" $n $j)
}

function print_runstats {
  printf '# %i Jobs in background. %i/%i started\n\n'  \
    `jobs -r|wc -l` $ran $maxprocs
}

# Bash array running keeps track of the background process numbers
# Start with nothing running (sentinel value will not be a process number
for ((i=0; i<$concurrent; i+=1 )); do running[$i]=123456789; done

ran=0
until
while [ $ran -lt $maxprocs ]; do
    for ((p=0; p<$concurrent; p+=1 )); do
	proc=${running[$p]}
	# Over all running processes...
	# is $proc still running?
	ps -p $proc | fgrep $proc > /dev/null
	if [ $? -ne '0' ] ; then
        # Not found  i.e. finished
        # Run another in background and store the proc number
            runproc $ran &
            running[$p]=$!
            (( ran += 1 ))
            if [ $ran -ge $maxprocs ]; then break 1; fi
	fi
    done
    sleep $tick
    # Status
    print_runstats
done

sleep $tick
  # Status
print_runstats
do [  `jobs -r | wc -l` -eq 0 ]
done

wait
printf "\n## FINISHED\n"

exit 0

sample_output=<<!
