datapath=~/Desktop/DarwinData/adhesion-tension_phasespace
for i in $(ls $datapath/); do
	for j in $(ls $datapath/$i); do
	./scripts/visualise_povray_script.sh $datapath/$i/$j/povray_data
	./scripts/binning.py $datapath/$i/$j
	done
	./scripts/multi_plot.py $datapath/$i
	./scripts/anovan.py $datapath/$i
done
