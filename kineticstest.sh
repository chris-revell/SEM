./ScEM_master 08.0 0.70 0.75 0.00 1 >> /dev/null & echo 075 000 1; sleep 10;
./ScEM_master 08.0 0.70 0.75 0.30 1 >> /dev/null & echo 075 000 2; sleep 10;
./ScEM_master 08.0 0.70 0.50 0.00 1 >> /dev/null & echo 075 010 1; sleep 10;
./ScEM_master 08.0 0.70 0.50 0.30 1 >> /dev/null & echo 075 010 1; sleep 10;
wait
./ScEM_master 08.0 0.70 0.75 0.00 2 >> /dev/null & echo 075 000 1; sleep 10;
./ScEM_master 08.0 0.70 0.75 0.30 2 >> /dev/null & echo 075 000 2; sleep 10;
./ScEM_master 08.0 0.70 0.50 0.00 2 >> /dev/null & echo 075 010 1; sleep 10;
./ScEM_master 08.0 0.70 0.50 0.30 2 >> /dev/null & echo 075 010 1; sleep 10;
wait
./ScEM_master 08.0 0.70 1.00 0.00 1 >> /dev/null & echo 075 000 1; sleep 10;
./ScEM_master 08.0 0.70 1.00 0.30 1 >> /dev/null & echo 075 000 2; sleep 10;
./ScEM_master 08.0 0.70 1.00 0.00 2 >> /dev/null & echo 075 010 1; sleep 10;
./ScEM_master 08.0 0.70 1.00 0.30 2 >> /dev/null & echo 075 010 1; sleep 10;
wait
