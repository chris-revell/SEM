echo 1 2
caffeinate -im ./ScEM_master 06.0 0.50 0.75 0.00 1 >> /dev/null & sleep 30;
caffeinate -im ./ScEM_master 06.0 0.50 0.75 0.20 1 >> /dev/null;
wait
echo 3 4
caffeinate -im ./ScEM_master 06.0 0.50 1.00 0.00 1 >> /dev/null & sleep 30;
caffeinate -im ./ScEM_master 06.0 0.50 1.00 0.20 1 >> /dev/null;
wait
echo 5 6
caffeinate -im ./ScEM_master 06.0 0.50 0.75 0.00 2 >> /dev/null & sleep 30;
caffeinate -im ./ScEM_master 06.0 0.50 0.75 0.20 2 >> /dev/null;
wait
echo 3 4
caffeinate -im ./ScEM_master 06.0 0.50 1.00 0.00 2 >> /dev/null & sleep 30;
caffeinate -im ./ScEM_master 06.0 0.50 1.00 0.20 2 >> /dev/null;
wait
echo 5 6
caffeinate -im ./ScEM_master 06.0 0.50 0.75 0.00 3 >> /dev/null & sleep 30;
caffeinate -im ./ScEM_master 06.0 0.50 0.75 0.20 3 >> /dev/null;
wait
echo 3 4
caffeinate -im ./ScEM_master 06.0 0.50 1.00 0.00 3 >> /dev/null & sleep 30;
caffeinate -im ./ScEM_master 06.0 0.50 1.00 0.20 3 >> /dev/null;
wait
echo 5 6
caffeinate -im ./ScEM_master 06.0 0.50 0.75 0.00 4 >> /dev/null & sleep 30;
caffeinate -im ./ScEM_master 06.0 0.50 0.75 0.20 4 >> /dev/null;
wait
echo 3 4
caffeinate -im ./ScEM_master 06.0 0.50 1.00 0.00 4 >> /dev/null & sleep 30;
caffeinate -im ./ScEM_master 06.0 0.50 1.00 0.20 4 >> /dev/null;
wait
