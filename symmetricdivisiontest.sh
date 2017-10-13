echo 1 2
caffeinate -im ./ScEM_master 06.0 0.00 0.50 0.00 1 >> /dev/null & sleep 30;
caffeinate -im ./ScEM_master 06.0 0.50 0.50 0.20 1 >> /dev/null;
echo 3 4
caffeinate -im ./ScEM_master 06.0 0.50 1.00 0.00 1 >> /dev/null & sleep 30;
caffeinate -im ./ScEM_master 06.0 0.50 1.00 0.00 2 >> /deve/null;
echo 5 6
caffeinate -im ./ScEM_master 06.0 0.50 0.50 0.00 2 >> /dev/null & sleep 30;
caffeinate -im ./ScEM_master 06.0 0.50 0.50 0.20 2 >> /dev/null;

