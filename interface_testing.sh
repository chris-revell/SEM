
for i in 0.25 1.00; do
  for j in 0.50 1.00 1.50 2.00 2.50 3.00; do
    for k in 0.01 0.05 0.10 0.15 0.20; do
      for l in 0.20 0.40 0.60 0.80 1.00 2.00; do
        for m in 1 2 3 4; do
          ./ScEM_master $i $j $k $l $m
        done
      done
    done
  done
done 
