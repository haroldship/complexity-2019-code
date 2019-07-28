#!/bin/bash

SNR="10 5"
NS="0 1 2 3"
S=(GMAbook LV FN SIR)
SS=(100 100 20 18)
LS=(200 200 40 36)

for ns in $NS; do
  s=${S[ns]}
  ss=${SS[ns]}
  ls=${LS[ls]}
  for snr in $SNR; do
    ds="${s}_n${ss}SNR${snr}"
    cd $ds
    if [ -f compLoss-mse-final.R ]; then
      r=$(Rscript --vanilla compLoss-mse-final.R | grep Non)
    elif [ -f compLoss-final.R ]; then
      r=$(Rscript --vanilla compLoss-final.R | grep Non)
    else
      r="shit"
    fi
    echo "$ds $r"
    cd ..
  done
done
