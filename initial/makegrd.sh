#!/bin/sh

for file in brn mcape mcin ehi ki lcl lfc pw qfu975 qfv975 qfwind975 qv1000 qv700 qv925 qv950 qv975 srh temp500 temp700 thetae975 tt
do
    echo "file: ${file}"
#    xyz2grd ${file}.bin -G${file}.grd -R1/500/1/500 -I1 -ZTLf
    xyz2grd ${file}.bin -G${file}.grd -R1/174/1/179 -I1 -ZTLf
done