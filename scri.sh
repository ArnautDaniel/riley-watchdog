for file in $(find /home/arnaut/Dropbox -mmin 1 -iname *.csv);
do ./latexscript.sh "${file##.}"; done
