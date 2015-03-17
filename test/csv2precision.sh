#!/bin/sh

CSVFILE="table.csv"

echo "\\begin{tabular}{|c|c|c|c|}"
echo "\\cline{3-4}"
echo "\\multicolumn{2}{c|}{~} & Bounds & Reachable \\\\"
echo "\\hline"

NB=$(cat ${CSVFILE} | grep -v "^#" | wc -l | sed -e 's/[ ].*$//')
NB=$(( ${NB} - 1 ))
for i in $(seq ${NB}) ; do
    LINE=$(cat ${CSVFILE} | grep -v "^#" | head -$(( ${i} + 1 )) | tail -1)
    BQ=$(echo "${LINE}" | sed -e 's/^[^"]*"\([^"]*\)".*$/\1/')
    BS4=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{1\}[^,]*, "//' | sed -e 's/",.*$//')
    BS6=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{2\}[^,]*, "//' | sed -e 's/",.*$//')
    BS8=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{3\}[^,]*, "//' | sed -e 's/",.*$//')
    REACH=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{4\}"//' | sed -e 's/",.*$//')
    DESCR=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{4\}"[^"]*", //')

    printf "\\multirow{4}{3cm}{${DESCR}}"

    echo " & Q & \$ ${BQ} \$ & \$ \\multirow{4}{4cm}{${REACH}} \$ \\\\"

    echo " & S4 & \$ ${BS4} \$ & \\\\"

    echo " & S6 & \$ ${BS6} \$ & \\\\"

    echo " & S8 & \$ ${BS8} \$ & \\\\"

    echo "\\hline"
done

echo "\\end{tabular}"
