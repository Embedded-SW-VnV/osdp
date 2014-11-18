#!/bin/sh

CSVFILE="table.csv"

TIMEOUT="12.00"

echo "\\\\begin{tabular}{|p{3.5cm}|c|c|c|c|c|c|c|}"
echo "\\\\cline{2-8}"
echo "\\\\multicolumn{1}{c|}{~} & \$n\$ & PI (s) & Q (s) & S4 (s) & S6 (s) & S8 (s) & S10 (s) \\\\\\\\"
echo "\\\\hline"

NB=$(cat ${CSVFILE} | grep -v "^#" | wc -l | sed -e 's/[ ].*$//')
NB=$(( ${NB} - 1 ))
for i in $(seq ${NB}) ; do
    LINE=$(cat ${CSVFILE} | grep -v "^#" | head -$(( ${i} + 1 )) | tail -1)
    N=$(echo "${LINE}" | sed -e 's/,.*$//')
    TPI=$(echo "${LINE}" | sed -e 's/^[^,]*, //' | sed -e 's/,.*$//')
    TQ=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{1\}//' | sed -e 's/,.*$//')
    TS4=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{2\}//' | sed -e 's/,.*$//')
    TS6=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{3\}//' | sed -e 's/,.*$//')
    TS8=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{4\}//' | sed -e 's/,.*$//')
    TS10=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{5\}//' | sed -e 's/,.*$//')
    DESCR=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{6\}"[^"]*", //')

    if [ ${TPI%.*} -ge ${TIMEOUT%.*} ] ; then TPI="---"; fi
    if [ ${TQ%.*} -ge ${TIMEOUT%.*} ] ; then TQ="---"; fi
    if [ ${TS4%.*} -ge ${TIMEOUT%.*} ] ; then TS4="---"; fi
    if [ ${TS6%.*} -ge ${TIMEOUT%.*} ] ; then TS6="---"; fi
    if [ ${TS8%.*} -ge ${TIMEOUT%.*} ] ; then TS8="---"; fi
    if [ ${TS10%.*} -ge ${TIMEOUT%.*} ] ; then TS10="---"; fi

    printf "${DESCR}"

    printf " & %d & ${TPI} & ${TQ} & ${TS4} & ${TS6} & ${TS8} & ${TS10} \\\\\\\\\n" \ ${N}

    echo "\\\\hline"
done

echo "\\\\end{tabular}"
echo ""
echo "On a Core2 @ 1.2GHz."
echo ""
echo "--- means timeout (${TIMEOUT} s)."
