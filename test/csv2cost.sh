#!/bin/sh

export LC_NUMERIC="en_US.UTF-8"

CSVFILE="table.csv"

TIMEOUT="60.00"

echo "\\begin{tabular}{|p{3.5cm}|c|c|c|c|c|}"
echo "\\cline{2-6}"
echo "\\multicolumn{1}{c|}{~} & \$n\$ & Q (s) & S4 (s) & S6 (s) & S8 (s) \\\\"
echo "\\hline"

NB=$(cat ${CSVFILE} | grep -v "^#" | wc -l | sed -e 's/[ ].*$//')
NB=$(( ${NB} - 1 ))
for i in $(seq ${NB}) ; do
    LINE=$(cat ${CSVFILE} | grep -v "^#" | head -$(( ${i} + 1 )) | tail -1)
    N=$(echo "${LINE}" | sed -e 's/,.*$//')
    TQ=$(echo "${LINE}" | sed -e 's/^[^,]*, //' | sed -e 's/,.*$//')
    TS4=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{1\}//' | sed -e 's/,.*$//')
    TS6=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{2\}//' | sed -e 's/,.*$//')
    TS8=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{3\}//' | sed -e 's/,.*$//')
    DESCR=$(echo "${LINE}" | sed -e 's/^[^,]*, \([^,]*, "[^"]*", \)\{4\}"[^"]*", //')

    if [ ${TQ%.*} -ge ${TIMEOUT%.*} ] ; then TQ="---"; fi
    if [ ${TS4%.*} -ge ${TIMEOUT%.*} ] ; then TS4="---"; fi
    if [ ${TS6%.*} -ge ${TIMEOUT%.*} ] ; then TS6="---"; fi
    if [ ${TS8%.*} -ge ${TIMEOUT%.*} ] ; then TS8="---"; fi

    printf "${DESCR}"

    printf " & %d & ${TQ} & ${TS4} & ${TS6} & ${TS8} \\\\\\\\\n" \ ${N}

    echo "\\hline"
done

echo "\\end{tabular}"
echo ""
echo "On a Core2 @ 1.2GHz."
echo ""
echo "--- means timeout (${TIMEOUT} s)."
