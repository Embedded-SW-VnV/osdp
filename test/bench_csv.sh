#!/bin/sh

export LC_NUMERIC="en_US.UTF-8"

TIMEOUT=60
TEST_PI="timeout ${TIMEOUT} ./test_pi2"
DESCRFILE=./descr

echo "n, Q (s), bounds Q, S4 (s), bounds S4, S6 (s), bounds S6, S8 (s), bounds S8, reachable, description"

NB=$(cat ${DESCRFILE} | grep -v "^#" | wc -l | sed -e 's/[ ].*$//')
for i in $(seq ${NB}) ; do
    LINE=$(cat ${DESCRFILE} | grep -v "^#" | head -${i} | tail -1)
    BENCH=$(echo ${LINE} | sed -e 's/[ ].*$//')
    NBVAR=$(echo ${LINE} | sed -e 's/^[^ ]*[ ]\([^ ]*\)[ ].*$/\1/')
    REACH=$(echo ${LINE} | sed -e 's/^[^ ]*[ ][^ ]*[ ]"\([^"]*\)".*$/\1/')
    DESCR=$(echo ${LINE} | sed -e 's/^[^ ]*[ ][^ ]*[ ]"[^"]*"[ ]//')
    FILE="${BENCH}.smt2"
    printf "${NBVAR}, "

    tosplit=${REACH}
    RESREACH=''
    while [ "${tosplit}" ] ; do
        elt=${tosplit%%,*}

        reach=$(printf "%.2f" ${elt})

        if [ -z "${RESREACH}" ] ; then
            RESREACH="${reach}"
        else
            RESREACH="${RESREACH}, ${reach}"
        fi

        [ "${tosplit}" = "${elt}" ] && tosplit='' || tosplit=${tosplit#*,}
    done

    NBEX="${FILE%.smt2}"
    CMD="${TEST_PI} ${NBEX#ex} 2"
    TMPLOG=tmplog_${FILE%.smt2}
    rm -f ${TMPLOG}
    /usr/bin/time ${CMD} > ${TMPLOG} 2>&1
    TOTALM=$(grep "elapsed" ${TMPLOG} | sed -e 's/^.* \([0-9]*:\)/\1/;s/:.*elapsed.*$//')
    TOTALS=$(grep "elapsed" ${TMPLOG} | sed -e 's/^.*://;s/elapsed.*$//')
    TOTAL=$(echo "${TOTALM} * 60 + ${TOTALS}" | bc -l)
    BOUNDS=$(tail -10 ${TMPLOG} | grep "bounds = " | tail -1 | sed -e 's/^bounds = //')
    rm -f ${TMPLOG}
    printf "%.2f, " "${TOTAL}"
    printf "\"${BOUNDS}\", "

    if [ "x${TOTAL%.*}" != "x" ] && [ ${TOTAL%.*} -ge ${TIMEOUT} ] ; then
        printf "${TIMEOUT}.00, \"\", "
        printf "${TIMEOUT}.00, \"\", "
        printf "${TIMEOUT}.00, \"\", "
    else

    CMD="${TEST_PI} ${NBEX#ex} 4"
    TMPLOG=tmplog_${FILE%.smt2}
    rm -f ${TMPLOG}
    /usr/bin/time ${CMD} > ${TMPLOG} 2>&1
    TOTALM=$(grep "elapsed" ${TMPLOG} | sed -e 's/^.* \([0-9]*:\)/\1/;s/:.*elapsed.*$//')
    TOTALS=$(grep "elapsed" ${TMPLOG} | sed -e 's/^.*://;s/elapsed.*$//')
    TOTAL=$(echo "${TOTALM} * 60 + ${TOTALS}" | bc -l)
    BOUNDS=$(tail -10 ${TMPLOG} | grep "bounds = " | tail -1 | sed -e 's/^bounds = //')
    rm -f ${TMPLOG}
    printf "%.2f, " "${TOTAL}"
    printf "\"${BOUNDS}\", "

    if [ "x${TOTAL%.*}" != "x" ] && [ ${TOTAL%.*} -ge ${TIMEOUT} ] ; then
        printf "${TIMEOUT}.00, \"\", "
        printf "${TIMEOUT}.00, \"\", "
    else

    CMD="${TEST_PI} ${NBEX#ex} 6"
    TMPLOG=tmplog_${FILE%.smt2}
    rm -f ${TMPLOG}
    /usr/bin/time ${CMD} > ${TMPLOG} 2>&1
    TOTALM=$(grep "elapsed" ${TMPLOG} | sed -e 's/^.* \([0-9]*:\)/\1/;s/:.*elapsed.*$//')
    TOTALS=$(grep "elapsed" ${TMPLOG} | sed -e 's/^.*://;s/elapsed.*$//')
    TOTAL=$(echo "${TOTALM} * 60 + ${TOTALS}" | bc -l)
    BOUNDS=$(tail -10 ${TMPLOG} | grep "bounds = " | tail -1 | sed -e 's/^bounds = //')
    rm -f ${TMPLOG}
    printf "%.2f, " "${TOTAL}"
    printf "\"${BOUNDS}\", "

    if [ "x${TOTAL%.*}" != "x" ] && [ ${TOTAL%.*} -ge ${TIMEOUT} ] ; then
        printf "${TIMEOUT}.00, \"\", "
    else

    CMD="${TEST_PI} ${NBEX#ex} 8"
    TMPLOG=tmplog_${FILE%.smt2}
    rm -f ${TMPLOG}
    /usr/bin/time ${CMD} > ${TMPLOG} 2>&1
    TOTALM=$(grep "elapsed" ${TMPLOG} | sed -e 's/^.* \([0-9]*:\)/\1/;s/:.*elapsed.*$//')
    TOTALS=$(grep "elapsed" ${TMPLOG} | sed -e 's/^.*://;s/elapsed.*$//')
    TOTAL=$(echo "${TOTALM} * 60 + ${TOTALS}" | bc -l)
    BOUNDS=$(tail -10 ${TMPLOG} | grep "bounds = " | tail -1 | sed -e 's/^bounds = //')
    rm -f ${TMPLOG}
    printf "%.2f, " "${TOTAL}"
    printf "\"${BOUNDS}\", "

    fi
    fi
    fi

    printf "\"${RESREACH}\", "

    printf "$(echo ${DESCR} | sed -e 's/\\/\\\\\\\\\\\\\\\\/g')\n"
done
