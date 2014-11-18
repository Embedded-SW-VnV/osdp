#!/bin/sh

TIMEOUT=12
DIR=${HOME}/T2/smt-ai/these/benchs/guarded_linear
SMT_AI="timeout ${TIMEOUT} ${HOME}/T2/smt-ai/trunk/src/smt-ai -v 4 -v policy:4"
TEST_PI="timeout ${TIMEOUT} ./test_pi.native"
DESCRFILE=./descr

echo "n, PI (s), bounds PI, Q (s), bounds Q, S4 (s), bounds S4, S6 (s), bounds S6, S8 (s), bounds S8, S10 (s), bounds S10, reachable, description"

NB=$(cat ${DESCRFILE} | grep -v "^#" | wc -l | sed -e 's/[ ].*$//')
for i in $(seq ${NB}) ; do
    LINE=$(cat ${DESCRFILE} | grep -v "^#" | head -${i} | tail -1)
    BENCH=$(echo ${LINE} | sed -e 's/[ ].*$//')
    NBVAR=$(echo ${LINE} | sed -e 's/^[^ ]*[ ]\([^ ]*\)[ ].*$/\1/')
    REACH=$(echo ${LINE} | sed -e 's/^[^ ]*[ ][^ ]*[ ]"\([^"]*\)".*$/\1/')
    DESCR=$(echo ${LINE} | sed -e 's/^[^ ]*[ ][^ ]*[ ]"[^"]*"[ ]//')
    FILE="${BENCH}.smt2"
    CMD="${SMT_AI} ${DIR}/${FILE}"
    TMPLOG=tmplog_${FILE%.smt2}
    rm -f ${TMPLOG}
    /usr/bin/time ${CMD} > ${TMPLOG} 2>&1
    TOTAL=$(grep "elapsed" ${TMPLOG} | sed -e 's/^.*://;s/elapsed.*$//')
    BOUNDS=$(tail -10 ${TMPLOG} | grep "state 1" | tail -1 | sed -e 's/^[^{]*{[ ]\([^}]*\)}.*$/\1/' | sed -e 's/[ ]$//')
    rm -f ${TMPLOG}
    printf "${NBVAR}, "
    printf "%.2f, " "${TOTAL}"

    tosplit=${REACH}
    RESBOUNDS=''
    RESREACH=''
    while [ "${tosplit}" ] ; do
        elt=${tosplit%%,*}

        var=$(echo "${elt}" | sed -e 's/^.*\(v_[^:]*\)[:].*$/\1/')
        reach=$(echo "${elt}" | sed -e 's/^[^:]*:[ ]|\([^|]*\)|.*$/\1/')
        bound=$(echo "${BOUNDS}" | sed -e "s/^.*${var}: |\([^|]*\)|.*$/\1/")

        if [ "x${bound}" != "x${BOUNDS}" ] ; then
            bound=$(printf "%.2f" ${bound})
        else
            bound1=$(echo "${BOUNDS}" | sed -e "s/^.*${var}: \[\([^;]*\);.*$/\1/")
            bound2=$(echo "${BOUNDS}" | sed -e "s/^.*${var}: \[[^;]*;\([^]]*\)\].*$/\1/")
            if [ "x${bound1}" != "x${BOUNDS}" -a "x${bound2}" != "x${BOUNDS}" ] ; then
                if [ $(echo "-(${bound1})" | bc) -ge ${bound2} ] ; then
                    bound=$(printf "%.2f" $(echo "-(${bound1})" | bc))
                else
                    bound=$(printf "%.2f" ${bound2})
                fi
            else
                bound="+\\\\infty"
            fi
        fi
        reach=$(printf "%.2f" ${reach})

        if [ -z "${RESBOUNDS}" ] ; then
            RESBOUNDS="${bound}"
            RESREACH="${reach}"
        else
            RESBOUNDS="${RESBOUNDS}, ${bound}"
            RESREACH="${RESREACH}, ${reach}"
        fi

        [ "${tosplit}" = "${elt}" ] && tosplit='' || tosplit=${tosplit#*,}
    done

    printf "\"${RESBOUNDS}\", "

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

    if [ "x${TOTAL%.*}" != "x" ] && [ ${TOTAL%.*} -ge ${TIMEOUT} ] ; then
        printf "${TIMEOUT}.00, \"\", "
    else

    CMD="${TEST_PI} ${NBEX#ex} 10"
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
    fi

    printf "\"${RESREACH}\", "

    printf "$(echo ${DESCR} | sed -e 's/\\/\\\\\\\\\\\\\\\\/g')\n"
done
