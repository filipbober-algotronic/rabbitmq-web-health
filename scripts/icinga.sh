#!/bin/bash

usage()
{
cat << EOF
usage: $0 options
This script checks health of a given component.

OPTIONS:
   -h      Show this message
   -r      Rabbit server url
   -c      Component
   -s      Subsystem
   -t      Timeout
   -f      Print full info

   Sample usage:
       ./icinga.sh -r <server> -c <component> -s <subsystem> -t <timeout>
	   ./icinga.sh -r <server> -c <component> -f

       ./icinga.sh -r localhost:50714/api/values -t 3 -c database -f
       ./icinga.sh -r localhost:50714/api/values -t 3 -c database -s dbConnection
       ./icinga.sh -r localhost:50714/api/values -t 3 -c system

EOF
}


server=
component=false
timeout=30
subsystem=false
full=false

while getopts “:ht:r:c:s:f” OPTION
do
     case $OPTION in
         h)
             usage
             exit 1
             ;;
         t)
             timeout=$OPTARG
             ;;
         r)
             server=$OPTARG
             ;;
         c)
             component=$OPTARG
             ;;
         s)
             subsystem=$OPTARG
			 ;;
	     f)
		     full=true
			 ;;
         ?)
             usage
             exit
             ;;
     esac
done



url=$server/$component

httpCode=$(curl --write-out %{http_code} --silent --output /dev/null -X GET $url --connect-timeout $timeout --max-time $timeout --head)
exitStatus=$?
if [ $httpCode != 200 ] || [ $exitStatus != 0 ]
  then
	echo "UNKNOWN- $component | Offline"
	exit 3
fi

JSON=$(curl --silent --connect-timeout $timeout --max-time $timeout -X GET $url)
exitStatus=$?
if [ $exitStatus != 0 ]
  then
	echo "CRITICAL- $component | Offline"
	exit 2
fi



if [ $subsystem == false ] || [ $full == true ]; then
  syshealthy=`echo ${JSON} | jq '.healthy'`
  if [ $syshealthy == "false" ]; then
   echo "WARNING- $component | Unhealthy"
   if [ $full == false ]; then exit 1; fi
  elif [ $syshealthy == "true" ]; then
   echo "OK- $component | Healthy"
   if [ $full == false ]; then exit 0; fi
  fi
fi

if [ $full == false ]; then
  result=`echo ${JSON} | jq --arg subsystem "$subsystem" --raw-output 'select(.| has("subsystems")) | select((.subsystems|length)>=1) | .subsystems | to_entries[] | {"key": .key, "value": .value} | select(.key == $subsystem) | "\(.value)"'`

  if [ $result == "Healthy" ]; then
    echo "OK- $subsystem | $result"
	exit 0
  elif [ $result == "Unhealthy" ]; then
    echo "WARNING- $subsystem | $result"
	exit 1
  fi

else
  echo ${JSON} | jq --raw-output 'select(.| has("subsystems")) | select((.subsystems|length)>=1) | .subsystems | to_entries[] | {"key": .key, "value": .value} | select(.value == "Healthy") | "  OK- \(.key) | \(.value)"'
  echo ${JSON} | jq --raw-output 'select(.| has("subsystems")) | select((.subsystems|length)>=1) | .subsystems | to_entries[] | {"key": .key, "value": .value} | select(.value == "Unhealthy") | "  WARNING- \(.key) | \(.value)"'
fi
