#! /bin/bash
clear

echo "Program to print user and machine details"

echo "Hello $USER"
echo 

echo "Today's date is `date +%d-%b-%Y`"
echo

echo "Right now follwoing users are online"
w | cut -d " " -f 1 - | grep -v USER | sort -u 
echo 

echo "System uptime info:"
uptime


