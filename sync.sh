#!/bin/bash

rsync -uav --exclude=".git*" --exclude="sync.sh" --exclude=".idea*" --exclude=".DS*" . mt418401@students.mimuw.edu.pl:~/Desktop/interpreter/Roksanka