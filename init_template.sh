#!/bin/bash

echo -n "Please enter a project name: "
read project_name

cp -r "$HOME/templates/CLAS/" "./${project_name}"

cd $project_name

grep -rl "CLAS" ./ | xargs sed -i "s/CLAS/${project_name}/g"

grep -rl "CLAS" ./ | xargs sed -i "s/CLAS/${project_name}/g"

mv "CLAS.cabal" "${project_name}.cabal"

rm -rf .git

git init

git add .

git commit -am "Created project ${project_name} with template"
