#!/bin/bash

if ! [ -d "./.git" ]
then
    echo "Should be executed from the repo root."
    exit 1
fi

#Use "sed" or "gsed" if avaliable.
seder="sed"
if hash gsed 2>/dev/null; then
    seder="gsed"
fi

if [[ $1 = "test" ]]
then
    places=("./core/tools/doc-fmt")
else
    places=("./doc" "./layers")
fi

for place in "${places[@]}"
do :
   before_md5="foo"
   after_md5="bar"
   while ! [ "$before_md5" = "$after_md5" ]
   do
       # Calculate md5 of the files in $place before formating
       before_md5=$(find $place -type f -exec md5sum {} \; | sort -k 2 | md5sum)

       # Remove trailing delimiters in headlines
       find $place -name "*.org" -type f -exec $seder -i 's/^\(*\+\s\+.*\)[;,.]$/\1/g' {} \;
       # Remove trailing spaces
       find $place -name "*.org" -type f -exec $seder -i 's/[ \t]*$//' {} \;
       # Remove #+HTML_HEAD_EXTRA: ... readtheorg.css" />
       find $place -name "*.org" -type f -exec $seder -i '/#+HTML_HEAD_EXTRA.*readtheorg.css.*/d' {} \;
       # Replace multiply empty lines with a single empty line
       find $place -name "*.org" -type f -exec $seder -i '/^$/N;/^\n$/D' {} \;
       # Replace :TOC_4_org: with :TOC_4_gh:
       find $place -name "*.org" -type f -exec $seder -i 's/:TOC_4_org:/:TOC_4_gh:/' {} \;
       # apply toc-org
       find $place -name "*.org" -type f -exec emacs -batch -l ./core/tools/doc-fmt/fmt.el '{}' -f apply-all \;

       # Calculate md5 of the files in $place after formating
       after_md5=$(find $place -type f -exec md5sum {} \; | sort -k 2 | md5sum)
   done
done
