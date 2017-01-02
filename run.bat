recode utf16..utf8 beers.txt
sbcl --load beer.lisp
cd output
del generated_menu.idml
7z a -tzip generated_menu.idml * -mx0 -r
cd ..
copy output\generated_menu.idml .
