#!/bin/bash
fns=()
c=0

fns[$c]="(as-monomial '(* x f a))"
c=$(($c+1))
fns[$c]="(as-monomial '(* x (expt f 3) a))"
c=$(($c+1))

fns[$c]="(as-polynomial '(* x f a))"
c=$(($c+1))
fns[$c]="(as-polynomial '(* x (expt f 3) a))"
c=$(($c+1))
fns[$c]="(as-polynomial '(+ (* -1 x) (* x w)))"
c=$(($c+1))

fns[$c]="(is-monomial '(+ (* x f)  (* f f)))"
c=$(($c+1))
fns[$c]="(is-monomial '(M 1 2 ((V 1 F) (V 1 X))))"
c=$(($c+1))

fns[$c]="(is-polynomial '(* f r))"
c=$(($c+1))
fns[$c]="(is-polynomial (as-polynomial '(* f r)))"
c=$(($c+1))
fns[$c]="(is-polynomial '(POLY ((M -1 1 ((V 1 X))) (M 1 2 ((V 1 W) (V 1 X))))))"
c=$(($c+1))

fns[$c]="(to-monomial '(* x f a))"
c=$(($c+1))
fns[$c]="(to-monomial '(* x (expt f 3) a))"
c=$(($c+1))
fns[$c]="(to-monomial (to-monomial '(* x (expt f 3) a)))"
c=$(($c+1))

fns[$c]="(to-polynomial '(* x f a))"
c=$(($c+1))
fns[$c]="(to-polynomial '(* x (expt f 3) a))"
c=$(($c+1))
fns[$c]="(to-polynomial '(+ (* -1 x) (* x w)))"
c=$(($c+1))
fns[$c]="(to-polynomial (to-polynomial '(+ (* -1 x) (* x w))))"
c=$(($c+1))

for f in polyplus polyminus polytimes
do
  fns[$c]="($f (to-polynomial '(+ (* -1 x) (* x w))) (to-polynomial '(* x f a)))"
  c=$(($c+1))
  fns[$c]="($f (to-polynomial '(* x (expt f 3) a)) (to-polynomial '(* x f a)))"
  c=$(($c+1))
done

if false ; then
fns[$c]="(monotimes )"
c=$(($c+1))
fns[$c]="(polyval )"
c=$(($c+1))
fns[$c]="(maxdegree )"
c=$(($c+1))
fns[$c]="(mindegree )"
c=$(($c+1))
fns[$c]="(coefficients )"
c=$(($c+1))
fns[$c]="(variables )"
c=$(($c+1))
fns[$c]="(monomials )"
c=$(($c+1))
fns[$c]="(varpowers )"
c=$(($c+1))
fns[$c]="(vars-of )"
c=$(($c+1))
fns[$c]="(monomial-degree )"
c=$(($c+1))
fns[$c]="(monomial-coefficient )"
c=$(($c+1))

fns[$c]="(pprint-polynomial )"
c=$(($c+1))
fns[$c]="(pprint-monomial )"
c=$(($c+1))
fi


tmp_dir=`mktemp -d`"/"

i=0

while [[ $i -lt $c ]]
do
  fn_name=$(echo ${fns[$i]} | awk '{ print $1 }' | cut -d "(" -f 2)
  echo "Running "${fns[$i]}
  echo \`${fns[$i]}\` ritornerà \`$(sbcl --noinform --load mvpoli.lisp --eval "(pprint ${fns[$i]})" --eval "(quit)")\` >> $tmp_dir$fn_name
  i=$(($i+1))
done

rsync -rdv --checksum --delete $tmp_dir examples/

rm -Rf $tmp_dir
