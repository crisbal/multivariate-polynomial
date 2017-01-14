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

fns[$c]="(monotimes (to-monomial '(* x f a)) (to-monomial '(* x w)))"
c=$(($c+1))
fns[$c]="(monotimes (to-monomial '(* x f a)) (to-monomial '(* x (expt f 3) a)))"
c=$(($c+1))

fns[$c]="(polyval '(* x y z) '(1 2 3))"
c=$(($c+1))
fns[$c]="(polyval '(* x y z) '(4 2 3))"
c=$(($c+1))
fns[$c]="(polyval '(* x (expt y 10) z) '(1 2 3))"
c=$(($c+1))

polys=(
  "'(+ (* x (expt y 10) z) (* y (expt x 10) q))"
  "'(* x (expt y 10) z)"
  "'(* 10 (expt y 10) z)"
)
for poly_n in $(seq 0 $((${#polys[@]}-1)))
do
  fns[$c]="(maxdegree ${polys[$poly_n]})"
  c=$(($c+1))

  fns[$c]="(mindegree ${polys[$poly_n]})"
  c=$(($c+1))

  fns[$c]="(coefficients ${polys[$poly_n]})"
  c=$(($c+1))

  fns[$c]="(variables ${polys[$poly_n]})"
  c=$(($c+1))

  fns[$c]="(monomials ${polys[$poly_n]})"
  c=$(($c+1))

  echo "(pprint-polynomial ${polys[$poly_n]})"
done


monoms=(
  "'(* x (expt y 10) z)"
  "'(* (expt x 4) (expt y 10) z)"
  "'(* (expt x 4) (expt y 10) (expt z 3))"
)
for monom_n in $(seq 0 $((${#monoms[@]}-1)))
do
  fns[$c]="(varpowers ${monoms[$monom_n]})"
  c=$(($c+1))

  fns[$c]="(vars-of ${monoms[$monom_n]})"
  c=$(($c+1))

  fns[$c]="(monomial-degree ${monoms[$monom_n]})"
  c=$(($c+1))

  fns[$c]="(monomial-coefficient ${monoms[$monom_n]})"
  c=$(($c+1))

  echo "(pprint-monomial ${monoms[$monom_n]})"
done

tmp_dir=`mktemp -d`"/"

i=0

while [[ $i -lt $c ]]
do
  fn_name=$(echo ${fns[$i]} | awk '{ print $1 }' | cut -d "(" -f 2)
  echo "Running "${fns[$i]}
  if test ! -f $tmp_dir$fn_name
  then
    echo "Ad esempio:" > $tmp_dir$fn_name
  fi
  echo \`${fns[$i]}\` ritornerà \`$(sbcl --noinform --load mvpoli.lisp --eval "(pprint ${fns[$i]})" --eval "(quit)")\` | sed 's/` (/`(/g' >> $tmp_dir$fn_name
  i=$(($i+1))
done

rsync -rdv --checksum --delete $tmp_dir examples/

rm -Rf $tmp_dir
