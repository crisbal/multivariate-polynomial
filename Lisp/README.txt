# multivariate-polynomial - Lisp

XXXNAME XXXSURNAME - XXXNUMBER
XXXCO_NAME XXXCO_SURNAME - XXXCO_NUMBER

## Scopo della libreria

Questa libreria Lisp espone varie funzioni che permettono di lavorare con polinomi e monomi.
In particolare è possibile parsare polinomi/monomi, farci operazioni di somma, sottrazione e moltiplicazione e calcolarne il valore in determinati punti.
Sono inoltre presenti funzioni di "utility".

## Funzionalità

Sono state implementate tutte le funzionalitè richieste nel testo del progetto.

Inoltre è stato aggiunto il supporto a:

* più coefficienti in un'espressione
    * `'(* 3 x y 42)` è valida e parsata correttamente
* coefficienti non direttamente numerici ma riconducibili ad un numero
    * `'(+ 42 (* (cos 30) x) (* (sin 60)))` è valida e parsata correttamente
* passare espressioni non parsate (e anche monomi) a tutte le funzioni che per specifica accettano oggetti `Polynomial`
    * `(POLYPLUS '(* 3 x y 42) '(M 1 2 ((V 1 X) (V 1 Y))))` è valida e valutata correttamente

## Implementazione

Alcune note sull'implementazione:

* si è fatto largo uso dei costrutti funzionali come `map` (mapcar), `filter` (remove-if-not), `reduce`, sia perchè affrontare il problema in questo era sia più adatto al LISP, ma anche perchè rende tutto più leggibile e interpretabile
* è stata usata la funzione/macro `progn` solo per le funzioni di `pprint` dove un approccio "imperativo" si è dimostrato più adatto
* si è cercato di tenere le funzioni quanto più semplici, contenute e atomiche
* si è usato `error` dove necessario (quasi esclusivamente per stampare errori nel parsing dei polinomi/monomi e per il riconoscimento di oggetti `Monomial` e `Polynomial` malformati)
* il codice è stato scritto ed indentato in emacs, preferendo gli spazi al posto dei TABs

## Documentazione strutture

Per comprendere meglio la lettura della documentazione e la comprensione del programma definiamo di seguito le strutture che torneranno utili per la descrizione della firma delle funzioni implementate.
Pur non esistendo il concetto di "tipo" e firma tipizzata in LISP speriamo che questa astrazione ci sia permessa, quantomeno per comprendere meglio la documentazione.

### VarExpt

* `VarExpt` è la rappresentazione all'interno di una `MonomialExpression` di una potenza.
* `VarExpt` è nella forma `'(expt Symbol Power)` oppure `'Symbol`
* `VarExpt` è validata dalla funzione `(expression-variable-p VarExpt)`

### MonomialExpression

* `MonomialExpression` è una espressione di un monomio. Può essere o un `number` oppure nella forma  `'(* (VarExpt|Coefficient)* )`
* `MonomialExpression`, nel caso non fosse un `number`, è validata da `(monomial-expression-p MonomialExpression)`
* `coefficient` è o un `Number` oppure un "qualcosa" `eval`utabile come `Number`
    * `42`
    * `'(cos 30)`

Esempio:

* `42`
* `'(* x 42 (expt y 3))`

### PolynomialExpression

* `PolynomialExpression` è una espressione di polinomio. Può essere una `MonomialExpression` oppure nella forma `'(+ MonomialExpression* )`
* `PolynomialExpression` è validata, nel caso non fosse una `MonomialExpression` da `polynomial-expression-p`

Esempio:

* `42`
* `'(+ (* x y))`

### VarPower

* `VarPower` è un lista rappresentante una potenza.
* `VarPower` è nella forma `'(v power symbol)`
* `VarPower` è validato dalla funzione `(is-varpower VarPower)`

### Monomial

* `Monomial` è una lista rappresentante un monomio.
* `Monomial` è nella forma `'(M Coefficient TotalDegree VarPowers)`.
    * `Coefficient` è il coefficiente del monomio, un numero
    * `TotalDegree` è il grado del monomio, un numero >= 0
    * `VarPowers` è una lista (anche vuota) di oggetti `VarPower`
* `Monomial` è validato dalla funzione `is-monomial`.

Ad esempio:

* `'(M 42 0 NIL)` è il monomio corrispondente alla `MonomialExpression` `42`
* `'(M 3 7 ((V 2 X) (V 5 Y)))` è il monomio corrispondente alla `MonomialExpression` `'(* 3 (expt x 2) (expt y 5))`

### Polynomial

* `Polynomial` è una lista rappresentante un polinomio.
* `Polynomial` è nella forma `(POLY Monomials)`.
    * `Monomials` è una lista (anche vuota) di oggetti `Monomial`
* `Polynomial` è validato dalla funzione `(is-polynomial Polynomial)`

### GenericMonomial

* `GenericMonomial` è usato per le funzioni che possono accettare indistintamente come struttura del parametro: `Monomial`, `MonomialExpression`.
* `GenericMonomial` viene sempre trasformato in `Monomial` tramite la funzione `(to-monomial GenericMonomial)`

### GenericPolynomial

* `GenericPolynomial` è usato per le funzioni che possono accettare indistintamente come struttura del parametro: `Monomial`, `Polynomial`, `PolynomialExpression`.
* `GenericPolynomial` viene sempre trasformato in `Polynomial` tramite la funzione `(to-polynomial GenericPolynomial)`

## Documentazione funzioni

Sono state descritte anche funzioni che, pur non essendo esplicitamente richieste, sono state aggiunti per completezza e per facilitare il lavoro e che si sono poi rivelate utili anche lato utente.

Tutti le funzioni controllano, dove possibile, che i parametri di input rispettino la struttura prevista, ritornando `NIL` o generando errori in caso contrario.


### as-monomial

`(as-monomial MonomialExpression me) -> Monomial m`

La funzione `as-monomial` ritorna il `Monomial m` che rappresenta il monomio risultante dal parsing dell’espressione `MonomialExpression me`

Un `SIMPLE-ERROR` è generato se l'espressione passata non è conforme alla struttura `MonomialExpression`

### as-polynomial

`(as-polynomial PolynomialExpression pe) -> Polynomial m`

La funzione `as-polynomial` ritorna il `Polynomial m` che rappresenta il polinomio risultante dal parsing dell’espressione `PolynomialExpression pe`

Un `SIMPLE-ERROR` è generato se l'espressione passata non è conforme alla struttura `PolynomialExpression`


### is-monomial

`(is-monomial Monomial m)`

La funzione`is-monomial` ritorna `T` quando `m` passato è effettivamente un `Monomial` ben formato e matematicamente corretto.
Ritorna `NIL` se `m` non è un `Monomial`.
Genera un `SIMPLE-ERROR` (con descrizione associata) se `m` è un `Monomial` ma è malformato/matematicamente errato (ad esempio se il `TotalDegree` non è conforme alle `VarsPowers`, o se il `Coefficient` non è numerico).

### is-polynomial

`(is-polynomial Polynomial p)`

La funzione`is-polynomial` ritorna `T` quando `p` passato è effettivamente un `Polynomial` ben formato e matematicamente corretto.
Ritorna `NIL` se `p` non è un `Polynomial`.
Genera un `SIMPLE-ERROR` (con descrizione associata) se `p` è un `Polynomial` ma è malformato/matematicamente errato (ad esempio se la lista che rappresenta `p` non è della lunghezza giusta).

### to-monomial

`(to-monomial GenericMonomial g) -> Monomial m`

La funzione `to-monomial` viene usata per fare il "casting" da `GenericMonomial` a `Monomial`, ritorna infatti il monomio associato a `g`.

La funzione, usata praticamente in ogni funzione che accetta `GenericMonomial`, si assicura che, in caso `GenericMonomial` sia un `Monomial`, questo sia ben formato, applicando prima anche un riodinamento. In caso di `Monomial` malformato verranno generati (SIMPLE-ERROR), gli stessi di `is-monomial`.

### to-polynomial

`(to-polynomial GenericPolynomial g) -> Polynomial p`

La funzione `to-polynomial` viene usata per fare il "casting" da `GenericPolynomial` a `Polynomial`, ritorna infatti il monomio associato a `g`.

La funzione, usata praticamente in ogni funzione che accetta `GenericPolynomial`, si assicura che, in caso `GenericPolynomial` sia un `Polynomial`, questo sia ben formato, applicando prima anche un riodinamento. In caso di `Polynomial` malformato verranno generati (SIMPLE-ERROR), gli stessi di `is-polynomial`.

### pprint-polynomial

`(pprint-polynomial GenericPolynomial g) -> NIL`

La funzione `pprint-polynomial` stampa su Standard Output la rappresentazione "grafica" del `GenericPolynomial g`.

### pprint-monomial

`(pprint-monomial Monomial m) -> NIL`

La funzione `pprint-monomial` stampa su Standard Output la rappresentazione "grafica" del `Monomial m`.

### polyplus

`(polyplus GenericPolynomial g1, GenericPolynomial g2) -> Polynomial r`

La funzione `polyplus` ritorna la somma dei `GenericPolynomial` `g1` e `g2`.

### polyminus

`(polyminus GenericPolynomial g1, GenericPolynomial g2) -> Polynomial r`

La funzione `polyminus` ritorna la differenza dei `GenericPolynomial` `g1` e `g2`.

### polytimes

`(polytimes GenericPolynomial g1, GenericPolynomial g2) -> Polynomial r`

La funzione `polytimes` ritorna la moltiplicazione dei `GenericPolynomial` `g1` e `g2`.

### monotimes

`(monotimes Monomial g1, Monomial g2) -> Polynomial r`

La funzione `monotimes` ritorna la moltiplicazione dei `Monomial` `g1` e `g2`.

### polyval

`(polyval GenericPolynomial p, NumberList v) -> Number r`

La funzione `polyval` effettua la valutazione del polinomio `p` nei punti espressi dalla lista di numeri `v`. `v` deve contenere un valore per ogni variabile ottenuta dalla funzione `variables`.

Viene generato un `SIMPLE-ERROR` in caso il numero di valori forniti sia minore dal numero di variabili ottenute con `variables`.

### maxdegree

`(maxdegree GenericPolynomial p) -> Number md`

La funzione `maxdegree` ritorna il grado più alto tra quello di tutti i `Monomial` di `p`.

### mindegree

`(mindegree GenericPolynomial p) -> Number md`

La funzione `mindegree` ritorna il grado più basso tra quello di tutti i `Monomial` di `p`.

### coefficients

`(coefficients GenericPolynomial p) -> NumberList n`

`coefficients` ritorna la lista dei coefficienti dei `Monomial` di `p`.

### variables

`(variables GenericPolynomial p) -> SymbolList n`

`variables` ritorna la lista delle variabili di `p`.

### monomials

`(monomials GenericPolynomial p) -> MonomialList l`

`monomials` ritorna la lista dei monomi di `p`.

### varpowers

`(varpowers GenericMonomial m) -> VarPowerList l`

`varpowers` ritorna la lista delle VarPower di m.

Alias di: `monomial-varpowers`

### vars-of

`(vars-of GenericMonomial m) -> SymbolList l`

`vars-of` ritorna la lista delle variabili di m.

Alias di: `monomial-variables`

### monomial-degree

`(monomial-degree GenericMonomial m) -> Number d`

`monomial-degree` ritorna il grado del monomio `m`.

### monomial-coefficient

`(monomial-coefficient GenericMonomial m) -> Number d`

`monomial-coefficient` ritorna il coefficiente del monomio `m`.

## Altro

* Il codice è stato scritto su emacs, con il pacchetto SLIME per l'integrazione emacs<->LISP
* Il codice è stato messo sotto version control tramite `git`
* Il codice è stato testato (anche) automaticamente usando il pacchetto/modulo LISP `clunit` per definire le unit test.
* L'esecuzione, compilazione, testing del progetto è stato gestito tramite un Makefile
