# multivariate-polynomial - Prolog

XXXNAME XXXSURNAME - XXXNUMBER
XXXCO_NAME XXXCO_SURNAME - XXXCO_NUMBER

## Come caricare la libreria

Direttamente al lancio di SWI-PL:

* `swipl mvpoli.pl`
 
Una volta avviato SWI-PL:

* `consult("mvpoli.pl").`

## Scopo della libreria

Questa libreria Prolog espone vari predicati che permettono di lavorare con polinomi e monomi.
In particolare è possibile parsare polinomi/monomi, farci operazioni di somma, sottrazione e moltiplicazione e calcolarne il valore in determinati punti.
Sono inoltre presenti predicati di "utility".

## Funzionalità

Sono state implementate tutte le funzionalitè richieste nel testo del progetto.

Inoltre è stato aggiunto il supporto a:

* più coefficienti in un'espressione
    * `3*x^7*42*y + 12` è valida e parsata correttamente
* coefficienti non direttamente numerici ma riconducibili ad un numero
    * `cos(30)*x*y*max(5, 42) + 3^2` è valida e parsata correttamente
* passare espressioni non parsate (e anche monomi) a tutti i predicati che per specifica accettano oggetti `Polynomial`
    * `polyplus(3*x, m(1,0,[]), R)` è valida e valutata correttamente
* passare da oggetto `Polynomial`/`Monomial` ad espressione nei predicati `as_monomial`/`as_polynomial`
    * richiamando `as_polynomial(E, poly([m(5, 1, [v(1, x)]), m(1, 1, [v(1, z)]), m(1, 3, [v(3, q)])]))` il programma farà il procedimento inverso ed `E = 5*x+z+q^3`
    * siamo a conoscenza che un particolare oggetto `Polynomial` potrebbe essere originato da infinite espressioni, ma quella proposta è la "minima" per generarlo.

Implementate ma commentate nella release finale:

* `as_monomial()` di espressioni che pur apparendo come polinomi sono in realtà monomi
    * `as_monomial(x+x+0+y-y, R)` ritorna `R = m(2, 1, [v(1, x)])`.

## Implementazione

Alcune note sull'implementazione:

* Generalmente è stato usato il "pattern" (se cosi' lo si vuole chiamare) dell'avere un predicato wrapper (che è quella che poi viene "esposta" all'esterno) che richiama un predicato worker che effettivamente tenta di dimostrare il goal. Questo perchè il wrapper attua generalmente controlli e/o perchè il worker è ricorsivo e/o per semplicità vengono passati solo i parametri strettamente necessari.
* Per ordinare i monomi e le varpower è stato usato `predsort` definendo dei predicati di comparazione. Usare predsort ci permette di ordinare i monomi e i polinomi secondo il criterio richiesto e allo stesso momento non ci vincola troppo nel caso dovesse cambiare la specifica dell'ordinamento.
* i predicati delle operazioni e di utility controllano esplicitamente che i parametri siano `nonvar`, dove ovviamente non è possibile fare chiamate "inverse". Ad esempio la chiamata di `polyplus(A, B, poly([m(1, 0, [])]))` produrrà `false` perchè è impossibile ricondursi al valore di A e di B.

## Documentazione strutture

Per comprendere meglio la lettura della documentazione e la comprensione del programma definiamo di seguito delle strutture che torneranno utili per la descrizione della firma dei predicati implementati.
Pur non esistendo il concetto di "tipo" e firma tipizzata in Prolog speriamo che questa astrazione ci sia permessa, quantomeno per comprendere meglio la documentazione.

### MonomialExpression

`MonomialExpression` è una espressione di un monomio,

Esempio:

* `sin(30)*x*y^2*42`
* `x`
* `42`

### PolynomialExpression

`PolynomialExpression` è una espressione di un polinomio (o anche una `MonomialExpression`)

Ad esempio:

* `x*2+y+42`
* `x*2-y-42`
* `42*x`

### VarPower

* `VarPower` è un "termine" rappresentante una potenza.
* `VarPower` è nella forma `v(Power, Symbol)`
* `VarPower` è validato dal predicato `is_varpower/1`

Ad esempio:

* `v(2,x)`
* `v(1,foo)`

### Monomial

* `Monomial` è un "termine" rappresentante un monomio.
* `Monomial` è nella forma `m(Coefficient, TotalDegree, VarPowers)`.
    * `Coefficient` è il coefficiente del monomio, un numero
    * `TotalDegree` è il grado del monomio, un numero >= 0
    * `VarPowers` è una lista (anche vuota) di oggetti `VarPower`
* `Monomial` è validato dal predicato `is_monomial/1`.

Ad esempio:
* `m(42, 0, [])` è il monomio corrispondente alla `MonomialExpression` `42`
* `m(3, 7, [v(2, x), v(5, y)])` è il monomio corrispondente alla `MonomialExpression` `3*x^2*y^5`

### Polynomial

* `Polynomial` è un termine rappresentante un polinomio.
* `Polynomial` è nella forma `poly(Monomials)`.
    * `Monomials` è una lista (anche vuota) di oggetti `Monomial`
* `Polynomial` è validato dal predicato `is_polynomial/1`

### GenericPolynomial

* `GenericPolynomial` è usato nei predicati che possono accettare indistintamente come struttura del parametro: `Monomial`, `Polynomial`, `PolynomialExpression`.
* `GenericPolynomial` viene sempre trasformato in `Polynomial` tramite il predicato `to_polynomial/2`

## Documentazione predicati

Sono stati descritti anche predicati che, pur non essendo esplicitamente richiesti, sono stati aggiunti per completezza e per facilitare il lavoro e che si sono poi rivelati utili anche lato utente.

Tutti i predicati controllano, dove possibile che i parametri di input siano della struttura corretta.

### as_monomial/2

`as_monomial(MonomialExpression me, Monomial m)`

Il predicato `as_monomial` è vero quando `Monomial m` è il termine che rappresenta il monomio risultante dal parsing dell’espressione `MonomialExpression me`

Il predicato è invertibile.

### as_polynomial/2

`as_polynomial(PolynomialExpression pe, Polynomial p)`

Il predicato `as_polynomial` è vero quando `Polynomial p` è il termine che rappresenta il monomio risultante dal parsing dell’espressione `PolynomialExpression pe`

Il predicato è invertibile.

Ad esempio:

* `as_polynomial(a^2 + a^3, poly([m(1, 2, [v(2, a)]), m(1, 3, [v(3, a)])])).`
* `as_polynomial(a*c+a^2+a*b+a,F).` verrà dimostrato per `F = poly([m(1, 1, [v(1, a)]), m(1, 2, [v(1, a), v(1, b)]), m(1, 2, [v(1, a), v(1, c)]), m(1, 2, [v(2, a)])]).`

### is_monomial/1

`is_monomial(Monomial m)`

Il predicato `is_monomial` è vero quando il `Monomial m` passato è effettivamente un termine di tipo `Monomial` ben formato e matematicamente corretto.

Ad esempio:

* Per `is_monomial(x*y^3*z*10).` la dimostrazione fallirà
* Per `is_monomial(m(10, 5, [v(1, x), v(3, y), v(1, z)])).` la dimostrazione avrà successo

### is_polynomial/1

`is_polynomial(Polynomial p)`

Il predicato `is_polynomial` è vero quando il `Polynomial p` passato è effettivamente un termine di tipo `Polynomial` ben formato e matematicamente corretto. Viene controllato in particolare che tutti i `Monomials` di `Polynomial p` siano monomi (tramite `is_monomial`).

Ad esempio:

* `is_polynomial(poly([m(1, 1, [v(1, a)]), m(1, 2, [v(1, a), v(1, b)]), m(1, 2, [v(1, a), v(1, c)]), m(1, 2, [v(2, a)])])).` darà come risultato `true.`
* `is_polynomial([m(1, 1, [v(1, a)]), m(1, 2, [v(1, a), v(1, b)]), m(1, 2, [v(1, a), v(1, c)]), m(1, 2, [v(2, a)])]).` avrà come risultato `false.`

### to_polynomial/2

`to_polynomial(GenericPolynomial g, Polynomial p)`

Il predicato `to_polynomial` è vero quando `p` è il `Polynomial` associato a `GenericPolynomial`. Viene usato quindi per fare il "casting" da `GenericPolynomial` a `Polynomial`.

Ad esempio:

* `to_polynomial(z*f*q^3,poly([m(1, 5, [v(1, f), v(3, q), v(1, z)])])).` risulterà `true.`
* `to_polynomial(poly([m(1, 5, [v(1, f), v(3, q), v(1, z)])]),poly([m(1, 5, [v(1, f), v(3, q), v(1, z)])])).` risulterà `true.`
* `to_polynomial(m(1, 5, [v(1, f), v(3, q), v(1, z)]),poly([m(1, 5, [v(1, f), v(3, q), v(1, z)])])).` risulterà `true.`


### pprint_polynomial/1

`pprint_polynomial(GenericPolynomial g)`

Il preditcato `pprint_polynomial` stampa su Standard Output la rappresentazione "grafica" del `GenericPolynomial g`.

Ad esempio:

* `pprint_polynomial(poly([m(314, 9, [v(8, g), v(1, r)]), m(1, 9, [v(9, q)])])).` stamperà a video `314*g^8*r + q^9`
* `pprint_polynomial(poly([m(1, 2, [v(2, x)])])).` stamperà a video `x^2`

### pprint_monomial/1

`pprint_monomial(Monomial m)`

Il preditcato `pprint_monomial` stampa su Standard Output la rappresentazione "grafica" del `Monomial m`.

Ad esempio:

* `pprint_monomial(m(314, 9, [v(8, g), v(1, r)])).` stamperà a video ` + 314*g^8*r`
* `pprint_monomial(m(1, 9, [v(9, q)])).` stamperà a video ` + q^9`
* `pprint_monomial(m(1, 2, [v(2, x)])).` stamperà a video ` + x^2`

### polyplus/3

`polyplus(GenericPolynomial g1, GenericPolynomial g2, Polynomial r)`

Il predicato `polyplus/3` è vero quando `r` è il polinomio risultante dalla somma di `g1` e `g2`.

Il predicato NON è invertibile.

Ad esempio:

* `polyplus(poly([m(3, 2, [v(1, f), v(1, x)]), m(1, 5, [v(4, x), v(1, y)])]), poly([m(3, 2, [v(1, f), v(1, x)]), m(1, 7, [v(4, x), v(3, y)])]), poly([m(6, 2, [v(1, f), v(1, x)]), m(1, 5, [v(4, x), v(1, y)]), m(1, 7, [v(4, x), v(3, y)])])).` verrà dimostrata

### polyminus/3

`polyminus(GenericPolynomial g1, GenericPolynomial g2, Polynomial r)`

Il predicato `polyminus/3` è vero quando `r` è il polinomio risultante dalla differenza di `g1` e `g2`.

Il predicato NON è invertibile.

Ad esempio:

* `polyminus(poly([m(3, 2, [v(1, f), v(1, x)]), m(1, 5, [v(4, x), v(1, y)])]), poly([m(3, 2, [v(1, f), v(1, x)]), m(1, 7, [v(4, x), v(3, y)])]), poly([m(1, 5, [v(4, x), v(1, y)]), m(-1, 7, [v(4, x), v(3, y)])])).` verrà dimostrata

### polytimes/3

`polytimes(GenericPolynomial g1, GenericPolynomial g2, Polynomial r)`

Il predicato `polytimes/3` è vero quando `r` è il polinomio risultante dalla moltiplicazione di `g1` e `g2`.

Il predicato NON è invertibile.

Ad esempio:

* `polytimes(poly([m(3, 2, [v(1, f), v(1, x)]), m(1, 5, [v(4, x), v(1, y)])]), poly([m(3, 2, [v(1, f), v(1, x)]), m(1, 7, [v(4, x), v(3, y)])]), poly([m(9, 4, [v(2, f), v(2, x)]), m(3, 7, [v(1, f), v(5, x), v(1, y)]), m(3, 9, [v(1, f), v(5, x), v(3, y)]), m(1, 12, [v(8, x), v(4, y)])])).` verrà dimostrata

### polyval/3

`polyval(GenericPolynomial p, NumberList v, Number r)`

Il predicato `polyval/3` è vero quando `r` è il numero risultante dalla valutazione del polinomio `p` valutato nei punti espressi dalla lista di numeri `v`. `v` deve contenere un valore per ogni variabile ottenuta dal predicato `variables/2`.

Il predicato NON è invertibile.

Ad esempio:

* `polyval(poly([m(1, 3, [v(3, y)])]), [2], 8).` verrà dimostrata
* `polyval(poly([m(1, 2, [v(2, x)]), m(-1, 2, [v(2, y)])]),  [12, 12], 0).` verrà dimostrata

### maxdegree/2

`maxdegree(GenericPolynomial p, Number md)`

Il predicato `maxdegree/2` è vero quando `md` è il grado più alto tra quello di tutti i `Monomial` di `p`.

Ad esempio:

* `maxdegree(poly([m(3, 0, []), m(1, 1, [v(1, c)]), m(1, 2, [v(2, q)]), m(1, 3, [v(3, x)])]),3).` ritornerà `true`
* `maxdegree(poly([m(1, 2, [v(2, q)]), m(1, 3, [v(3, x)]), m(1, 6, [v(6, y)])]),6).` ritornerà `true`

### mindegree/2

`mindegree(GenericPolynomial p, Number md)`

Il predicato `mindegree/2` è vero quando `md` è il grado più basso tra quello di tutti i `Monomial` di `p`.

Ad esempio:

* `mindegree(poly([m(3, 0, []), m(1, 1, [v(1, c)]), m(1, 2, [v(2, q)]), m(1, 3, [v(3, x)])]),0).` ritornerà `true`
* `mindegree(poly([m(1, 2, [v(2, q)]), m(1, 3, [v(3, x)]), m(1, 6, [v(6, y)])]),2).` ritornerà `true`

### coefficients/2

`coefficients(GenericPolynomial p, NumberList n)`

Il predicato `coefficients/2` è vero quando `n` è la lista dei coefficienti dei `Monomial` di `p`.

Il predicato NON è invertibile.

Ad esempio:

* `coefficients(f+g+f*g, [1, 1, 1]).` risulterà `true.`
* `coefficients(f+g+f*g+40, F).` risulterà `F = [40, 1, 1, 1].`

### variables/2

`variables(GenericPolynomial p, SymbolList s)`

Il predicato `variables/2` è vero quando `s` è la lista delle variabili dei `Monomial` di `p`.

Il predicato NON è invertibile.

Ad esempio:

* `variables(f+g+f*g,[f, g]).` risulterà `true.`
* `variables(poly([m(1, 1, [v(1, f)]), m(1, 1, [v(1, g)]), m(1, 2, [v(1, f), v(1, g)])]),[f, g]).` risulterà `true.`

### monomials/2

`monomials(GenericPolynomial p, MonomialList m)`

Il predicato `monomials/2` è vero quando `m` è la lista dei `Monomial` di `p`.

Ad esempio:

* Il predicato `monomials(poly([m(2, 4, [v(4, t)]), m(1, 5, [v(1, g), v(4, k)])]), [m(2, 4, [v(4, t)]), m(1, 5, [v(1, g), v(4, k)])]).` verrà dimostrato

## Altro

* Il codice è stato messo sotto version control tramite `git`
* Il codice è stato testato (anche) automaticamente usando il sistema di unit testing integrato a SWIPL
* L'esecuzione, compilazione, testing del progetto è stato gestito tramite un Makefile
    * Il Makefile si occupava anche di costruire il file mvpoli.pl a partire da una serie di file più piccoli, uno per ogni predicato diverso
