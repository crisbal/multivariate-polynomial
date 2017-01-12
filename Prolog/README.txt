# multivariate-polynomial - Prolog

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

* Generalmente è stato usato il "pattern" (se cosi' lo si vuole chiamare) dell'avere un predicato wrapper (che è quella che poi viene "esposta" all'esterno) che richiama un predicato worker che effettivamente fa il lavoro richiesto. Questo perchè il wrapper attua generalmente controlli e/o perchè il worker è ricorsivo e/o per semplicità vengono passati solo i parametri strettamente necessari.
* Per ordinare i monomi e le varpower è stato usato `predsort` definendo dei predicati di comparazione. Usare predsort ci permette di ordinare i monomi e i polinomi secondo il criterio richiesto e allo stesso momento non ci vincola troppo nel caso dovesse cambiare la specifica dell'ordinamento.
* i predicati delle operazioni e di utility controllano esplicitamente che i parametri siano `nonvar`, dove ovviamente non è possibile fare chiamate "inverse". Ad esempio la chiamata di `polyplus(A, B, poly([m(1, 0, [])]))` produrrà `false` perchè è impossibile ricondursi al valore di A e di B.

## Documentazione "tipi"

Per comprendere meglio la lettura della documentazione e la comprensione del programma definiamo di seguito dei "tipi" di oggetti, che torneranno utili più sotto per la descrizione della firma dei predicati implementati. 
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

* `GenericPolynomial` è usato nei predicati che possono accettare indistintamente come tipo di parametro: `Monomial`, `Polynomial`, `PolynomialExpression`.
* `GenericPolynomial` viene sempre trasformato in `Polynomial` tramite il predicato `to_polynomial/2`  

## Documentazione predicati

Sono stati descritti anche predicati che, pur non essendo esplicitamente richiesti, sono stati aggiunti per completezza e per facilitare il lavoro e che si sono poi rivelati utili anche lato utente.

Tutti i predicati controllano, dove possibile che i parametri di input siano del "tipo" corretto. 

### as_monomial/2

`as_monomial(MonomialExpression me, Monomial m)`

Il predicato `as_monomial` è vero quando `Monomial m` è il termine che rappresenta il monomio risultante dal parsing dell’espressione `MonomialExpression me`

Il predicato è invertibile.

### as_polynomial/2

`as_polynomial(PolynomialExpression pe, Polynomial p)`

Il predicato `as_polynomial` è vero quando `Polynomial p` è il termine che rappresenta il monomio risultante dal parsing dell’espressione `PolynomialExpression pe`

Il predicato è invertibile.

### is_monomial/1

`is_monomial(Monomial m)`

Il predicato `is_monomial` è vero quando il `Monomial m` passato è effettivamente un termine di tipo `Monomial` ben formato e matematicamente corretto.

### is_polynomial/1

`is_polynomial(Polynomial p)`

Il predicato `is_polynomial` è vero quando il `Polynomial p` passato è effettivamente un termine di tipo `Polynomial` ben formato e matematicamente corretto. Viene controllato in particolare che tutti i `Monomials` di `Polynomial p` siano monomi (tramite `is_monomial`).


### to_polynomial/2

`to_polynomial(GenericPolynomial g, Polynomial p)`

Il predicato `to_polynomial` è vero quando `p` è il `Polynomial` associato a `GenericPolynomial`. Viene usato quindi per fare il "casting" da `GenericPolynomial` a `Polynomial`.

### pprint_polynomial/1

`pprint_polynomial(GenericPolynomial g)`

Il preditcato `pprint_polynomial` stampa su Standard Output la rappresentazione "grafica" del `GenericPolynomial g`.

### pprint_monomial/1

`pprint_monomial(Monomial m)`

Il preditcato `pprint_monomial` stampa su Standard Output la rappresentazione "grafica" del `Monomial m`.

### polyplus/3

`polyplus(GenericPolynomial g1, GenericPolynomial g2, Polynomial r)`

Il predicato `polyplus/3` è vero quando `r` è il polinomio risultante dalla somma di `g1` e `g2`.

Il predicato NON è invertibile.

### polyminus/3

`polyminus(GenericPolynomial g1, GenericPolynomial g2, Polynomial r)`

Il predicato `polyminus/3` è vero quando `r` è il polinomio risultante dalla differenza di `g1` e `g2`.

Il predicato NON è invertibile.

### polytimes/3

`polytimes(GenericPolynomial g1, GenericPolynomial g2, Polynomial r)`

Il predicato `polytimes/3` è vero quando `r` è il polinomio risultante dalla moltiplicazione di `g1` e `g2`.

Il predicato NON è invertibile.

### polyval/3

`polyval(GenericPolynomial p, NumberList v, Number r)`

Il predicato `polyval/3` è vero quando `r` è il numero risultante dalla valutazione del polinomio `p` valutato nei punti espressi dalla lista di numeri `v`. `v` deve contenere un valore per ogni variabile ottenuta dal predicato `variables/2`.

Il predicato NON è invertibile.

### maxdegree/2

`maxdegree(GenericPolynomial p, Number md)`

Il predicato `maxdegree/2` è vero quando `md` è il grado più alto tra quello di tutti i `Monomial` di `p`.

### mindegree/2

`mindegree(GenericPolynomial p, Number md)`

Il predicato `mindegree/2` è vero quando `md` è il grado più basso tra quello di tutti i `Monomial` di `p`.

### coefficients/2

`coefficients(GenericPolynomial p, NumberList n)`

Il predicato `coefficients/2` è vero quando `n` è la lista dei coefficienti dei `Monomial` di `p`. 

Il predicato NON è invertibile.

### variables/2

`variables(GenericPolynomial p, SymbolList s)`

Il predicato `variables/2` è vero quando `s` è la lista delle variabili dei `Monomial` di `p`.

Il predicato NON è invertibile.

### monomials/2

`monomials(GenericPolynomial p, MonomialList m)`

Il predicato `monomials/2` è vero quando `m` è la lista dei `Monomial` di `p`.