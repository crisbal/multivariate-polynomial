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
* passare espressioni non parsate (e anche monomi) a tutte le funzioni che per specifica accettano oggetti `Polynomial`
    * `polyplus(3*x, m(1,0,[]), R)` è valida e valutata correttamente
* passare da oggetto `Polynomial`/`Monomial` ad espressione nelle funzioni `as_monomial`/`as_polynomial`
    * richiamando `as_polynomial(E, poly([m(5, 1, [v(1, x)]), m(1, 1, [v(1, z)]), m(1, 3, [v(3, q)])]))` il programma farà il procedimento inverso ed `E = 5*x+z+q^3`
    * siamo a conoscenza che un particolare oggetto `Polynomial` potrebbe essere originato da infinite espressioni, ma quella proposta è la "minima" per generarlo.

## Implementazione

Alcune note sull'implementazione:

* Generalmente è stato usato il "pattern" (se cosi' lo si vuole chiamare) dell'avere una funzione wrapper (che è quella che poi viene "esposta" all'esterno) che richiama una funzione worker che effettivamente fa il lavoro richiesto. Questo perchè la funzione wrapper attua generalmente controlli e/o perchè la funzione worker è ricorsiva e pe semplicità vengono passati solo i parametri strettamente necessari.
* Per ordinare i monomi e le varpower è stato usato `predsort` definendo delle funzioni di comparazione. Usare predsort ci permette di ordinare i monomi e i polinomi secondo il criterio richiesto e allo stesso momento non ci vincola troppo nel caso dovesse cambiare la specifica dell'ordinamento.
* le funzioni delle operazioni e di utility controllano esplicitamente che i parametri siano `nonvar` quando impossibile fare altro. Ad esempio la chiamata di `polyplus(A, B, poly([m(1, 0, [])]))` produrrà `false` perchè è impossibile ricondursi al valore di A e di B.

## Esempi di utilizzo