# multivariate-polynomial - Lisp

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
* è stata usata la funzione/macro `progn` solo per le funzioni di `pprint` dove un approccio "imperativo" era forse più adatto
* si è cercato di tenere le funzioni quanto più semplici e atomiche possibili
* si è usato `error` dove necessario (quasi esclusivamente per stampare errori nel parsing dei polinomi/monomi)
* il codice è stato scritto ed indentato con emacs, usando spazi

## Esempi di utilizzo