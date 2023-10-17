_________________________________________

       PROGETTO PER IL CORSO DI
LINGUAGGI DI PROGRAMMAZIONE E COMPILATORI
           A.A. 2004-2005
            "C DIAMANTE"
_________________________________________
________
      
 INDICE
________


1. Moduli e funzioni del progetto
2. Note implementative
3. Estensioni implementate
4. Note sulle estensioni implementate
5. Invocare il compilatore
6. Testing




_________________________________

1. Moduli e funzioni del progetto
_________________________________


ast.ml:
file dell'albero della sintassi astratta

lexer.mll:
file per la generazione dell'analizzatore sintattico

parser.mly:
file per la gestione del parser

type.ml:
type checker comprende le seguenti funzioni:

	- typeof:
	restituisce il tipo di una o due espressioni

	- typeof_aexpr:
	restituisce il tipo di una espressione atomica

	- prom:
	gestisce le promozioni di tipo

	- prom_log:
	restituisce il tipo booleano solo se 

	- wfs:
	controlla i tipi e in caso di errore di tipo genera i relativi raise Type_error

	- wfs2:
	esamina i tipi del corpo delle funzioni e delle funzioni esterne


compilatore.ml:
esegue la compilazione vera e propria e comprende le seguenti funzioni:

	- conversione_tipi:
	converte tipo Ast in tipi JVM

	- conversione_tipi2:
	converte tipo Ast in tipi JVM

	- compila_gdecl:
	funzione principale di compilazione funzioni chiamata dal main.ml

	- compila_comando:
	genera il codice relativo ad i comandi
 
	- tipo_coppia:
	restituisce il tipo di una coppia mista di Int-Float nel caso di operazioni binarie. E' invocata da compila_expr 

	- compila_e_converti:
	date due espressioni di tipo misto Int-Float, chiama la compila_expr su entrambe con le dovute Convert al momento giusto 

	- compila_condizione:
	genera il codice relativo ad un if-else

	- compila_expr:
	genera il codice relativo per ogni espressione contenuta negli stmt

	- compila_aexpr:
	genera il codice per espressioni atomiche, ossia per variabili e array


main.ml:
come prima cosa esegue dei controlli (se il file da compilare esiste, se ha la giusta estensione, se la sintassi da prompt è corretta). In caso di errore il programma termina dando un avviso di ciò che è accaduto.
Se non ci sono errori viene invocato il parser, il type checker e quindi il compilatore.
Una volta eseguita la compilazione, viene generato il file .class dal file .j appena creato invocando jasmin.





______________________

2. Note implementative
______________________

...

__________________________

3. Estensioni implementate
__________________________


6.1.1: Arricchire la sintassi del linguaggio con il costrutto iterativo for
6.1.2: Arricchire la sintassi del linguaggio con il costrutto iterativo while
6.1.4: Arricchire la sintassi del linguaggio con il comando switch
6.4.2: Arricchire il linguaggio con gli operatori -- e ++ (sia le varianti prefisse che quelle postfisse e trattarli come comandi).





_____________________________________

4. Note sulle estensioni implementate
_____________________________________

Per l'implementazione di tutte le estensioni abbiamo aggiunto nuove regole di parsing.

In particolare per quanto riguarda l'estensione 6.4.2, abbiamo aggiunto delle regole di parsing per la modifica della semantica. Al posto di agire sul compilatore abbiamo preferito generare delle regole che interpretassero i comandi ++ e -- come degli assegnamenti rispettivamente di +1 e -1 rispetto all'espressione da valutare (identificatore o array).






__________________________

5. Invocare il compilatore
__________________________


Per invocare il compilatore e compilare un file sorgente usare la seguente sintassi:
./compila <nomefile>.c
Verrà generato il file <nomefile>.j e da questo il file <nomefile>.class

Per l'esecuzione a questo punto basta:
java <nomefile> 





__________

6. Testing
__________


Nella cartella del progetto sono presenti alcuni sorgenti c utilizzati per testare il compilatore.
Sono i seguenti:

- test_array.c:
testa il funzionamento del compilatore per quanto riguarda gli array, i cicli for (estensione 6.1.1) e testa l'estensione 6.4.2

- test_fact.c:
testa la chiamata a funzioni e il comportamento con gli array

- test_qsort.c:
testa la chiamata a funzioni ed a funzioni esterne e il comportamento con gli array

- test_switch.c:
testa l'estensione 6.1.4
                              
