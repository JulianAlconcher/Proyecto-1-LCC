:- module(init, [ init/2 ]).

/**
 * init(-Grid, -NumOfColumns).
 * 
 * Predicado especificando la grilla inicial, que será mostrada al comienzo del juego, donde
 * Grid es una lista con los números que conforman la grilla, y NumOfColumns es la cantidad de columnas, 
 * determinando las dimensiones de la misma.
 */

 init([
   2,2,64,8,16,
	2,4,32,2,32,
	2,8,2,2,2,
	2,16,2,8,4,
	2,4,2,4,2,
	2,16,2,8,2,
	2,8,2,4,2,
	4,2,4,2,4
   ], 5).