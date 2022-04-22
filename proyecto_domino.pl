%ITAM
%Proyecto de programación 2 Inteligencia Artificial
%Integrantes:
%-Lucía Lizardi
%-Alejandro Bermudez
%-Rogelio Torres
%-Ana Cristina Sanchez

%dynamic predicate indicators
/**
Al usar dynamic, le decimos a prolog que la definición de los
predicados cambiará con el tiempo.
"A dynamic predicate is introduced using dynamic/1,
after which clauses may be added using assertz/1. " De documentación en línea de prolog

Assert: Nos permite agregar hechos o reglas a la base de conocimientos
dinámicamente.

Retract: Nos permite borrar hechos y reglas de la base de conocimientos
dinámicamente.
**/
:-dynamic myGame/1.              %Guarda y actualiza nuestras fichas.
:-dynamic dominoes/1.            %Guarda y actualiza las fichas de dominó que se pueden jugar y se van jugando.
:-dynamic myCounter/1.           %Guarda y actualiza cuantas fichas tenemos.
:-dynamic vsCounter/1.           %Guarda y actualiza cuantas fichas tiene el contrincante.
:-dynamic leftSide/1.            %Guarda y actualiza el valor del lado izquierdo del juego/tablero.
:-dynamic rightSide/1.           %Guarda y actualiza el valor del lado derecho del juego/tablero.
:-dynamic dominoesGroup/1.       %Guarda y actualiza el valor de los grupos de fichas de dominó. Hay 7 grupos (0-6) y cada grupo tiene 8 fichas.
:-dynamic pozoCounter/1.         %Guarda y actualiza el valor de fichas que hay en el pozo.
:-dynamic vsFails/1.             %Guarda y actualiza el valor de las fallas de nuestro contrincante (No tiene ficha y pasa)
:-dynamic myPossibleMoves/1.     %Guarda y actualiza el valor de los movimientos que podemos hacer, cuando nuestras fichas coinciden con los extremos del tablero
:-dynamic myMove/1.              %Guarda y actualiza el valor de mis movimientos.
:-dynamic board/1.               %Guarda y actualiza el valor del tablero.

%myCounterR(i), myCounter(o)
myCounter(7).                    %Contador de mis fichas
%vsCounterR(i), vsCounter(o)
vsCounter(7).                    %Contador de las fichas del oponente
%pozoCounterR(i), pozoCounter(o)
pozoCounter(14).                 %Contador de las fichas que hay en el pozo
%myGame(i), myGame(o)
myGame([]).                      %Llista para guardar las fichas que me tocan y voy ocupando en el juego, se agregarán si tengo que comer del pozo.
dominoesGroup([7,7,7,7,7,7,7]).  %Todos los grupos de dominó empiezan con 7 fichas que no se han utilizado.
vsFails([]).                     %Al principio no conocemos los fallos del contrincante
dominoes([[0, 0],                %Tenemos la lista de sublistas con todas las fichas de dominó que nos puedan tocar.
            [1, 0],[1, 1],
            [2, 0],[2, 1],[2, 2],
            [3, 0],[3, 1],[3, 2],[3, 3],
            [4, 0],[4, 1],[4, 2],[4, 3],[4, 4],
            [5, 0],[5, 1],[5, 2],[5, 3],[5, 4],[5, 5],
            [6, 0],[6, 1],[6, 2],[6, 3],[6, 4],[6, 5],[6, 6]]).

%game_start
/**
Regla que nos ayuda cuando empezamos el juego.
Al inicio de cada juego tenemos que decirle a dwight las 7 fichas iniciales que agarramos.
**/
game_start():-
    %write("Escribe las 7 fichas que nos tocaron con formato [numA|numB]. y escribe done. cuando acabes!"),nl, %Pregunta por las fichas
    write("Write our first 7 dominoes in this format: [numberA,numberB]. and write done when you're done!"),nl, %Pregunta por las fichas
    again,                                                       %Nos ayuda a poner un punto de retroceso. Esto se repetirá hasta que la ficha que le digamos done.
    read(DominoX),                                               %Leemos las fichas de dominó que nos tocarón
    myGame(X),                                                   %Vemos que tenemos en nuestro juego
    %append(i,i,o)
    append(X,[DominoX],Y),                                       %Nos regresa la concatenación de X, DominoX. Es la lista que tiene mis fichas de juego, mas la nueva ficha.
    %retract(i)
    retract(myGame(X)),                                          %Borramos de la lista el valor de X(lista antes dee meter la nueva ficha)
   %assert(i)
    assert(myGame(Y)),                                           %Introducimos el valor de la nueva lista Y con la nueva ficha que tomamos
    %dominoes(o)
    dominoes(DOMINO),                                            %Pedimos que nos traiga toda la lista de fichas que hay actualmente
    delete(DOMINO,DominoX,NEWDOMINO),                            %Borra elementos que hagan match en la lista ya que tendremos conocimiento de nuestras fichas
    retract(dominoes(DOMINO)),                                   %Borramos de la lista el valor de DOMINO.
    assert(dominoes(NEWDOMINO)),                                 %Introducimos el valor de las fichas menos las que ya conocemos que tenemos.
    write(my_pieces:(Y)),nl,
    DominoX==done,!.                                             %Mientras el usuario no escriba done. Preguntaremos cuántas fichas tenemos. Cada vez que esta clausula falla, regresamos a read.

%again: Una forma de poner puntos de retroceso.
again.
again:-
    again.

%First_move
/*First Move: Nos permite actualizar la primera ficha del juego del tablero. Poniendo del lado izquierdo del tablero A y derecho B */
first_move([A|TailA]):-
     getTail(TailA,B),
     assert(rightSide(B)),
     assert(leftSide(A)).

%getTail(i,o)
/*Regresa la cola de cualquier lista*/
getTail([A|_],B):-
    B is A.
getTail2([_|A],B):-
    B is A.

%modifyGroup()
/**
Esta regla decremente el número de elementos que guardamos en la lista dominoesGroup
**/
modifyGroup(G):-
    dominoesGroup(Y),                                           %Qué hay en los grupos de dominó?
    %nth(o,i,o)
    %nth0(?Index, ?List, ?Elem) -> Verdadero cuando Elem está en el índice I de la lista
    nth0(G,Y,Z),                                                %Z está en el índice G de la lista?
    I is G+1,                                                   %Aumento mi índice
    %nth1()
    nth1(I,Y,_,W),
    A is Z-1,
    nth0(G,B,A,W),
    retract(dominoesGroup(Y)),
    assert(dominoesGroup(B)).

/*Turno del oponente
Regla que analiza el turno del oponente
*/
oponentMove:-
    myCounter(C),                                               %Checo cuántas fichas tengo.
    C==0,                                                       %Si ya no tengo fichas, ganamos el juego.
    write("We won!!!!!!"),!.
oponentMove:-
    write("Our oponent had dominoes to play? yes/no"),nl,       %Pregunta si el oponente pudo jugar (tirar fichas).
    read(Answ),
    Answ==yes,                                                  %Si el jugador tiró alguna ficha....
    write("What domino did he/she play?"),nl,                   %Qué ficha tiró
    read(DominoX),
    dominoes(DOMINO),                                           %Pedimos que nos traiga toda la lista de fichas que hay actualmente
    delete(DOMINO,DominoX,NEWDOMINO),                           %Borra elementos que hagan match en la lista ya que tendremos conocimiento nuevo de la ficha tirada por el oponente
    retract(dominoes(DOMINO)),                                  %Borramos de la lista de valores de las fichas totales el valor de DOMINO (la ficha del oponente).
    assert(dominoes(NEWDOMINO)),                                %Introducimos el valor de las fichas menos la que acabamos de conocer del contrincante.
    write("In which side of the board our oponent played? left=1/right=0"),nl, %Tenemos que saber de qué lado se puso la ficha
    read(Side),                                                 %Se lee el lado
    updateSides(DominoX,Side),                                  %Llamamos a la regla que nos actualiza los lados del tablero.
    getTail(DominoX,A),                                         %Pedimos la cola de la ficha DominoX que acaba de tirar el oponente
    getTail2(DominoX,B),
    modifyGroup(A),
    modifyGroup(B),
    vsCounter(X),                                               %Traemos el contador de fichas del contrincante.
    Y is X-1,                                                    %Como logró tirar una ficha, le restamos una al contador.
    retract(vsCounter(X)),                                      %Borramos de la lista que cuenta las fichas del contrincante el valor actual.
    assert(vsCounter(Y)),                                       %Introducimos el valor nuevo de las fichas del contrincante.
    write("oponent has: "),write(Y),write(" pieces left."),nl,
    myMove.                                                     %Nos toca jugar nuestras fichas...
oponentMove:-                                                   %Si el oponente no pudo jugar- i.e tuvo que tomar fichas del pozo...
    write("Looks like the oponent had to grab from the 'pozo' ( ͡° ͜ʖ ͡°)"),nl,
    write("How many dominoes did he/she have to grab?"),nl,     %Preguntamos cuántas fichas tomó
    read(NumPozo),
    pozoCounter(X),                                             %Cuantas fichas hay en el pozo?
    Y is X-NumPozo,                                             %Actualizamos el número de fichas que hay en el pozo restándole las que tomó el contrincante
    vsCounter(V),                                               %Checamos la cantidad de fichas que tiene el contrincante
    N is V+NumPozo,                                             %El nuevo número de fichas que tiene el contrincante es el que teníamos mas las que agarró del pozo
    retract(vsCounter(V)),                                      %actualizamos el valor del # de fichas del contrincante
    assert(vsCounter(N)),                                       %...
    retract(pozoCounter(X)),                                    %actualizamos el # de fichas que hay en el pozo
    assert(pozoCounter(Y)),                                     %...
    rightSide(RightS),                                          %Preguntamos qué número hay en el lado derecho ya que es un fallo de contrincante y lo queremos guardar
    leftSide(LeftS),                                            %Preguntamoe qué número hay en el lado izquierdo del juego ya que es fallo del contrincante y queremos guardar el dato
    vsFails(F),                                                 %Traemos la lista con los fallos del contrincante
    %append(i,i,o)
    append(F,[LeftS],S),                                        %Le agreramos a la lista el valor del lado derecho e izquierdo que sabemos lo hizo pasar
    append(S, [RightS], Z),                                     %...
    retract(vsFails(F)),                                        %Borramos de la lista con los fallos del contrincante anteriores a los que sabemos ahor
    assert(vsFails(Z)),                                         %Introducimos el valor con la nueva lista de los fallos del contrincante
    pozoCounter(0),                                             %Si el contador del pozo es 0 -> ...
    write("oponent has: "),write(N),write(" pieces left."),nl,
    myMove.                                                     %Nos toca jugar
oponentMove:-                                                   %Si sigue habiendo fichas en el pozo ->...
    oponentMove.                                                %Se hace el proceso recursivamente.

%updateSides(i,i)
/***updateSides es una regla que nos permite actualizar los lados que hay en el juego después de que pusimos alguna ficha.*/
updateSides(DominoX, Side):-
    (Side=1) -> updateLeft1(DominoX);                       %Si le damos el valor de entrada left, entonces actualizará el lado izquierdo del juego.
    updateRight(DominoX).                                   %En otro caso, actualizará el lado derecho

%updateRight(i)
updateRight([A|[TailA]]):-
    rightSide(Right),                                           %Traemos el valor actual del extremo derecho
    Right==A,                                                   %Si el valor del extremo derecho es igual al número A de la ficha [A|B]
    retractall(rightSide(_)),                                   %Entonces se borra lo que hay en el extremo derecho
    retractall(rightSide(_)),                                   %...
    assert(rightSide(TailA)), write("new right: "),write(TailA),nl,!.                                 %El nuevo extremo derecho es el de la cola de la ficha que entró!
updateRight([A|_]):-                                            %Si el valor del extremo derecho no es igual al valor A ... de la ficha [A|B]
    retractall(rightSide(_)),                                   %Entonces se borra lo que hay en el extremo derecho
    retractall(rightSide(_)),                                   %...
    assert(rightSide(A)), write("new right: "),write(A),nl,                                       %El nuevo extremo derecho del juego es A!
    !.

%updateLeft(i)
updateLeft1([A|TailA]):-
    leftSide(Left),
    (A==Left)->retractall(leftSide(_)),getTail(TailA,B),assert(leftSide(B)),write("new left: "),write(B);
    retractall(leftSide(_)),assert(leftSide(A)), write("new left: "),write(A).

%Regla que nos permite utilizar nuestro turno
%myMove
myMove:-
    vsCounter(C),                                                %Checo cuántas fichas tiene el contrincante.
    C==0,                                                        %Si el contrincante ya no tiene fichas, él/ella ganó el juego
    write("The oponent won!! I'll keep learning for the next time!"),nl,!.
myMove:-
    myGame(X),                                                   %Traigo la lista con todas mis fichas actuales
    retractall(myPossibleMoves(_)),                              %
    assert(myPossibleMoves([])),                                 %añado mis movimientos posibles.
    whatToMove(X),                                               %checo qué puedo mover dada mis fichas en mano
    myPossibleMoves([[A|B]|_]),
    dominoSide([A|B],Z),                                         %me dice en qué parte dee tablero voy a tirar
    write("►►►►►►►►►►►►►►►►►"),nl,
    write("We played: "), write([A|B]),nl,
    delete(X,[A|B],M),                                          %borro [A|B] que tengo en la lista y mi nueva lista es M
    retract(myGame(X)),                                         %borro las fichas anteriores que tenía en mano de la lista
    assert(myGame(M)),                                          %mis nuevas fichas en mano son M.
    write("My pieces:"),write((M)),nl,
    updateSides([A|B],Z),                                       %como puse ficha, tengo que actualizar los extremos del juego.
    getTail2([A|B],C),                                          %...
    modifyGroup(A),
    modifyGroup(C),
    %myCounter(C),
    %NC is C-1,
    %retract(myCounter(C)),
    %assert(myCounter(NC)),
    oponentMove.                                                %turno del oponente
myMove:-                                                         %Si no tengo ficha.
    write("Looks like we don't have dominoes to play. We will have to reach out to the 'pozo' (҂◡_◡)"),nl,
    getFromPozo.

%getFromPozo:-
/**Regla que nos da la lógica para cuando tenemos que comer del pozo**/
getFromPozo:-
    pozoCounter(0),                                             %si ya no hay fichas en el pozo, pasamos y es turno del oponente.
    write("We pass!"),nl,
    oponentMove.
getFromPozo:-                                                   %si hay fichas, agarramos ficha una por una.
    write("What domino did we get from the pozo?"),nl,
    read(DominoX),
    myGame(X),                                                  %introducimos en la base de conocimiento de nuestro juego la nueva lista de fichas que tenemos al comer del pozo
    append(X,[DominoX],Y),                                      %...introduzco a la lista la nueva ficha
    retract(myGame(X)),                                         %...borro la lista anteriro
    assert(myGame(Y)),                                          %...me quedo con la lista con la nueva ficha
    dominoes(DOMINO),                                           %traigo la lista de las fichas de dominó que no se han jugado
    delete(DOMINO,DominoX,NEWDOMINO),                           %voy quitando las fichas que he agarrrado de la lista de las fichas que no conozco
    retract(dominoes(DOMINO)),                                  %...
    assert(dominoes(NEWDOMINO)),                                %...
    pozoCounter(P),                                             %decremento el contador de fichas en el pozo
    Q is P-1,                                                   %...
    retract(pozoCounter(P)),                                    %...
    assert(pozoCounter(Q)),                                     %...
    %myCounter(R),
    %S is R+1,
    %retract(myCounter(R)),
    %assert(myCounter(S)),
    myMove.                                                     %intento jugar de nuevo y es la llamada recursiva de mi turno.


%dominoSide(i,i)
dominoSide(X, SideX):-
    rightSide(RightS),                                          %consulto que valor está en el extremo derecho
    member(RightS,X),                                           %si el extremo derecho coincide con el valor X de la lista,
    SideX is 0,                                                 %entonces lo cambiamos a 0
    write("We're playing on the right side. "),nl,              %se juega del lado derecho
    !.
dominoSide(_,SideX):-                                           %si no coinice con el lado derecho
    SideX is 1,                                                 %jugamos del lado izquierdo
    write("We're playing on the left side. "),nl,
    !.

%oponentMoveFirst
/*regla en caso de que el oponente juegue primero*/
oponentMoveFirst:-                                              %si el jugador que sale es el oponente entonces
    write("What domino did the oponent play?"),nl,              %tenemos que saber con qué ficha salió
    read(DominoX),
    dominoes(DOMINO),                                           %actualizamos nuestra lista de fichas que no se han jugado y borramos la que acaba de poner el oponente
    delete(DOMINO,DominoX,NEWDOMINO),                           %...
    retract(dominoes(DOMINO)),                                  %...
    assert(dominoes(NEWDOMINO)),                                %...
    getTail(DominoX,A),getTail2(DominoX,B),
    modifyGroup(A),
    modifyGroup(B),
    vsCounter(X),                                               
    Y is X-1,                                                    
    retract(vsCounter(X)),                                      
    assert(vsCounter(Y)),
    write("new tail: "),write(A),nl,write("new head: "),write(B),nl,
    first_move(DominoX),                                        %nos vamos a la regla de primer movimiento que actualiza por primera vez los extremos del juego.
    myMove,!.                                                   %toca que hagamos nuestro juego.

%whatToMove(i)
whatToMove([]).
whatToMove([H|T]) :-
    rightSide(Y),                                               %Consulta qué hay en el extremo derecho del tablero.
    leftSide(X),                                                %Consulta qué hay en el extremo izquiero del tablero.
    myPossibleMoves(W),                                         %Consulta los movimientos posibles que hay.
    (member(X,H) ; member(Y,H)),                                %Si X está en H, o
    append(W, [H] , Z),                                         %agrego a la nueva lista los valores que puedo jugarr.
    retract(myPossibleMoves(W)),                                %borrro los pasados.
    assert(myPossibleMoves(Z)),                                 %mis nuevos valores por jugar me los dio Z.
    whatToMove(T),!.                                            %lo que puedo mover es la cola de la lista que tengo de mi juego.
whatToMove([_|T]):-
whatToMove(T),!.

%dwight
/**Al llamar a la regla dwight, empieza el juego y mandamos a llamar la funcionalidad de todo el código!**/
dwight:-
    write(" "),nl,
    write("Hello, there! My name is  Dwight! - Domino enthusiast! ☺ "),nl,
    %write("¡Hola! Me llamo Dwight! Me encanta el dominó ☺ "),nl,
    write(" "),nl,
    %write("Fui diseñado por un grupo de estudiantes del ITAM para jugar dominó. ¡Empezemos! "),nl,
    write("I was designed by a group of ITAM students to play dominoes. Let's start! "),nl,
    write(" "),nl,
    game_start,                                                    %Llamamos a la regla game_start para comenzar el juego. Esta nos pedirá las 7 fichas que sacamos.
    myGame(Z),                                                     %reviso cuál es mi juego actua.
    delete(Z,done,Y),                                              %borro "done" de la lista de mijuego actual ya que este se agregó cuando preguntamos las fichas.
    retract(myGame(Z)),                                            %borro mi juego actual (antes de borrar done)
    assert(myGame(Y)),                                             %actualizo mi juego después de que he borrado la palabra done.
    %write("¿Quién tiró la primera ficha? nosotros=nos / oponente=op "),nl, %pregubtamos quien tirró la primera ficha.
    write("Who played the first domino? us/oponent"),nl,
    read(Answer),
    Answer==us,
    write("What domino did we play? "),nl,
    read(DominoX),
    myGame(G),                                                     %tenemos que actualizar las fichas que tenemos en mano
    delete(G,DominoX,N),                                           %...
    retract(myGame(G)),                                            %...
    assert(myGame(N)),                                             %...
    first_move(DominoX),                                           %llamamos a la regla de first_move porque tenemos que actualizar el tablero por primera vez
    getTail(DominoX,A),
    getTail2(DominoX,B),
    modifyGroup(A),
    modifyGroup(B),
    write("new tail: "),write(A),nl,write("new head: "),write(B),nl,
    oponentMove.                                                  %Le toca al oponente
dwight:-
    oponentMoveFirst.

