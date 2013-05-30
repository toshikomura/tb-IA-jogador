:- prolog_language('pop11').
false -> popmemlim;
false -> pop_prolog_lim;
10e7 -> pop_callstack_lim;
true -> popdprecision;
12 -> pop_pr_places;
:- prolog_language('prolog').

concatena( [], L, L).

concatena( [ X | Xcauda], Lista, [ X | Xcauda_2]) :-
    concatena( Xcauda, Lista, Xcauda_2).

adjacente( [ [ PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [ PoteB] ], a, Jogada) :-
    (
    proximo_campo( PoteA, PoteA_prox, BA1, [], [ BA1, BA2, BA3, BA4, BB1, BB2, BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA_prox, Proxima_jogada, PoteB, Saida),
    Jogada is Saida
    );
    (
    proximo_campo( PoteA, PoteA_prox, BA2, [ BA1], [ BA2, BA3, BA4, BB1, BB2, BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA_prox, Proxima_jogada, PoteB, Saida),
    Jogada is Saida
    );
    (
    proximo_campo( PoteA, PoteA_prox, BA3, [ BA1, BA2], [ BA3, BA4, BB1, BB2, BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA_prox, Proxima_jogada, PoteB, Saida),
    Jogada is Saida
    );
    (
    proximo_campo( PoteA, PoteA_prox, BA4, [ BA1, BA2, BA3], [ BA4, BB1, BB2, BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA_prox, Proxima_jogada, PoteB, Saida),
    Jogada is Saida
    ).

adjacente( [ [ PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [ PoteB] ], b, Jogada) :-
    (
    proximo_campo( PoteB, PoteB_prox, BB1, [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA, Proxima_jogada, PoteB_prox, Saida),
    Jogada is Saida
    );
    (
    proximo_campo( PoteB, PoteB_prox, BB2, [ BA1, BA2, BA3, BA4, BB1], [ BB2, BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA, Proxima_jogada, PoteB_prox, Saida),
    Jogada is Saida
    );
    (
    proximo_campo( PoteB, PoteB_prox, BB3, [ BA1, BA2, BA3, BA4, BB1, BB2], [ BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA, Proxima_jogada, PoteB_prox, Saida),
    Jogada is Saida
    );
    (
    proximo_campo( PoteB, PoteB_prox, BB4, [ BA1, BA2, BA3, BA4, BB1, BB2, BB3], [ BB4], Proxima_jogada),
    arruma_saida( PoteA, Proxima_jogada, PoteB_prox, Saida),
    Jogada is Saida
    ).

arruma_saida( PoteA, [ BA1, BA2, BA3, BA4, BB1, BB2, BB3, BB4], PoteB, Saida ) :-
    Saida is [[ PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [ PoteB]].

proximo_campo_r( Pote, Pote, Sementes, Sementes, [], []).

proximo_campo_r( Pote, Pote, 0, 0, X, X).

proximo_campo_r( Pote, Pote_prox, Sementes, Sementes_prox, [ X | Xcauda], [ Y | Ycauda]) :-
    Sementes > 0,
    Sementes_prox_aux is Sementes - 1,
    (
        (X is 1, Y is 0, Pote_prox_aux is Pote + 2, !);
        (Y is X + 1, Pote_prox_aux is Pote)
    ),
    proximo_campo_r( Pote_prox_aux, Pote_prox, Sementes_prox_aux, Sementes_prox, Xcauda, Ycauda), !.

proximo_campo_controle( Pote, Pote, 0, Sementes_prox, X, X).

proximo_campo_controle( Pote, Pote_prox, Sementes, Sementes_prox, X, Y) :-
    Sementes > 0,
    proximo_campo_r( Pote, Pote_prox_aux, Sementes, Sementes_prox_aux, X, Saida),
    proximo_campo_controle( Pote_prox_aux, Pote_prox_aux2, Sementes_prox_aux, Sementes_prox_aux2, Saida, Z),
    Pote_prox is Pote_prox_aux2,
    Y is Z.
%    Sementes_prox is Sementes_prox_aux2.

proximo_campo( Pote, Pote_prox, Sementes, Antes, [X | Xcauda], Y) :-
    Sementes > 0,
    Lista2 is  [ 0 | Lista],
    proximo_campo_r( Pote, Pote_prox_aux, Sementes, Sementes_prox, Xcauda, Lista),
    concatena( Antes, Lista2, Depois),
    proximo_campo_controle( Pote_prox_aux, Pote_prox_aux2, Sementes_prox, Sementes_prox_aux2, Depois, Depois2),
    Pote_prox is Pote_prox_aux2,
    Y is Depois2.

% testes
% campo inicial
% adjacente( [[0], [4,4,4,4], [4,4,4,4], [0] ], a, Saida).
% zero como primeiro parametro
% adjacente( [[0], [0,4,4,4], [4,4,4,4], [0] ], a, Saida).
% ultrapassa o campo
% adjacente( [[0], [7,4,4,4], [4,4,4,4], [0] ], a, Saida).
% adjacente( [[0], [8,4,4,4], [4,4,4,4], [0] ], a, Saida).
% adjacente( [[0], [9,7,2,1], [8,3,1,5], [0] ], a, Saida).
% adjacente( [[0], [12,7,2,1], [8,3,1,9], [0] ], b, Saida).
