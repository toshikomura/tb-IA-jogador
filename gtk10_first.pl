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

% Principal, jogada plausível por poda alpha-beta
plausivel(Tabuleiro,Jogador,Altura,Proximo):-
    assertz(eh_maquina(Jogador)),
    assertz(altura_maxima(Altura)),
    jogada_plausivel_poda_ab(Tabuleiro,Jogador,0,Proximo),
    melhor_jogada(T,F),
    retract(melhor_jogada(T,F)),
    retract(eh_maquina(Jogador)),
    retract(altura_maxima(Altura)),
    !.

% Acha a melhor jogada direta, chama o min para os adjacentes
% isto é o primeiro MAX.
jogada_plausivel_poda_ab(T,J,A,_):-
    assertz(melhor_jogada(T,-900000)),
    adjacente(T,J,T2),
    oponente(J,J2),
    A2 is A + 1,
    assertz(pilha_alpha_beta([[-90000,90000]])), %Alpha/Beta inicial -Infinito +Infinito
    min(T2,J2,A2,F),
    insere_melhor_jogada(T2,F),
    pilha_alpha_beta(X),
    retract(pilha_alpha_beta(X)),
    fail.

% Melhor jogada foi definida
jogada_plausivel_poda_ab(_,_,_,P):-
    melhor_jogada(P,_),
    !.

% max()
max(T,J,A,F):-                      %Altura máxima
    altura_maxima(A),
    fae(T,J,F),
    remove_pilha_alpha_beta(_),
    !.

max([PoteA,TA,TB,PoteB],J,_,F):-    %Fim de jogo
    PoteA >= 12,
    fae([PoteA,TA,TB,PoteB],J,F),
    remove_pilha_alpha_beta(_),
    !;
    PoteB >= 12,
    fae([PoteA,TA,TB,PoteB],J,F),
    remove_pilha_alpha_beta(_),
    !.

max(Tabuleiro,Jogador,Altura,FAE):-
    adjacente(Tabuleiro,Jogador,Tabuleiro2),
    oponente(Jogador,Jogador2),
    Altura2 is Altura + 1,
    pilha_alpha_beta([[Alpha,Beta]|_]),
    insere_pilha_alpha_beta([Alpha,Beta]),
    min(Tabuleiro2,Jogador2,Altura2,FAE2),  %Recursão min()
    maior(Alpha,FAE2,Alpha2),
    remove_pilha_alpha_beta(_),
    insere_pilha_alpha_beta([Alpha2,Beta]),
    Alpha2 >= Beta,                         %Poda
    FAE is Alpha2,
    remove_pilha_alpha_beta(_),
    !.

max(_,_,_,F):-                              %Max definido
    pilha_alpha_beta([[Alpha,_]|_]),
    F is Alpha,
    remove_pilha_alpha_beta(_),
    !.

% min()
min(T,J,A,F):-                      %Altura máxima
    altura_maxima(A),
    fae(T,J,F),
    remove_pilha_alpha_beta(_),
    !.

min([PoteA,TA,TB,PoteB],J,_,F):-    %Fim de jogo
    PoteA >= 12,
    fae([PoteA,TA,TB,PoteB],J,F),
    remove_pilha_alpha_beta(_),
    !;
    PoteB >= 12,
    fae([PoteA,TA,TB,PoteB],J,F),
    remove_pilha_alpha_beta(_),
    !.

min(Tabuleiro,Jogador,Altura,FAE):-
    adjacente(Tabuleiro,Jogador,Tabuleiro2),
    oponente(Jogador,Jogador2),
    Altura2 is Altura + 1,
    pilha_alpha_beta([[Alpha,Beta]|_]),
    insere_pilha_alpha_beta([Alpha,Beta]),
    max(Tabuleiro2,Jogador2,Altura2,FAE2),  %Recursão max()
    menor(Beta,FAE2,Beta2),
    remove_pilha_alpha_beta(_),
    insere_pilha_alpha_beta([Alpha,Beta2]),
    Alpha >= Beta2,                         %Poda
    FAE is Beta2,
    remove_pilha_alpha_beta(_),
    !.

min(_,_,_,F):-                          %Min definido
    pilha_alpha_beta([[_,Beta]|_]),
    F is Beta,
    remove_pilha_alpha_beta(_),
    !.

% Aqui A é maquina
fae([PA,_,_,_],_,F):-
    eh_maquina(a),
    PA >= 12,
    F is 90000,
    !.

fae([_,_,_,PB],_,F):-
    eh_maquina(a),
    PB >= 12,
    F is -90000,
    !.

fae([PA,TA,TB,PB],b,F).        %A jogou


fae([PA,TA,TB,PB],a,F).        %B jogou


% Aqui B é maquina
fae([PA,_,_,_],_,F):-
    eh_maquina(b),
    PA >= 12,
    F is -90000,
    !.

fae([_,_,_,PB],_,F):-
    eh_maquina(b),
    PB >= 12,
    F is 90000,
    !.

fae([PA,TA,TB,PB],a,F). %B jogou


fae([PA,TA,TB,PB],b,F). %A jogou


% Predicados suporte
insere_melhor_jogada(T,F):-
    melhor_jogada(T2,F2),
    F > F2,
    retract(melhor_jogada(T2,F2)),
    assertz(melhor_jogada(T,F)),
    !.

insere_melhor_jogada(T,F):-
    F == 90000,
    eh_maquina(J),
    fae(T,J,F),
    melhor_jogada(T2,F2),
    retract(melhor_jogada(T2,F2)),
    assertz(melhor_jogada(T,F)),
    !.

insere_melhor_jogada(_,_):-
    !.

insere_pilha_alpha_beta(V):-
    !,
    pilha_alpha_beta(X),
    retract(pilha_alpha_beta(X)),
    assertz(pilha_alpha_beta([V|X])).

remove_pilha_alpha_beta(_):-
    !,
    pilha_alpha_beta([X|Y]),
    retract(pilha_alpha_beta([X|Y])),
    assertz(pilha_alpha_beta(Y)).

maior(X,Y,X):-
    X >= Y,
    !.
maior(_,Y,Y):-
    !.
menor(X,Y,X):-
    X =< Y,
    !.
menor(_,Y,Y):-
    !.

oponente(a,b):-
    !.
oponente(b,a):-
    !.

:- dynamic eh_maquina/1.
:- dynamic altura_maxima/1.
:- dynamic melhor_jogada/2.
:- dynamic pilha_alpha_beta/1.
