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

% Acha a melhor jogada direta, chama o max para os adjacentes
% isto é, o 'primeiro max'.
jogada_plausivel_poda_ab(T,J,A,_):-
    assertz(melhor_jogada(T,-900000)),
    adjacente(T,J,T2),
    oponente(J,J2),
    A2 is A + 1,
    assertz(pilha_alpha_beta([[-90000,90000]])), %Alpha/Beta inicial -Infinito +Infinito
    max(T2,J2,A2,F),
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

fae([PA,TA,TB,PB],b,F):-        %A jogou


fae([PA,TA,TB,PB],a,F):-        %B jogou


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

fae([PA,TA,TB,PB],a,F):- %B jogou


fae([PA,TA,TB,PB],b,F):- %A jogou


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
