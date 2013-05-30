% Principal, jogada plausível por poda alpha-beta
plausivel(Tabuleiro,Jogador,Altura,Proximo):-
    assertz(eh_maquina(Jogador)),
    assertz(altura_maxima(Altura)),
    jogada_plausivel_poda_ab(Tabuleiro,Jogador,0,Proximo),
    melhor_jogada(T,F),
    retract(melhor_jogada(T,F)),
    retract(eh_maquina(Jogador)),
    retract(altura_maxima(Altura)),!.

% Acha a melhor jogada direta, chama o min para os adjacentes
% isto é o primeiro MAX.
jogada_plausivel_poda_ab(T,J,A,_):-
    assertz(melhor_jogada(T,-90000)),
    adjacente(T,J,T2),
    oponente(J,J2),
    A2 is A + 1,
    melhor_jogada(_,Alpha),
    assertz(pilha_alpha_beta([[Alpha,90000]])), %Alpha/Beta inicial
    min(T2,J2,A2,F),            %Recursão min()
    insere_melhor_jogada(T2,F),
    pilha_alpha_beta(X),
    retract(pilha_alpha_beta(X)),
    fail.

% Melhor jogada foi definida
jogada_plausivel_poda_ab(_,_,_,P):-
    melhor_jogada(P,_),!.

% max()
max(T,J,A,F):-                      %Altura máxima
    altura_maxima(A),
    fae(T,J,F),
    remove_pilha_alpha_beta(_),!.

max([[PoteA],TA,TB,[PoteB]],J,_,F):-    %Fim de jogo
    PoteA >= 12,
    fae([[PoteA],TA,TB,[PoteB]],J,F),
    remove_pilha_alpha_beta(_),!;
    PoteB >= 12,
    fae([[PoteA],TA,TB,[PoteB]],J,F),
    remove_pilha_alpha_beta(_),!.

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
    remove_pilha_alpha_beta(_),!.

max(_,_,_,F):-                              %Max definido
    pilha_alpha_beta([[Alpha,_]|_]),
    F is Alpha,
    remove_pilha_alpha_beta(_),!.

% min()
min(T,J,A,F):-                      %Altura máxima
    altura_maxima(A),
    fae(T,J,F),
    remove_pilha_alpha_beta(_),!.

min([ [PoteA],TA,TB,[PoteB]],J,_,F):-    %Fim de jogo
    PoteA >= 12,
    fae([[PoteA],TA,TB,[PoteB]],J,F),
    remove_pilha_alpha_beta(_),!;
    PoteB >= 12,
    fae([[PoteA],TA,TB,[PoteB]],J,F),
    remove_pilha_alpha_beta(_),!.

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
    remove_pilha_alpha_beta(_),!.

min(_,_,_,F):-                          %Min definido
    pilha_alpha_beta([[_,Beta]|_]),
    F is Beta,
    remove_pilha_alpha_beta(_),!.

% Aqui A é maquina
fae([[PA],_,_,_],_,F):-
    eh_maquina(a),
    PA >= 12,
    F is 90000,!.

fae([_,_,_,[PB]],_,F):-
    eh_maquina(a),
    PB >= 12,
    F is -90000,!.

fae([PA,TA,TB,PB],b,F):-        %A jogou
    !.

fae([PA,TA,TB,PB],a,F):-        %B jogou
    !.

% Aqui B é maquina
fae([[PA],_,_,_],_,F):-
    eh_maquina(b),
    PA >= 12,
    F is -90000,!.

fae([_,_,_,[PB]],_,F):-
    eh_maquina(b),
    PB >= 12,
    F is 90000,!.

fae([PA,TA,TB,PB],a,F):- %B jogou
    !.

fae([PA,TA,TB,PB],b,F):- %A jogou
    !.

% Predicados suporte
insere_melhor_jogada(T,F):-
    melhor_jogada(T2,F2),
    F > F2,                         %T é a melhor jogada
    retract(melhor_jogada(T2,F2)),
    assertz(melhor_jogada(T,F)),!.

insere_melhor_jogada(T,F):-
    melhor_jogada(T2,-90000), % Melhor jogada é tabuleiro inicial
    F == -90000,
    retract(melhor_jogada(T2,-90000)),
    assertz(melhor_jogada(T,F)),!.

insere_melhor_jogada(T,F):- %Vence agora ou depois ?
    F == 90000, % Jogada T vence
    eh_maquina(J),
    fae(T,J,F), % T vence agora !, então T é a melhor
    melhor_jogada(T2,F2),
    retract(melhor_jogada(T2,F2)),
    assertz(melhor_jogada(T,F)),!.

insere_melhor_jogada(_,_):-!.

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
    X >= Y,!.
maior(_,Y,Y):-!.

menor(X,Y,X):-
    X =< Y,!.
menor(_,Y,Y):-!.

oponente(a,b):-!.
oponente(b,a):-!.

:- dynamic eh_maquina/1.
:- dynamic altura_maxima/1.
:- dynamic melhor_jogada/2.
:- dynamic pilha_alpha_beta/1.

% Adjacentes Mancala (trabalho anterior, não alterar).
%--------------------------------------------------------------------------
soma1(1,0,2):-!.

soma1(X,Y,0):-
	Y is X+1,!.

obtem_valor_pos([X|_],1,X).

obtem_valor_pos([_|A],Pos,Y):-
	not(Pos<0),
	ProxPos is Pos-1,
	obtem_valor_pos(A,ProxPos,Y),!.

zera_posicao([_|A],1,[0|A]).

zera_posicao([X|A],Pos,[X|Y]):-
	not(Pos<0),
	ProxPos is Pos-1,
	zera_posicao(A,ProxPos,Y),!.

proxima_posicao(8,1).

proxima_posicao(X,Y):-
	Y is X+1,!.

soma_valor_pos([[X|A],B,1,Pecas],[V,[Y|A],B]):-
	Pecas>0,
	soma1(X,Y,V),!.

soma_valor_pos([A,[X|B],5,Pecas],[V,A,[Y|B]]):-
	Pecas>0,
	soma1(X,Y,V),!.

soma_valor_pos([[X|A],B,Pos,Pecas],[V,[X|Anovo],B]):-
	Pecas>0,
	Pos<5,
	ProxPos is Pos-1,
	soma_valor_pos([A,B,ProxPos,Pecas],[V,Anovo,B]),!.

soma_valor_pos([A,[X|B],Pos,Pecas],[V,A,[X|Bnovo]]):-
	Pecas>0,
	Pos>5,
	Pos<9,
	ProxPos is Pos-1,
	soma_valor_pos([A,B,ProxPos,Pecas],[V,A,Bnovo]),!.

distribui([P,A,B,_,0],[P,A,B]).

distribui([Pote,A,B,Pos,Pecas],[PoteF,TabA,TabB]):-
	soma_valor_pos([A,B,Pos,Pecas],[VPote,Anovo,Bnovo]),
	proxima_posicao(Pos,ProxPos),
	ProxPecas is Pecas-1,
	PoteNovo is Pote+VPote,
	distribui([PoteNovo,Anovo,Bnovo,ProxPos,ProxPecas],[PoteF,TabA,TabB]),!.

jogada([Pote,A,B,Pos],a,[PoteF,TabA,TabB]):-
	Pos>0,
	Pos<5,
	obtem_valor_pos(A,Pos,ValPos),
	not(ValPos=<0),
	zera_posicao(A,Pos,Anovo),
	proxima_posicao(Pos,ProxPos),
	distribui([Pote,Anovo,B,ProxPos,ValPos],[PoteF,TabA,TabB]).

jogada([Pote,A,B,Pos],a,[PoteF,TabA,TabB]):-
	Pos>0,
	Pos<5,
	proxima_posicao(Pos,ProxPos),
	jogada([Pote,A,B,ProxPos],a,[PoteF,TabA,TabB]).

jogada([Pote,A,B,Pos],b,[PoteF,TabA,TabB]):-
	Pos>4,
	Pos<9,
	PosRelativa is Pos-4,
	obtem_valor_pos(B,PosRelativa,ValPos),
	not(ValPos=<0),
	zera_posicao(B,PosRelativa,Bnovo),
	proxima_posicao(Pos,ProxPos),
	distribui([Pote,A,Bnovo,ProxPos,ValPos],[PoteF,TabA,TabB]).

jogada([Pote,A,B,Pos],b,[PoteF,TabA,TabB]):-
	Pos>4,
	Pos<9,
	proxima_posicao(Pos,ProxPos),
	jogada([Pote,A,B,ProxPos],b,[PoteF,TabA,TabB]).

% Fail se o jogo estiver vencido
adjacente([PoteA,A,B,PoteB],_,[PoteA,A,B,PoteB]):-
	PoteA>=12,!,fail;
	PoteB>=12,!,fail.

adjacente([PoteA,A,B,PoteB],a,[[PoteAF],TabA,TabB,PoteB]):-
	jogada([PoteA,A,B,1],a,[PoteAF,TabA,TabB]).

adjacente([PoteA,A,B,PoteB],b,[PoteA,TabA,TabB,[PoteBF]]):-
	jogada([PoteB,A,B,5],b,[PoteBF,TabA,TabB]).
