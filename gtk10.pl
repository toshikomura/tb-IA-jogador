% memoria dinamica para:
% 1 - pilha do alpha e beta
% 2 - altura maxima
% 3 - melhor jogada
% 4 - jogador que eh a maquina
:- dynamic conjunto_d_alphas_e_betas/1.
:- dynamic altura_maxima/1.
:- dynamic melhor_jogada/2.
:- dynamic jogador_q_eh_maquina/1.

% ----------------------------------------------------------------
% TRABALHO UM 1 DE IA DEVOLVE PROXIMO CAMPO DO MANCALA
% ----------------------------------------------------------------
% Soma
soma_mais_um( 1, 0, 2):-
    !.

soma_mais_um( Antigo, Novo, 0):-
    Novo is Antigo + 1,
    !.
% Fim da soma

% Pega o valor de pecas de uma posicao
pega_valor_posicao( [ X| _], 1, X).

pega_valor_posicao( [ _| Z], Pos, Y):-
    not( Pos < 0),
    Prox_Pos is Pos - 1,
    pega_valor_posicao( Z, Prox_Pos, Y),
    !.
% Fim do Pega o valor de pecas de uma posicao

% Zera uma posicao
zera_posicao( [ _| Z], 1, [ 0| Z]).

zera_posicao( [ X| Z], Pos, [ X| Y]):-
    not( Pos < 0),
    Prox_Pos is Pos - 1,
    zera_posicao( Z, Prox_Pos, Y),
    !.
% Fim do Zera uma posicao

% Calcula a proxima posicao
% Se na ultima posicao volta para a primeira
proxima_posicao( 8, 1).

proxima_posicao( Pos, Prox_Pos):-
    Prox_Pos is Pos + 1,
    !.
% Fim do Calcula a proxima posicao

% Soma pecas em uma posicao
soma_pecas_posicao( [ [ X| Tab_A], Tab_B, 1, Pecas], [ Z, [ Y| Tab_A], Tab_B]):-
    Pecas > 0,
    soma_mais_um( X, Y, Z),
    !.

soma_pecas_posicao( [ Tab_A, [ X| Tab_B], 5, Pecas], [ Z, Tab_A, [ Y| Tab_B]]):-
    Pecas > 0,
    soma_mais_um( X, Y, Z),
    !.

% Campo A
soma_pecas_posicao( [ [ X| Tab_A], Tab_B, Pos, Pecas], [ Z, [ X| Novo_Tab_A], Tab_B]):-
    Pecas > 0,
    Pos < 5,
    Prox_Pos is Pos - 1,
    soma_pecas_posicao( [ Tab_A, Tab_B, Prox_Pos, Pecas], [ Z, Novo_Tab_A, Tab_B]),
    !.

% Campo B
soma_pecas_posicao( [ Tab_A, [ X| Tab_B], Pos, Pecas], [ Z, Tab_A, [ X| Novo_Tab_B]]):-
    Pecas > 0,
    Pos > 5,
    Pos < 9,
    Prox_Pos is Pos - 1,
    soma_pecas_posicao( [ Tab_A, Tab_B, Prox_Pos, Pecas], [ Z, Tab_A, Novo_Tab_B]),
    !.
% Fim do Soma pecas em uma posicao

% Distribui pecas no tabuleiro
distribui( [ Pote_Final, Tab_A, Tab_B, _, 0], [ Pote_Final, Tab_A, Tab_B]).

distribui( [ Pote, Tab_A, Tab_B, Pos, Pecas], [ Pote_Final, Prox_Tab_A, Prox_Tab_B]):-
    soma_pecas_posicao( [ Tab_A, Tab_B, Pos, Pecas], [ Valor_Pote, Novo_A, Novo_B]),
    proxima_posicao( Pos, Prox_Pos),
    Prox_Pecas is Pecas - 1,
    Pote_Novo is Pote + Valor_Pote,
    distribui( [ Pote_Novo, Novo_A, Novo_B, Prox_Pos, Prox_Pecas], [ Pote_Final, Prox_Tab_A, Prox_Tab_B]),
    !.
% Fim do Distribui pecas no tabuleiro

% Faz uma jogada
% Campo A
proxima_jogada( [ Pote, Tab_A, Tab_B, Pos], a, [ Pote_Final, Prox_Tab_A, Prox_Tab_B]):-
    Pos > 0,
    Pos < 5,
    pega_valor_posicao( Tab_A, Pos, Valor_Pos),
    not( Valor_Pos =< 0),
    zera_posicao( Tab_A, Pos, Novo_A),
    proxima_posicao( Pos, Prox_Pos),
    distribui( [ Pote, Novo_A, Tab_B, Prox_Pos, Valor_Pos], [ Pote_Final, Prox_Tab_A, Prox_Tab_B]).

% Campo A
proxima_jogada( [ Pote, Tab_A, Tab_B, Pos], a, [ Pote_Final, Prox_Tab_A, Prox_Tab_B]):-
    Pos > 0,
    Pos < 5,
    proxima_posicao( Pos, Prox_Pos),
    proxima_jogada( [ Pote, Tab_A, Tab_B, Prox_Pos], a, [ Pote_Final, Prox_Tab_A, Prox_Tab_B]).

% Campo B
proxima_jogada( [ Pote, Tab_A, Tab_B, Pos], b, [ Pote_Final, Prox_Tab_A, Prox_Tab_B]):-
    Pos > 4,
    Pos < 9,
    Pos_Relativa is Pos - 4,
    pega_valor_posicao( Tab_B, Pos_Relativa, Valor_Pos),
    not( Valor_Pos =< 0),
    zera_posicao( Tab_B, Pos_Relativa, Novo_B),
    proxima_posicao( Pos ,Prox_Pos),
    distribui( [ Pote, Tab_A, Novo_B, Prox_Pos, Valor_Pos], [ Pote_Final, Prox_Tab_A, Prox_Tab_B]).

% Campo B
proxima_jogada( [ Pote, Tab_A, Tab_B, Pos], b, [ Pote_Final, Prox_Tab_A, Prox_Tab_B]):-
    Pos > 4,
    Pos < 9,
    proxima_posicao( Pos, Prox_Pos),
    proxima_jogada( [ Pote, Tab_A, Tab_B, Prox_Pos], b, [ Pote_Final, Prox_Tab_A, Prox_Tab_B]).
% Fim do Faz uma jogada

% Calcula jogadas do mancala
% Jogo ganho
adjacente( [ PoteA, Tab_A, Tab_B, PoteB], _, [ PoteA, Tab_A, Tab_B, PoteB]):-
    PoteA >= 12,
    !,
    fail;
    PoteB >= 12,
    !,
    fail.

% Jogadas para A
adjacente( [ PoteA, Tab_A, Tab_B, PoteB], a, [ [ PoteA_Final], Prox_Tab_A, Prox_Tab_B, PoteB]):-
    proxima_jogada( [ PoteA, Tab_A, Tab_B, 1], a, [ PoteA_Final, Prox_Tab_A, Prox_Tab_B]).

% Jogadas para B
adjacente( [ PoteA, Tab_A, Tab_B, PoteB], b, [ PoteA, Prox_Tab_A, Prox_Tab_B, [ PoteB_Final]]):-
    proxima_jogada( [ PoteB, Tab_A, Tab_B, 5], b, [ PoteB_Final, Prox_Tab_A, Prox_Tab_B]).
% Fim do Calcula jogadas do mancala

% ----------------------------------------------------------------
% FIM DO TRABALHO UM 1 DE IA DEVOLVE PROXIMO CAMPO DO MANCALA
% ----------------------------------------------------------------

% ----------------------------------------------------------------
% TRABALHO UM 3 DE IA DEVOLVE O MELHOR CAMPO DO MANCALA
% ----------------------------------------------------------------

% ALGORITMO PADA ALPHA-BETA
% Constroi a arvore de campos possivei ate altura determinada
% Calcula a FAE para as folhas da arvore
% Inicia o backtracking (comeca a subir na arvore)
% Se
%
%   no nivel de max pega o maior valor da FAE
%   no nivel de min pega o menor valor da FAE
%
% e manda para o nivel acima ate chegar na raiz
%
% Calculo da Funcao de Avalicao Estatica

% ----------------------------------------------------------------
% HEURISTICA DA FAE
% ----------------------------------------------------------------
%
% Se o Pote da maquina eh maior ou igual a 12 a maquina ganhou
% Se o Pote do adversario ..... o adversario ganhou
%
% Caso contrario soma a quantidade de pecas da maquina
% menos a quantidade de pecas do oponente
% mais/menos a quantidade de 1s com peso 5
% mais/menos a quantidade de 0s com peso 2
% e soma com
% a quantidade de pecas no Pote da maquina com peso 10
% menos a a quantidade de pecas do oponente com peso 10
%
% ----------------------------------------------------------------
% HEURISTICA DA FAE
% ----------------------------------------------------------------

% Aqui A eh maquina
% Maquina ganhou
fae( [ [PoteA], _, _, _], _, Valor_Fae):-
    jogador_q_eh_maquina( a), % Maquina eh a
    PoteA >= 12,
    Valor_Fae is 999999,
    !.

% Maquina perdeu
fae( [ _, _, _, [PoteB]], _, Valor_Fae):-
    jogador_q_eh_maquina( a), % Maquina eh a
    PoteB >= 12,
    Valor_Fae is -999999,
    !.

% A jogou
fae( [ [PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [PoteB]], b, Valor_Fae):-
    jogador_q_eh_maquina( a), % Maquina eh a

    % Calcula peso dos potes
    Peso_PoteA is PoteA * 10,
    Peso_PoteB is PoteB * 10,
    Peso_Pote is Peso_PoteA - Peso_PoteB,

    % Calcula peso do campo
    PesoA is BA1 + BA2 + BA3 + BA4,
    PesoB is BB1 + BB2 + BB3 + BB4,

    % Verifica se tem 0 e 1s
    (
        (( BA1 is 1; BA2 is 1; BA3 is 1; BA4 is 1; BB1 is 1; BB2 is 1; BB3 is 1; BB4 is 1),
            (
            (( BA1 is 1, PesoC1_aux1 is 8); PesoC1_aux1 is 0),
            (( BA2 is 1, PesoC1_aux2 is 8); PesoC1_aux2 is 0),
            (( BA3 is 1, PesoC1_aux3 is 8); PesoC1_aux3 is 0),
            (( BA4 is 1, PesoC1_aux4 is 8); PesoC1_aux4 is 0),
            (( BB1 is 1, PesoC1_aux5 is 8); PesoC1_aux5 is 0),
            (( BB2 is 1, PesoC1_aux6 is 8); PesoC1_aux6 is 0),
            (( BB3 is 1, PesoC1_aux7 is 8); PesoC1_aux7 is 0),
            (( BB4 is 1, PesoC1_aux8 is 8); PesoC1_aux8 is 0)
            ),
        PesoC1 is PesoC1_aux1 + PesoC1_aux2 + PesoC1_aux3 + PesoC1_aux4 + PesoC1_aux5 + PesoC1_aux6 + PesoC1_aux7 + PesoC1_aux8);
        PesoC1 is 0
    ),
    (
        (( BA1 is 0; BA2 is 0; BA3 is 0; BA4 is 0; BB1 is 0; BB2 is 0; BB3 is 0; BB4 is 0),
            (
            (( BA1 is 0, PesoC0_aux1 is 3); PesoC0_aux1 is 0),
            (( BA2 is 0, PesoC0_aux2 is 3); PesoC0_aux2 is 0),
            (( BA3 is 0, PesoC0_aux3 is 3); PesoC0_aux3 is 0),
            (( BA4 is 0, PesoC0_aux4 is 3); PesoC0_aux4 is 0),
            (( BB1 is 0, PesoC0_aux5 is 3); PesoC0_aux5 is 0),
            (( BB2 is 0, PesoC0_aux6 is 3); PesoC0_aux6 is 0),
            (( BB3 is 0, PesoC0_aux7 is 3); PesoC0_aux7 is 0),
            (( BB4 is 0, PesoC0_aux8 is 3); PesoC0_aux8 is 0)
            ),
        PesoC0 is PesoC0_aux1 + PesoC0_aux2 + PesoC0_aux3 + PesoC0_aux4 + PesoC0_aux5 + PesoC0_aux6 + PesoC0_aux7 + PesoC0_aux8);
        PesoC0 is 0
    ),

    Peso_Campo_aux2 is PesoA - PesoB,
    (
        (Peso_Campo_aux2 < 0, Peso_Campo_aux is (Peso_Campo_aux2 * -1) *2);
        Peso_Campo_aux is Peso_Campo_aux2
    ),
    Peso_Campo is (Peso_Campo_aux - PesoC1 + PesoC0) * 15,
    Valor is Peso_Campo - Peso_Pote,
    (
        (Valor > 0, Valor_Fae is Valor * -1);
        Valor_Fae is Valor
    ),
    !.

% B jogou
fae( [ [PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [PoteB]], a, Valor_Fae):-
    jogador_q_eh_maquina( a), % Maquina eh a

    % Calcula peso dos potes
    Peso_PoteA is PoteA * 10,
    Peso_PoteB is PoteB * 10,
    Peso_Pote is Peso_PoteB - Peso_PoteA,

    % Calcula peso do campo
    PesoA is BA1 + BA2 + BA3 + BA4,
    PesoB is BB1 + BB2 + BB3 + BB4,

    % Verifica se tem 0 e 1s
    (
        (( BA1 is 1; BA2 is 1; BA3 is 1; BA4 is 1; BB1 is 1; BB2 is 1; BB3 is 1; BB4 is 1),
            (
            (( BA1 is 1, PesoC1_aux1 is 3); PesoC1_aux1 is 0),
            (( BA2 is 1, PesoC1_aux2 is 3); PesoC1_aux2 is 0),
            (( BA3 is 1, PesoC1_aux3 is 3); PesoC1_aux3 is 0),
            (( BA4 is 1, PesoC1_aux4 is 3); PesoC1_aux4 is 0),
            (( BB1 is 1, PesoC1_aux5 is 3); PesoC1_aux5 is 0),
            (( BB2 is 1, PesoC1_aux6 is 3); PesoC1_aux6 is 0),
            (( BB3 is 1, PesoC1_aux7 is 3); PesoC1_aux7 is 0),
            (( BB4 is 1, PesoC1_aux8 is 3); PesoC1_aux8 is 0)
            ),
        PesoC1 is PesoC1_aux1 + PesoC1_aux2 + PesoC1_aux3 + PesoC1_aux4 + PesoC1_aux5 + PesoC1_aux6 + PesoC1_aux7 + PesoC1_aux8);
        PesoC1 is 0
    ),
    (
        (( BA1 is 0; BA2 is 0; BA3 is 0; BA4 is 0; BB1 is 0; BB2 is 0; BB3 is 0; BB4 is 0),
            (
            (( BA1 is 0, PesoC0_aux1 is 8); PesoC0_aux1 is 0),
            (( BA2 is 0, PesoC0_aux2 is 8); PesoC0_aux2 is 0),
            (( BA3 is 0, PesoC0_aux3 is 8); PesoC0_aux3 is 0),
            (( BA4 is 0, PesoC0_aux4 is 8); PesoC0_aux4 is 0),
            (( BB1 is 0, PesoC0_aux5 is 8); PesoC0_aux5 is 0),
            (( BB2 is 0, PesoC0_aux6 is 8); PesoC0_aux6 is 0),
            (( BB3 is 0, PesoC0_aux7 is 8); PesoC0_aux7 is 0),
            (( BB4 is 0, PesoC0_aux8 is 8); PesoC0_aux8 is 0)
            ),
        PesoC0 is PesoC0_aux1 + PesoC0_aux2 + PesoC0_aux3 + PesoC0_aux4 + PesoC0_aux5 + PesoC0_aux6 + PesoC0_aux7 + PesoC0_aux8);
        PesoC0 is 0
    ),

    Peso_Campo_aux2 is PesoB - PesoA,
    (
        (Peso_Campo_aux2 < 0, Peso_Campo_aux is (Peso_Campo_aux2 * -1) *2);
        Peso_Campo_aux is Peso_Campo_aux2
    ),
    Peso_Campo is (Peso_Campo_aux + PesoC1 - PesoC0) * 15,
    Valor is Peso_Campo - Peso_Pote,
    (
        (Valor < 0, Valor_Fae is Valor * -1);
        Valor_Fae is Valor
    ),
    !.

% Aqui B eh maquina
% Maquina ganhou
fae( [ [PoteA], _, _, _], _, Valor_Fae):-
    jogador_q_eh_maquina( b), % Maquina eh b
    PoteA >= 12,
    Valor_Fae is -999999,
    !.

% Maquina perdeu
fae( [ _, _, _, [PoteB]], _, Valor_Fae):-
    jogador_q_eh_maquina( b), % Maquina eh b
    PoteB >= 12,
    Valor_Fae is 999999,
    !.

% B jogou
fae( [ [PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [PoteB]], a, Valor_Fae):-
    jogador_q_eh_maquina( b), % Maquina eh b

    % Calcula peso dos potes
    Peso_PoteA is PoteA * 10,
    Peso_PoteB is PoteB * 10,
    Peso_Pote is Peso_PoteB - Peso_PoteA,

    % Calcula peso do campo
    PesoA is BA1 + BA2 + BA3 + BA4,
    PesoB is BB1 + BB2 + BB3 + BB4,

    % Verifica se tem 0 e 1s
    (
        (( BA1 is 1; BA2 is 1; BA3 is 1; BA4 is 1; BB1 is 1; BB2 is 1; BB3 is 1; BB4 is 1),
            (
            (( BA1 is 1, PesoC1_aux1 is 8); PesoC1_aux1 is 0),
            (( BA2 is 1, PesoC1_aux2 is 8); PesoC1_aux2 is 0),
            (( BA3 is 1, PesoC1_aux3 is 8); PesoC1_aux3 is 0),
            (( BA4 is 1, PesoC1_aux4 is 8); PesoC1_aux4 is 0),
            (( BB1 is 1, PesoC1_aux5 is 8); PesoC1_aux5 is 0),
            (( BB2 is 1, PesoC1_aux6 is 8); PesoC1_aux6 is 0),
            (( BB3 is 1, PesoC1_aux7 is 8); PesoC1_aux7 is 0),
            (( BB4 is 1, PesoC1_aux8 is 8); PesoC1_aux8 is 0)
            ),
        PesoC1 is PesoC1_aux1 + PesoC1_aux2 + PesoC1_aux3 + PesoC1_aux4 + PesoC1_aux5 + PesoC1_aux6 + PesoC1_aux7 + PesoC1_aux8);
        PesoC1 is 0
    ),
    (
        (( BA1 is 0; BA2 is 0; BA3 is 0; BA4 is 0; BB1 is 0; BB2 is 0; BB3 is 0; BB4 is 0),
            (
            (( BA1 is 0, PesoC0_aux1 is 3); PesoC0_aux1 is 0),
            (( BA2 is 0, PesoC0_aux2 is 3); PesoC0_aux2 is 0),
            (( BA3 is 0, PesoC0_aux3 is 3); PesoC0_aux3 is 0),
            (( BA4 is 0, PesoC0_aux4 is 3); PesoC0_aux4 is 0),
            (( BB1 is 0, PesoC0_aux5 is 3); PesoC0_aux5 is 0),
            (( BB2 is 0, PesoC0_aux6 is 3); PesoC0_aux6 is 0),
            (( BB3 is 0, PesoC0_aux7 is 3); PesoC0_aux7 is 0),
            (( BB4 is 0, PesoC0_aux8 is 3); PesoC0_aux8 is 0)
            ),
        PesoC0 is PesoC0_aux1 + PesoC0_aux2 + PesoC0_aux3 + PesoC0_aux4 + PesoC0_aux5 + PesoC0_aux6 + PesoC0_aux7 + PesoC0_aux8);
        PesoC0 is 0
    ),

    Peso_Campo_aux2 is PesoB - PesoA,
    (
        (Peso_Campo_aux2 < 0, Peso_Campo_aux is (Peso_Campo_aux2 * -1) *2);
        Peso_Campo_aux is Peso_Campo_aux2
    ),
    Peso_Campo is (Peso_Campo_aux - PesoC1 + PesoC0) * 15,
    Valor is Peso_Campo - Peso_Pote,
    (
        (Valor > 0, Valor_Fae is Valor * -1);
        Valor_Fae is Valor
    ),
    !.

% B jogou
fae( [ [PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [PoteB]], b, Valor_Fae):-
    jogador_q_eh_maquina( b), % Maquina b

    % Calcula peso dos potes
    Peso_PoteA is PoteA * 10,
    Peso_PoteB is PoteB * 10,
    Peso_Pote is Peso_PoteA - Peso_PoteB,

    % Calcula peso do campo
    PesoA is BA1 + BA2 + BA3 + BA4,
    PesoB is BB1 + BB2 + BB3 + BB4,

    % Verifica se tem 0 e 1s
    (
        (( BA1 is 1; BA2 is 1; BA3 is 1; BA4 is 1; BB1 is 1; BB2 is 1; BB3 is 1; BB4 is 1),
            (
            (( BA1 is 1, PesoC1_aux1 is 3); PesoC1_aux1 is 0),
            (( BA2 is 1, PesoC1_aux2 is 3); PesoC1_aux2 is 0),
            (( BA3 is 1, PesoC1_aux3 is 3); PesoC1_aux3 is 0),
            (( BA4 is 1, PesoC1_aux4 is 3); PesoC1_aux4 is 0),
            (( BB1 is 1, PesoC1_aux5 is 3); PesoC1_aux5 is 0),
            (( BB2 is 1, PesoC1_aux6 is 3); PesoC1_aux6 is 0),
            (( BB3 is 1, PesoC1_aux7 is 3); PesoC1_aux7 is 0),
            (( BB4 is 1, PesoC1_aux8 is 3); PesoC1_aux8 is 0)
            ),
        PesoC1 is PesoC1_aux1 + PesoC1_aux2 + PesoC1_aux3 + PesoC1_aux4 + PesoC1_aux5 + PesoC1_aux6 + PesoC1_aux7 + PesoC1_aux8);
        PesoC1 is 0
    ),
    (
        (( BA1 is 0; BA2 is 0; BA3 is 0; BA4 is 0; BB1 is 0; BB2 is 0; BB3 is 0; BB4 is 0),
            (
            (( BA1 is 0, PesoC0_aux1 is 8); PesoC0_aux1 is 0),
            (( BA2 is 0, PesoC0_aux2 is 8); PesoC0_aux2 is 0),
            (( BA3 is 0, PesoC0_aux3 is 8); PesoC0_aux3 is 0),
            (( BA4 is 0, PesoC0_aux4 is 8); PesoC0_aux4 is 0),
            (( BB1 is 0, PesoC0_aux5 is 8); PesoC0_aux5 is 0),
            (( BB2 is 0, PesoC0_aux6 is 8); PesoC0_aux6 is 0),
            (( BB3 is 0, PesoC0_aux7 is 8); PesoC0_aux7 is 0),
            (( BB4 is 0, PesoC0_aux8 is 8); PesoC0_aux8 is 0)
            ),
        PesoC0 is PesoC0_aux1 + PesoC0_aux2 + PesoC0_aux3 + PesoC0_aux4 + PesoC0_aux5 + PesoC0_aux6 + PesoC0_aux7 + PesoC0_aux8);
        PesoC0 is 0
    ),

    Peso_Campo_aux2 is PesoA - PesoB,
    (
        (Peso_Campo_aux2 < 0, Peso_Campo_aux is (Peso_Campo_aux2 * -1) *2);
        Peso_Campo_aux is Peso_Campo_aux2
    ),
    Peso_Campo is (Peso_Campo_aux + PesoC1 - PesoC0) * 15,
    Valor is Peso_Campo - Peso_Pote,
    (
        (Valor < 0, Valor_Fae is Valor * -1);
        Valor_Fae is Valor
    ),
    !.

% Fim do Calculo da Funcao de Avalicao Estatica

% MIN
% Verifica se alcancou a altura maxima
min( Tabuleiro, Jogador, Altura, Valor_Fae):-
    altura_maxima( Altura),
    fae( Tabuleiro, Jogador, Valor_Fae),
    retira_um_elemento_alpha_beta( _),
    write('Min Jogador '),
    write( Jogador),
    write('\nAltura '),
    write( Altura),
    write('\nTabuleiro '),
    write( Tabuleiro),
    write('\n FAE '),
    write( Valor_Fae),
    write('\n'),
    !.

% Verifica se o tabuleiro eh fim de jogo
min( [ [PoteA], TA, TB, [PoteB]], Joagdor, _, Valor_Fae):-
    PoteA >= 12,
    fae( [ [ PoteA], TA, TB, [ PoteB]], Jogador, Valor_Fae),
    retira_um_elemento_alpha_beta( _),
    !;
    PoteB >= 12,
    fae( [ [ PoteA], TA, TB, [ PoteB]], Jogador, Valor_Fae),
    retira_um_elemento_alpha_beta( _),
    !.

% Pega os melhores Betas
min( Tabuleiro, Jogador, Altura, Valor_Fae):-
    adjacente( Tabuleiro, Jogador, Prox_Tabuleiro),
    adversario( Jogador, Prox_Jogador),
    Prox_Altura is Altura + 1,
    conjunto_d_alphas_e_betas( [ [ Alpha, Beta] | _]),
    coloca_um_elemento_alpha_beta( [ Alpha, Beta]),
    max( Prox_Tabuleiro, Prox_Jogador, Prox_Altura, Valor_Fae_R), % Chamada Recursiva do max()
    menor( Beta, Valor_Fae_R, Menor_Beta),
    write('Menor Beta '),
    write(Beta),
    write(' '),
    write(Valor_Fae_R),
    write('\n'),
    retira_um_elemento_alpha_beta( _),
    coloca_um_elemento_alpha_beta( [ Alpha, Menor_Beta]),
    Alpha >= Menor_Beta, % Poda
    Valor_Fae is Menor_Beta,
    retira_um_elemento_alpha_beta( _),
    !.

% Min quando tem o valor da FAE definido pro nivel corrente
min( _, _, _, Valor_Fae):-
    conjunto_d_alphas_e_betas( [ [ _, Beta]| _]),
    Valor_Fae is Beta,
    retira_um_elemento_alpha_beta( _),
    !.
% Fim do MIN

% MAX
% Verifica se alcancou a altura maxima
max( Tabuleiro, Jogador, Altura, Valor_Fae):-
    altura_maxima( Altura),
    fae( Tabuleiro, Jogador, Valor_Fae),
    retira_um_elemento_alpha_beta( _),
    write('Max Jogador '),
    write( Jogador),
    write('\nAltura '),
    write( Altura),
    write('\nTabuleiro '),
    write( Tabuleiro),
    write('\n FAE '),
    write( Valor_Fae),
    write('\n'),
    !.

% Veifica se o tabuleiro eh fim de jogo
max( [ [PoteA], TA, TB, [PoteB]], Jogador, _, Valor_Fae):-
    PoteA >= 12,
    fae( [ [PoteA], TA, TB, [PoteB]], Jogador, Valor_Fae),
    retira_um_elemento_alpha_beta( _),
    !;
    PoteB >= 12,
    fae( [ [PoteA], TA, TB, [PoteB]], Jogador, Valor_Fae),
    retira_um_elemento_alpha_beta( _),
    !.

% Pega os melhores alphas
max( Tabuleiro, Jogador, Altura, Valor_Fae):-
    adjacente( Tabuleiro, Jogador, Prox_Tabuleiro),
    adversario( Jogador, Prox_Jogador),
    Prox_Altura is Altura + 1,
    conjunto_d_alphas_e_betas( [ [ Alpha, Beta]| _]),
    coloca_um_elemento_alpha_beta( [ Alpha, Beta]),
    min( Prox_Tabuleiro, Prox_Jogador, Prox_Altura, Valor_Fae_R), % Recursão min()
    maior( Alpha, Valor_Fae_R, Maior_Alpha),
    retira_um_elemento_alpha_beta( _),
    coloca_um_elemento_alpha_beta( [ Maior_Alpha, Beta]),
    Maior_Alpha >= Beta, % Poda
    Valor_Fae is Maior_Alpha,
    retira_um_elemento_alpha_beta( _),
    !.

% Max quando tem o valor da FAE definido pro nivel corrente
max( _, _, _, Valor_Fae):-
    conjunto_d_alphas_e_betas( [ [ Alpha, _]| _]),
    Valor_Fae is Alpha,
    retira_um_elemento_alpha_beta( _),
    !.
% Fim do MAX

% Predicados que insere a melhor jogada
% Tabuleiro eh a melhor jogada
coloca_melhor_jogada( Tabuleiro, Valor_Fae):-
    melhor_jogada( Tabuleiro_aux, Valor_Fae_aux),
    Valor_Fae > Valor_Fae_aux,
    retract( melhor_jogada( Tabuleiro_aux, Valor_Fae_aux)),
    assertz( melhor_jogada( Tabuleiro, Valor_Fae)),
    !.

% A melhor jogada eh o tabuleiro inicial
coloca_melhor_jogada( Tabuleiro, Valor_Fae):-
    melhor_jogada( Tabuleiro_axu, -99999),
    Valor_Fae == -999999,
    retract( melhor_jogada( Tabuleiro_aux, -999999)),
    assertz( melhor_jogada( Tabuleiro, Valor_Fae)),
    !.

% Checa se vence agora ou depois
coloca_melhor_jogada( Tabuleiro, Valor_Fae):-
    Valor_Fae == 999999, % Jogada do tabuleiro vence
    jogador_q_eh_maquina( Jogador),
    fae( Tabuleiro, Jogador, Valor_Fae), % Ele eh o melhor se vence agora
    melhor_jogada( Tabuleiro_aux, Valor_Fae_aux),
    retract( melhor_jogada( Tabuleiro_aux, Valor_Fae_aux)),
    assertz( melhor_jogada( Tabuleiro, Valor_Fae)),
    !.

coloca_melhor_jogada( _, _):-
    !.
% Fim do Predicados que insere a melhor jogada

% Operacaoes no conjunto de alphas e betas
% Insere um elemento no conjunto alpha beta
coloca_um_elemento_alpha_beta( Elemento_Alpha_Beta):-
    !,
    conjunto_d_alphas_e_betas( Elementos),
    retract( conjunto_d_alphas_e_betas( Elementos)),
    assertz( conjunto_d_alphas_e_betas( [ Elemento_Alpha_Beta| Elementos])).

% Reomve um elemento do conjunto  alpha beta
retira_um_elemento_alpha_beta( _):-
    !,
    conjunto_d_alphas_e_betas( [ Elemento_F| Elementos]),
    retract( conjunto_d_alphas_e_betas( [ Elemento_F| Elementos])),
    assertz( conjunto_d_alphas_e_betas( Elementos)).
% Fim das Operacoes no conjunto de alphas e betas

% Calcula maior
maior( X, Y, X):-
    X >= Y,
    !.
maior( _, Y, Y):-
    !.

% Calcula menor
menor( X, Y, X):-
    X =< Y,
    !.
menor( _, Y, Y):-
    !.

% Identifica que eh o adversario
adversario( a, b):-
    !.
adversario( b, a):-
    !.

% Acha a melhor jogada direta, chama o min para os adjacentes
% isto é o primeiro MAX.
jogada_plausivel_poda_ab( Tabuleiro, Jogador, Altura, _):-
    assertz( melhor_jogada( Tabuleiro, -999999)), % Melhor jogada eh - infinito
    adjacente( Tabuleiro, Jogador, Prox_Tabuleiro), % Calcula proximo campo
    adversario( Jogador, Adversario), % Determina que eh o adversario da maquina
    Prox_Altura is Altura + 1, % Aumenta a altura
    melhor_jogada( _, Alpha),
    assertz( conjunto_d_alphas_e_betas( [ [ -999999, 999999]])), %Alpha/Beta inicial -Infinito +Infinito
    min( Prox_Tabuleiro, Adversario, Prox_Altura, Valor_Fae), % Calculo do min para os adjacentes
%    fae( Prox_Tabuleiro, Jogador, Valor_Fae_aux),
    coloca_melhor_jogada( Prox_Tabuleiro, Valor_Fae), % Coloca a melhor jogada

    % Retira elemento da pilha alpha beta
    conjunto_d_alphas_e_betas( X),
    retract( conjunto_d_alphas_e_betas( X)),
    fail.

% Melhor jogada foi definida
jogada_plausivel_poda_ab( _, _, _, Melhor_Tabuleiro):-
    melhor_jogada( Melhor_Tabuleiro, _),
    !.

% Jogadas normais
plausivel( Tabuleiro, Jogador, Altura, Proximo):-
    assertz( jogador_q_eh_maquina( Jogador)), % Coloca o jogado que eh a maquina em memoria
    assertz( altura_maxima( Altura)), % Coloca altura maxima em memoria
    jogada_plausivel_poda_ab( Tabuleiro, Jogador, 0, Proximo), % Chama o poda alpha beta para calcular o melhor campo
    melhor_jogada( Melhor_Tabuleiro, Valor_Fae),
    retract( melhor_jogada( Melhor_Tabuleiro, Valor_Fae)),
    write('Melhor jogada '),
    write( Melhor_Tabuleiro),
    write('\n FAE '),
    write( Valor_Fae),
    write('\n'),
    retract( jogador_q_eh_maquina( Jogador)),
    retract( altura_maxima( Altura)),
    !.

% ----------------------------------------------------------------
% TRABALHO UM 3 DE IA DEVOLVE O MELHOR CAMPO DO MANCALA
% ----------------------------------------------------------------

% teste
% plausivel( [ [0], [ 4, 4, 4, 4], [ 4, 4, 4, 4], [0]], a, 2, Proximo).
% plausivel( [ [0], [ 4, 4, 4, 4], [ 4, 4, 4, 4], [0]], b, 2, Proximo).
% plausivel( [ [0], [ 4, 4, 4, 4], [ 4, 4, 4, 4], [0]], a, 3, Proximo).
% plausivel( [ [0], [ 4, 4, 4, 4], [ 4, 4, 4, 4], [0]], b, 3, Proximo).
% plausivel( [ [4], [ 0, 0, 0, 0], [ 4, 4, 4, 4], [0]], a, 3, Proximo).
% plausivel( [ [0], [ 4, 4, 4, 4], [ 0, 0, 0, 0], [4]], b, 3, Proximo).
% plausivel( [ [11], [ 2, 2, 2, 2], [ 1, 4, 4, 4], [0]], a, 2, Proximo).
% plausivel( [ [0], [ 1, 2, 2, 2], [ 2, 4, 4, 4], [11]], a, 2, Proximo).
% plausivel( [ [0], [ 1, 2, 2, 2], [ 2, 4, 4, 4], [11]], b, 2, Proximo).
% plausivel( [ [2], [ 5, 7, 4, 2], [ 4, 2, 4, 0], [2]], a, 2, Proximo).
% plausivel( [ [2], [ 5, 7, 4, 2], [ 4, 2, 4, 0], [2]], b, 2, Proximo).
% plausivel( [ [2], [ 5, 7, 4, 2], [ 4, 2, 4, 0], [2]], a, 20, Proximo).
% plausivel( [ [11], [ 2, 2, 2, 2], [ 1, 4, 4, 4], [0]], a, 20, Proximo).
% plausivel( [ [5], [ 4, 3, 1, 2], [ 1, 5, 1, 6], [4]], a, 2, Proximo).
