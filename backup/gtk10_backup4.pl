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

pega_valor_posicao( [ _| A], Pos, Y):-
    not( Pos < 0),
    ProxPos is Pos - 1,
    pega_valor_posicao( A, ProxPos, Y),
    !.
% Fim do Pega o valor de pecas de uma posicao

% Zera uma posicao
zera_posicao( [ _| A], 1, [ 0| A]).

zera_posicao( [ X| A], Pos, [ X| Y]):-
    not( Pos < 0),
    ProxPos is Pos - 1,
    zera_posicao( A, ProxPos, Y),
    !.
% Fim do Zera uma posicao

% Calcula a proxima posicao
proxima_posicao( 8, 1).

proxima_posicao( X, Y):-
    Y is X + 1,
    !.
% Fim do Calcula a proxima posicao

% Soma pecas em uma posicao
soma_valor_pos( [ [ X| A], B, 1, Pecas], [ V, [ Y| A], B]):-
    Pecas > 0,
    soma_mais_um( X, Y, V),
    !.

soma_valor_pos( [ A, [ X| B], 5, Pecas], [ V, A, [ Y| B]]):-
    Pecas > 0,
    soma_mais_um( X, Y, V),
    !.

soma_valor_pos( [ [ X| A], B, Pos, Pecas], [ V, [ X| Anovo], B]):-
    Pecas > 0,
    Pos < 5,
    ProxPos is Pos - 1,
    soma_valor_pos( [ A, B, ProxPos, Pecas], [ V, Anovo, B]),
    !.

soma_valor_pos( [ A, [ X| B], Pos, Pecas], [ V, A, [ X| Bnovo]]):-
    Pecas > 0,
    Pos > 5,
    Pos < 9,
    ProxPos is Pos - 1,
    soma_valor_pos( [ A, B, ProxPos, Pecas], [ V, A, Bnovo]),
    !.
% Fim do Soma pecas em uma posicao

% Distribui pecas no tabuleiro
distribui( [ P, A, B, _, 0], [ P, A, B]).

distribui( [ Pote, A, B, Pos, Pecas], [ PoteF, TabA, TabB]):-
    soma_valor_pos( [ A, B, Pos, Pecas], [ VPote, Anovo, Bnovo]),
    proxima_posicao( Pos, ProxPos),
    ProxPecas is Pecas - 1,
    PoteNovo is Pote + VPote,
    distribui( [ PoteNovo, Anovo, Bnovo, ProxPos, ProxPecas], [ PoteF, TabA, TabB]),
    !.
% Fim do Distribui pecas no tabuleiro

% Faz uma jogada
% Campo A
jogada( [ Pote, A, B, Pos], a, [ PoteF, TabA, TabB]):-
    Pos > 0,
    Pos < 5,
    pega_valor_posicao( A, Pos, ValPos),
    not( ValPos =< 0),
    zera_posicao( A, Pos, Anovo),
    proxima_posicao( Pos, ProxPos),
    distribui( [ Pote, Anovo, B, ProxPos, ValPos], [ PoteF, TabA, TabB]).

% Campo A
jogada( [ Pote, A, B, Pos], a, [ PoteF, TabA, TabB]):-
    Pos > 0,
    Pos < 5,
    proxima_posicao( Pos, ProxPos),
    jogada( [ Pote, A, B, ProxPos], a, [ PoteF, TabA, TabB]).

% Campo B
jogada( [ Pote, A, B, Pos], b, [ PoteF, TabA, TabB]):-
    Pos > 4,
    Pos < 9,
    PosRelativa is Pos - 4,
    pega_valor_posicao( B, PosRelativa, ValPos),
    not( ValPos =< 0),
    zera_posicao( B, PosRelativa, Bnovo),
    proxima_posicao( Pos ,ProxPos),
    distribui( [ Pote, A, Bnovo, ProxPos, ValPos], [ PoteF, TabA, TabB]).

% Campo B
jogada( [ Pote, A, B, Pos], b, [ PoteF, TabA, TabB]):-
    Pos > 4,
    Pos < 9,
    proxima_posicao( Pos, ProxPos),
    jogada( [ Pote, A, B, ProxPos], b, [ PoteF, TabA, TabB]).
% Fim do Faz uma jogada

% Calcula jogadas do mancala
% Jogo ganho
adjacente( [ PoteA, A, B, PoteB], _, [ PoteA, A, B, PoteB]):-
    PoteA >= 12,
    !,
    fail;
    PoteB >= 12,
    !,
    fail.

adjacente( [ PoteA, A, B, PoteB], a, [ [ PoteAF], TabA, TabB, PoteB]):-
    jogada( [ PoteA, A, B, 1], a, [ PoteAF, TabA, TabB]).

adjacente( [ PoteA, A, B, PoteB], b, [ PoteA, TabA, TabB, [ PoteBF]]):-
    jogada( [ PoteB, A, B, 5], b, [ PoteBF, TabA, TabB]).
% Fim do Calcula jogadas do mancala

% ----------------------------------------------------------------
% FIM DO TRABALHO UM 1 DE IA DEVOLVE PROXIMO CAMPO DO MANCALA
% ----------------------------------------------------------------

% ----------------------------------------------------------------
% TRABALHO UM 3 DE IA DEVOLVE O MELHOR CAMPO DO MANCALA
% ----------------------------------------------------------------

% ALGORITMO PADA ALPHA-BETA

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
% menos a quantidade de 1s com peso 5
% menos a quantidade de 0s com peso 2
% e soma com
% a quantidade de pecas no Pote da maquina com peso 10
% menos a a quantidade de pecas do ....... com peso 10
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
            (( BA1 is 1, PesoC1_aux1 is 5); PesoC1_aux1 is 0),
            (( BA2 is 1, PesoC1_aux2 is 5); PesoC1_aux2 is 0),
            (( BA3 is 1, PesoC1_aux3 is 5); PesoC1_aux3 is 0),
            (( BA4 is 1, PesoC1_aux4 is 5); PesoC1_aux4 is 0)
            ),
        PesoC1 is PesoC1_aux1 + PesoC1_aux2 + PesoC1_aux3 + PesoC1_aux4);
        PesoC1 is 0
    ),
    (
        (( BA1 is 0; BA2 is 0; BA3 is 0; BA4 is 0; BB1 is 0; BB2 is 0; BB3 is 0; BB4 is 0),
            (
            (( BB1 is 0, PesoC0_aux1 is 3); PesoC0_aux1 is 0),
            (( BB2 is 0, PesoC0_aux2 is 3); PesoC0_aux2 is 0),
            (( BB3 is 0, PesoC0_aux3 is 3); PesoC0_aux3 is 0),
            (( BB4 is 0, PesoC0_aux4 is 3); PesoC0_aux4 is 0)
            ),
        PesoC0 is PesoC0_aux1 + PesoC0_aux2 + PesoC0_aux3 + PesoC0_aux4);
        PesoC0 is 0
    ),

    Peso_Campo is PesoB - PesoA + PesoC1 - PesoC0,
    Valor_Fae is Peso_Pote + Peso_Campo,
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
            (( BA1 is 1, PesoC1_aux1 is 5); PesoC1_aux1 is 0),
            (( BA2 is 1, PesoC1_aux2 is 5); PesoC1_aux2 is 0),
            (( BA3 is 1, PesoC1_aux3 is 5); PesoC1_aux3 is 0),
            (( BA4 is 1, PesoC1_aux4 is 5); PesoC1_aux4 is 0)
            ),
        PesoC1 is PesoC1_aux1 + PesoC1_aux2 + PesoC1_aux3 + PesoC1_aux4);
        PesoC1 is 0
    ),
    (
        (( BA1 is 0; BA2 is 0; BA3 is 0; BA4 is 0; BB1 is 0; BB2 is 0; BB3 is 0; BB4 is 0),
            (
            (( BB1 is 0, PesoC0_aux1 is 3); PesoC0_aux1 is 0),
            (( BB2 is 0, PesoC0_aux2 is 3); PesoC0_aux2 is 0),
            (( BB3 is 0, PesoC0_aux3 is 3); PesoC0_aux3 is 0),
            (( BB4 is 0, PesoC0_aux4 is 3); PesoC0_aux4 is 0)
            ),
        PesoC0 is PesoC0_aux1 + PesoC0_aux2 + PesoC0_aux3 + PesoC0_aux4);
        PesoC0 is 0
    ),

    Peso_Campo is PesoA - PesoB - PesoC1 + PesoC0,
    Valor_Fae is Peso_Pote + Peso_Campo,
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
            (( BA1 is 1, PesoC1_aux1 is 5); PesoC1_aux1 is 0),
            (( BA2 is 1, PesoC1_aux2 is 5); PesoC1_aux2 is 0),
            (( BA3 is 1, PesoC1_aux3 is 5); PesoC1_aux3 is 0),
            (( BA4 is 1, PesoC1_aux4 is 5); PesoC1_aux4 is 0)
            ),
        PesoC1 is PesoC1_aux1 + PesoC1_aux2 + PesoC1_aux3 + PesoC1_aux4);
        PesoC1 is 0
    ),
    (
        (( BA1 is 0; BA2 is 0; BA3 is 0; BA4 is 0; BB1 is 0; BB2 is 0; BB3 is 0; BB4 is 0),
            (
            (( BB1 is 0, PesoC0_aux1 is 3); PesoC0_aux1 is 0),
            (( BB2 is 0, PesoC0_aux2 is 3); PesoC0_aux2 is 0),
            (( BB3 is 0, PesoC0_aux3 is 3); PesoC0_aux3 is 0),
            (( BB4 is 0, PesoC0_aux4 is 3); PesoC0_aux4 is 0)
            ),
        PesoC0 is PesoC0_aux1 + PesoC0_aux2 + PesoC0_aux3 + PesoC0_aux4);
        PesoC0 is 0
    ),

    Peso_Campo is PesoA - PesoB + PesoC1 - PesoC0,
    Valor_Fae is Peso_Pote + Peso_Campo,
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
            (( BA1 is 1, PesoC1_aux1 is 5); PesoC1_aux1 is 0),
            (( BA2 is 1, PesoC1_aux2 is 5); PesoC1_aux2 is 0),
            (( BA3 is 1, PesoC1_aux3 is 5); PesoC1_aux3 is 0),
            (( BA4 is 1, PesoC1_aux4 is 5); PesoC1_aux4 is 0)
            ),
        PesoC1 is PesoC1_aux1 + PesoC1_aux2 + PesoC1_aux3 + PesoC1_aux4);
        PesoC1 is 0
    ),
    (
        (( BA1 is 0; BA2 is 0; BA3 is 0; BA4 is 0; BB1 is 0; BB2 is 0; BB3 is 0; BB4 is 0),
            (
            (( BB1 is 0, PesoC0_aux1 is 3); PesoC0_aux1 is 0),
            (( BB2 is 0, PesoC0_aux2 is 3); PesoC0_aux2 is 0),
            (( BB3 is 0, PesoC0_aux3 is 3); PesoC0_aux3 is 0),
            (( BB4 is 0, PesoC0_aux4 is 3); PesoC0_aux4 is 0)
            ),
        PesoC0 is PesoC0_aux1 + PesoC0_aux2 + PesoC0_aux3 + PesoC0_aux4);
        PesoC0 is 0
    ),

    Peso_Campo is PesoB - PesoA - PesoC1 + PesoC0,
    Valor_Fae is Peso_Pote + Peso_Campo,
    !.

% Fim do Calculo da Funcao de Avalicao Estatica

% MIN
% Verifica se alcancou a altura maxima
min( Tabuleiro, Jogador, Altura, Valor_Fae):-
    altura_maxima( Altura),
    fae( Tabuleiro, Jogador, Valor_Fae),
    retira_um_elemento_alpha_beta( _),
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
