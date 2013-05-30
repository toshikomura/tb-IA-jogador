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

% concatena duas listas
concatena( [], L, L).

concatena( [ X | Xcauda], Lista, [ X | Xcauda_2]) :-
    concatena( Xcauda, Lista, Xcauda_2).

% Calcula um proximo campo para o mancala
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

proximo_campo( Pote, Pote_prox, Sementes, Antes, [X | Xcauda], Y) :-
    Sementes > 0,
    Lista2 is  [ 0 | Lista],
    proximo_campo_r( Pote, Pote_prox_aux, Sementes, Sementes_prox, Xcauda, Lista),
    concatena( Antes, Lista2, Depois),
    proximo_campo_controle( Pote_prox_aux, Pote_prox_aux2, Sementes_prox, Sementes_prox_aux2, Depois, Depois2),
    Pote_prox is Pote_prox_aux2,
    Y is Depois2.

% Prepara a saida
arruma_saida( PoteA, [ BA1, BA2, BA3, BA4, BB1, BB2, BB3, BB4], PoteB, Saida ) :-
    Saida is [[ PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [ PoteB]].


% Calcula as possiveis jogadas do mancala a partir de um campo
adjacente( [ [ PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [ PoteB] ], a, Jogada) :-
    (
    proximo_campo( PoteA, PoteA_prox, BA1, [], [ BA1, BA2, BA3, BA4, BB1, BB2, BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA_prox, Proxima_jogada, PoteB, Saida),
    Jogada is Saida
    )
    ;
    (
    proximo_campo( PoteA, PoteA_prox, BA2, [ BA1], [ BA2, BA3, BA4, BB1, BB2, BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA_prox, Proxima_jogada, PoteB, Saida),
    Jogada is Saida
    )
    ;
    (
    proximo_campo( PoteA, PoteA_prox, BA3, [ BA1, BA2], [ BA3, BA4, BB1, BB2, BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA_prox, Proxima_jogada, PoteB, Saida),
    Jogada is Saida
    )
    ;
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
    )
    ;
    (
    proximo_campo( PoteB, PoteB_prox, BB2, [ BA1, BA2, BA3, BA4, BB1], [ BB2, BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA, Proxima_jogada, PoteB_prox, Saida),
    Jogada is Saida
    )
    ;
    (
    proximo_campo( PoteB, PoteB_prox, BB3, [ BA1, BA2, BA3, BA4, BB1, BB2], [ BB3, BB4], Proxima_jogada),
    arruma_saida( PoteA, Proxima_jogada, PoteB_prox, Saida),
    Jogada is Saida
    )
    ;
    (
    proximo_campo( PoteB, PoteB_prox, BB4, [ BA1, BA2, BA3, BA4, BB1, BB2, BB3], [ BB4], Proxima_jogada),
    arruma_saida( PoteA, Proxima_jogada, PoteB_prox, Saida),
    Jogada is Saida
    ).
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
    !.

% Verifica se o tabuleiro eh fim de jogo
min( [ [PoteA], TA, TB, [PoteB]], Joagdor, _, Valor_Fae):-
    PoteA >= 12,
    fae( [ [ PoteA], TA, TB, [ PoteB]], Jogador, Valor_Fae),
    !;
    PoteB >= 12,
    fae( [ [ PoteA], TA, TB, [ PoteB]], Jogador, Valor_Fae),
    !.

% Pega os melhores Betas
min( Tabuleiro, Jogador, Altura, Valor_Fae):-
    adjacente( Tabuleiro, Jogador, Prox_Tabuleiro),
    adversario( Jogador, Prox_Jogador),
    Prox_Altura is Altura + 1,
    conjunto_d_alphas_e_betas( [ [ Alpha, Beta] | Elementos]),
    max( Prox_Tabuleiro, Prox_Jogador, Prox_Altura, Valor_Fae_R), % Chamada Recursiva do max()
    menor( Beta, Valor_Fae_R, Menor_Beta),
    Alpha >= Menor_Beta, % Poda
    Valor_Fae is Menor_Beta,
    !.

% Min quando tem o valor da FAE definido pro nivel corrente
min( _, _, _, Valor_Fae):-
    conjunto_d_alphas_e_betas( [ [ _, Beta]| _]),
    Valor_Fae is Beta,
    !.
% Fim do MIN

% MAX
% Verifica se alcancou a altura maxima
max( Tabuleiro, Jogador, Altura, Valor_Fae):-
    altura_maxima( Altura),
    fae( Tabuleiro, Jogador, Valor_Fae),
    !.

% Veifica se o tabuleiro eh fim de jogo
max( [ [PoteA], TA, TB, [PoteB]], Jogador, _, Valor_Fae):-
    PoteA >= 12,
    fae( [ [PoteA], TA, TB, [PoteB]], Jogador, Valor_Fae),
    !;
    PoteB >= 12,
    fae( [ [PoteA], TA, TB, [PoteB]], Jogador, Valor_Fae),
    !.

% Pega os melhores alphas
max( Tabuleiro, Jogador, Altura, Valor_Fae):-
    adjacente( Tabuleiro, Jogador, Prox_Tabuleiro),
    adversario( Jogador, Prox_Jogador),
    Prox_Altura is Altura + 1,
    conjunto_d_alphas_e_betas( [ [ Alpha, Beta]| _]),
    min( Prox_Tabuleiro, Prox_Jogador, Prox_Altura, Valor_Fae_R), % Recursão min()
    maior( Alpha, Valor_Fae_R, Maior_Alpha),
    Maior_Alpha >= Beta, % Poda
    Valor_Fae is Maior_Alpha,
    !.

% Max quando tem o valor da FAE definido pro nivel corrente
max( _, _, _, Valor_Fae):-
    conjunto_d_alphas_e_betas( [ [ Alpha, _]| _]),
    Valor_Fae is Alpha,
    !.
% Fim do MAX

% Predicados que insere a melhor jogada
% Tabuleiro eh a melhor jogada
insere_melhor_jogada( Tabuleiro, Valor_Fae):-
    melhor_jogada( Tabuleiro_aux, Valor_Fae_aux),
    Valor_Fae > Valor_Fae_aux,
    retract( melhor_jogada( Tabuleiro_aux, Valor_Fae_aux)),
    assertz( melhor_jogada( Tabuleiro, Valor_Fae)),
    !.

% A melhor jogada eh o tabuleiro inicial
insere_melhor_jogada( Tabuleiro, Valor_Fae):-
    melhor_jogada( Tabuleiro_axu, -99999),
    Valor_Fae == -999999,
    retract( melhor_jogada( Tabuleiro_aux, -999999)),
    assertz( melhor_jogada( Tabuleiro, Valor_Fae)),
    !.

% Checa se vence agora ou depois
insere_melhor_jogada( Tabuleiro, Valor_Fae):-
    Valor_Fae == 999999, % Jogada do tabuleiro vence
    jogador_q_eh_maquina( Jogador),
    fae( Tabuleiro, Jogador, Valor_Fae), % Ele eh o melhor se vence agora
    melhor_jogada( Tabuleiro_aux, Valor_Fae_aux),
    retract( melhor_jogada( Tabuleiro_aux, Valor_Fae_aux)),
    assertz( melhor_jogada( Tabuleiro, Valor_Fae)),
    !.

insere_melhor_jogada( _, _):-
    !.
% Fim do Predicados que insere a melhor jogada

% Operacaoes no conjunto de alphas e betas
% Insere um elemento no conjunto alpha beta
insere_um_elemento_alpha_beta( Elemento_Alpha_Beta):-
    !,
    conjunto_d_alphas_e_betas( Elementos),
    retract( conjunto_d_alphas_e_betas( Elementos)),
    assertz( conjunto_d_alphas_e_betas( [ Elemento_Alpha_Beta| Elementos])),
    conjunto_d_alphas_e_betas( A). % Somente para checagem

% Reomve um elemento do conjunto  alpha beta
remove_um_elemento_alpha_beta( _):-
    !,
    conjunto_d_alphas_e_betas( [ Elemento_F| Elementos]),
    retract( conjunto_d_alphas_e_betas( [ Elemento_F| Elementos])),
    assertz( conjunto_d_alphas_e_betas( Elementos)),
    conjunto_d_alphas_e_betas( A). % Somente para checagem
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
    assertz( conjunto_d_alphas_e_betas( [ [ -999999, 999999]])), %Alpha/Beta inicial -Infinito +Infinito
    min( Prox_Tabuleiro, Adversario, Prox_Altura, Valor_Fae), % Calculo do min para os adjacentes
    fae( Prox_Tabuleiro, Jogador, Valor_Fae_aux),
    insere_melhor_jogada( Prox_Tabuleiro, Valor_Fae_aux), % Coloca a melhor jogada

    % Retira elemento da pilha alpha beta
    conjunto_d_alphas_e_betas( X),
    retract( conjunto_d_alphas_e_betas( X)),
    fail.

% Melhor jogada foi definida
jogada_plausivel_poda_ab( _, _, _, Melhor_Tabuleiro):-
    melhor_jogada( Melhor_Tabuleiro, _),
    !.

% Principal, jogada plausível por poda alpha-beta
% Nao existe jogada a ser feita
%plausivel( [ [PoteA], [ 0, 0, 0, 0], TB, [PoteB] ], a, _, Proximo):-
%    Proximo is [ [PoteA], [ 0, 0, 0, 0], TB, [PoteB]],
%    !.

%plausivel( [ [PoteA], TA, [ 0, 0, 0, 0], [PoteB] ], b, _, Proximo):-
%    Proximo is [ [PoteA], TA, [ 0, 0, 0, 0], [PoteB]],
%    !.

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
