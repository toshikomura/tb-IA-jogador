:- prolog_language('pop11').
false -> popmemlim;
false -> pop_prolog_lim;
10e7 -> pop_callstack_lim;
true -> popdprecision;
12 -> pop_pr_places;
:- prolog_language('prolog').

% memoria dinamica para:
% 1 - pilha do alpha e beta
% 2 - altura maxima
% 3 - melhor jogada
% 4 - jogador que eh a maquina
:- dynamic conjunto_d_alphas_e_betas/1.
:- dynamic altura_maxima/1.
:- dynamic melhor_jogada/2.
:- dynamic jogador_q_eh_maquina/1.

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

% Calculo da Funcao de Avalicao Estatica
% Aqui A é maquina

% Maquina ganhou
fae( [ [PoteA], _, _, _], _, Valor_Fae):-
    jogador_q_eh_maquina( a),
    PoteA >= 12,
    Valor_Fae is 999999,
    !.

% Maquina perdeu
fae( [ _, _, _, [PoteB]], _, Valor_Fae):-
    jogador_q_eh_maquina( a),
    PoteB >= 12,
    Valor_Fae is -999999,
    !.

% A jogou
fae( [ [PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [PoteB]], b, Valor_Fae):-
    jogador_q_eh_maquina( a),
    Peso_PoteA is PoteA * 10,
    Peso_PoteB is PoteB * 10,
    Peso_Pote is Peso_PoteA - Peso_PoteB,
    PesoA is BA1 + BA2 + BA3 + BA4,
    PesoB is BB1 + BB2 + BB3 + BB4,
    Peso_Campo is PesoA - PesoB,
    Valor_Fae is Peso_Pote + Peso_Campo,
    !.

% B jogou
fae( [ [PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [PoteB]], a, Valor_Fae):-
    jogador_q_eh_maquina( a),
    Peso_PoteA is PoteA * 10,
    Peso_PoteB is PoteB * 10,
    Peso_Pote is Peso_PoteB - Peso_PoteA,
    PesoA is BA1 + BA2 + BA3 + BA4,
    PesoB is BB1 + BB2 + BB3 + BB4,
    Peso_Campo is PesoB - PesoA,
    Valor_Fae is Peso_Pote + Peso_Campo,
    !.

% Aqui B é maquina
% Maquina ganhou
fae( [ [PoteA], _, _, _], _, Valor_Fae):-
    jogador_q_eh_maquina( b),
    PoteA >= 12,
    Valor_Fae is -999999,
    !.

% Maquina perdeu
fae( [ _, _, _, [PoteB]], _, Valor_Fae):-
    jogador_q_eh_maquina( b),
    PoteB >= 12,
    Valor_Fae is 999999,
    !.

% B jogou
fae( [ [PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [PoteB]], a, Valor_Fae):-
    jogador_q_eh_maquina( b),
    Peso_PoteA is PoteA * 10,
    Peso_PoteB is PoteB * 10,
    Peso_Pote is Peso_PoteB - Peso_PoteA,
    PesoA is BA1 + BA2 + BA3 + BA4,
    PesoB is BB1 + BB2 + BB3 + BB4,
    Peso_Campo is PesoB - PesoA,
    Valor_Fae is Peso_Pote + Peso_Campo,
    !.

% B jogou
fae( [ [PoteA], [ BA1, BA2, BA3, BA4], [ BB1, BB2, BB3, BB4], [PoteB]], b, Valor_Fae):-
    jogador_q_eh_maquina( b),
%    write( '\n\nJogador = b- '),
%    write( '\nCampo = '),
%    write( PoteA),
%    write( ' | '),
%    write( BA1),
%    write( ' '),
%    write( BA2),
%    write( ' '),
%    write( BA3),
%    write( ' '),
%    write( BA4),
%    write( ' | '),
%    write( BB1),
%    write( ' '),
%    write( BB2),
%    write( ' '),
%    write( BB3),
%    write( ' '),
%    write( BB4),
%    write(' | '),
%    write( PoteB),
%    write('\n\n'),
    Peso_PoteA is PoteA * 10,
    Peso_PoteB is PoteB * 10,
    Peso_Pote is Peso_PoteA - Peso_PoteB,
    PesoA is BA1 + BA2 + BA3 + BA4,
    PesoB is BB1 + BB2 + BB3 + BB4,
    Peso_Campo is PesoA - PesoB,
    Valor_Fae is Peso_Pote + Peso_Campo,
    !.

% min()
% Verifica se alcancou a altura maxima
min( Tabuleiro, Jogador, Altura, Valor_Fae):-
    altura_maxima( Altura),
    fae( Tabuleiro, Jogador, Valor_Fae),
%    remove_um_elemento_alpha_beta( _),
    !.

% Verifica se o tabuleiro eh fim de jogo
min( [ [PoteA], TA, TB, [PoteB]], Joagdor, _, Valor_Fae):-
    PoteA >= 12,
    fae( [ [ PoteA], TA, TB, [ PoteB]], Jogador, Valor_Fae),
%    remove_um_elemento_alpha_beta( _),
    !;
    PoteB >= 12,
    fae( [ [ PoteA], TA, TB, [ PoteB]], Jogador, Valor_Fae),
%    remove_um_elemento_alpha_beta( _),
    !.

min( Tabuleiro, Jogador, Altura, Valor_Fae):-
    adjacente( Tabuleiro, Jogador, Prox_Tabuleiro),
    adversario( Jogador, Prox_Jogador),
    Prox_Altura is Altura + 1,
    conjunto_d_alphas_e_betas( [ [ Alpha, Beta] | Elementos]),
%    insere_um_elemento_alpha_beta( [ Alpha, Beta]),
    max( Prox_Tabuleiro, Prox_Jogador, Prox_Altura, Valor_Fae_R), % Chamada Recursiva do max()
    menor( Beta, Valor_Fae_R, Menor_Beta),
%    remove_um_elemento_alpha_beta( _),
    Alpha >= Menor_Beta, % Poda
    Valor_Fae is Menor_Beta,
    !.

% Min definido
min( _, _, _, Valor_Fae):-
    conjunto_d_alphas_e_betas( [ [ _, Beta]| _]),
    Valor_Fae is Beta,
%    remove_um_elemento_alpha_beta( _),
    !.

% max()
% Verifica se alcancou a altura maxima
max( Tabuleiro, Jogador, Altura, Valor_Fae):-
    altura_maxima( Altura),
    fae( Tabuleiro, Jogador, Valor_Fae),
%    remove_um_elemento_alpha_beta( _),
    !.

% Veifica se o tabuleiro eh fim de jogo
max( [ [PoteA], TA, TB, [PoteB]], Jogador, _, Valor_Fae):-
    PoteA >= 12,
    fae( [ [PoteA], TA, TB, [PoteB]], Jogador, Valor_Fae),
%    remove_um_elemento_alpha_beta( _),
    !;
    PoteB >= 12,
    fae( [ [PoteA], TA, TB, [PoteB]], Jogador, Valor_Fae),
%    remove_um_elemento_alpha_beta( _),
    !.

max( Tabuleiro, Jogador, Altura, Valor_Fae):-
    adjacente( Tabuleiro, Jogador, Prox_Tabuleiro),
    adversario( Jogador, Prox_Jogador),
    Prox_Altura is Altura + 1,
    conjunto_d_alphas_e_betas( [ [ Alpha, Beta]| _]),
%    insere_um_elemento_alpha_beta( [ Alpha, Beta]),
    min( Prox_Tabuleiro, Prox_Jogador, Prox_Altura, Valor_Fae_R), % Recursão min()
    maior( Alpha, Valor_Fae_R, Alpha_Maior),
%    remove_um_elemento_alpha_beta( _),
%    insere_um_elemento_alpha_beta( [ Alpha_Maior, Beta]),
    Maior_Alpha >= Beta, % Poda
    Valor_Fae is Maior_Alpha,
%    remove_um_elemento_alpha_beta( _),
    !.

%Max definido
max( _, _, _, Valor_Fae):-
    conjunto_d_alphas_e_betas( [ [ Alpha, _]| _]),
    Valor_Fae is Alpha,
%    remove_um_elemento_alpha_beta( _),
    !.

% Predicados suporte que insere a melhor jogada
% Tabuleiro eh a melhor jogada
insere_melhor_jogada( Tabuleiro, Valor_Fae):-
    melhor_jogada( Tabuleiro_aux, Valor_Fae_aux),
    Valor_Fae > Valor_Fae_aux,
    write('Trocando a melhor jogada Fae ='),
    write(Valor_Fae),
    write('\n'),
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

% Identifica que eh o adversario da maquina
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
    fae( Prox_Tabuleiro, Adversario, Valor_Fae_aux),
    insere_melhor_jogada( Prox_Tabuleiro, Valor_Fae_aux), % Coloca a melhor jogada
%    remove_um_elemento_alpha_beta( _),

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
plausivel( [ PoteA, [ 0, 0, 0, 0], TB, PoteB ], a, _, Proximo):-
    Proximo is [ PoteA, [ 0, 0, 0, 0], TB, PoteB],
    !.

plausivel( [ PoteA, TA, [ 0, 0, 0, 0], PoteB ], b, _, Proximo):-
    Proximo is [ PoteA, TA, [ 0, 0, 0, 0], PoteB],
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

% teste
% plausivel( [ [0], [ 4, 4, 4, 4], [ 4, 4, 4, 4], [0]], a, 2, Proximo).
% plausivel( [ [0], [ 4, 4, 4, 4], [ 4, 4, 4, 4], [0]], b, 2, Proximo).
% plausivel( [ [0], [ 4, 4, 4, 4], [ 4, 4, 4, 4], [0]], a, 3, Proximo).
% plausivel( [ [0], [ 4, 4, 4, 4], [ 4, 4, 4, 4], [0]], b, 3, Proximo).
% plausivel( [ [4], [ 0, 0, 0, 0], [ 4, 4, 4, 4], [0]], a, 3, Proximo).
% plausivel( [ [0], [ 4, 4, 4, 4], [ 0, 0, 0, 0], [4]], b, 3, Proximo).
