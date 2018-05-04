:- use_module(library(random)).
:- use_module(library(lists)).

:- dynamic game_state/1, hand/2, deck/2, player/2, board/2, turn/1, firstTurn/1.

% player(Id, Life)

% card(Id, Nome, Atk, Def)
card(1, 'Mago Negro', 2500, 2100).
card(2, 'Dragao Branco de Olhos Azuis', 3000, 2500).
card(3, 'Rainha dos Insetos', 2200, 2400).
card(4, 'O criador', 2300, 3000).
card(5, 'Cyber Anja Dakini', 2700, 2400).
card(6, 'Mago do Tempo', 500, 400).
card(7, 'Boxeador', 1800, 1000).
card(8, 'Kuriboh', 300, 200).
card(9, 'Saffira Rainha dos Dragoes', 2500, 2400).
card(10, 'Levia-Dragon Daedalus', 2600, 1500).
card(11, 'Oraculo do Coveiro', 2000, 1500).
card(12, 'Jinzo', 2400, 1500).
card(13, 'Caveira Negra', 3200, 2500).
card(14, 'O Pescador Lendario', 2500, 2100).
card(15, 'Cyber-Stein', 700, 500).

startPlayer(PlayerId) :-
    createPlayer(PlayerId),
    createPlayerBoard(PlayerId),
    createPlayerDeck(PlayerId),
    createPlayerHand(PlayerId),
    drawInitialHand(PlayerId, 5).

createPlayerBoard(PlayerId) :-
		assert(board(PlayerId, [0,0,0])).

createTurn() :- 
    assert(turn(1)).

changeTurn() :-
    turn(X),
    X == 1 -> erase(turn(_)), assert(turn(2)) ; erase(turn(_)), assert(turn(1)).

nextPlayer(CurrentPlayer, NextPlayer) :-
    CurrentPlayer == 1 -> NextPlayer = 2 ;
    CurrentPlayer == 2 -> NextPlayer = 1 ;
    false.

createPlayer(Id) :-
		assert(player(Id, 4000)).

summon(Card) :-
    turn(Player),
    isInHand(Player, Card),
    \+ boardIsFull(Player),
    board(Player, [X, Y, Z]),
    (
        X == 0 -> erase(board(Player, _)), assert(board(Player, [Card, Y, Z]))
    ;   Y == 0 -> erase(board(Player, _)), assert(board(Player, [X, Card, Z]))
    ;   Z == 0 -> erase(board(Player, _)), assert(board(Player, [X, Y, Card]))
    ;   false
    ), 
    removeFromHand(Card),
    firstTurn(FirstTurn),
    FirstTurn -> erase(firstTurn(_)), assert(firstTurn(false)), changeTurn() ; true.

removeFromHand(Card) :-
    turn(Player),
    isInHand(Player, Card),
    hand(Player, Hand),
    delete(Hand, Card, NewHand),
    erase(hand(Player, _)),
    assert(hand(Player, NewHand)).

boardIsFull(Player) :-
    board(Player, [X, Y, Z]),
    X \= 0,
    Y \= 0,
    Z \= 0.

/* Used to make the monster leave the field */
destroyMonster(Player, Card) :-
    board(Player, [_, Y, Z]),
    erase(board(Player, [Card, Y, Z])),
    assert(board(Player, [0, Y, Z])).

isOnBoard(Player, Card) :-
    board(Player, List),
    memberCheck(Card, List).

isInHand(Player, Card) :-
    hand(Player, List),
    memberCheck(Card, List).

memberCheck(Elem, List) :-
    member(Elem, List), 
    !.

/* Used to resolve battle between monsters in attack position */
battleInAttackPosition(AttackingMonster, OpponentMonster) :-
    turn(AttackingPlayer),
    nextPlayer(AttackingPlayer, Opponent),
    isOnBoard(AttackingPlayer, AttackingMonster),
    isOnBoard(Opponent, OpponentMonster),
    card(_,AttackingMonster,AttackingMonsterAtk,_),
    card(_,OpponentMonster, OpponentMonsterAtk,_),
    (
        AttackingMonsterAtk > OpponentMonsterAtk -> deduceLifePoints(Opponent, AttackingMonsterAtk - OpponentMonsterAtk), destroyMonster(Opponent, OpponentMonster)
    ;   OpponentMonsterAtk > AttackingMonsterAtk -> deduceLifePoints(AttackingPlayer, OpponentMonsterAtk - AttackingMonsterAtk), destroyMonster(AttackingPlayer, AttackingMonster)
    ;   AttackingMonsterAtk =:= OpponentMonsterAtk -> destroyMonster(Opponent, OpponentMonster), destroyMonster(AttackingPlayer, AttackingMonster)
    ; false
    ), changeTurn().

deduceLifePoints(Player, Amount) :-
		player(Player, LifePoints),
		erase(player(Player, _)),
		assert(player(Player, LifePoints - Amount)).


createPlayerDeck(Player) :-
    findall(Nome, card(_, Nome, _, _), B),
    random_permutation(B, C),
    assert(deck(Player, C)).

createPlayerHand(PlayerId) :-
    assert(hand(PlayerId, [])).

pop([X|List],X,List).

push(X, List, [X | List]).

updateHand(Player, Card) :-
		hand(Player, Hand), erase(hand(Player, _)),
		push(Card, Hand, NewHand),
		assert(hand(Player, NewHand)).

draw() :-
    turn(Player),
    draw(Player).

draw(Player) :-
		deck(Player, Deck),
		pop(Deck, Card, Rest),
		updateDeck(Player, Rest),
		updateHand(Player, Card).

/* Used to draw player's initial hand */
drawInitialHand(_, 0).
drawInitialHand(Player, Number) :-
    Number > 0,
    draw(Player),
    UpdatedNumber is Number - 1,
    drawInitialHand(Player, UpdatedNumber).


updateDeck(Player, X) :-
		erase(deck(Player, _)),
		assert(deck(Player, X)).

erase(X) :-
		erase1(X),
		fail.
erase(_).

erase1(X) :-
		retract(X).
erase1(_).

initialize:-
		write("Yugioh Game"), nl,
		startPlayer(1),
        startPlayer(2),
        createTurn(),
        assert(firstTurn(true)).

:- retractall(game_state(_)), initialize.
