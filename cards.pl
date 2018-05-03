:- use_module(library(random)).

:- dynamic game_state/1, hand/2, deck/2, player/2, board/2.

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
    createPlayerHand(PlayerId).

createPlayerBoard(PlayerId) :-
		assert(board(PlayerId, [0,0,0])).

createPlayer(Id) :-
		assert(player(Id, 4000)).

summon(Player, Card) :-
		board(Player, [_, Y, Z]),
		erase(board(Player, _)),
		assert(board(Player, [Card, Y, Z])).

/* Used to make the monster leave the field */
destroyMonster(Player, Card) :-
    erase(board(Player, [Card, Y, Z])),
    assert(board(Player, [_, Y, Z])).

/* Used to resolve battle between monsters in attack position */
battleInAttackPosition(AttackingPlayer, AttackingMonster, Opponent, OpponentMonster) :-
    card(_,AttackingMonster,AttackingMonsterAtk,_),
    card(_,OpponentMonster, OpponentMonsterAtk,_),
    AttackingMonsterAtk > OpponentMonsterAtk,
    deduceLifePoints(Opponent, AttackingMonsterAtk - OpponentMonsterAtk),
    destroyMonster(Opponent, OpponentMonster).

deduceLifePoints(Player, Amount) :-
		player(Player, LifePoints),
		erase(player(Player, _)),
		assert(player(Player, LifePoints - Amount)).


createPlayerDeck(Player) :-
    findall(card(Id, Nome, Atk, Def), card(Id, Nome, Atk, Def), B),
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

draw(Player) :-
		deck(Player, Deck),
		pop(Deck, Card, Rest),
		updateDeck(Player, Rest),
		updateHand(Player, Card).

/* Used to draw player's initial hand */
drawInitialHand(Player, 0).
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
		startPlayer(2).

:- retractall(game_state(_)), initialize.
