:- use_module(library(random)).

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

startGame(PlayerOne, PlayerTwo) :-
    createPlayer(PlayerOne),
    createPlayer(PlayerTwo),
    createBoard(PlayerOne),
    createBoard(PlayerTwo),
    createDeck(PlayerOne),
    createDeck(PlayerTwo),
    createHand(PlayerOne),
    createHand(PlayerTwo).

createBoard(Player) :- assert(board(Player, [0,0,0])).
    
createPlayer(Name) :- assert(player(Name, 4000)).

summon(Player, Card) :- board(Player, [X, Y, Z]), erase(board(Player, _)), assert(board(Player, [Card, 0, 0])).

deduceLifePoints(Player, Amount) :- player(Player, LifePoints), erase(player(Player, _)), assert(player(Player, LifePoints - Amount)).

createDeck(Player) :- 
    findall(card(Id, Nome, Atk, Def), card(Id, Nome, Atk, Def), B),
    random_permutation(B, C),
    assert(deck(Player, C)).

createHand(Player) :-
    assert(hand(Player, [])).

pop([X|List],X,List).

push(X, List, [X | List]).

updateHand(Player, Card) :- hand(Player, Hand), erase(hand(Player, _)), push(Card, Hand, NewHand), assert(hand(Player, NewHand)).

draw(Player) :- deck(Player, Deck), pop(Deck, Card, Rest), updateDeck(Player, Rest), updateHand(Player, Card).

updateDeck(Player, X) :- erase(deck(Player, _)), assert(deck(Player, X)).

erase(X) :- erase1(X), fail.
erase(X).

erase1(X) :- retract(X).
erase1(X).

    
