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

createDeck(Player) :- 
    findall(card(Id, Nome, Atk, Def), card(Id, Nome, Atk, Def), B),
    random_permutation(B, C),
    assert(deck(Player, C)).

deck(Player) :- listing(deck(Player, _)).


    
