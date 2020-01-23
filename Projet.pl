%:- unknown(trace,fail). %inconnu
:- dynamic(negatif/1).

:- dynamic(chimpanze/0).
:- dynamic(gorille/0).
:- dynamic(cochon_dinde/0).
:- dynamic(cochon/0).
:- dynamic(chat/0).
:- dynamic(chien/0).
:- dynamic(cheval/0).
:- dynamic(kangourou/0).
:- dynamic(aigle/0).
:- dynamic(corbeau/0).
:- dynamic(sardine/0).
:- dynamic(brochet/0).
:- dynamic(poulpe/0).
:- dynamic(dauphin/0).
:- dynamic(fourmi/0).
:- dynamic(mouche/0).
:- dynamic(moustique/0).
:- dynamic(poule/0).
:- dynamic(iguane/0).
:- dynamic(lion/0).
:- dynamic(elephant/0).
:- dynamic(abeille/0).
:- dynamic(cameleon/0).
:- dynamic(crabe/0).
:- dynamic(tortue_mer/0).
:- dynamic(tortue_terre/0).
:- dynamic(etoile_de_mer/0).
:- dynamic(ver_de_terre/0).
:- dynamic(flamant_rose/0).
:- dynamic(cobra/0).
:- dynamic(ours/0).

%-------------------------------
% BASE DES REGLES
%-------------------------------

%----- Base 1 ------ 
oiseau :- vertebre, tetrapode, ovipare, bipede, aile, bec, plumes.
mammifere :- vertebre, tetrapode, allaite.
poisson :- vertebre, aquatique, ovipare, branchie, nageoires.
reptile :- vertebre, tetrapode, sang_froid,  ecaille, ovipare.
arthropode :- exosquelette, invertebre, corps_segmente. 

%----- Base 2 ------ 
placentaires :- mammifere, placenta.
ongule :- placentaires, sabot.
marsupiaux :- mammifere, larve_marsupiale, poil. 
insecte :- arthropode, six_pattes, taille_petite.
crustaces :- arthropode, aquatique, tetrapode. 

%----- Base 3 ------ 
rapace :- oiseau, vole, serres, carnivore, bipede. 
rongeur :- placentaires, quadripede, taille_petite, seulement_incisives, poil.
felin :- placentaires, quadripede, griffe, griffe_retractile, poil, carnivore.
canide :- placentaires, quadripede, poil, carnivore, griffe. 
cephalopode :- aquatique, tentacule, invertebre, mollusque. 

%##############################################
%############# Regles animales ################

chimpanze :- placentaires, primate, taille_moyenne, omnivore. 
gorille :- placentaires, primate, taille_grande, omnivore.
cochon_dinde :- rongeur, domestique.
chat :- felin, taille_moyenne, domestique.
chien :- canide, taille_moyenne, domestique, tendance_omnivore.
cochon :- ongule, omnivore, taille_moyenne, domestique, quadripede.
cheval :- ongule, herbivore, taille_grande, quadripede, domestique, poil.
kangourou :- marsupiaux, bipede, taille_grande, herbivore.
lion :- felin, taille_grande, criniere. 
elephant :- placentaires, taille_grande, trompe, defense, herbivore. 
ours :- placentaires, taille_grande, poil, pelage_dense, omnivore, griffe. 

aigle :- rapace, diurnes, taille_moyenne.
corbeau :- oiseau, vole, taille_moyenne, charognard, nuisible.
poule :- oiseau, taille_moyenne, domestique, herbivore, tendance_omnivore. 
flamant_rose :- oiseau, vole, carnivore, taille_grande, plumage_rose.

sardine :- poisson, taille_petite, omnivore.
brochet :- poisson, carnivore, taille_moyenne.
poulpe :- cephalopode, carnivore, huit_tentacule.
dauphin :- placentaires, aquatique, taille_grande, carnivore. 
crabe :- crustaces, pinces, taille_moyenne.
etoile_de_mer :- taille_moyenne, aquatique, carnivore, tentacule, echinoderme. 

iguane :- reptile, quadripede, herbivore, taille_moyenne, crete.
cameleon :- reptile, quadripede, insectivore, taille_moyenne, change_de_couleur, yeux_independants. 
tortue_mer :- reptile, carapace, taille_moyenne, aquatique, quadripede.
tortue_terre :- reptile, carapace, taille_moyenne, quadripede, griffe.
cobra :- reptile, carnivore, rampant, taille_moyenne, venimeux, collerette. 

fourmi :- insecte, social, nuisible, omnivore.
moustique :- insecte, aile, nuisible, suceur_de_sang, omnivore.  
mouche :- insecte, aile, nuisible, omnivore.
abeille :- insecte, social, aile, herbivore, pollinisateur. 
ver_de_terre  :- invertebre, rampant, taille_petite, vit_sous_terre. 

/*
mouton
hyene
panthere
leopard
tigre
jaguard
cocinelle
bourdon
sauterelles
mite
cafard
ver de terre 
serpent
python
rhinocéros
cobra
anguille
girafe
panda
paresseux
loup
dinde
cerf
elan
baleine
ecureuil
zebre
chevre
chauve-souris
autruche
crevettes
moule
*/

%-------------------------------
% BASE DES FAITS 
%-------------------------------
base(mammifere).
base(poisson).
base(oiseau).
base(reptile).
base(arthropode).

base(placentaires).
base(insecte).
base(marsupiaux).
base(ongule).
base(crustaces).

base(rapace).
base(rongeur).
base(felin).
base(canide).
base(cephalopode).

animal(chimpanze).
animal(gorille).
animal(cochon_dinde).
animal(chat).
animal(chien).
animal(cochon).
animal(cheval).
animal(kangourou).
animal(lion).
animal(elephant).
animal(ours).

animal(aigle).
animal(corbeau).
animal(poule).
animal(flamant_rose).

animal(sardine).
animal(brochet).
animal(poulpe).
animal(dauphin).
animal(crabe).
animal(etoile_de_mer).

animal(iguane).
animal(cameleon).
animal(tortue_mer).
animal(tortue_terre).
animal(cobra).

animal(fourmi).
animal(moustique).
animal(mouche).
animal(abeille).
animal(ver_de_terre).




animal :-
	si((animal(A), effacer([A],Trace,[])),
	ecrire_succes([A],Trace),
	write('Desole, aucun animal ne correspond a la description donnee')).



%############## ICI EXPERT ################

expertiser(L)  :- 
	si(effacer(L,Trace,[]),
	ecrire_succes(L, Trace), 
	ecrire_echec(L)).



ecrire_succes(L) :-print_conjonction(L,succes).

ecrire_echec(L)  :-print_conjonction(L,echec).

ecrire_succes(L, Trace) :-
     print_conjonction(L,succes),
     afficher_trace(Trace).
     %print(Trace).

print_conjonction([T],Etat) :-
	! , write('l''animal '),write(T),
	si(Etat=succes,
	write(' est etabli'),
	write(' n''est pas etabli')),nl.

print_conjonction(L,Etat) :-
	write('la conjonction de faits '),print_conjonction(L),
	si(Etat=succes,
	write(' est etablie'),
	write(' n''est pas etablie')), nl.

print_conjonction([]).

print_conjonction([T])  :-
	!, write(T).

print_conjonction([T|Q]):-
	write(T), write(' et '), 
	print_conjonction(Q).

si(C,A,_):-
	C,!,A.

si(_,_,S) :-
	S.


rule(T,CorpsListe) :-
	clause(T,CorpsTerme),
	termeToListe(CorpsTerme,CorpsListe).


% termeToListe/2 en mode (in,out)

termeToListe(true,[]) :- !.

termeToListe(Terme,[Terme]) :-
	atom(Terme),!.

termeToListe(Terme,[T|Q]):-
	arg(1,Terme,T),
	arg(2,Terme,TT),
	termeToListe(TT,Q).


afficher_trace(T) :-
	write('COMMENT : '), nl, afficher_trace(T,0).

%---------------------
afficher_trace([],_) :- !.

afficher_trace([[C]],Indent) :-
	atom(C),!,tab(Indent), 
	write('l''animal (possede/est) '),write(C),nl.

afficher_trace([[C|Q]],Indent) :-
	atom(C),!,tab(Indent), write(C), write(' ?'), nl,
	NewIndent is Indent + 5,
	afficher_trace(Q, NewIndent).

afficher_trace([X|Y], Indent) :-
	afficher_trace([X], Indent),
	afficher_trace(Y, Indent).


afficherPourquoi([]).

afficherPourquoi([T|Q]) :-
	write('Je pose cette question pour etablir l''animal '), 
	write(T),afficherPourquoiBis(Q).

afficherPourquoiBis([]).

afficherPourquoiBis([T|Q]) :-
	write(' puis '),write(T),
	afficherPourquoiBis(Q).


not(P) :- P , ! , fail.           %1
not(_). 

%##############################

effacer([],[],_).

effacer([But|_], _, _) :- negatif(But), !, fail.

effacer([But|AutresButs],[[But|TSousButs]|TAutresButs], Pourquoi) :-
	rule(But,SousButs),
	effacer(SousButs, TSousButs,[But|Pourquoi]), !,
	effacer(AutresButs, TAutresButs, Pourquoi).

effacer([But|AutresButs], [[But]|TAutresButs], Pourquoi) :-
	not(animal(But)),not(base(But)),	
	write('Est ce que l''animal (possede/est) '), write(But), write(' ? (o./n./p.):'),nl,
	afficherPourquoi(Pourquoi),nl,
	read(Rep),nl,
	si(Rep='o',
	(asserta(But),elimination(But), effacer(AutresButs, TAutresButs, Pourquoi)),
	si(Rep='n',(asserta(negatif(But)),fail), true)). %true si on ne sais pas vraiment. 


%######################
elimination(But) :-
	si(But = placenta,
	(asserta(negatif(ovipare)), asserta(negatif(larve_marsupiale))),
	si(But = vertebre,
	(asserta(negatif(invertebre))),
	si(But = invertebre,
	(asserta(negatif(vertebre))),
	si(But = quadripede,
	(asserta(negatif(bipede))),
	si(But = bipede,
	(asserta(negatif(quadripede))),
	si(But = taille_grande,
	(asserta(negatif(taille_moyenne)), asserta(negatif(taille_petite))),
	si(But = taille_moyenne,
	(asserta(negatif(taille_grande)), asserta(negatif(taille_petite))),
	si(But = taille_petite,
	(asserta(negatif(taille_moyenne)), asserta(negatif(taille_grande))),
	si(But = tetrapode,
	(asserta(negatif(arthropode)), asserta(negatif(cephalopode))),
	si(But = griffe,
	(asserta(negatif(sabot))),
	si(But = sabot,
	(asserta(negatif(griffe))),
	true))))))))))).
	
