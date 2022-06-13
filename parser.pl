% СЛОВАРЬ
noun([N | S], S, n(N), C) :- is_noun(N, C).
is_noun(man, [sing, cons]).
is_noun(men, [plur, any]).
is_noun(woman, [sing, cons]).
is_noun(women, [plur, any]).
is_noun(child, [sing, cons]).
is_noun(children, [plur, any]).
is_noun(book, [sing, cons]).
is_noun(books, [plur, any]).
is_noun(candle, [sing, cons]).
is_noun(candles, [plur, any]).
is_noun(owl, [sing, vow]).
is_noun(owls, [plur, any]).
is_noun(table, [sing, cons]).
is_noun(tables, [plur, any]).
is_noun(room, [sing, cons]).
is_noun(rooms, [plur, any]).

pronoun([N | S], S, pn(N), C) :-  is_pronoun(N, C).
is_pronoun(i, [sing, sub]).
is_pronoun(me, [any, obj]).
is_pronoun(my, [any, poss]).
is_pronoun(you, [plur, sub, obj]).
is_pronoun(your, [any, poss]).
is_pronoun(he, [sing, sub]).
is_pronoun(him, [any, obj]).
is_pronoun(his, [any, poss]).
is_pronoun(she, [sing, sub]).
is_pronoun(her, [any, obj, poss]).
is_pronoun(they, [plur, sub]).
is_pronoun(them, [any, obj]).
is_pronoun(their, [any, poss]).

is_pronoun(this, [sing, dem]).
is_pronoun(that, [sing, dem]).
is_pronoun(these, [plur, dem]).
is_pronoun(those, [plur, dem]).

article([N | S], S, a(N), C) :- is_article(N, C).
is_article(a, [sing, cons, indef]).
is_article(an, [sing, vow, indef]).
is_article(the, [any, any, def]).

verb([N | S], S, v(N), C) :- is_verb(N, C).
is_verb(am, [pn(i), pres]).
is_verb(is, [sing, pres]).
is_verb(are, [plur, pres]).
is_verb(was, [sing, past]).
is_verb(were, [plur, past]).
is_verb(like, [plur, pres]).
is_verb(likes, [sing, pres]).
is_verb(liked, [any, past]).
is_verb(look, [plur, pres]).
is_verb(looks, [sing, pres]).
is_verb(looked, [any, past]).
is_verb(read, [any, pres, past]).
is_verb(reads, [sing, pres]).
is_verb(hold, [plur, pres]).
is_verb(holds, [sing, pres]).
is_verb(held, [any, past]).
is_verb(play, [plur, pres]).
is_verb(playes, [sing, pres]).
is_verb(played, [any, past]).

adjective([N | S], S, adj(N), C) :- is_adjective(N, C).
is_adjective(nice, [cons]).
is_adjective(cozy, [cons]).
is_adjective(huge, [cons]).
is_adjective(pretty, [cons]).
is_adjective(amazing, [vow]).
is_adjective(attractive, [vow]).
is_adjective(blue, [cons]).
is_adjective(light, [cons]).
is_adjective(dark, [cons]).

adverb([N | S], S, adv(N), C) :- is_adverb(N, C).
is_adverb(firmly, [v, aft]).
is_adverb(thoughtfully, [v, aft]).
is_adverb(slowly, [v, bef, aft]).
is_adverb(carefully, [v, bef, aft]).
is_adverb(always, [v, bef]).
is_adverb(never, [v, bef]).
is_adverb(definitely, [v, bef]).
is_adverb(very, [adj, cons]).
is_adverb(really, [adj, cons]).
is_adverb(incredibly, [adj, vow]).
is_adverb(extremely, [adj, vow]).

numeral([N | S], S, nm(N), C) :- is_numeral(N, C).
is_numeral(one, [sing]).
is_numeral(two, [plur]).
is_numeral(three, [plur]).
is_numeral(twenty, [plur, comp]).
is_numeral(thirty, [plur, comp]).

preposition([N | S], S, p(N)) :- is_prepos(N).
is_prepos(in).
is_prepos(on).
is_prepos(at).
is_prepos(with).
is_prepos(without).

 
% ГРАММАТИКА
parser(S) :- sentence(S, [], T, _), write(T),nl, pptree(T), display_tree(T).

sentence(S, S2, st(NG, VG), C):- check_words(S), noun_group(S, S1, NG, C, [sub], E1), err(E1), verb_group(S1, S2, VG, NG, C, E2), err(E2).


noun_group(S, S1, ng(N), C, _, 0) :- noun(S, S1, N, C), !.
noun_group(S, S2, ng(DEF, N), C, _, 0) :- definition_group(S, S1, DEF, _, _), noun(S1, S2, N, C), !.
noun_group(S, S2, ng(A, N), C, _, ERR) :- article(S, S1, A, C1), noun(S1, S2, N, C), check_article(C1, [], C, ng(A, N), ERR), !.
noun_group(S, S2, ng(PN, N), [C|T], _, ERR) :- pronoun(S, S1, PN, C1), noun(S1, S2, N, [C|T]), check_pronoun([poss, C], C1, PN, N, ERR), !.
noun_group(S, S3, ng(A, DEF, N), C, _, ERR) :- article(S, S1, A, C1), definition_group(S1, S2, DEF, [C2 | _], [ADJ | _]), noun(S2, S3, N, C), 
					       check_article(C1, C2, C, ng(A, ADJ, N), ERR), !.
noun_group(S, S3, ng(PN, DEF, N), [C|T], _, ERR) :- pronoun(S, S1, PN, C1), definition_group(S1, S2, DEF, _, _), noun(S2, S3, N, [C|T]), 
						    check_pronoun([poss, C], C1, PN, N, ERR), !.
noun_group(S, S3, ng(ADV, DEF, N), C, _, ERR) :- adverb(S, S1, ADV, C1), definition_group(S1, S2, DEF, _, [ADJ | _]), noun(S2, S3, N, C), 
						 check_adverb(adj, C1, ADV, ADJ, ERR), !.
noun_group(S, S4, ng(A, ADV, DEF, N), C, _, ERR) :- article(S, S1, A, C1), adverb(S1, S2, ADV, [C3|С2]), 
						    definition_group(S2, S3, DEF, _, [ADJ | _]), noun(S3, S4, N, C), 
						    check_adverb(adj, [C3|С2], ADV, ADJ, E1), 
						    (E1==0, check_article(C1, С2, C, ng(A, ADV, N), E2), ERR=E2, ! ; 
						     E1==1, ERR=1, !).
noun_group(S, S4, ng(PN, ADV, DEF, N), [C|T], _, ERR) :- pronoun(S, S1, PN, C1), adverb(S1, S2, ADV, [C3|С2]), 
						         definition_group(S2, S3, DEF, _, [ADJ | _]), noun(S3, S4, N, [C|T]), 
						         check_adverb(adj, [C3|С2], ADV, ADJ, E1),
						         (E1==0, check_pronoun([poss, C], C1, PN, N, E2), ERR=E2, ! ; 
						          E1==1, ERR=1, !).
noun_group(S, S1, ng(PN), C, C1, ERR) :- pronoun(S, S1, PN, C), check_pronoun(C1, C, PN, PN, ERR), !.
noun_group(S, S2, ng(NMG, N), C, _, ERR) :- numeral_group(S, S1, NMG, C1, E1), noun(S1, S2, N, C),
					    (E1==0, check_numeral(C1, C, NMG, N, E2), ERR=E2, ! ; 
					     E1==1, ERR=1, !).
noun_group(S, S3, ng(NMG, DEF, N), C, _, ERR) :- numeral_group(S, S1, NMG, C1, E1), definition_group(S1, S2, DEF, _, _), noun(S2, S3, N, C),
					    	 (E1==0, check_numeral(C1, C, NMG, N, E2), ERR=E2, ! ; 
					     	  E1==1, ERR=1, !).
noun_group(S, S4, ng(NMG, ADV, DEF, N), C, _, ERR) :- numeral_group(S, S1, NMG, C1, E1), adverb(S1, S2, ADV, C2), 
						      definition_group(S2, S3, DEF, _, [ADJ | _]), noun(S3, S4, N, C),
					    	      (E1==0, check_numeral(C1, C, NMG, N, E2),
						     		(E2==0, check_adverb(adj, C2, ADV, ADJ, E3), ERR=E3, ! ;
								 E2==1, ERR=1, !) ; 
					     	       E1==1, ERR=1, !).

noun_group(_, _, _, _, [sub], 1) :- write("Недопустимая конструкция именной группы."), !.


verb_group(S, S1, vg(V), NG1, C, ERR) :- verb(S, S1, V, C1), check_verb(C, C1, V, NG1, ERR), !.
verb_group(S, S2, vg(V, NG), NG1, C, ERR) :- verb(S, S1, V, C1), check_verb(C, C1, V, NG1, E1),
					     (E1==0, noun_group(S1, S2, NG, _, [obj], E2), ERR=E2, ! ; 
					      E1==1, ERR=1, !).
verb_group(S, S2, vg(V, PG), NG1, C, ERR) :- verb(S, S1, V, C1), check_verb(C, C1, V, NG1, E1), 
					     (E1==0, prepos_group(S1, S2, PG, [obj], E2), ERR=E2, ! ; 
					      E1==1, ERR=1, !).
verb_group(S, S2, vg(V, ADV), NG1, C, ERR) :- verb(S, S1, V, C1), check_verb(C, C1, V, NG1, E1), 
					      (E1==0, adverb(S1, S2, ADV, C2), check_adverb([v, aft], C2, ADV, V, E2), ERR=E2, ! ; 
					       E1==1, ERR=1, !).
verb_group(S, S2, vg(ADV, V), NG1, C, ERR) :- adverb(S, S1, ADV, C2), verb(S1, S2, V, C1), check_verb(C, C1, V, NG1, E1), 
					      (E1==0, check_adverb([v, bef], C2, ADV, V, E2), ERR=E2, ! ; 
					       E1==1, ERR=1, !).
verb_group(S, S3, vg(V, NG, ADV), NG1, C, ERR) :- verb(S, S1, V, C1), check_verb(C, C1, V, NG1, E1), 
					          (E1==0, noun_group(S1, S2, NG, _, [obj], E2), 
							  (E2==0, adverb(S2, S3, ADV, C2), check_adverb([v, aft], C2, ADV, V, E3), ERR=E3, !; 
							   E2==1, ERR=1, !) ; 
						   E1==1, ERR=1, !).
verb_group(S, S3, vg(V, NG, PG), NG1, C, ERR) :- verb(S, S1, V, C1), check_verb(C, C1, V, NG1, E1),
					         (E1==0, noun_group(S1, S2, NG, _, [obj], E2),
							 (E2==0, prepos_group(S2, S3, PG, [obj], E3), ERR=E3, ! ; 
							  E2==1, ERR=1, !) ; 
						  E1==1, ERR=1, !).
verb_group(S, S3, vg(V, ADV, PG), NG1, C, ERR) :- verb(S, S1, V, C1), check_verb(C, C1, V, NG1, E1),
					          (E1==0, adverb(S1, S2, ADV, C2), check_adverb([v, aft], C2, ADV, V, E2),
							 (E2==0, prepos_group(S2, S3, PG, [obj], E3), ERR=E3, ! ; 
							  E2==1, ERR=1, !) ; 
						   E1==1, ERR=1, !).
verb_group(S, S4, vg(V, NG, ADV, PG), NG1, C, ERR) :- verb(S, S1, V, C1), check_verb(C, C1, V, NG1, E1), 
					              (E1==0, noun_group(S1, S2, NG, _, [obj], E2), 
							  (E2==0, adverb(S2, S3, ADV, C2), check_adverb([v, aft], C2, ADV, V, E3), 
								 (E3==0, prepos_group(S3, S4, PG, [obj], E4), ERR=E4, ! ;
								  E3==1, ERR=1, !); 
							   E2==1, ERR=1, !) ; 
						   E1==1, ERR=1, !).
verb_group(S, S3, vg(ADV, V, NG), NG1, C, ERR) :- adverb(S, S1, ADV, C2), verb(S1, S2, V, C1), check_verb(C, C1, V, NG1, E1),
					      	  (E1==0, check_adverb([v, bef], C2, ADV, V, E2),
						 	(E2==0, noun_group(S2, S3, NG, _, [obj], E3), ERR=E3, ! ;
							 E2==1, ERR=1, !) ; 
					       	   E1==1, ERR=1, !).
verb_group(S, S3, vg(ADV, V, PG), NG1, C, ERR) :- adverb(S, S1, ADV, C2), verb(S1, S2, V, C1), check_verb(C, C1, V, NG1, E1),
					      	  (E1==0, check_adverb([v, bef], C2, ADV, V, E2),
						 	(E2==0, prepos_group(S2, S3, PG, [obj], E3), ERR=E3, ! ;
							 E2==1, ERR=1, !) ; 
					       	   E1==1, ERR=1, !).
verb_group(S, S3, vg(ADV, V, ADV2), NG1, C, ERR) :- adverb(S, S1, ADV, C2), verb(S1, S2, V, C1), check_verb(C, C1, V, NG1, E1),
					      	    (E1==0, check_adverb([v, bef], C2, ADV, V, E2),
						 	(E2==0, adverb(S2, S3, ADV2, C3), check_adverb([v, aft], C3, ADV2, V, E3), ERR=E3, ! ;
							 E2==1, ERR=1, !) ; 
					       	     E1==1, ERR=1, !).
verb_group(S, S4, vg(ADV, V, NG, ADV2), NG1, C, ERR) :- adverb(S, S1, ADV, C2), verb(S1, S2, V, C1), check_verb(C, C1, V, NG1, E1),
					      	        (E1==0, check_adverb([v, bef], C2, ADV, V, E2),
						 		(E2==0, noun_group(S2, S3, NG, _, [obj], E3), 
									(E3==0, adverb(S3, S4, ADV2, C4), 
										check_adverb([v, aft], C4, ADV2, V, E4), ERR=E4, ! ;
									 E3==1, ERR=1, !);
							 	 E2==1, ERR=1, !) ; 
					       	   	 E1==1, ERR=1, !).
verb_group(S, S4, vg(ADV, V, NG, PG), NG1, C, ERR) :- adverb(S, S1, ADV, C2), verb(S1, S2, V, C1), check_verb(C, C1, V, NG1, E1),
					      	      (E1==0, check_adverb([v, bef], C2, ADV, V, E2),
							      (E2==0, noun_group(S2, S3, NG, _, [obj], E3),
								      (E3==0, prepos_group(S3, S4, PG, [obj], E4), ERR=E4, ! ;
								       E3==1, ERR=1, !);
							       E2==1, ERR=1, !) ; 
					       	       E1==1, ERR=1, !).
verb_group(S, S4, vg(ADV, V, ADV2, PG), NG1, C, ERR) :- adverb(S, S1, ADV, C2), verb(S1, S2, V, C1), check_verb(C, C1, V, NG1, E1),
					      	      (E1==0, check_adverb([v, bef], C2, ADV, V, E2),
							      (E2==0, adverb(S2, S3, ADV2, C3), check_adverb([v, aft], C3, ADV2, V, E3),
								      (E3==0, prepos_group(S3, S4, PG, [obj], E4), ERR=E4, ! ;
								       E3==1, ERR=1, !);
							       E2==1, ERR=1, !) ; 
					       	       E1==1, ERR=1, !).
verb_group(S, S5, vg(ADV, V, NG, ADV2, PG), NG1, C, ERR) :- adverb(S, S1, ADV, C2), verb(S1, S2, V, C1), check_verb(C, C1, V, NG1, E1),
					      	            (E1==0, check_adverb([v, bef], C2, ADV, V, E2), 
								    (E2==0, noun_group(S2, S3, NG, _, [obj], E3),
									    (E3==0, adverb(S3, S4, ADV2, C4),
										check_adverb([v, aft], C4, ADV2, V, E4), 
										(E4==0,  prepos_group(S4, S5, PG, [obj], E5), ERR=E5, ! ;
										 E4==1, ERR=1, !) ;
									     E3==1, ERR=1, !);
							 	     E2==1, ERR=1, !) ; 
					       	   	     E1==1, ERR=1, !).
verb_group(_, _, _, _, _, 1) :- write("Недопустимая конструкция глагольной группы.").


definition_group(S, S2, dg(ADJ, DG), [C | T], [ADJ | T2]) :- adjective(S, S1, ADJ, C), definition_group(S1, S2, DG, T, T2), !.
definition_group(S, S1, dg(ADJ), [C], [ADJ]) :- adjective(S, S1, ADJ, C).

numeral_group(S, S2, nmg(M1, M2), [plur], 0) :- numeral(S, S1, M1, [_, comp]), numeral(S1, S2, M2, [_]), !.
numeral_group(S, S2, nmg(M1, M2), [], 1) :- numeral(S, S1, M1, _), numeral(S1, S2, M2, _), write("Несогласована группа числительного "),
					    write_nm(nmg(M1, M2)), write("(недопустимое построение составного числительного)."),!.
numeral_group(S, S1, nmg(M), [C], 0) :- numeral(S, S1, M, [C | _]), !.

prepos_group(S, S2, pg(P, NG), C1, ERR) :- preposition(S, S1, P), noun_group(S1, S2, NG, _, C1, ERR), !.
prepos_group(S, S3, pg(P, NG, PG), C1, ERR) :- preposition(S, S1, P), noun_group(S1, S2, NG, _, C1, ERR1), 
					   prepos_group(S2, S3, PG, C1, ERR2), max(ERR1, ERR2, ERR).


err(1) :- !, fail. 
err(0). 


% ПРОВЕРКА КОНТЕКСТА (СОГЛАСОВАНИЯ)
check_article(C1, C2, C, ng(a(A), AD, n(N)), ERR) :- check_art_number(C1, C, ng(a(A), n(N)), E1),  
						     (E1==0, check_art_vowel(C1, C2, C, ng(a(A), AD), E2), ERR=E2, ! ; 
						      E1==1, ERR=1, !).
check_article(C1, C2, C, G, ERR) :- check_art_number(C1, C, G, E1), 
				    (E1==0, check_art_vowel(C1, C2, C, G, E2), ERR=E2, ! ; 
				     E1==1, ERR=1, !).

check_art_number([any | _], _, _, 0) :- !.
check_art_number([C | _], [C | _], _, 0) :- !.
check_art_number(C1, C2, ng(a(A), n(N)), 1) :- write("Heсогласован артикль "), write_w(A), write_c(C1), 
				               write(" с существительным "), write_w(N), write_c(C2), write(" в числе.").

check_art_vowel([_, _, def], _, _, ng(a(A), adv(D)), 1) :- write("Heсогласован артикль "), write_w(A), 
				                 	   write(" с наречием "), write_w(D), 
							   write(" (определенный артикль не употребляется с наречием)."), !.
check_art_vowel([_, any, _], _, _, _, 0) :- !.
check_art_vowel([_, C, _], [], [_, C], _, 0) :- !.
check_art_vowel([_, C, _], [C], _, _, 0) :- !.
check_art_vowel([_, C1, _], _, [_, C2], ng(a(A), n(N)), 1) :- write("Heсогласован артикль "), write_w(A), write_c(C1),
				               		      write(" с существительным "), write_w(N), write_c(C2), 
							      write(" по первой букве существительного.").
check_art_vowel([_, C1, _], [C2], _, ng(a(A), adj(J)), 1) :- write("Heсогласован артикль "), write_w(A), write_c(C1), 
				                 	     write(" с прилагательным "), write_w(J), write_c(C2), 
							     write(" по первой букве прилагательного.").
check_art_vowel([_, C1, _], [C2], _, ng(a(A), adv(D)), 1) :- write("Heсогласован артикль "), write_w(A), write_c(C1), 
				                 	     write(" с наречием "), write_w(D), write_c(C2), 
							     write(" по первой букве наречия.").

check_numeral([C], [C | _], _, _, 0) :- !.
check_numeral([C1], [C2 | _], NMG, n(N), 1) :- write("Heсогласовано числительное "), write_nm(NMG), write_c(C1),
                                               write(" с существительным "), write_w(N), write_c(C2), write(" в числе.").


check_verb(_, _, v(is), ng(pn(i)), 1) :- write("Несогласован глагол "), write_w(is), 
				  	 write(" с "), write_w(i), write(" в лице (местоимение 'i' употребляется с глаголом 'am')."), !.
check_verb([C | _], [C | _], _, _, 0) :- !.
check_verb(_, [any | _], _, _, 0) :- !.
check_verb(_, _, v(am), ng(pn(i)), 0) :- !. 
check_verb(_, _, v(am), NG, 1) :- write("Несогласован глагол "), write_w(am), 
				  write(" с "), write_n(NG), write(" в лице (глагол 'am' употребляется только с местоимением 'i')."), !.
check_verb(C1, C2, v(V), NG, 1) :- write("Несогласован глагол "), write_w(V), write_c(C2), 
			           write(" с "), write_n(NG), write_c(C1), write(" в числе.").


check_pronoun([C | _], [_ | C1], _, _, 0) :- elem(C, C1), !.
check_pronoun([poss, C], [C, dem], _, _, 0) :- !.
check_pronoun([poss, C], [C1, dem], pn(PN), n(N), 1) :- write("Несогласовано местоимение "), write_w(PN), write_c(C1),
						        write(" с существительным "), write_w(N), write_c(C), write(" в числе."), !.
check_pronoun([C, _], [_, dem], _, _, 1) :- C==obj, ! ; C==sub, !.
check_pronoun([C | _], [_ | C1], pn(PN), _, 1) :- write("Несогласовано местоимение "), write_w(PN), write("("), write_cc(C1), 
					       write(" по форме (нужен: "), write_c(C), write(").").

check_adverb(adj, [adj | _], _, _, 0) :- !.
check_adverb([v, C], [v | C1], _, _, 0) :- elem(C, C1), !.
check_adverb(adj, _, adv(D), adj(J), 1) :- write("Несогласовано наречие "), write_w(D), write(" с прилагательным "), write_w(J), 
					   write(" (данное наречие не употребляется с прилагательными).").
check_adverb([v | _], [adj | _], adv(D), v(V), 1) :- write("Несогласовано наречие "), write_w(D), write(" с глаголом "), write_w(V), 
					     	     write(" (данное наречие не употребляется с глаголами).").
check_adverb([v | _], [v | C], adv(D), v(V), 1) :- write("Несогласовано наречие "), write_w(D), write(" с глаголом "), write_w(V), 
					     	   write(" (данное наречие должно употребляется "), write_c(C), write(").").

check_words([]) :- !.
check_words([X | T]) :- (is_noun(X, _),! ; is_pronoun(X, _),! ; is_article(X, _),! ; is_verb(X, _),! ; 
			 is_adjective(X, _),! ; is_adverb(X, _),! ; is_prepos(X),! ; is_numeral(X, _),!), check_words(T), !.
check_words([X | _]) :- write("Слова "), write_w(X), write(" не существует в словаре."), nl, print_dictionary(), !, fail.


% ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
max(X, Y, M) :- X>=Y, M=X, !;  X<Y, M=Y. 

elem(_, []) :- !, fail.
elem(X, [X | _]) :- !.
elem(X, [_ | T]) :- elem(X, T).


% ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ПЕЧАТИ
write_w(W) :- write("'"), write(W), write("'").

write_c([X | _]) :- write_c(X).
write_c(plur) :- write("(множ.ч.)").
write_c(sing) :- write("(един.ч.)").
write_c(cons) :- write("(согл.)").
write_c(vow) :- write("(гласн.)").
write_c(sub) :- write("cубъект").
write_c(obj) :- write("объект").
write_c(poss) :- write("притяжат.").
write_c(bef) :- write("перед глаголом").
write_c(aft) :- write("после глагола").

write_cc([X]) :- write_c(X), write(")"), !.
write_cc([X | T]) :- write_c(X), write(", "), write_cc(T).

write_n(ng(n(N))) :- write("существительным "), write_w(N).
write_n(ng(_, n(N))) :- write("существительным "), write_w(N).
write_n(ng(_, _, n(N))) :- write("существительным "), write_w(N).
write_n(ng(_, _, _, n(N))) :- write("существительным "), write_w(N).
write_n(ng(pn(PN))) :- write("местоимением "), write_w(PN).

write_nm(nmg(nm(M))) :- write_w(M).
write_nm(nmg(nm(M1), nm(M2))) :- write("'"), write(M1), write(' '), write(M2), write("'").


write_set([X]) :- write(X), write(.), nl, !.
write_set([X | T]) :- write(X), write(", "), write_set(T).


% ОТОБРАЖЕНИЕ И ПЕЧАТЬ ДЕРЕВА
use_module(library(pce)).

display_tree(Tree) :- sformat(A, 'Дерево синтаксического анализа'),
		      new(D, window(A)),
		      send(D, size, size(1300, 500)),
		      functor(Tree, F, _), full_functor(F, FF),
		      new(S, tree(text(FF))),
		      send(S, neighbour_gap, 10),
			
		      display_t(Tree, S), 

		      send(S, direction, vertical),
		      send(D, display, S),
		      send(D, open).

display_t(T, _) :- atom(T), !.
display_t(T, S) :- term_sons(T, Sons),
		   send_list(S, son, Sons),
		   findall(_, (
		   		arg(N, T, TT), 
				elem_n(N, Sons, SS), 
				display_t(TT, SS)),
			   _).

term_sons(T, [S]) :- arg(_, T, X), atom(X), new(S, node(text(X))), !.
term_sons(T, Sons) :- findall(S, (
		    		 arg(_, T, X), 
		    		 functor(X, F, _), 
		    		 full_functor(F, FF), 
		    		 new(S, node(text(FF))) 
			 	), 
			      Sons).
elem_n(1, [X | _], X) :- !.
elem_n(N, [_ | T], X) :- N1 is N-1, elem_n(N1, T, X).


pptree(T) :- pptree(0, T), nl, !.
pptree(_, X) :- atom(X), % atom(man) --> true.
		write(X), !. 
pptree(T, X) :- functor(X, F, N), % functor(ng(a(the),n(candles)), F, N) --> F = ng, N = 2.
		full_functor(F, FF),
		nl, tab(T),
		write(FF), write('('),
		T1 is T + 3,
		process_args(1, N, X, T1), % последоваетльная обработка аргументов
		(arg(_, X, A), atom(A), write(')'), ! ; 
		 nl, tab(T), write(')')).

process_args(N, N, X, T) :- arg(N, X, X2), % arg(N, ng(a(the),n(candles)), X2) --> N = 1, X2 = a(the)  ;  N = 2, X2 = n(candles).
			    pptree(T, X2), !.
process_args(N1, N, X, T) :- arg(N1, X, X2),
			     pptree(T, X2),
			     N2 is N1 + 1,
			     process_args(N2, N, X, T).

full_functor(st, sentence).
full_functor(ng, noun_group).
full_functor(vg, verb_group).
full_functor(dg, definition_group).
full_functor(nmg, numeral_group).
full_functor(pg, preposition_group).
full_functor(n, noun).
full_functor(v, verb).
full_functor(pn, pronoun).
full_functor(a, article).
full_functor(adj, adjective).
full_functor(adv, adverb).
full_functor(nm, numeral).
full_functor(p, preposition).


% ДОПОЛНИТЕЛЬНЫЕ СПРАВОЧНЫЕ ФУНКЦИИ
print_dictionary :- write("---------------------------------------------------------"), nl, write("СЛОВАРЬ:"), nl, 
		    write("Nouns: "), findall(N, is_noun(N, _), Set), write_set(Set),
		    write("Pronouns: "), findall(R, is_pronoun(R, _), Set0), write_set(Set0),
		    write("Articles: "), findall(A, is_article(A, _), Set1), write_set(Set1),
		    write("Verbs: "), findall(V, is_verb(V, _), Set2), write_set(Set2),
		    write("Adjectives: "), findall(J, is_adjective(J, _), Set3), write_set(Set3),
		    write("Adverbs: "), findall(D, is_adverb(D, _), Set4), write_set(Set4),
		    write("Numerals: "), findall(M, is_numeral(M, _), Set44), write_set(Set44),
		    write("Prepositions: "), findall(P, is_prepos(P), Set5), write_set(Set5), 
		    write("---------------------------------------------------------"), !.


print_example :- print_ex([this, extremely, pretty, woman, always, reads, an, incredibly, huge, blue, book, thoughtfully, in, her, dark, room, with, thirty, three, candles]).

print_ex(S) :- nl, write("Предложение: "), nl, write_sent(S), nl, nl, write("Синтаксический разбор предложения:"), nl, parser(S), nl.

write_sent([X]) :- write(X), write(.), !.
write_sent([X | T]) :- write(X), write(" "), write_sent(T).


print_errors_examples :- \+ print_ex([a, attractive, pretty, nice, women, looks, at, the, huge, book, with, an, cozy, candles, on, the, table]), nl,nl,
			 \+ print_ex([a, attractive, pretty, nice, woman, looks, at, the, huge, book, with, an, cozy, candles, on, the, table]), nl,nl, 
			 \+ print_ex([an, attractive, pretty, nice, women, looks, at, the, huge, book, with, an, cozy, candles, on, the, table]), nl,nl,
			 \+ print_ex([an, attractive, pretty, nice, women, looks, at, the, huge, book, with, a, cozy, candles, on, the, table]), nl,nl,
			 \+ print_ex([children, carefully, playes, with, these, candles]), nl,nl, 
			 \+ print_ex([she, look, at, the, candles]), nl,nl,nl,
			 \+ print_ex([he, likes, she, with, their, light, cozy, candles]), nl,nl,
			 \+ print_ex([he, likes, her, with, them, light, cozy, candles]), nl,nl,
			 \+ print_ex([him, likes, her, with, their, light, cozy, candles]), nl,nl,nl,
			 \+ print_ex([the, very, pretty, woman, always, reads]), nl,nl,
			 \+ print_ex([a, extremely, pretty, woman, always, reads]), nl,nl,nl,
			 \+ print_ex([she, reads, definitely, in, her, room]), nl,nl,
			 \+ print_ex([she, firmly, holds, an, owl]), nl,nl,
			 \+ print_ex([she, very, holds, an, owl]), nl,nl,nl,
			 \+ print_ex([they, like, twenty, twenty, owls]), nl,nl,
			 \+ print_ex([they, like, twenty, two, owl]), nl,nl.
