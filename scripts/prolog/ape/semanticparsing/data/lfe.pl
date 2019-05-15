:- dynamic(logical_syntactic_pattern/1).
logical_syntactic_pattern('[verb->subject]').
apply_pattern_to_target('[verb->subject]', DRSFacts, C1, C2) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_subject_from_verb(DRSFacts, P1, _ , C2).

logical_syntactic_pattern('[verb->pp,pp_constraint(in,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(in,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,in),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(until,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(until,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,until),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->adv]').
apply_pattern_to_target('[verb->adv]', DRSFacts, C1, C2) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_adv_from_verb(DRSFacts, P1, _ , C2).

logical_syntactic_pattern('[verb->pp,pp_constraint(on,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(on,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,on),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(for,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(for,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,for),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(in,pp->rel),rel->robject]').
apply_pattern_to_target('[verb->pp,pp_constraint(in,pp->rel),rel->robject]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,in),
    get_rel_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_robject_from_rel(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[verb->pp,pp_constraint(at,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(at,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,at),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(as,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(as,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,as),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[object->verb,verb->subject]').
apply_pattern_to_target('[object->verb,verb->subject]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_subject_from_verb(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[pobject->adj]').
apply_pattern_to_target('[pobject->adj]', DRSFacts, C1, C2) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_adj_from_pobject(DRSFacts, P1, _ , C2).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(with,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(with,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,with),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->adv]').
apply_pattern_to_target('[object->verb,verb->adv]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_adv_from_verb(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[lobject->rel,rel->robject]').
apply_pattern_to_target('[lobject->rel,rel->robject]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_rel_from_lobject(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_robject_from_rel(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[self]').
apply_pattern_to_target('[self]', DRSFacts, C1, C2) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_self(DRSFacts, P1, _ , C2).

logical_syntactic_pattern('[subject->verb,verb->pp,pp_constraint(at,pp->dep)]').
apply_pattern_to_target('[subject->verb,verb->pp,pp_constraint(at,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_subject(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,at),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(as,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(as,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,as),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(from,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(from,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,from),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(in,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(in,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,in),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(during,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(during,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,during),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(at,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(at,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,at),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[subject->verb,verb->pp,pp_constraint(in,pp->dep)]').
apply_pattern_to_target('[subject->verb,verb->pp,pp_constraint(in,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_subject(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,in),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[subject->verb,verb->pp,pp_constraint(as,pp->dep)]').
apply_pattern_to_target('[subject->verb,verb->pp,pp_constraint(as,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_subject(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,as),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(on,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(on,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,on),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(with,pp->rel),rel->robject]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(with,pp->rel),rel->robject]', DRSFacts, C1, C5) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,with),
    get_rel_from_pp(DRSFacts, P3, _ , C4),
    get_predicate_from_word_index(DRSFacts, C4, P4),
    get_robject_from_rel(DRSFacts, P4, _ , C5).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(within,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(within,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,within),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[adj->verb,verb->subject]').
apply_pattern_to_target('[adj->verb,verb->subject]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_subject_from_verb(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[adj->verb,verb->pp,pp_constraint(in,pp->dep)]').
apply_pattern_to_target('[adj->verb,verb->pp,pp_constraint(in,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,in),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[dep->pp,pp_constraint(in,pp->verb),verb->subject]').
apply_pattern_to_target('[dep->pp,pp_constraint(in,pp->verb),verb->subject]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_dep(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,in),
    get_verb_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_subject_from_verb(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[dep->pp,pp_constraint(in,pp->pp),pp_constraint(by,pp->dep)]').
apply_pattern_to_target('[dep->pp,pp_constraint(in,pp->pp),pp_constraint(by,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_dep(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,in),
    get_pp_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,by),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[dep->pp,pp_constraint(under,pp->verb),verb->subject]').
apply_pattern_to_target('[dep->pp,pp_constraint(under,pp->verb),verb->subject]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_dep(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,under),
    get_verb_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_subject_from_verb(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[dep->pp,pp_constraint(at,pp->verb),verb->subject]').
apply_pattern_to_target('[dep->pp,pp_constraint(at,pp->verb),verb->subject]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_dep(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,at),
    get_verb_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_subject_from_verb(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[adj->pobject]').
apply_pattern_to_target('[adj->pobject]', DRSFacts, C1, C2) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pobject_from_adj(DRSFacts, P1, _ , C2).

logical_syntactic_pattern('[dep->pp,pp_constraint(to,pp->verb),verb->subject]').
apply_pattern_to_target('[dep->pp,pp_constraint(to,pp->verb),verb->subject]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_dep(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,to),
    get_verb_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_subject_from_verb(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[dep->pp,pp_constraint(toward,pp->verb),verb->subject]').
apply_pattern_to_target('[dep->pp,pp_constraint(toward,pp->verb),verb->subject]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_dep(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,toward),
    get_verb_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_subject_from_verb(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[adj->verb,verb->adv]').
apply_pattern_to_target('[adj->verb,verb->adv]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_adv_from_verb(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[rel->robject]').
apply_pattern_to_target('[rel->robject]', DRSFacts, C1, C2) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_robject_from_rel(DRSFacts, P1, _ , C2).

logical_syntactic_pattern('[rel->lobject]').
apply_pattern_to_target('[rel->lobject]', DRSFacts, C1, C2) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_lobject_from_rel(DRSFacts, P1, _ , C2).

logical_syntactic_pattern('[ladj->rel,rel->robject]').
apply_pattern_to_target('[ladj->rel,rel->robject]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_rel_from_ladj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_robject_from_rel(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[subject->verb,verb->object]').
apply_pattern_to_target('[subject->verb,verb->object]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_subject(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_object_from_verb(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[object->verb]').
apply_pattern_to_target('[object->verb]', DRSFacts, C1, C2) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(for,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(for,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,for),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(to,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(to,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,to),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[adj->verb,verb->pp,pp_constraint(to,pp->dep)]').
apply_pattern_to_target('[adj->verb,verb->pp,pp_constraint(to,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,to),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[adj->verb,verb->subject,has_part->object]').
apply_pattern_to_target('[adj->verb,verb->subject,has_part->object]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_subject_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_object_from_has_part(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[verb->object]').
apply_pattern_to_target('[verb->object]', DRSFacts, C1, C2) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_object_from_verb(DRSFacts, P1, _ , C2).

logical_syntactic_pattern('[verb->pp,pp_constraint(with,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(with,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,with),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(among,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(among,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,among),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->subject,has_part->object]').
apply_pattern_to_target('[verb->subject,has_part->object]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_subject_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_object_from_has_part(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(above,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(above,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,above),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(around,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(around,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,around),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(across,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(across,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,across),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(down,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(down,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,down),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(near,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(near,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,near),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(outside,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(outside,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,outside),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(upon,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(upon,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,upon),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(along,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(along,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,along),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(by,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(by,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,by),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[adj->verb,verb->pp,pp_constraint(along,pp->dep)]').
apply_pattern_to_target('[adj->verb,verb->pp,pp_constraint(along,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,along),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[adj->verb,verb->pp,pp_constraint(on,pp->dep)]').
apply_pattern_to_target('[adj->verb,verb->pp,pp_constraint(on,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,on),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[adj->verb,verb->pp,pp_constraint(up,pp->dep)]').
apply_pattern_to_target('[adj->verb,verb->pp,pp_constraint(up,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,up),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[verb->pp,pp_constraint(within,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(within,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,within),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(from,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(from,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,from),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[verb->pp,pp_constraint(at,pp->rel),rel->robject]').
apply_pattern_to_target('[verb->pp,pp_constraint(at,pp->rel),rel->robject]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,at),
    get_rel_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_robject_from_rel(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[verb->iobject]').
apply_pattern_to_target('[verb->iobject]', DRSFacts, C1, C2) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_iobject_from_verb(DRSFacts, P1, _ , C2).

logical_syntactic_pattern('[verb->pp,pp_constraint(to,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(to,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,to),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[object->verb,verb->subject,has_part->object]').
apply_pattern_to_target('[object->verb,verb->subject,has_part->object]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_subject_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_object_from_has_part(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(over,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(over,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,over),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[verb->pp,pp_constraint(from,pp->rel),rel->robject]').
apply_pattern_to_target('[verb->pp,pp_constraint(from,pp->rel),rel->robject]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,from),
    get_rel_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_robject_from_rel(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[adj->verb,verb->pp,pp_constraint(with,pp->dep)]').
apply_pattern_to_target('[adj->verb,verb->pp,pp_constraint(with,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,with),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[adj->verb,verb->pp,pp_constraint(as,pp->dep)]').
apply_pattern_to_target('[adj->verb,verb->pp,pp_constraint(as,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,as),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[adj->verb,verb->pp,pp_constraint(for,pp->dep)]').
apply_pattern_to_target('[adj->verb,verb->pp,pp_constraint(for,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,for),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[pp_constraint(from,pp->verb),verb->subject]').
apply_pattern_to_target('[pp_constraint(from,pp->verb),verb->subject]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    check_pp_predicate(P1,from),
    get_verb_from_pp(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_subject_from_verb(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[pp_constraint(from,pp->dep)]').
apply_pattern_to_target('[pp_constraint(from,pp->dep)]', DRSFacts, C1, C2) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    check_pp_predicate(P1,from),
    get_dep_from_pp(DRSFacts, P1, _ , C2).

logical_syntactic_pattern('[subject->verb,verb->pp,pp_constraint(with,pp->dep)]').
apply_pattern_to_target('[subject->verb,verb->pp,pp_constraint(with,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_subject(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,with),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(at,pp->rel),rel->robject]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(at,pp->rel),rel->robject]', DRSFacts, C1, C5) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,at),
    get_rel_from_pp(DRSFacts, P3, _ , C4),
    get_predicate_from_word_index(DRSFacts, C4, P4),
    get_robject_from_rel(DRSFacts, P4, _ , C5).

logical_syntactic_pattern('[dep->pp,pp_constraint(under,pp->pp),pp_constraint(by,pp->dep)]').
apply_pattern_to_target('[dep->pp,pp_constraint(under,pp->pp),pp_constraint(by,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_dep(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,under),
    get_pp_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,by),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[dep->pp,pp_constraint(under,pp->pp),pp_constraint(for,pp->dep)]').
apply_pattern_to_target('[dep->pp,pp_constraint(under,pp->pp),pp_constraint(for,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_dep(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,under),
    get_pp_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,for),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[dep->pp,pp_constraint(under,pp->pp),pp_constraint(on,pp->rel),rel->robject]').
apply_pattern_to_target('[dep->pp,pp_constraint(under,pp->pp),pp_constraint(on,pp->rel),rel->robject]', DRSFacts, C1, C5) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_dep(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,under),
    get_pp_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,on),
    get_rel_from_pp(DRSFacts, P3, _ , C4),
    get_predicate_from_word_index(DRSFacts, C4, P4),
    get_robject_from_rel(DRSFacts, P4, _ , C5).

logical_syntactic_pattern('[verb->pp,pp_constraint(on,pp->rel),rel->robject]').
apply_pattern_to_target('[verb->pp,pp_constraint(on,pp->rel),rel->robject]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,on),
    get_rel_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_robject_from_rel(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[object->verb,verb->pp,pp_constraint(against,pp->dep)]').
apply_pattern_to_target('[object->verb,verb->pp,pp_constraint(against,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_object(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,against),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[verb->rel,rel->robject]').
apply_pattern_to_target('[verb->rel,rel->robject]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_rel_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_robject_from_rel(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[subject->verb,verb->pp,pp_constraint(to,pp->dep)]').
apply_pattern_to_target('[subject->verb,verb->pp,pp_constraint(to,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_subject(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,to),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[verb->pp,pp_constraint(against,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(against,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    check_pp_predicate(P2,against),
    get_dep_from_pp(DRSFacts, P2, _ , C3).

logical_syntactic_pattern('[adj->verb,verb->pp,pp_constraint(from,pp->dep)]').
apply_pattern_to_target('[adj->verb,verb->pp,pp_constraint(from,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    check_pp_predicate(P3,from),
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[adj->verb,verb->pp,pp_constraint(by,pp->dep)]').
apply_pattern_to_target('[adj->verb,verb->pp,pp_constraint(by,pp->dep)]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_verb_from_adj(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    get_pp_from_verb(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    P3 = modifier_pp(_,by,_)-_,
    get_dep_from_pp(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[dep->pp,pp_constraint(as,pp->verb),verb->subject]').
apply_pattern_to_target('[dep->pp,pp_constraint(as,pp->verb),verb->subject]', DRSFacts, C1, C4) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_dep(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    P2 = modifier_pp(_,as,_)-_,
    get_verb_from_pp(DRSFacts, P2, _ , C3),
    get_predicate_from_word_index(DRSFacts, C3, P3),
    get_subject_from_verb(DRSFacts, P3, _ , C4).

logical_syntactic_pattern('[verb->pp,pp_constraint(under,pp->dep)]').
apply_pattern_to_target('[verb->pp,pp_constraint(under,pp->dep)]', DRSFacts, C1, C3) :-
    get_predicate_from_word_index(DRSFacts, C1, P1),
    get_pp_from_verb(DRSFacts, P1, _ , C2),
    get_predicate_from_word_index(DRSFacts, C2, P2),
    P2 = modifier_pp(_,under,_)-_,
    get_dep_from_pp(DRSFacts, P2, _ , C3).

