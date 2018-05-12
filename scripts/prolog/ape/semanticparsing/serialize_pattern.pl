/*
Usage: serialize_pattern(+PatternList)
*/
serialize_lfe(PatternList) :-
    open('semanticparsing/data/lfe.pl',append,Stream),
    string:term_to_atom(PatternList,LFEName),
    fmt_write(Stream,"logical_syntactic_pattern(\'%S\').\n",args(LFEName)),
    write_rule_head(Stream,PatternList),
    write_rule_body(Stream,PatternList,1,_),
    close(Stream).

/*
Usage: write_rule_head(+Stream,+PatternList)
*/
write_rule_head(Stream,PatternList) :-
    string:term_to_atom(PatternList,RuleID),
    write(Stream,'apply_pattern_to_target(\''),
    write(Stream,RuleID),
    write(Stream,'\', DRSFacts, C1, C'),
    basics:length(PatternList,L1),
    L2 is L1 + 1,
    write(Stream,L2),
    write(Stream,') :-').

/*
Usage: write_rule_body(+Stream,+PatternList,+Counter1,-CounterN)
*/
write_rule_body(Stream,[Pattern1,Pattern2|RestPattern],Counter1,_) :-
    write_meta_pattern(Pattern1,Stream,Counter1,Counter2),
    write(Stream,','),
    write_rule_body(Stream,[Pattern2|RestPattern],Counter2,_).
    
write_rule_body(Stream,[Pattern],Counter1,_) :- 
    write_meta_pattern(Pattern,Stream,Counter1,_),
    write(Stream,'.\n\n').
    
/*
Usage: write_meta_pattern(+PName,+Stream,+Counter,-Counter2)
*/    
write_meta_pattern(pp_constraint(Preposition,PName),Stream,Counter,Counter2) :-
    !,
    write(Stream,'\n    get_predicate_from_word_index(DRSFacts, C'),
    write(Stream,Counter),
    write(Stream,', P'),
    write(Stream,Counter),
    write(Stream,'),'),
    write(Stream,'\n    '),
    write(Stream,'P'),
    write(Stream,Counter),
    write(Stream,' = modifier_pp(_,'),
    write(Stream,Preposition),
    write(Stream,',_)-_,'),
    dependency_pair(PName,RName),
    write(Stream,'\n    '),
    write(Stream,RName),
    write(Stream,'(DRSFacts, P'),
    write(Stream,Counter),
    write(Stream,', _ , C'),
    Counter2 is Counter + 1,
    write(Stream,Counter2),
    write(Stream,')').

write_meta_pattern(PName,Stream,Counter,Counter2) :-
    write(Stream,'\n    get_predicate_from_word_index(DRSFacts, C'),
    write(Stream,Counter),
    write(Stream,', P'),
    write(Stream,Counter),
    write(Stream,'),'),
    dependency_pair(PName,RName),
    write(Stream,'\n    '),
    write(Stream,RName),
    write(Stream,'(DRSFacts, P'),
    write(Stream,Counter),
    write(Stream,', _ , C'),
    Counter2 is Counter + 1,
    write(Stream,Counter2),
    write(Stream,')').

/*
Facts about meta-patterns
*/
dependency_pair('verb->subject','get_subject_from_verb').
dependency_pair('verb->object','get_object_from_verb').
dependency_pair('verb->iobject','get_iobject_from_verb').
dependency_pair('verb->pp','get_pp_from_verb').
dependency_pair('verb->adv','get_adv_from_verb').    
dependency_pair('pp->dep','get_dep_from_pp').    
dependency_pair('pp->verb','get_verb_from_pp').    
dependency_pair('adv->verb','get_verb_from_adv').    
dependency_pair('verb->adj','get_adj_from_verb').    
dependency_pair('adj->pobject','get_pobject_from_adj').
dependency_pair('adj->sobject','get_sobject_from_adj').   
dependency_pair('adj->tobject','get_tobject_from_adj').    
dependency_pair('adj->verb','get_verb_from_adj').    
dependency_pair('rel->lobject','get_lobject_from_rel').    
dependency_pair('rel->robject','get_robject_from_rel').   
dependency_pair('subject->verb','get_verb_from_subject').    
dependency_pair('object->verb','get_verb_from_object').    
dependency_pair('iobject->verb','get_verb_from_iobject').    
dependency_pair('dep->pp','get_pp_from_dep').    
dependency_pair('lobject->rel','get_rel_from_lobject').    
dependency_pair('robject->rel','get_rel_from_robject').    
dependency_pair('pobject->adj','get_adj_from_pobject').
dependency_pair('sobject->adj','get_adj_from_sobject').    
dependency_pair('tobject->adj','get_adj_from_tobject').
dependency_pair('rel->pp','get_pp_from_rel').
dependency_pair('pp->rel','get_rel_from_pp').
dependency_pair('pp->pp','get_pp_from_pp').
dependency_pair('has_part->object','get_object_from_has_part').
dependency_pair('object->has_part','get_has_part_from_object').
dependency_pair('ladj->rel','get_rel_from_ladj').
dependency_pair('radj->rel','get_rel_from_radj').
dependency_pair('rel->ladj','get_ladj_from_rel').
dependency_pair('rel->radj','get_radj_from_rel').
dependency_pair('verb->rel','get_rel_from_verb').
dependency_pair('self','get_self').    