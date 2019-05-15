:- dynamic(fn_synonym/5).
:- index(fn_synonym/5,trie).
fn_synonym(X, Y, _, X, Y).
lvp(Lexem,POS,'Death',[pair('Protagonist','[verb->subject]',required),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Time','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('perish','predicate','Death',Lexem,POS).
fn_synonym('perish','predicate','Death','die','predicate').
lvp(Lexem,POS,'Death',[pair('Protagonist','[verb->subject]',required),pair('Cause','[verb->object]',required),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Time','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('die-of','predicate','Death',Lexem,POS).
lvp(Lexem,POS,'Death',[pair('Protagonist','[verb->subject]',required),pair('Cause','[verb->pp,pp_constraint(from,pp->dep)]',required),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Time','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('perish','predicate','Death',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[verb->subject]',required),pair('Employer','[verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[verb->pp,pp_constraint(as,pp->dep)]',optional),pair('Task','[verb->pp,pp_constraint(on,pp->dep)]',optional)]) :- fn_synonym('work','predicate','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[verb->subject]',required),pair('Employer','[verb->pp,pp_constraint(for,pp->dep)]',optional),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[verb->pp,pp_constraint(as,pp->dep)]',optional),pair('Task','[verb->pp,pp_constraint(on,pp->dep)]',optional)]) :- fn_synonym('work','predicate','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[verb->subject]',required),pair('Employer','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[verb->pp,pp_constraint(as,pp->dep)]',optional),pair('Task','[verb->pp,pp_constraint(on,pp->dep)]',optional)]) :- fn_synonym('work','predicate','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[verb->subject]',required),pair('Employer','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[verb->pp,pp_constraint(as,pp->dep)]',optional),pair('Task','[verb->pp,pp_constraint(on,pp->dep)]',optional)]) :- fn_synonym('work','predicate','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[pobject->adj]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
fn_synonym('job','object','Being_employed','stint','object').
fn_synonym('job','object','Being_employed','position','object').
fn_synonym('job','object','Being_employed','work','object').
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[pobject->adj]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[pobject->adj]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[pobject->adj]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[pobject->adj]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[pobject->adj]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[object->verb,verb->pp,pp_constraint(as,pp->dep)]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[object->verb,verb->pp,pp_constraint(as,pp->dep)]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[object->verb,verb->pp,pp_constraint(as,pp->dep)]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[object->verb,verb->pp,pp_constraint(as,pp->dep)]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[object->verb,verb->pp,pp_constraint(as,pp->dep)]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[object->verb,verb->pp,pp_constraint(as,pp->dep)]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->object]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->object]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->object]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->object]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->object]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->object]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->pp,pp_constraint(as,pp->dep)]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->pp,pp_constraint(as,pp->dep)]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->pp,pp_constraint(as,pp->dep)]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->pp,pp_constraint(as,pp->dep)]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->pp,pp_constraint(as,pp->dep)]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[lobject->rel,rel->robject]',required),pair('Employer','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[subject->verb,verb->pp,pp_constraint(as,pp->dep)]',required)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Task','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Task','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Task','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Task','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Task','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Field','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Task','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[adj->verb,verb->subject]',required),pair('Employer','[adj->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Place','[adj->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Field','[adj->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Position','[adj->verb,verb->pp,pp_constraint(as,pp->dep)]',required)]) :- fn_synonym('employed','property','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[verb->subject]',required),pair('Position','[verb->object]',required)]) :- fn_synonym('be','predicate','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Human_gender',[pair('Person','[verb->subject]',required),pair('Gender','[verb->object]',required)]) :- fn_synonym('be','predicate','Human_gender',Lexem,POS).
lvp(Lexem,POS,'Human_gender',[pair('Person','[object->verb,verb->subject]',required),pair('Gender','[lobject->rel,rel->robject]',required)]) :- fn_synonym('gender','object','Human_gender',Lexem,POS).
lvp(Lexem,POS,'Human_gender',[pair('Person','[lobject->rel,rel->robject]',required),pair('Gender','[subject->verb,verb->object]',required)]) :- fn_synonym('gender','object','Human_gender',Lexem,POS).
lvp(Lexem,POS,'Human_gender',[pair('Person','[object->verb,verb->subject]',required),pair('Gender','[pobject->adj]',required)]) :- fn_synonym('gender','object','Human_gender',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[verb->subject]',required),pair('Location','[verb->pp,pp_constraint(in,pp->dep)]',required),pair('Co-Resident','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('live','predicate','Residence',Lexem,POS).
fn_synonym('live','predicate','Residence','reside','predicate').
fn_synonym('live','predicate','Residence','camp','predicate').
fn_synonym('live','predicate','Residence','dwell','predicate').
fn_synonym('live','predicate','Residence','lodge','predicate').
fn_synonym('live','predicate','Residence','occupy','predicate').
fn_synonym('live','predicate','Residence','room','predicate').
fn_synonym('live','predicate','Residence','stay','predicate').
fn_synonym('live','predicate','Residence','inhabit','predicate').
lvp(Lexem,POS,'Residence',[pair('Resident','[verb->subject]',required),pair('Location','[verb->pp,pp_constraint(on,pp->dep)]',required),pair('Co-Resident','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('live','predicate','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[verb->subject]',required),pair('Location','[verb->pp,pp_constraint(above,pp->dep)]',required),pair('Co-Resident','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('live','predicate','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[verb->subject]',required),pair('Location','[verb->pp,pp_constraint(around,pp->dep)]',required),pair('Co-Resident','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('live','predicate','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[verb->subject]',required),pair('Location','[verb->pp,pp_constraint(across,pp->dep)]',required),pair('Co-Resident','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('live','predicate','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[verb->subject]',required),pair('Location','[verb->pp,pp_constraint(at,pp->dep)]',required),pair('Co-Resident','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('live','predicate','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[verb->subject]',required),pair('Location','[verb->pp,pp_constraint(down,pp->dep)]',required),pair('Co-Resident','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('live','predicate','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[verb->subject]',required),pair('Location','[verb->pp,pp_constraint(near,pp->dep)]',required),pair('Co-Resident','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('live','predicate','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[verb->subject]',required),pair('Location','[verb->pp,pp_constraint(outside,pp->dep)]',required),pair('Co-Resident','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('live','predicate','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[verb->subject]',required),pair('Location','[verb->pp,pp_constraint(upon,pp->dep)]',required),pair('Co-Resident','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('live','predicate','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[verb->subject]',required),pair('Location','[verb->pp,pp_constraint(along,pp->dep)]',required),pair('Co-Resident','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('live','predicate','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[object->verb,verb->subject]',required),pair('Location','[lobject->rel,rel->robject]',required)]) :- fn_synonym('resident','object','Residence',Lexem,POS).
fn_synonym('resident','object','Residence','occupant','object').
fn_synonym('resident','object','Residence','dweller','object').
fn_synonym('resident','object','Residence','inhabitant','object').
lvp(Lexem,POS,'Residence',[pair('Resident','[object->verb,verb->subject]',required),pair('Location','[object->verb,verb->pp,pp_constraint(from,pp->dep)]',required)]) :- fn_synonym('resident','object','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[object->verb,verb->subject]',required),pair('Location','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',required)]) :- fn_synonym('resident','object','Residence',Lexem,POS).
lvp(Lexem,POS,'Residence',[pair('Resident','[adj->verb,verb->subject]',required),pair('Location','[adj->verb,verb->pp,pp_constraint(in,pp->dep)]',required),pair('Co-Resident','[adj->verb,verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('inhabited','property','Residence',Lexem,POS).
fn_synonym('inhabited','property','Residence','camped','property').
lvp(Lexem,POS,'Being_born',[pair('Person','[adj->verb,verb->subject]',required),pair('Place','[adj->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Time','[adj->verb,verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('born','property','Being_born',Lexem,POS).
lvp(Lexem,POS,'Renting',[pair('Lessee','[verb->subject]',required),pair('Goods','[verb->object]',required),pair('Lessor','[verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Money','[verb->pp,pp_constraint(at,pp->rel),rel->robject]',optional)]) :- fn_synonym('rent','predicate','Renting',Lexem,POS).
fn_synonym('rent','predicate','Renting','charter','predicate').
fn_synonym('rent','predicate','Renting','lease','predicate').
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->subject]',required),pair('Goods','[verb->object]',required),pair('Seller','[verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Recipient','[verb->pp,pp_constraint(for,pp->dep)]',optional),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[verb->pp,pp_constraint(at,pp->rel),rel->robject]',optional)]) :- fn_synonym('buy','predicate','Commerce_buy',Lexem,POS).
fn_synonym('buy','predicate','Commerce_buy','purchase','predicate').
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->subject]',required),pair('Goods','[verb->object]',required),pair('Seller','[verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Recipient','[verb->pp,pp_constraint(for,pp->dep)]',optional),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[verb->pp,pp_constraint(for,pp->dep)]',optional)]) :- fn_synonym('buy','predicate','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->subject]',required),pair('Goods','[verb->object]',required),pair('Seller','[verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Recipient','[verb->pp,pp_constraint(for,pp->dep)]',optional),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('buy','predicate','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->subject]',required),pair('Goods','[verb->object]',required),pair('Seller','[verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Recipient','[verb->iobject]',optional),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[verb->pp,pp_constraint(at,pp->rel),rel->robject]',optional)]) :- fn_synonym('buy','predicate','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->subject]',required),pair('Goods','[verb->object]',required),pair('Seller','[verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Recipient','[verb->iobject]',optional),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[verb->pp,pp_constraint(for,pp->dep)]',optional)]) :- fn_synonym('buy','predicate','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->subject]',required),pair('Goods','[verb->object]',required),pair('Seller','[verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Recipient','[verb->iobject]',optional),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('buy','predicate','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[object->verb,verb->subject]',required),pair('Goods','[lobject->rel,rel->robject]',required),pair('Seller','[object->verb,verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Recipient','[object->verb,verb->pp,pp_constraint(for,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[object->verb,verb->pp,pp_constraint(at,pp->rel),rel->robject]',optional)]) :- fn_synonym('purchase','object','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[object->verb,verb->subject]',required),pair('Goods','[lobject->rel,rel->robject]',required),pair('Seller','[object->verb,verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Recipient','[object->verb,verb->pp,pp_constraint(for,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[object->verb,verb->pp,pp_constraint(for,pp->dep)]',optional)]) :- fn_synonym('purchase','object','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[object->verb,verb->subject]',required),pair('Goods','[lobject->rel,rel->robject]',required),pair('Seller','[object->verb,verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Recipient','[object->verb,verb->pp,pp_constraint(for,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('purchase','object','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->subject]',required),pair('Goods','[verb->pp,pp_constraint(for,pp->dep)]',required),pair('Money','[verb->object]',required)]) :- fn_synonym('pay','predicate','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->subject]',required),pair('Goods','[verb->pp,pp_constraint(for,pp->dep)]',required),pair('Money','[verb->object]',optional),pair('Seller','[verb->iobject]',required)]) :- fn_synonym('pay','predicate','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[object->verb,verb->subject]',required),pair('Goods','[lobject->rel,rel->robject]',required)]) :- fn_synonym('buyer','object','Commerce_buy',Lexem,POS).
fn_synonym('buyer','object','Commerce_buy','purchaser','object').
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->pp,pp_constraint(to,pp->dep)]',required),pair('Goods','[verb->object]',required),pair('Seller','[verb->subject]',required),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[verb->pp,pp_constraint(at,pp->rel),rel->robject]',optional)]) :- fn_synonym('sell','predicate','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->pp,pp_constraint(to,pp->dep)]',required),pair('Goods','[verb->object]',required),pair('Seller','[verb->subject]',required),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[verb->pp,pp_constraint(for,pp->dep)]',optional)]) :- fn_synonym('sell','predicate','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->iobject]',required),pair('Goods','[verb->object]',required),pair('Seller','[verb->subject]',required),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[verb->pp,pp_constraint(at,pp->rel),rel->robject]',optional)]) :- fn_synonym('sell','predicate','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Commerce_buy',[pair('Buyer','[verb->iobject]',required),pair('Goods','[verb->object]',required),pair('Seller','[verb->subject]',required),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Money','[verb->pp,pp_constraint(for,pp->dep)]',optional)]) :- fn_synonym('sell','predicate','Commerce_buy',Lexem,POS).
lvp(Lexem,POS,'Possession',[pair('Owner','[verb->subject]',required),pair('Possession','[verb->object]',required)]) :- fn_synonym('own','predicate','Possession',Lexem,POS).
fn_synonym('own','predicate','Possession','get','predicate').
fn_synonym('own','predicate','Possession','have','predicate').
fn_synonym('own','predicate','Possession','possess','predicate').
lvp(Lexem,POS,'Possession',[pair('Owner','[object->verb,verb->subject]',required),pair('Possession','[lobject->rel,rel->robject]',required)]) :- fn_synonym('owner','object','Possession',Lexem,POS).
fn_synonym('owner','object','Possession','possessor','object').
lvp(Lexem,POS,'Possession',[pair('Owner','[dep->pp,pp_constraint(in,pp->verb),verb->subject]',required),pair('Possession','[lobject->rel,rel->robject]',required)]) :- fn_synonym('possession','object','Possession',Lexem,POS).
lvp(Lexem,POS,'Possession',[pair('Owner','[object->verb,verb->subject]',required),pair('Possession','[lobject->rel,rel->robject]',required)]) :- fn_synonym('ownership','object','Possession',Lexem,POS).
lvp(Lexem,POS,'Cooking',[pair('Cook','[verb->subject]',required),pair('Food','[verb->object]',required),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Recipient','[verb->pp,pp_constraint(for,pp->dep)]',optional)]) :- fn_synonym('cook','predicate','Cooking',Lexem,POS).
fn_synonym('cook','predicate','Cooking','bake','predicate').
fn_synonym('cook','predicate','Cooking','make','predicate').
fn_synonym('cook','predicate','Cooking','fry','predicate').
fn_synonym('cook','predicate','Cooking','grill','predicate').
fn_synonym('cook','predicate','Cooking','boil','predicate').
fn_synonym('cook','predicate','Cooking','roast','predicate').
lvp(Lexem,POS,'Dressing',[pair('Wearer','[verb->subject]',required),pair('Clothing','[verb->pp,pp_constraint(in,pp->dep)]',required)]) :- fn_synonym('dress','predicate','Dressing',Lexem,POS).
fn_synonym('dress','predicate','Dressing','dress-up','predicate').
lvp(Lexem,POS,'Dressing',[pair('Wearer','[verb->subject]',required),pair('Clothing','[verb->object]',required)]) :- fn_synonym('put-on','predicate','Dressing',Lexem,POS).
fn_synonym('put-on','predicate','Dressing','pull-on','predicate').
lvp(Lexem,POS,'Dressing',[pair('Wearer','[verb->subject]',required),pair('Clothing','[verb->object]',required)]) :- fn_synonym('wear','predicate','Dressing',Lexem,POS).
lvp(Lexem,POS,'Travel',[pair('Traveler','[verb->subject]',required),pair('Source','[verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Goal','[verb->pp,pp_constraint(to,pp->dep)]',optional)]) :- fn_synonym('commute','predicate','Travel',Lexem,POS).
fn_synonym('commute','predicate','Travel','travel','predicate').
fn_synonym('commute','predicate','Travel','journey','predicate').
lvp(Lexem,POS,'Travel',[pair('Traveler','[object->verb,verb->subject]',required),pair('Source','[object->verb,verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Goal','[object->verb,verb->pp,pp_constraint(to,pp->dep)]',optional)]) :- fn_synonym('trip','object','Travel',Lexem,POS).
fn_synonym('trip','object','Travel','journey','object').
fn_synonym('trip','object','Travel','tour','object').
fn_synonym('trip','object','Travel','expedition','object').
fn_synonym('trip','object','Travel','getaway','object').
fn_synonym('trip','object','Travel','safari','object').
fn_synonym('trip','object','Travel','excursion','object').
fn_synonym('trip','object','Travel','odyssey','object').
fn_synonym('trip','object','Travel','voyage','object').
lvp(Lexem,POS,'Create_Organization',[pair('Creator','[verb->subject]',required),pair('Organization','[verb->object]',required),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('found','predicate','Create_Organization',Lexem,POS).
fn_synonym('found','predicate','Create_Organization','create','predicate').
fn_synonym('found','predicate','Create_Organization','make','predicate').
fn_synonym('found','predicate','Create_Organization','form','predicate').
fn_synonym('found','predicate','Create_Organization','yield','predicate').
lvp(Lexem,POS,'Create_Organization',[pair('Creator','[object->verb,verb->subject]',required),pair('Organization','[lobject->rel,rel->robject]',required),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('founder','object','Create_Organization',Lexem,POS).
fn_synonym('founder','object','Create_Organization','creator','object').
lvp(Lexem,POS,'Education',[pair('Student','[verb->subject]',required),pair('Institution','[verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Degree','[verb->object]',optional),pair('Subject','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('obtain','predicate','Education',Lexem,POS).
fn_synonym('obtain','predicate','Education','get','predicate').
fn_synonym('obtain','predicate','Education','earn','predicate').
fn_synonym('obtain','predicate','Education','receive','predicate').
lvp(Lexem,POS,'Education',[pair('Student','[verb->subject]',required),pair('Institution','[verb->pp,pp_constraint(in,pp->dep)]',required)]) :- fn_synonym('enroll','predicate','Education',Lexem,POS).
lvp(Lexem,POS,'Education',[pair('Student','[verb->subject]',required),pair('Institution','[verb->pp,pp_constraint(to,pp->dep)]',required)]) :- fn_synonym('go','predicate','Education',Lexem,POS).
lvp(Lexem,POS,'Education',[pair('Student','[verb->subject]',required),pair('Institution','[verb->object]',required)]) :- fn_synonym('enter','predicate','Education',Lexem,POS).
fn_synonym('enter','predicate','Education','attend','predicate').
lvp(Lexem,POS,'Education',[pair('Student','[verb->subject]',required),pair('Institution','[verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Subject','[verb->object]',optional)]) :- fn_synonym('study','predicate','Education',Lexem,POS).
lvp(Lexem,POS,'Education',[pair('Student','[verb->subject]',required),pair('Institution','[verb->pp,pp_constraint(from,pp->dep)]',required)]) :- fn_synonym('graduate','predicate','Education',Lexem,POS).
lvp(Lexem,POS,'Giving',[pair('Donor','[verb->subject]',required),pair('Recipient','[verb->pp,pp_constraint(to,pp->dep)]',required),pair('Theme','[verb->object]',optional)]) :- fn_synonym('donate','predicate','Giving',Lexem,POS).
fn_synonym('donate','predicate','Giving','contribute','predicate').
lvp(Lexem,POS,'Giving',[pair('Donor','[object->verb,verb->subject]',required),pair('Recipient','[object->verb,verb->pp,pp_constraint(to,pp->dep)]',required)]) :- fn_synonym('donor','object','Giving',Lexem,POS).
fn_synonym('donor','object','Giving','contributor','object').
lvp(Lexem,POS,'Giving',[pair('Donor','[verb->subject]',required),pair('Recipient','[verb->object]',required),pair('Theme','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('endow','predicate','Giving',Lexem,POS).
lvp(Lexem,POS,'Giving',[pair('Donor','[object->verb,verb->subject]',required),pair('Recipient','[object->verb,verb->pp,pp_constraint(to,pp->dep)]',required),pair('Theme','[lobject->rel,rel->robject]',optional)]) :- fn_synonym('donation','object','Giving',Lexem,POS).
fn_synonym('donation','object','Giving','contribution','object').
lvp(Lexem,POS,'Personal_relationship',[pair('Marriage_Partner_1','[adj->verb,verb->subject,has_part->object]',required),pair('Marriage_Partner_2','[adj->verb,verb->subject,has_part->object]',required)]) :- fn_synonym('married','property','Personal_relationship',Lexem,POS).
lvp(Lexem,POS,'Personal_relationship',[pair('Marriage_Partner_1','[adj->verb,verb->subject]',required),pair('Marriage_Partner_2','[adj->verb,verb->pp,pp_constraint(to,pp->dep)]',required)]) :- fn_synonym('married','property','Personal_relationship',Lexem,POS).
lvp(Lexem,POS,'Personal_relationship',[pair('Marriage_Partner_1','[object->verb,verb->subject,has_part->object]',required),pair('Marriage_Partner_2','[object->verb,verb->subject,has_part->object]',required)]) :- fn_synonym('couple','object','Personal_relationship',Lexem,POS).
lvp(Lexem,POS,'Personal_relationship',[pair('Mother','[object->verb,verb->subject]',required),pair('Child','[lobject->rel,rel->robject]',required)]) :- fn_synonym('mother','object','Personal_relationship',Lexem,POS).
lvp(Lexem,POS,'Personal_relationship',[pair('Daughter','[object->verb,verb->subject]',required),pair('Parent','[lobject->rel,rel->robject]',required)]) :- fn_synonym('daughter','object','Personal_relationship',Lexem,POS).
lvp(Lexem,POS,'Personal_relationship',[pair('Parent','[object->verb,verb->subject]',required),pair('Child','[lobject->rel,rel->robject]',required)]) :- fn_synonym('parent','object','Personal_relationship',Lexem,POS).
lvp(Lexem,POS,'Personal_relationship',[pair('Child','[object->verb,verb->subject]',required),pair('Parent','[lobject->rel,rel->robject]',required)]) :- fn_synonym('child','object','Personal_relationship',Lexem,POS).
lvp(Lexem,POS,'Personal_relationship',[pair('Wife','[object->verb,verb->subject]',required),pair('Husband','[lobject->rel,rel->robject]',required)]) :- fn_synonym('wife','object','Personal_relationship',Lexem,POS).
lvp(Lexem,POS,'Personal_relationship',[pair('Wife','[lobject->rel,rel->robject]',required),pair('Husband','[object->verb,verb->subject]',required)]) :- fn_synonym('husband','object','Personal_relationship',Lexem,POS).
lvp(Lexem,POS,'Personal_relationship',[pair('Parent','[lobject->rel,rel->robject]',required),pair('Son','[object->verb,verb->subject]',required)]) :- fn_synonym('son','object','Personal_relationship',Lexem,POS).
lvp(Lexem,POS,'Personal_relationship',[pair('Father','[object->verb,verb->subject]',required),pair('Child','[lobject->rel,rel->robject]',required)]) :- fn_synonym('father','object','Personal_relationship',Lexem,POS).
lvp(Lexem,POS,'People_by_origin',[pair('Person','[verb->subject]',required),pair('Origin','[verb->object]',required)]) :- fn_synonym('be','predicate','People_by_origin',Lexem,POS).
lvp(Lexem,POS,'People_by_origin',[pair('Person','[lobject->rel,rel->robject]',required),pair('Origin','[subject->verb,verb->object]',required)]) :- fn_synonym('ancestry','object','People_by_origin',Lexem,POS).
fn_synonym('ancestry','object','People_by_origin','nationality','object').
lvp(Lexem,POS,'People_by_origin',[pair('Person','[object->verb,verb->subject]',required),pair('Origin','[pobject->adj]',required)]) :- fn_synonym('ancestry','object','People_by_origin',Lexem,POS).
lvp(Lexem,POS,'People_by_religion',[pair('Person','[verb->subject]',required),pair('Religion','[verb->object]',required)]) :- fn_synonym('be','predicate','People_by_religion',Lexem,POS).
lvp(Lexem,POS,'People_by_religion',[pair('Person','[lobject->rel,rel->robject]',required),pair('Religion','[subject->verb,verb->object]',required)]) :- fn_synonym('religion','object','People_by_religion',Lexem,POS).
lvp(Lexem,POS,'Collaboration',[pair('Partner1','[verb->subject]',required),pair('Partner2','[verb->pp,pp_constraint(with,pp->dep)]',required),pair('Undertaking','[verb->pp,pp_constraint(on,pp->dep)]',optional)]) :- fn_synonym('work','predicate','Collaboration',Lexem,POS).
fn_synonym('work','predicate','Collaboration','partner','predicate').
fn_synonym('work','predicate','Collaboration','team-up','predicate').
fn_synonym('work','predicate','Collaboration','conspire','predicate').
fn_synonym('work','predicate','Collaboration','cooperate','predicate').
fn_synonym('work','predicate','Collaboration','collaborate','predicate').
lvp(Lexem,POS,'Collaboration',[pair('Partner1','[verb->subject]',required),pair('Partner2','[verb->pp,pp_constraint(with,pp->dep)]',required),pair('Undertaking','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('work','predicate','Collaboration',Lexem,POS).
lvp(Lexem,POS,'Collaboration',[pair('Partner1','[object->verb,verb->subject]',required),pair('Partner2','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',required),pair('Undertaking','[object->verb,verb->pp,pp_constraint(over,pp->dep)]',optional)]) :- fn_synonym('cooperation','object','Collaboration',Lexem,POS).
fn_synonym('cooperation','object','Collaboration','collaboration','object').
lvp(Lexem,POS,'Collaboration',[pair('Partner1','[object->verb,verb->subject]',required),pair('Partner2','[lobject->rel,rel->robject]',required)]) :- fn_synonym('partner','object','Collaboration',Lexem,POS).
fn_synonym('partner','object','Collaboration','collaborator','object').
lvp(Lexem,POS,'Quitting',[pair('Employee','[verb->subject]',required),pair('Employer','[verb->object]',required)]) :- fn_synonym('leave','predicate','Quitting',Lexem,POS).
lvp(Lexem,POS,'Quitting',[pair('Employee','[verb->subject]',required),pair('Position','[verb->object]',required)]) :- fn_synonym('quit','predicate','Quitting',Lexem,POS).
lvp(Lexem,POS,'Quitting',[pair('Employee','[verb->subject]',required),pair('Position','[verb->pp,pp_constraint(as,pp->dep)]',optional),pair('Employer','[verb->pp,pp_constraint(from,pp->dep)]',optional)]) :- fn_synonym('resign','predicate','Quitting',Lexem,POS).
fn_synonym('resign','predicate','Quitting','step-down','predicate').
fn_synonym('resign','predicate','Quitting','retire','predicate').
lvp(Lexem,POS,'Quitting',[pair('Employee','[verb->subject]',required),pair('Position','[verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Employer','[verb->pp,pp_constraint(from,pp->rel),rel->robject]',optional)]) :- fn_synonym('resign','predicate','Quitting',Lexem,POS).
lvp(Lexem,POS,'Quitting',[pair('Employee','[object->verb,verb->subject]',required),pair('Position','[object->verb,verb->pp,pp_constraint(as,pp->dep)]',optional),pair('Employer','[object->verb,verb->pp,pp_constraint(from,pp->dep)]',optional)]) :- fn_synonym('resignation','object','Quitting',Lexem,POS).
lvp(Lexem,POS,'Medical_conditions',[pair('Patient','[verb->subject]',required),pair('Ailment','[verb->object]',required)]) :- fn_synonym('get','predicate','Medical_conditions',Lexem,POS).
lvp(Lexem,POS,'Medical_conditions',[pair('Patient','[adj->verb,verb->subject]',required),pair('Ailment','[adj->verb,verb->pp,pp_constraint(with,pp->dep)]',required)]) :- fn_synonym('infected','property','Medical_conditions',Lexem,POS).
fn_synonym('infected','property','Medical_conditions','diagnosed','property').
fn_synonym('infected','property','Medical_conditions','ill','property').
lvp(Lexem,POS,'Medical_conditions',[pair('Patient','[verb->subject]',required),pair('Ailment','[verb->pp,pp_constraint(from,pp->dep)]',required)]) :- fn_synonym('suffer','predicate','Medical_conditions',Lexem,POS).
lvp(Lexem,POS,'Age',[pair('Person','[object->verb,verb->subject]',required),pair('Age','[lobject->rel,rel->robject]',required)]) :- fn_synonym('age','object','Age',Lexem,POS).
lvp(Lexem,POS,'Age',[pair('Person','[dep->pp,pp_constraint(at,pp->verb),verb->subject]',required),pair('Age','[lobject->rel,rel->robject]',required)]) :- fn_synonym('age','object','Age',Lexem,POS).
lvp(Lexem,POS,'Age',[pair('Person','[dep->pp,pp_constraint(under,pp->verb),verb->subject]',required),pair('Age','[lobject->rel,rel->robject]',required)]) :- fn_synonym('age','object','Age',Lexem,POS).
lvp(Lexem,POS,'Age',[pair('Person','[verb->subject]',required),pair('Age','[verb->object]',required)]) :- fn_synonym('be','predicate','Age',Lexem,POS).
lvp(Lexem,POS,'Award',[pair('Person','[verb->iobject]',required),pair('Award','[verb->object]',required),pair('Organization','[verb->subject]',optional),pair('Time','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('assign','predicate','Award',Lexem,POS).
fn_synonym('assign','predicate','Award','give','predicate').
fn_synonym('assign','predicate','Award','grant','predicate').
fn_synonym('assign','predicate','Award','award','predicate').
lvp(Lexem,POS,'Award',[pair('Person','[adj->verb,verb->pp,pp_constraint(to,pp->dep)]',required),pair('Award','[adj->verb,verb->subject]',required),pair('Time','[adj->verb,verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('awarded','property','Award',Lexem,POS).
fn_synonym('awarded','property','Award','given','property').
fn_synonym('awarded','property','Award','granted','property').
fn_synonym('awarded','property','Award','assigned','property').
lvp(Lexem,POS,'Using',[pair('Agent','[verb->subject]',required),pair('Instrument','[verb->object]',required)]) :- fn_synonym('use','predicate','Using',Lexem,POS).
fn_synonym('use','predicate','Using','operate','predicate').
fn_synonym('use','predicate','Using','apply','predicate').
fn_synonym('use','predicate','Using','employ','predicate').
fn_synonym('use','predicate','Using','exercise','predicate').
fn_synonym('use','predicate','Using','utilize','predicate').
lvp(Lexem,POS,'Using',[pair('Agent','[lobject->rel,rel->robject]',required),pair('Instrument','[lobject->rel,rel->robject]',required)]) :- fn_synonym('use','object','Using',Lexem,POS).
fn_synonym('use','object','Using','application','object').
fn_synonym('use','object','Using','operation','object').
fn_synonym('use','object','Using','utilization','object').
lvp(Lexem,POS,'Building',[pair('Agent','[verb->subject]',required),pair('Instrument','[verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Created_entity','[verb->object]',required)]) :- fn_synonym('build','predicate','Building',Lexem,POS).
fn_synonym('build','predicate','Building','construct','predicate').
fn_synonym('build','predicate','Building','assemble','predicate').
fn_synonym('build','predicate','Building','make','predicate').
fn_synonym('build','predicate','Building','put-together','predicate').
fn_synonym('build','predicate','Building','weld','predicate').
fn_synonym('build','predicate','Building','piece-together','predicate').
lvp(Lexem,POS,'Building',[pair('Agent','[object->verb,verb->subject]',required),pair('Instrument','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',optional),pair('Created_entity','[lobject->rel,rel->robject]',required)]) :- fn_synonym('construction','object','Building',Lexem,POS).
lvp(Lexem,POS,'Killing',[pair('Killer','[verb->subject]',required),pair('Victim','[verb->object]',required),pair('Instrument','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('kill','predicate','Killing',Lexem,POS).
fn_synonym('kill','predicate','Killing','annihilate','predicate').
fn_synonym('kill','predicate','Killing','assassinate','predicate').
fn_synonym('kill','predicate','Killing','butcher','predicate').
fn_synonym('kill','predicate','Killing','destroy','predicate').
fn_synonym('kill','predicate','Killing','decapitate','predicate').
fn_synonym('kill','predicate','Killing','drown','predicate').
fn_synonym('kill','predicate','Killing','eliminate','predicate').
fn_synonym('kill','predicate','Killing','exterminate','predicate').
fn_synonym('kill','predicate','Killing','massacre','predicate').
lvp(Lexem,POS,'Killing',[pair('Killer','[object->verb,verb->subject]',required),pair('Victim','[lobject->rel,rel->robject]',required)]) :- fn_synonym('killer','object','Killing',Lexem,POS).
fn_synonym('killer','object','Killing','murderer','object').
fn_synonym('killer','object','Killing','slaughterer','object').
fn_synonym('killer','object','Killing','assassinator','object').
lvp(Lexem,POS,'Arrest',[pair('Authorities','[verb->subject]',required),pair('Suspect','[verb->object]',required),pair('Charges','[verb->pp,pp_constraint(for,pp->dep)]',optional)]) :- fn_synonym('arrest','predicate','Arrest',Lexem,POS).
fn_synonym('arrest','predicate','Arrest','apprehend','predicate').
fn_synonym('arrest','predicate','Arrest','summon','predicate').
fn_synonym('arrest','predicate','Arrest','nab','predicate').
lvp(Lexem,POS,'Arrest',[pair('Authorities','[dep->pp,pp_constraint(under,pp->pp),pp_constraint(by,pp->dep)]',required),pair('Suspect','[dep->pp,pp_constraint(under,pp->verb),verb->subject]',required),pair('Charges','[dep->pp,pp_constraint(under,pp->pp),pp_constraint(for,pp->dep)]',optional)]) :- fn_synonym('arrest','object','Arrest',Lexem,POS).
fn_synonym('arrest','object','Arrest','apprehension','object').
lvp(Lexem,POS,'Arrest',[pair('Authorities','[dep->pp,pp_constraint(under,pp->pp),pp_constraint(by,pp->dep)]',required),pair('Suspect','[dep->pp,pp_constraint(under,pp->verb),verb->subject]',required),pair('Charges','[dep->pp,pp_constraint(under,pp->pp),pp_constraint(on,pp->rel),rel->robject]',optional)]) :- fn_synonym('arrest','object','Arrest',Lexem,POS).
lvp(Lexem,POS,'Arrest',[pair('Authorities','[verb->subject]',required),pair('Suspect','[verb->object]',required),pair('Charges','[verb->pp,pp_constraint(on,pp->rel),rel->robject]',optional)]) :- fn_synonym('arrest','predicate','Arrest',Lexem,POS).
lvp(Lexem,POS,'Attack',[pair('Assailant','[verb->subject]',required),pair('Victim','[verb->object]',required)]) :- fn_synonym('attack','predicate','Attack',Lexem,POS).
fn_synonym('attack','predicate','Attack','ambush','predicate').
fn_synonym('attack','predicate','Attack','assail','predicate').
fn_synonym('attack','predicate','Attack','assault','predicate').
fn_synonym('attack','predicate','Attack','bomb','predicate').
fn_synonym('attack','predicate','Attack','hit','predicate').
fn_synonym('attack','predicate','Attack','raid','predicate').
fn_synonym('attack','predicate','Attack','strike','predicate').
lvp(Lexem,POS,'Attack',[pair('Assailant','[object->verb,verb->subject]',required),pair('Victim','[object->verb,verb->pp,pp_constraint(on,pp->dep)]',required)]) :- fn_synonym('attack','object','Attack',Lexem,POS).
fn_synonym('attack','object','Attack','airstrike','object').
fn_synonym('attack','object','Attack','assailant','object').
fn_synonym('attack','object','Attack','assault','object').
fn_synonym('attack','object','Attack','bombardment','object').
fn_synonym('attack','object','Attack','bombing','object').
fn_synonym('attack','object','Attack','infiltration','object').
fn_synonym('attack','object','Attack','onset','object').
fn_synonym('attack','object','Attack','strike','object').
lvp(Lexem,POS,'Attack',[pair('Assailant','[object->verb,verb->subject]',required),pair('Victim','[object->verb,verb->pp,pp_constraint(against,pp->dep)]',required)]) :- fn_synonym('attack','object','Attack',Lexem,POS).
lvp(Lexem,POS,'Leadership',[pair('Leader','[verb->subject]',required),pair('Governed','[verb->object]',required),pair('Role','[verb->pp,pp_constraint(as,pp->dep)]',optional)]) :- fn_synonym('lead','predicate','Leadership',Lexem,POS).
fn_synonym('lead','predicate','Leadership','administer','predicate').
fn_synonym('lead','predicate','Leadership','chair','predicate').
fn_synonym('lead','predicate','Leadership','command','predicate').
fn_synonym('lead','predicate','Leadership','govern','predicate').
fn_synonym('lead','predicate','Leadership','head','predicate').
fn_synonym('lead','predicate','Leadership','reign','predicate').
fn_synonym('lead','predicate','Leadership','rule','predicate').
lvp(Lexem,POS,'Leadership',[pair('Leader','[object->verb,verb->subject]',required),pair('Governed','[lobject->rel,rel->robject]',required),pair('Role','[self]',optional)]) :- fn_synonym('leader','object','Leadership',Lexem,POS).
fn_synonym('leader','object','Leadership','boss','object').
fn_synonym('leader','object','Leadership','chairman','object').
fn_synonym('leader','object','Leadership','captain','object').
fn_synonym('leader','object','Leadership','chief','object').
fn_synonym('leader','object','Leadership','commander','object').
fn_synonym('leader','object','Leadership','director','object').
fn_synonym('leader','object','Leadership','executive','object').
fn_synonym('leader','object','Leadership','governor','object').
fn_synonym('leader','object','Leadership','head','object').
fn_synonym('leader','object','Leadership','president','object').
fn_synonym('leader','object','Leadership','ruler','object').
lvp(Lexem,POS,'Manufacturing',[pair('Producer','[verb->subject]',required),pair('Product','[verb->object]',required)]) :- fn_synonym('manufacture','predicate','Manufacturing',Lexem,POS).
fn_synonym('manufacture','predicate','Manufacturing','fabricate','predicate').
fn_synonym('manufacture','predicate','Manufacturing','produce','predicate').
lvp(Lexem,POS,'Manufacturing',[pair('Producer','[object->verb,verb->subject]',required),pair('Product','[lobject->rel,rel->robject]',required)]) :- fn_synonym('manufacturer','object','Manufacturing',Lexem,POS).
fn_synonym('manufacturer','object','Manufacturing','maker','object').
fn_synonym('manufacturer','object','Manufacturing','producer','object').
lvp(Lexem,POS,'Manufacturing',[pair('Producer','[object->verb,verb->subject]',required),pair('Product','[lobject->rel,rel->robject]',required)]) :- fn_synonym('manufacture','object','Manufacturing',Lexem,POS).
fn_synonym('manufacture','object','Manufacturing','fabrication','object').
fn_synonym('manufacture','object','Manufacturing','production','object').
fn_synonym('manufacture','object','Manufacturing','manufacturing','object').
lvp(Lexem,POS,'Earnings',[pair('Earner','[verb->subject]',required),pair('Money','[verb->object]',required),pair('Goods','[verb->pp,pp_constraint(on,pp->dep)]',optional)]) :- fn_synonym('earn','predicate','Earnings',Lexem,POS).
fn_synonym('earn','predicate','Earnings','make','predicate').
fn_synonym('earn','predicate','Earnings','net','predicate').
lvp(Lexem,POS,'Earnings',[pair('Earner','[object->verb,verb->subject]',required),pair('Money','[lobject->rel,rel->robject]',required),pair('Goods','[object->verb,verb->pp,pp_constraint(on,pp->dep)]',optional)]) :- fn_synonym('income','object','Earnings',Lexem,POS).
fn_synonym('income','object','Earnings','earnings','object').
fn_synonym('income','object','Earnings','net','object').
fn_synonym('income','object','Earnings','profit','object').
fn_synonym('income','object','Earnings','revenue','object').
lvp(Lexem,POS,'Beat_opponent',[pair('Winner','[verb->subject]',required),pair('Loser','[verb->object]',required),pair('Competition','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('beat','predicate','Beat_opponent',Lexem,POS).
fn_synonym('beat','predicate','Beat_opponent','defeat','predicate').
fn_synonym('beat','predicate','Beat_opponent','demolish','predicate').
fn_synonym('beat','predicate','Beat_opponent','prevail','predicate').
fn_synonym('beat','predicate','Beat_opponent','upend','predicate').
lvp(Lexem,POS,'Beat_opponent',[pair('Winner','[verb->subject]',required),pair('Loser','[verb->object]',required),pair('Competition','[verb->pp,pp_constraint(at,pp->dep)]',optional)]) :- fn_synonym('beat','predicate','Beat_opponent',Lexem,POS).
lvp(Lexem,POS,'Beat_opponent',[pair('Loser','[lobject->rel,rel->robject]',required),pair('Competition','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('defeat','object','Beat_opponent',Lexem,POS).
lvp(Lexem,POS,'Beat_opponent',[pair('Loser','[lobject->rel,rel->robject]',required),pair('Competition','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',optional)]) :- fn_synonym('defeat','object','Beat_opponent',Lexem,POS).
lvp(Lexem,POS,'Cure',[pair('Healer','[verb->subject]',required),pair('Patient','[verb->rel,rel->robject]',required),pair('Disease','[verb->object]',required),pair('Medication','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('cure','predicate','Cure',Lexem,POS).
fn_synonym('cure','predicate','Cure','alleviate','predicate').
fn_synonym('cure','predicate','Cure','ease','predicate').
fn_synonym('cure','predicate','Cure','heal','predicate').
fn_synonym('cure','predicate','Cure','palliate','predicate').
fn_synonym('cure','predicate','Cure','rehabilitate','predicate').
fn_synonym('cure','predicate','Cure','remedy','predicate').
fn_synonym('cure','predicate','Cure','treat','predicate').
lvp(Lexem,POS,'Cure',[pair('Healer','[verb->subject]',required),pair('Patient','[verb->object]',required),pair('Medication','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('cure','predicate','Cure',Lexem,POS).
lvp(Lexem,POS,'Travel',[pair('Traveler','[verb->subject]',required),pair('Goal','[verb->object]',required)]) :- fn_synonym('visit','predicate','Travel',Lexem,POS).
fn_synonym('visit','predicate','Travel','enter','predicate').
lvp(Lexem,POS,'Travel',[pair('Traveler','[verb->subject]',required),pair('Goal','[verb->pp,pp_constraint(in,pp->dep)]',required)]) :- fn_synonym('arrive','predicate','Travel',Lexem,POS).
lvp(Lexem,POS,'Travel',[pair('Traveler','[verb->subject]',required),pair('Goal','[verb->pp,pp_constraint(at,pp->dep)]',required)]) :- fn_synonym('arrive','predicate','Travel',Lexem,POS).
lvp(Lexem,POS,'Travel',[pair('Traveler','[verb->subject]',required),pair('Goal','[verb->pp,pp_constraint(to,pp->dep)]',required)]) :- fn_synonym('come','predicate','Travel',Lexem,POS).
fn_synonym('come','predicate','Travel','return','predicate').
fn_synonym('come','predicate','Travel','approach','predicate').
lvp(Lexem,POS,'Travel',[pair('Traveler','[lobject->rel,rel->robject]',required),pair('Goal','[subject->verb,verb->pp,pp_constraint(in,pp->dep)]',required)]) :- fn_synonym('arrival','object','Travel',Lexem,POS).
lvp(Lexem,POS,'Travel',[pair('Traveler','[lobject->rel,rel->robject]',required),pair('Goal','[subject->verb,verb->pp,pp_constraint(at,pp->dep)]',required)]) :- fn_synonym('arrival','object','Travel',Lexem,POS).
lvp(Lexem,POS,'Travel',[pair('Traveler','[lobject->rel,rel->robject]',required),pair('Goal','[subject->verb,verb->pp,pp_constraint(to,pp->dep)]',required)]) :- fn_synonym('visit','object','Travel',Lexem,POS).
fn_synonym('visit','object','Travel','approach','object').
fn_synonym('visit','object','Travel','return','object').
fn_synonym('visit','object','Travel','entry','object').
lvp(Lexem,POS,'Travel',[pair('Traveler','[verb->subject]',required),pair('Source','[verb->object]',required)]) :- fn_synonym('leave','predicate','Travel',Lexem,POS).
fn_synonym('leave','predicate','Travel','exit','predicate').
fn_synonym('leave','predicate','Travel','escape','predicate').
lvp(Lexem,POS,'Travel',[pair('Traveler','[verb->subject]',required),pair('Source','[verb->pp,pp_constraint(from,pp->dep)]',required)]) :- fn_synonym('leave','predicate','Travel',Lexem,POS).
fn_synonym('leave','predicate','Travel','depart','predicate').
fn_synonym('leave','predicate','Travel','vanish','predicate').
lvp(Lexem,POS,'Contacting',[pair('Communicator','[verb->subject]',required),pair('Addressee','[verb->object]',required)]) :- fn_synonym('call','predicate','Contacting',Lexem,POS).
fn_synonym('call','predicate','Contacting','contact','predicate').
fn_synonym('call','predicate','Contacting','e-mail','predicate').
fn_synonym('call','predicate','Contacting','call-in','predicate').
fn_synonym('call','predicate','Contacting','call-up','predicate').
fn_synonym('call','predicate','Contacting','fax','predicate').
fn_synonym('call','predicate','Contacting','mail','predicate').
fn_synonym('call','predicate','Contacting','phone','predicate').
fn_synonym('call','predicate','Contacting','reach','predicate').
fn_synonym('call','predicate','Contacting','ring','predicate').
fn_synonym('call','predicate','Contacting','telephone','predicate').
fn_synonym('call','predicate','Contacting','cable','predicate').
lvp(Lexem,POS,'Contacting',[pair('Communicator','[verb->subject]',required),pair('Addressee','[verb->pp,pp_constraint(to,pp->dep)]',required)]) :- fn_synonym('write','predicate','Contacting',Lexem,POS).
lvp(Lexem,POS,'Contacting',[pair('Communicator','[object->verb,verb->subject]',required),pair('Addressee','[object->verb,verb->pp,pp_constraint(to,pp->dep)]',required)]) :- fn_synonym('call','object','Contacting',Lexem,POS).
lvp(Lexem,POS,'Contacting',[pair('Communicator','[object->verb,verb->subject]',required),pair('Addressee','[object->verb,verb->pp,pp_constraint(with,pp->dep)]',required)]) :- fn_synonym('contact','object','Contacting',Lexem,POS).
lvp(Lexem,POS,'Protest',[pair('Protester','[verb->subject]',required),pair('Side','[verb->pp,pp_constraint(against,pp->dep)]',required)]) :- fn_synonym('protest','predicate','Protest',Lexem,POS).
fn_synonym('protest','predicate','Protest','demonstrate','predicate').
lvp(Lexem,POS,'Protest',[pair('Protester','[object->verb,verb->subject]',required),pair('Side','[object->verb,verb->pp,pp_constraint(against,pp->dep)]',required)]) :- fn_synonym('protest','object','Protest',Lexem,POS).
fn_synonym('protest','object','Protest','demonstration','object').
lvp(Lexem,POS,'Submitting_documents',[pair('Submittor','[verb->subject]',required),pair('Documents','[verb->object]',required),pair('Authority','[verb->pp,pp_constraint(to,pp->dep)]',optional)]) :- fn_synonym('submit','predicate','Submitting_documents',Lexem,POS).
fn_synonym('submit','predicate','Submitting_documents','file','predicate').
fn_synonym('submit','predicate','Submitting_documents','turn-in','predicate').
fn_synonym('submit','predicate','Submitting_documents','hand-in','predicate').
lvp(Lexem,POS,'Performing',[pair('Performer','[verb->subject]',required),pair('Role','[verb->object]',optional),pair('Performance','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Costar','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('play','predicate','Performing',Lexem,POS).
fn_synonym('play','predicate','Performing','star','predicate').
fn_synonym('play','predicate','Performing','feature','predicate').
lvp(Lexem,POS,'Performing',[pair('Performer','[verb->subject]',required),pair('Role','[verb->rel,rel->robject]',optional),pair('Performance','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Costar','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('play','predicate','Performing',Lexem,POS).
lvp(Lexem,POS,'Performing',[pair('Performer','[verb->subject]',required),pair('Role','[verb->pp,pp_constraint(as,pp->dep)]',optional),pair('Performance','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Costar','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('act','predicate','Performing',Lexem,POS).
fn_synonym('act','predicate','Performing','appear','predicate').
lvp(Lexem,POS,'Performing',[pair('Performer','[verb->subject]',required),pair('Performance','[verb->pp,pp_constraint(in,pp->dep)]',optional),pair('Costar','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('co-star','predicate','Performing',Lexem,POS).
lvp(Lexem,POS,'Releasing_from_custody',[pair('Authorities','[verb->subject]',required),pair('Suspect','[verb->object]',required),pair('Place','[verb->pp,pp_constraint(from,pp->dep)]',optional)]) :- fn_synonym('release','predicate','Releasing_from_custody',Lexem,POS).
fn_synonym('release','predicate','Releasing_from_custody','set-free','predicate').
fn_synonym('release','predicate','Releasing_from_custody','let-go','predicate').
lvp(Lexem,POS,'Releasing_from_custody',[pair('Suspect','[adj->verb,verb->subject]',required),pair('Place','[adj->verb,verb->pp,pp_constraint(from,pp->dep)]',required)]) :- fn_synonym('released','property','Releasing_from_custody',Lexem,POS).
lvp(Lexem,POS,'Sign_agreement',[pair('Signatory','[verb->subject]',required),pair('Agreement','[verb->object]',required)]) :- fn_synonym('sign','predicate','Sign_agreement',Lexem,POS).
lvp(Lexem,POS,'Sign_agreement',[pair('Signatory','[verb->subject]',required),pair('Agreement','[verb->pp,pp_constraint(to,pp->dep)]',required)]) :- fn_synonym('accede','predicate','Sign_agreement',Lexem,POS).
lvp(Lexem,POS,'Publishing',[pair('Author','[verb->subject]',required),pair('Work','[verb->object]',required),pair('Publisher','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('publish','predicate','Publishing',Lexem,POS).
fn_synonym('publish','predicate','Publishing','release','predicate').
lvp(Lexem,POS,'Publishing',[pair('Author','[verb->subject]',required),pair('Work','[verb->object]',required),pair('Publisher','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('publish','predicate','Publishing',Lexem,POS).
lvp(Lexem,POS,'Publishing',[pair('Author','[verb->rel,rel->robject]',required),pair('Work','[verb->object]',required),pair('Publisher','[verb->subject]',required)]) :- fn_synonym('publish','predicate','Publishing',Lexem,POS).
lvp(Lexem,POS,'Losing',[pair('Owner','[verb->subject]',required),pair('Money','[verb->object]',required),pair('Goods','[verb->pp,pp_constraint(on,pp->dep)]',optional)]) :- fn_synonym('lose','predicate','Losing',Lexem,POS).
lvp(Lexem,POS,'Losing',[pair('Owner','[object->verb,verb->subject]',required),pair('Money','[lobject->rel,rel->robject]',required),pair('Goods','[object->verb,verb->pp,pp_constraint(on,pp->dep)]',optional)]) :- fn_synonym('loss','object','Losing',Lexem,POS).
lvp(Lexem,POS,'Losing',[pair('Owner','[verb->subject]',required),pair('Goods','[verb->object]',required)]) :- fn_synonym('lose','predicate','Losing',Lexem,POS).
lvp(Lexem,POS,'Surrendering',[pair('Authorities','[verb->pp,pp_constraint(to,pp->dep)]',required),pair('Fugitive','[verb->subject]',required),pair('Charges','[verb->pp,pp_constraint(on,pp->rel),rel->robject]',optional)]) :- fn_synonym('surrender','predicate','Surrendering',Lexem,POS).
fn_synonym('surrender','predicate','Surrendering','give-up','predicate').
lvp(Lexem,POS,'Text_creation',[pair('Author','[verb->subject]',required),pair('Text','[verb->object]',required)]) :- fn_synonym('write','predicate','Text_creation',Lexem,POS).
fn_synonym('write','predicate','Text_creation','author','predicate').
fn_synonym('write','predicate','Text_creation','compose','predicate').
fn_synonym('write','predicate','Text_creation','draft','predicate').
fn_synonym('write','predicate','Text_creation','jot-down','predicate').
fn_synonym('write','predicate','Text_creation','pen','predicate').
fn_synonym('write','predicate','Text_creation','type','predicate').
fn_synonym('write','predicate','Text_creation','type-in','predicate').
fn_synonym('write','predicate','Text_creation','write-up','predicate').
lvp(Lexem,POS,'Theft',[pair('Perpetrator','[verb->subject]',required),pair('Goods','[verb->object]',required),pair('Victim','[verb->pp,pp_constraint(from,pp->dep)]',optional)]) :- fn_synonym('steal','predicate','Theft',Lexem,POS).
fn_synonym('steal','predicate','Theft','thieve','predicate').
fn_synonym('steal','predicate','Theft','swipe','predicate').
fn_synonym('steal','predicate','Theft','snitch','predicate').
fn_synonym('steal','predicate','Theft','snatch','predicate').
fn_synonym('steal','predicate','Theft','shoplift','predicate').
fn_synonym('steal','predicate','Theft','pilfer','predicate').
fn_synonym('steal','predicate','Theft','pickpocket','predicate').
fn_synonym('steal','predicate','Theft','filch','predicate').
fn_synonym('steal','predicate','Theft','embezzle','predicate').
lvp(Lexem,POS,'Undressing',[pair('Wearer','[verb->subject]',required),pair('Clothing','[verb->object]',required)]) :- fn_synonym('take-off','predicate','Undressing',Lexem,POS).
fn_synonym('take-off','predicate','Undressing','kick-off','predicate').
fn_synonym('take-off','predicate','Undressing','remove','predicate').
fn_synonym('take-off','predicate','Undressing','slip','predicate').
fn_synonym('take-off','predicate','Undressing','strip-off','predicate').
fn_synonym('take-off','predicate','Undressing','pull-off','predicate').
fn_synonym('take-off','predicate','Undressing','throw-off','predicate').
fn_synonym('take-off','predicate','Undressing','peel-off','predicate').
lvp(Lexem,POS,'Growing_food',[pair('Grower','[verb->subject]',required),pair('Food','[verb->object]',required)]) :- fn_synonym('grow','predicate','Growing_food',Lexem,POS).
fn_synonym('grow','predicate','Growing_food','raise','predicate').
lvp(Lexem,POS,'Participation',[pair('Participant','[verb->subject]',required),pair('Event','[verb->pp,pp_constraint(in,pp->dep)]',required),pair('CoParticipant','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('participate','predicate','Participation',Lexem,POS).
fn_synonym('participate','predicate','Participation','engage','predicate').
fn_synonym('participate','predicate','Participation','involve','predicate').
fn_synonym('participate','predicate','Participation','take-part','predicate').
lvp(Lexem,POS,'Participation',[pair('Participant','[adj->verb,verb->subject]',required),pair('Event','[adj->verb,verb->pp,pp_constraint(in,pp->dep)]',required),pair('CoParticipant','[adj->verb,verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('involved','property','Participation',Lexem,POS).
fn_synonym('involved','property','Participation','engaged','property').
lvp(Lexem,POS,'Borrow_lend',[pair('Borrower','[verb->subject]',required),pair('Theme','[verb->object]',required),pair('Lender','[verb->pp,pp_constraint(from,pp->dep)]',optional)]) :- fn_synonym('borrow','predicate','Borrow_lend',Lexem,POS).
lvp(Lexem,POS,'Borrow_lend',[pair('Borrower','[verb->pp,pp_constraint(to,pp->dep)]',optional),pair('Theme','[verb->object]',required),pair('Lender','[verb->subject]',required)]) :- fn_synonym('lend','predicate','Borrow_lend',Lexem,POS).
lvp(Lexem,POS,'Restaurant',[pair('Restaurant_name','[object->verb,verb->subject]',required),pair('Price','[pobject->adj]',optional),pair('Cuisine','[pobject->adj]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('restaurant','object','Restaurant',Lexem,POS).
fn_synonym('restaurant','object','Restaurant','eating-place','object').
fn_synonym('restaurant','object','Restaurant','food-place','object').
fn_synonym('restaurant','object','Restaurant','dining-place','object').
lvp(Lexem,POS,'Cure',[pair('Healer','[verb->subject]',required),pair('Disease','[verb->object]',required),pair('Medication','[verb->pp,pp_constraint(with,pp->dep)]',optional)]) :- fn_synonym('cure','predicate','Cure',Lexem,POS).
lvp(Lexem,POS,'Publishing',[pair('Author','[verb->subject]',required),pair('Work','[verb->object]',required),pair('Venue','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('publish','predicate','Publishing',Lexem,POS).
lvp(Lexem,POS,'Publishing',[pair('Author','[verb->subject]',required),pair('Work','[verb->object]',required),pair('Topic','[verb->pp,pp_constraint(on,pp->dep)]',optional)]) :- fn_synonym('publish','predicate','Publishing',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[verb->object]',required),pair('Employer','[verb->subject]',required),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('employ','predicate','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Import_export',[pair('Importer','[verb->subject]',optional),pair('Exporter','[verb->pp,pp_constraint(from,pp->dep)]',optional),pair('Goods','[verb->object]',required)]) :- fn_synonym('import','predicate','Import_export',Lexem,POS).
lvp(Lexem,POS,'Import_export',[pair('Importer','[verb->pp,pp_constraint(to,pp->dep)]',optional),pair('Exporter','[verb->subject]',optional),pair('Goods','[verb->object]',required)]) :- fn_synonym('export','predicate','Import_export',Lexem,POS).
lvp(Lexem,POS,'Scheduling',[pair('Agent','[verb->subject]',required),pair('Event','[verb->object]',required),pair('Place','[verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('schedule','predicate','Scheduling',Lexem,POS).
fn_synonym('schedule','predicate','Scheduling','arrange','predicate').
fn_synonym('schedule','predicate','Scheduling','set-up','predicate').
lvp(Lexem,POS,'Scheduling',[pair('Agent','[verb->subject]',required),pair('Event','[verb->object]',required),pair('Place','[verb->pp,pp_constraint(at,pp->dep)]',optional)]) :- fn_synonym('schedule','predicate','Scheduling',Lexem,POS).
lvp(Lexem,POS,'Scheduling',[pair('Agent','[verb->subject]',required),pair('Event','[verb->object]',required),pair('Place','[verb->pp,pp_constraint(to,pp->dep)]',optional)]) :- fn_synonym('schedule','predicate','Scheduling',Lexem,POS).
lvp(Lexem,POS,'Human_gender',[pair('Person','[object->verb,verb->subject]',required),pair('Gender','[self]',required)]) :- fn_synonym('gender','object','Human_gender',Lexem,POS).
lvp(Lexem,POS,'Age',[pair('Person','[lobject->rel,rel->robject]',required),pair('Age','[subject->verb,verb->object]',required)]) :- fn_synonym('age','object','Age',Lexem,POS).
lvp(Lexem,POS,'People_by_origin',[pair('Person','[object->verb,verb->subject]',required),pair('Origin','[self]',required)]) :- fn_synonym('ancestry','object','People_by_origin',Lexem,POS).
lvp(Lexem,POS,'Being_employed',[pair('Employee','[object->verb,verb->subject]',required),pair('Position','[self]',required),pair('Employer','[object->verb,verb->pp,pp_constraint(at,pp->dep)]',optional),pair('Place','[object->verb,verb->pp,pp_constraint(in,pp->dep)]',optional)]) :- fn_synonym('job','object','Being_employed',Lexem,POS).
lvp(Lexem,POS,'Movie',[pair('Film','[adj->verb,verb->subject]',required),pair('Release Year','[adj->verb,verb->pp,pp_constraint(in,pp->dep)]',required)]) :- fn_synonym('released','property','Movie',Lexem,POS).
