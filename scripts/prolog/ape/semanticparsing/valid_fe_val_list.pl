is_valid_fe_val_ele('Age',X) :- integer(X).
is_valid_fe_val_ele('Time',X) :- integer(X).
is_valid_fe_val_ele('Gender','male').
is_valid_fe_val_ele('Gender','female').
is_valid_fe_val_ele('Religion','christian').
is_valid_fe_val_ele('Religion','buddhist').
is_valid_fe_val_ele('Religion','baptist').
is_valid_fe_val_ele('Religion','muslim').
is_valid_fe_val_ele('Religion','zealot').
is_valid_fe_val_ele('Religion','mormon').
is_valid_fe_val_ele('Religion','jew').

filtered_fe_name('Age').
filtered_fe_name('Gender').
filtered_fe_name('Religion').
filtered_fe_name('Time').

is_valid_fe_val(X,Y) :- filtered_fe_name(X),
                        !,
                        is_valid_fe_val_ele(X,Y).
is_valid_fe_val(X,Y) :- not filtered_fe_name(X),
                        not is_valid_fe_val_ele(_,Y).
