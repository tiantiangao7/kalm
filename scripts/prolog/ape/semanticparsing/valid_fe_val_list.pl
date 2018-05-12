is_valid_strict_fe_val('Age',X) :- integer(X).
is_valid_strict_fe_val('Time',X) :- integer(X).
is_valid_strict_fe_val('Time',when).
is_valid_strict_fe_val('Gender','male').
is_valid_strict_fe_val('Gender','female').
is_valid_strict_fe_val('Religion','christian').
is_valid_strict_fe_val('Religion','buddhist').
is_valid_strict_fe_val('Religion','baptist').
is_valid_strict_fe_val('Religion','muslim').
is_valid_strict_fe_val('Religion','zealot').
is_valid_strict_fe_val('Religion','mormon').
is_valid_strict_fe_val('Religion','jew').
is_valid_strict_fe_val('Money','dollar').
is_valid_strict_fe_val('Price','cheap').
is_valid_strict_fe_val('Price','expensive').

strict_filtered_fe_name('Age').
strict_filtered_fe_name('Gender').
strict_filtered_fe_name('Religion').
strict_filtered_fe_name('Time').
strict_filtered_fe_name('Money').
strict_filtered_fe_name('Price').

is_valid_fe_val(_,what) :- !.
is_valid_fe_val('Money','money') :- !.
is_valid_fe_val('Gender','gender') :- !.
is_valid_fe_val('Religion','religion') :- !.
is_valid_fe_val('Age','age') :- !.
is_valid_fe_val('Time','time') :- !.

is_valid_fe_val(X,Y) :- (strict_filtered_fe_name(X)
                         ->
                         is_valid_strict_fe_val(X,Y)
                         ;
                         not is_valid_strict_fe_val(_,Y)
                        ).
