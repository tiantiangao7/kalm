1 Roberta is a person. Thelma is a person. Robin is a person. Pete is a person.
  [note] isa relation. 
         person belongs to People frame. the LU itself is defined as the person FE.

2 Roberta is a female. Thelma is a female.
  [note] in framenet, male and female are defined in Body_description_holistic frame, which is unrelated to gender.

3 Robin is male. Pete is male.
  [note] same as above

4 Exclude that a person is male and that the person is female.
  [note] same as above

5 If there is a job then exactly one person holds that job.
  [note] job is a LU in being_employed frame. but, it does not serve as a FE in the examples. 
         here, job will be replaced by an atom bind to rule body.

6 If there is a person then the person holds exactly two jobs.
  [note] same as above. 

7 Chef is a job. Guard is a job. Nurse is a job. Operator is a job. Police is a job. Teacher is a
job. Actor is a job. Boxer is a job.
  [note] isa relation

8 If a person holds a job as a nurse then that person is a male.
  [note] see encoding in frames.pl. here, person will be instantiated with an instance.

9. If a person holds a job as an actor then that person is a male.

10 If a first person holds a job as a chef and a second person holds a job as a telephone operator 
   then the second person is a husband of the first person.
   [note] see encoding in frames.pl 

11 If a first person is a husband of a second person then the first person is male.
12 If a first person is a husband of a second person then the second person is female.
13 Exclude that Roberta holds a job as boxer.

14 Exclude that Pete is educated.
   [note] being educated is not included in framenet.

15 If a person holds a job as nurse then the person is educated.
16 If a person holds a job as a police officer then that person is educated.
17 If a person holds a job as a teacher then the person is educated.
18 Exclude that Roberta holds a job as a chef.
19 Exclude that Roberta holds a job as a police officer.
20 Exclude that a person holds a job as a chef and that the same person holds a job as a police
officer.
