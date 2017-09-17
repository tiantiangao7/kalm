% being_imployed frame
frame(employed-as,'be+a+pp','being_employed',
      [role(employee,subject,person),role(position,object,profession)]).
frame(work-as,'v+pp','being_employed',
      [role(employee,subject,person),role(position,object,profession)]).
frame(hold-job,'lv+n','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).      
frame(own-job,'lv+n','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).      
frame(has-job,'lv+n','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).
frame(hold-work,'lv+n','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).      
frame(own-work,'lv+n','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).      
frame(has-work,'lv+n','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).
frame(job,'be+n','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).
frame(job,'n+be','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).	  
frame(employment,'be+p+n','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).
frame(stint,'be+n','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).
frame(stint,'n+be','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).	  	  
frame(work,'be+n','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).
frame(work,'n+be','being_employed',
	  [role(employee,subject,person),role(position,object,profession)]).	  
	  
% personal_relationship
frame(husband,'be+n','personal_relationship',
	  [role(entity1,subject,person),role(entity2,object,person)]).
frame(husband,'n+be','personal_relationship',
	  [role(entity1,subject,person),role(entity2,object,person)]).
frame(couple,'has_part+be+n','personal_relationship',
	  [role(entity1,subject,person),role(entity2,object,person)]).
frame(married,'has_part+be+a','personal_relationship',
	  [role(entity1,subject,person),role(entity2,object,person)]).	  
frame(married-to,'be+a+pp','personal_relationship',
	  [role(entity1,subject,person),role(entity2,object,person)]).	
frame(marriage,'has_part+be+pp+n','personal_relationship',
	  [role(entity1,subject,person),role(entity2,object,person)]).	    

% color
frame(color,'be+n','color',
          [role(entity,subject,object),role(color,object,color)]).
frame(color,'n+be','color',
          [role(entity,subject,object),role(color,object,color)]).
frame(color,'lv+n','color',
          [role(entity,subject,object),role(color,object,color)]).
frame(red,a,'color',
          [role(entity,subject,object),role(color,object,color)]).

% ingestion
frame(drink,v,ingestion,[role(ingestor,subject,person),role(ingestibles,object,object)]).
frame(have,v,ingestion,[role(ingestor,subject,person),role(ingestibles,object,object)]).

% ingest_substance
frame(smoke,v,ingestion,[role(ingestor,subject,person),role(ingestibles,object,object)]).

% residence
frame(live-in,'v+pp',residence,[role(resident,subject,person),role(location,object,location)]).
frame(dwell-in,'v+pp',residence,[role(resident,subject,person),role(location,object,location)]).
frame(lodge-in,'v+pp',residence,[role(resident,subject,person),role(location,object,location)]).
frame(reside-in,'v+pp',residence,[role(resident,subject,person),role(location,object,location)]).
frame(camp-in,'v+pp',residence,[role(resident,subject,person),role(location,object,location)]).
frame(stay-in,'v+pp',residence,[role(resident,subject,person),role(location,object,location)]).
frame(inhabitant,'n+of+n',residence,[role(resident,subject,person),role(location,object,location)]).
frame(inhabitant,'be+n',residence,[role(resident,subject,person),role(location,object,location)]).
frame(dweller,'n+of+n',residence,[role(resident,subject,person),role(location,object,location)]).
frame(dweller,'n+of+n',residence,[role(resident,subject,person),role(location,object,location)]).
frame(occupant,'be+n',residence,[role(resident,subject,person),role(location,object,location)]).
frame(occupant,'be+n',residence,[role(resident,subject,person),role(location,object,location)]).
frame(camped,'be+a+pp',residence,[role(resident,subject,person),role(location,object,location)]).

% adjacency
frame(next-to,'be+a+pp',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(neighboring,'be+a+pp',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(neighbor,'be+n',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(neighbor,'n+be',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(neighbor,v,adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(neighbor,'has_part+v',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(adjacency,'be+pp+n',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(contiguity,'be+pp+n',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(adjacent,'be+a+pp',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(contiguous,'be+a+pp',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(border,v,adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(border,'has_part+v',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(adjoin,v,adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(adjoin,'has_part+v',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(abut,v,adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
frame(abut,'has_part+v',adjacency,[role(figure1,subject,person),role(figure2,object,person)]).
